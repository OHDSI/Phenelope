# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of Phenelope
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

.createRecommendListViaLlmFromConceptList <- function(query,
                                                      closestConditionConcept,
                                                      conceptList,
                                                      llmClient,
                                                      connection,
                                                      connectionDetails,
                                                      cdmDatabaseSchema,
                                                      type = "phoebe",
                                                      minCount = 500,
                                                      previousResults,
                                                      excludedConditions = "none",
                                                      belowMinimumCountApproach = "TEST ALL",
                                                      clinicalDefinition = "",
                                                      clinicalContext = "",
                                                      excludedVocabularies = c("ICDO3"),
                                                      bucketSize) {
  if (type == "phoebe") {
    text <- "PHOEBE"
  } else {
    text <- "included concepts"
  }

  conceptList <- Filter(is.numeric, conceptList) # ensure no garbage was included (rare)
  message("For concept(s): ", paste(conceptList, collapse = ", "))
  message("Getting GenAI similarity response for ", text, " for query: ", query)

  message("--Getting concept set expression")

  if (is.null(excludedVocabularies)) {
    excludedVocabularies <- c("")
  }
  sqlFilename <- "FullConcepts.sql"
  conceptList <- conceptList[!is.na(conceptList)]
  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = sqlFilename,
    packageName = "Phenelope",
    dbms = connectionDetails$dbms,
    cdm_database_schema = cdmDatabaseSchema,
    concept_list = paste(conceptList, collapse = ", "),
    excludedVocabularies = paste(sprintf("'%s'", excludedVocabularies), collapse = ", ")
  )

  conceptList <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  if (nrow(conceptList) != 0) {
    conceptList$phoebe <- F
  }

  message("--Finding ", type, " results for concept set")
  if (type == "phoebe") {
    recs <- .getPhoebeData(c(conceptList$conceptId))
    recsFinal <- recs

    if (nrow(recsFinal) != 0) { # check if phoebe had any recommendations
      recsFinal <- recsFinal[!(recsFinal$conceptId %in% c(conceptList$conceptId)), ]
      sqlFilename <- "checkDomainsForConcepts.sql"
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = sqlFilename,
        packageName = "Phenelope",
        dbms = connectionDetails$dbms,
        cdm_database_schema = cdmDatabaseSchema,
        concept_list = paste(recsFinal$conceptId, collapse = ", "),
        excludedVocabularies = paste(sprintf("'%s'", excludedVocabularies), collapse = ", ")
      )

      concepts <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
      concepts$phoebe <- T

      # Substrings to exclude
      excludeWords <- c(
        "finding$",
        "^Disorder of",
        "^Finding of",
        "^Disease of",
        "Injury of",
        "by site$",
        "by body site$",
        "by mechanism$",
        "of body region$",
        "of anatomical site$",
        "of specific body structure$"
      )

      exceptionWords <- c("due to", "caused by")

      # Filter rows
      concepts <- concepts[!(
        sapply(concepts$conceptSetTarget, function(x) any(grepl(paste(excludeWords, collapse = "|"), x))) &
          !sapply(concepts$conceptSetTarget, function(x) any(grepl(paste(exceptionWords, collapse = "|"), x)))
      ), ]

      concepts <- rbind(concepts, conceptList)
    } else { # no phoebe recommendations
      concepts <- conceptList
    }
  } else { # else test against included concepts
    recs <- .getPhoebeData(c(conceptList$conceptId)) # get phoebe data on this pass solely for the record counts

    concepts <- conceptList
    recsFinal <- conceptList
  }

  if (nrow(recs) != 0) { # no phoebe recs
    concepts <- merge(concepts, unique(recs[, c("conceptId", "conceptSetTarget", "recordCount")]), all.x = T)
  } else { # no phoebe recs
    concepts$recordCount <- NA
  }

  concepts <- concepts |>
    mutate(recordCount = if_else(is.na(.data$recordCount), 0, .data$recordCount)) |>
    mutate(aboveMin = .data$recordCount >= minCount) |>
    arrange(desc(.data$phoebe), desc(.data$aboveMin))

  message("\n--Current number of concepts: ", nrow(concepts))

  previousRun <- data.frame()
  if (!is.null(previousResults)) {
    previousRun <- previousResults |>
      filter(.data$conceptId %in% concepts$conceptId)
    concepts <- concepts |>
      filter(!.data$conceptId %in% previousResults$conceptId)
    message("--skipping previously analyzed concepts yields: ", nrow(concepts))
  }

  conceptsToUse <- concepts
  if (type != "phoebe") { # add in the main concept on the second pass through
    temp <- conceptsToUse[1, ]
    temp$conceptId <- closestConditionConcept
    temp$conceptSetTarget <- query
    conceptsToUse <- rbind(conceptsToUse, temp)
  }

  results <- data.frame()

  # read in the basic prompt

  promptUp <- system.file("prompts", "LLM_Prompt_for_PHOEBE.txt", package = "Phenelope")
  originalLines <- readLines(promptUp)

  #test which in the concept list need to be tested
  testList <- NULL
  noTestList <- NULL

  for(conceptUp in seq_len(nrow(concepts))) {
    if(belowMinimumCountApproach == "TEST ALL" |
       concepts$aboveMin[[conceptUp]] == T |
       (belowMinimumCountApproach == "TEST PHOEBE" & concepts$phoebe[[conceptUp]] == T) |
       (belowMinimumCountApproach == "EXCLUDE ALL" & concepts$aboveMin[[conceptUp]] == T) |
       (belowMinimumCountApproach == "INCLUDE ALL" & concepts$aboveMin[[conceptUp]] == T)) {
      testList <- rbind(testList, concepts[conceptUp,])
    } else { #add concept to listed of untested as per min count and disposition
      resultsDf <- NULL
      resultsDf$conceptId <- concepts$conceptId[[conceptUp]]
      resultsDf$suggestedCondition <- concepts$conceptSetTarget[[conceptUp]]
      resultsDf$mainCondition <- baseCondition
      resultsDf$excludedConditions <-  ""
      resultsDf$proposedInExcluded <-  ""
      resultsDf$finalAnswer <-  ""
      resultsDf$rationaleForAnswer <-  ""
      resultsDf$confidenceLevel <-  ""
      resultsDf$tested <- F

      if (belowMinimumCountApproach == "EXCLUDE ALL") {
        resultsDf$finalAnswer <- "NO"
        resultsDf$rationaleForAnswer <- "Untested"
      }

      if (belowMinimumCountApproach == "INCLUDE ALL") {
        resultsDf$finalAnswer <- "YES"
        resultsDf$rationaleForAnswer <- "Untested"
      }

      if (belowMinimumCountApproach == "TEST PHOEBE" & concepts$phoebe[[conceptUp]] == F & concepts$aboveMin[[conceptUp]] == F) {
        resultsDf$finalAnswer <- "YES"
        resultsDf$rationaleForAnswer <- "Untested"
      }
      columnsToFront <- c("suggestedCondition", "conceptId", "mainCondition", "finalAnswer", "rationaleForAnswer", "confidenceLevel")
      # Rearrange the DataFrame
      resultsDf <- resultsDf |>
        select(all_of(columnsToFront), everything())
      noTestList <- rbind(noTestList, resultsDf)

    }
  }

  cost <- 0

  if (!is.null(testList)) {
    concepts <- testList
  }

  concepts$conceptSetTarget <- gsub("[\\[\\]]", " ", concepts$conceptSetTarget) #remove any [ or ] from name (interferes with json structure)
  if (nrow(concepts) != 0) {
    startPoint <- 1
    endPoint <- min(bucketSize, nrow(concepts))
    while(startPoint <= nrow(concepts)) {
      cat(paste0("--Querying LLM - Analyzing concepts ", startPoint, " through ", endPoint, " of ", nrow(concepts), "\r"))
      concepts$aboveMin[1] <- T # always test the first concept

      # for (conceptUp in 1:nrow(concepts)) {
      #   if (conceptUp > 1) {
      #     cat(paste0("\r--Querying LLM - Analyzing ", conceptUp, " of ", nrow(concepts)))
      #   }

      testCondition <- concepts[startPoint:endPoint, c("conceptId", "conceptSetTarget")]
      # testConceptId <- concepts$conceptId[[conceptUp]]
      baseCondition <- query

      updatedLines <- gsub("MAIN_CONDITION", baseCondition, originalLines)
      json_all <- jsonlite::toJSON(testCondition)
      updatedLines <- gsub("SUGGESTED_CONDITION", json_all, updatedLines)
      updatedLines <- gsub("EXCLUDED_CONDITIONS", excludedConditions, updatedLines)
      updatedLines <- gsub("CLINICAL_CONTEXT", clinicalContext, updatedLines)
      updatedLines <- gsub("ADDITIONAL_INFORMATION", clinicalDefinition, updatedLines)

      prompt <- paste(updatedLines, collapse = "\n")

      retryLimit <- 10 # Maximum number of retries
      attempt <- 0 # Initial attempt counter
      success <- FALSE # Flag to indicate success

      while (attempt <= retryLimit && !success) { # llm will mislabel column headers occasionally - usually fixed with a re-try
        tryCatch(
          {
            attempt <- attempt + 1 # Increment the attempt count

            if(attempt > 1) {
              writeLines(prompt, "e:/shared/llm/joel/pe/prompt.txt")
            }

            systemPrompt <- "You are an expert medical doctor specializing in healthcare data analysis. Your primary function is to analyze healthcare data, including electronic health records, to infer causal relationships between exposures and health outcomes."

            llmClient$set_system_prompt(systemPrompt)
            llmClient$set_turns(list()) # Reset the chat
            text <- llmClient$chat_structured(prompt,
                                              echo = "none",
                                              type = ellmer::type_array(ellmer::type_object(
                                                conceptId = ellmer::type_string(),
                                                suggestedCondition = ellmer::type_string(),
                                                excludedConditions = ellmer::type_string(),
                                                proposedInExcluded = ellmer::type_string(),
                                                finalAnswer = ellmer::type_string(),
                                                rationaleForAnswer = ellmer::type_string(),
                                                confidenceLevel = ellmer::type_string()
                                              ))
            )

            if (is.character(text)) {
              if (jsonlite::validate(text)) {
                text <- jsonlite::fromJSON(text)
              }
            }

            resultsDf <- data.frame(text)
            resultsDf$tested <- T

            # resultsDf$suggestedCondition <- testCondition
            # resultsDf$conceptId <- testConceptId
            resultsDf$mainCondition <- baseCondition
            # resultsDf$exclusions <- excludedConditions
            resultsDf$cost <- sprintf("%.5f", llmClient$get_cost())

            columnsToFront <- c("suggestedCondition", "conceptId", "mainCondition", "finalAnswer", "rationaleForAnswer", "confidenceLevel")

            ### exclude phoebe recommended concepts with low confidence
            # if (as.numeric(sub("%", "", resultsDf$confidenceLevel)) < 50 & concepts$phoebe[[conceptUp]] == T &
            #     resultsDf$finalAnswer == "YES") {
            #   resultsDf$finalAnswer <- "NO"
            #   resultsDf$rationaleForAnswer <- paste0("Set to NO: ", resultsDf$rationaleForAnswer)
            # }

            # Rearrange the DataFrame
            resultsDf <- resultsDf |>
              select(all_of(columnsToFront), everything())

            results <- rbind(results, resultsDf)

            success <- TRUE
            cost <- cost + llmClient$get_cost()
          },
          error = function(e) {
            # Handle the error: print a message and increment the attempt counter
            message(paste("Attempt", attempt, "failed:", e$message))
            message(paste0("Failure on: ***", testCondition, "***"))
            if (grepl("abort", e$message, ignore.case = TRUE)) {
              cat("Stopping the run as requested.\n")
              stop("Execution stopped by user.")
            }
            if (attempt >= retryLimit) {
              message(paste("Reached attempt limit."))
              userInput <- readline(prompt = "Do you want to continue with the next concept? y/n ")
              if (userInput == tolower("n")) {
                cat("Stopping the run as requested.\n")
                stop("Execution stopped by user.")
              }
            }
            return(NULL) # Return NULL in case of error
          }
        )
      }

      if (!success) {
        message("All attempts failed for test condition ", testCondition, ". Skipping to next test condition.")
        return(NULL) # Skip to the next iteration of the outer loop
      }

      startPoint <- endPoint + 1
      endPoint <- min(endPoint + bucketSize, nrow(concepts))
    }

    message("\n--Total cost was $", sprintf("%.3f", cost))

    fullResults <- results
    if(length(noTestList)) { #add in the untested if any
      fullResults <- rbind(fullResults, noTestList)
    }

    if (type == "phoebe") { # only join this for phoebe results
      if (nrow(recsFinal) != 0) {
        fullResults <- fullResults |>
          mutate(conceptId = as.integer(.data$conceptId)) |>
          left_join(unique(recsFinal[, c("conceptId", "recordCount")]), by = c("conceptId" = "conceptId"))
      } else { # no phoebe recs
        fullResults <- fullResults |>
          mutate(conceptId = as.integer(.data$conceptId))
      }
    }

    if (nrow(previousRun) > 0) {
      message("--Number of new concepts: ", nrow(fullResults))
      message("--Number of previous concepts: ", nrow(previousRun))
      fullResults <- unique(rbind(
        fullResults[, 1:min(ncol(fullResults), ncol(previousResults))],
        previousResults[, 1:min(ncol(fullResults), ncol(previousResults))]
      )) # add on results from previous run if there were any
    }
  } else { # all concepts previously tested
    message("--No new concepts, skipping to next part of process")
    fullResults <- previousResults # results are same as previous
  }

  fullResults <- unique(fullResults)

  message("--Number of total concepts: ", nrow(fullResults))

  return(fullResults)
}

# Function to determine file type and read accordingly
.readDocument <- function(filePath) {
  # Get the file extension
  fileExt <- tools::file_ext(filePath)
  if (fileExt == "docx" || fileExt == "doc") {
    # Read Word document
    doc <- officer::read_docx(filePath)
    # Extract text
    textData <- officer::docx_summary(doc)
    textContent <- paste(textData$text, collapse = "\n")
    return(textContent)
  } else if (fileExt == "txt") {
    # Read plain text file
    textContent <- readLines(filePath, warn = FALSE)
    return(paste(textContent, collapse = "\n"))
  } else {
    stop("Unsupported file type!")
  }
}

.compileAdditionalInformation <- function(docList) {
  if (is.null(docList) | docList == "") {
    return("")
  } else {
    finalDoc <- NULL
    for (docUp in 1:length(docList)) {
      doc <- .readDocument(docList[[docUp]])
      finalDoc <- paste0(finalDoc, "\n************************************\n", doc)
    }
    finalDoc <- paste0(finalDoc, "\n************************************\n")
    return(finalDoc)
  }
}

.getPhoebeData <- function(concepts) {
  phoebeUrlstring <- "https://hecate.pantheon-hds.com/api/concepts/%d/phoebe"

  phoebeData <- list()
  for (conceptUp in 1:length(concepts)) {
    cat(paste0("--Searching PHOEBE - Analyzing ", conceptUp, " of ", length(concepts), "\r"))
    url <- sprintf(phoebeUrlstring, concepts[[conceptUp]])
    response <- httr::GET(url)

    if (httr::status_code(response) == 200) {
      contextText <- httr::content(response, "text", encoding = "UTF-8")
      if (contextText == "[]") {
        data <- NULL
      } else {
        data <- jsonlite::fromJSON(contextText)
        data <- data |>
          SqlRender::snakeCaseToCamelCaseNames()
        phoebeData[[length(phoebeData) + 1]] <- data
      }
    } else {
      stop(sprintf(
        "Error in phoebe search for concept %s: %s",
        conceptUp,
        httr::status_code(response)
      ))
    }
  }
  phoebeData <- unique(bind_rows(phoebeData))
  phoebeData <- phoebeData[!is.na(phoebeData$conceptId),]
  return(phoebeData)
}
