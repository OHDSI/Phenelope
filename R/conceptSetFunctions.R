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
                                                      excludedConcepts = "none",
                                                      belowMinimumCountApproach = "TEST ALL",
                                                      clinicalDefinition = "",
                                                      clinicalContext,
                                                      excludedVocabularies = c("ICDO3"),
                                                      domain,
                                                      phoebeExclusions = phoebeExclusions,
                                                      bucketSize,
                                                      conditionForFiles,
                                                      outputDirectory) {
  if (type == "phoebe") {
    text <- "PHOEBE"
  } else {
    text <- "included concepts"
  }

  conceptList <- Filter(is.numeric, conceptList) # ensure no garbage was included (rare)
  message("For concept(s): ", paste(conceptList, collapse = ", "))
  message("Getting GenAI similarity response for ", text, " for query: ", query)

  message("--Getting concept set expression")

  connection2 <- suppressMessages(DatabaseConnector::connect(connectionDetails = connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection2))

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

  conceptList <- DatabaseConnector::querySql(connection = connection2, sql = sql, snakeCaseToCamelCase = TRUE)
  conceptList$conceptSetTarget <- conceptList$conceptName

  if(nrow(conceptList) == 0) {
    message(paste0("\n...no concepts found for ", query))
    return(NULL)  # Return NULL in case of no concepts
  }

  if (nrow(conceptList) != 0) {
    conceptList$phoebe <- F
  }

  message("--Finding ", type, " results for concept set")
  if (type == "phoebe") {
    # recs <- .getPhoebeData(c(conceptList$conceptId))
    recs <- .getAnyPhoebeData(c(conceptList$conceptId))

    if(!is.null(recs)) {
      if(nrow(recs) != 0) {
        if(length(phoebeExclusions)) {
          recs <- recs[!(recs$relationshipId %in% c(phoebeExclusions)),]
        }
        recs <- recs[!(recs$conceptId %in% c(conceptList$conceptId)), ]
      }
    }
    recsFinal <- recs

    if (nrow(recsFinal) != 0) { # check if phoebe had any recommendations
      sqlFilename <- "checkDomainsForConcepts.sql"
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = sqlFilename,
        packageName = "Phenelope",
        dbms = connectionDetails$dbms,
        cdm_database_schema = cdmDatabaseSchema,
        concept_list = paste(recsFinal$conceptId, collapse = ", "),
        excludedVocabularies = paste(sprintf("'%s'", excludedVocabularies), collapse = ", ")
      )

      concepts <- DatabaseConnector::querySql(connection = connection2, sql, snakeCaseToCamelCase = TRUE)
      concepts$conceptSetTarget <- concepts$conceptName

      if(nrow(concepts) > 0) {
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
      } else { # no valid phoebe recommendations - possible to get phoebe recommendations from hecate not in db
        concepts <- conceptList
      }
    } else { # no phoebe recommendations
      concepts <- conceptList
    }
  } else { # else test against included concepts
    if(minCount > 0) { #need to get record count as it is used to determine eligible concepts
      recs <- .getAnyPhoebeData(c(conceptList$conceptId)) # get phoebe data on this pass solely for the record counts
    } else { #don't need to get record counts on this pass as it won't be used to determine eligible concepts
      recs <- data.frame() #set to empty df
    }

    concepts <- conceptList
    recsFinal <- conceptList
  }

  if (nrow(recs) != 0) { # phoebe recs found
    concepts <- merge(concepts, unique(recs[, c("conceptId", "conceptSetTarget", "recordCount")]), all.x = T)
  } else { # no phoebe recs
    concepts$recordCount <- NA
  }

  concepts <- concepts |>
    dplyr::mutate(recordCount = dplyr::if_else(is.na(.data$recordCount), 0, .data$recordCount)) |>
    dplyr::mutate(aboveMin = .data$recordCount >= minCount) |>
    dplyr::arrange(desc(.data$phoebe), desc(.data$aboveMin))

  message("\n--Current number of concepts: ", nrow(concepts))

  previousRun <- data.frame()
  if (!is.null(previousResults)) {
    previousRun <- previousResults |>
      dplyr::filter(.data$conceptId %in% concepts$conceptId)
    concepts <- concepts |>
      dplyr::filter(!.data$conceptId %in% previousResults$conceptId)
    message("--skipping previously analyzed concepts yields: ", nrow(concepts))
  }

  if (nrow(concepts) > 500) { #for large sets
    #remove the clearly "no" concepts
    message("\n--Removing concepts that clearly do not belong...")
    updatedConcepts <- removeClearNo(query = query,
                                     conceptList = concepts,
                                     llmClient = llmClient,
                                     clinicalDefinition = clinicalDefinition,
                                     clinicalContext = clinicalContext,
                                     bucketSize = 200)

    concepts <- concepts[!(concepts$conceptId %in% c(updatedConcepts$conceptId)),]
    message(paste0("--", nrow(concepts), " concepts remain to be fully tested.\n"))

    # save to dataframe as a csv
    if(nrow(updatedConcepts) > 0) {
      utils::write.csv(updatedConcepts, file.path(outputDirectory, paste0(conditionForFiles, "_removedConcepts_", type, ".csv")), row.names = F)
    }
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

  if(bucketSize > 1) {
    promptUp <- system.file("prompts", "LLM_Prompt_for_PHOEBE_generic.txt", package = "Phenelope")
  } else {
    promptUp <- system.file("prompts", "LLM_Prompt_for_PHOEBE_single_generic.txt", package = "Phenelope")
  }
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
      resultsDf$suggestedConcept <- concepts$conceptSetTarget[[conceptUp]]
      resultsDf$mainCondition <- query
      resultsDf$excludedConcepts <-  ""
      resultsDf$proposedInExcluded <-  ""
      resultsDf$finalAnswer <-  ""
      resultsDf$rationaleForAnswer <-  ""
      resultsDf$confidenceLevel <-  ""
      resultsDf$tested <- F

      resultsDf <- data.frame(resultsDf)

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

      resultsDf$cost <- 0
      columnsToFront <- c("suggestedConcept", "conceptId", "mainCondition", "finalAnswer", "rationaleForAnswer", "confidenceLevel")
      # Rearrange the DataFrame
      resultsDf <- resultsDf |>
        dplyr::select(all_of(columnsToFront), everything())
      noTestList <- rbind(noTestList, resultsDf)
    }
  }

  cost <- 0
  llmClient$set_turns(list()) # Reset the chat

  if (!is.null(testList)) {
    concepts <- testList
  }

  concepts$conceptSetTarget <- gsub("\\[|\\]", " ", concepts$conceptSetTarget) #remove any [ or ] from name (interferes with json structure)
  if (nrow(concepts) != 0) {
    startPoint <- 1
    endPoint <- min(bucketSize, nrow(concepts))
    while(startPoint <= nrow(concepts)) {
      cat(paste0("--Querying LLM - Analyzing concepts ", startPoint, " through ", endPoint, " of ", nrow(concepts), "  \r"))
      concepts$aboveMin[1] <- T # always test the first concept

      testCondition <- concepts[startPoint:endPoint, c("conceptId", "conceptSetTarget")]
      baseCondition <- query

      updatedLines <- gsub("MAIN_CONCEPT", baseCondition, originalLines)

      if(bucketSize == 1) {
        testConditionShort <- concepts[startPoint:endPoint, c("conceptSetTarget")]
        updatedLines <- gsub("SUGGESTED_CONCEPT_SHORT", testConditionShort, updatedLines)
      }

      json_all <- jsonlite::toJSON(testCondition)
      updatedLines <- gsub("SUGGESTED_CONCEPT", json_all, updatedLines)

      updatedLines <- gsub("EXCLUDED_CONCEPTS", excludedConcepts, updatedLines)
      updatedLines <- gsub("CLINICAL_CONTEXT", clinicalContext, updatedLines)
      updatedLines <- gsub("ADDITIONAL_INFORMATION", clinicalDefinition, updatedLines)

      prompt <- paste(updatedLines, collapse = "\n")
      lastPrompt <- prompt
      saveLastPrompt(prompt)

      retryLimit <- 10 # Maximum number of retries
      attempt <- 0 # Initial attempt counter
      success <- FALSE # Flag to indicate success

      while (attempt <= retryLimit && !success) { # llm will mislabel column headers occasionally - usually fixed with a re-try
        tryCatch(
          {
            attempt <- attempt + 1 # Increment the attempt count

            systemPrompt <- "You are an expert medical doctor specializing in healthcare data analysis. Your primary function is to analyze healthcare data, including electronic health records, to infer causal relationships between exposures and health outcomes."

            llmClient$set_system_prompt(systemPrompt)

            fullBucket <- FALSE
            bucketAttempt <- 0
            while(!fullBucket) {
              bucketAttempt <- bucketAttempt + 1
              bucketItems <- (endPoint - startPoint) + 1
              llmClient$set_turns(list()) # Reset the chat
              text <- llmClient$chat_structured(prompt,
                                                echo = "none",
                                                type = ellmer::type_array(ellmer::type_object(
                                                  conceptId = ellmer::type_string(),
                                                  suggestedConcept = ellmer::type_string(),
                                                  excludedConcepts = ellmer::type_string(),
                                                  proposedInExcluded = ellmer::type_string(),
                                                  finalAnswer = ellmer::type_enum(values = c("YES","NO")),
                                                  rationaleForAnswer = ellmer::type_string(),
                                                  confidenceLevel = ellmer::type_enum(values = c("CLEAR","BORDERLINE"))
                                                ))
              )

              if (is.character(text)) {
                if (jsonlite::validate(text)) {
                  text <- jsonlite::fromJSON(text)
                }
              }

              resultsDf <- data.frame(text)
              if(nrow(resultsDf) == bucketItems) { #same rows sent out as received
                fullBucket <- TRUE
                bucketAttempt <- 0
              } else {
                cat(paste0("---Querying LLM - Analyzing concepts ", startPoint, " through ", endPoint, " of ", nrow(concepts), "\r"))

                if(bucketAttempt == 10) {
                  stop("LLM connection issue...stopping")
                  saveLastPrompt(prompt)
                }
              }
            }
            resultsDf$tested <- T

            resultsDf$mainCondition <- baseCondition
            resultsDf$model <- llmClient$get_model()
            resultsDf$cost <- sprintf("%.5f", llmClient$get_cost())

            columnsToFront <- c("suggestedConcept", "conceptId", "mainCondition", "finalAnswer", "rationaleForAnswer", "confidenceLevel")

            # Rearrange the DataFrame
            resultsDf <- resultsDf |>
              dplyr::select(all_of(columnsToFront), everything())

            results <- rbind(results, resultsDf)

            success <- TRUE
            cost <- cost + llmClient$get_cost()
            llmClient$set_turns(list()) # Reset the chat
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

    message("\n\n--Total cost was $", sprintf("%.3f", cost))

    fullResults <- results
    if(length(noTestList)) { #add in the untested if any
      fullResults <- rbind(fullResults, noTestList)
    }

    if (type == "phoebe") { # only join this for phoebe results
      if (nrow(recsFinal) != 0) {
        fullResults <- fullResults |>
          dplyr::mutate(conceptId = as.integer(.data$conceptId)) |>
          dplyr::left_join(unique(recsFinal[, c("conceptId", "recordCount")]), by = c("conceptId" = "conceptId"))
      } else { # no phoebe recs
        fullResults <- fullResults |>
          dplyr::mutate(conceptId = as.integer(.data$conceptId))
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

  if(nrow(concepts) != 0) {
    saveLastPrompt(lastPrompt)
  }

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

.getAnyPhoebeData <- function(concepts) { #depends on the length of the concept list
  if(length(concepts) == 1) {
    phoebeData <- .getPhoebeData(concepts)
  } else if(length(concepts) > 1) {
    phoebeData <- .getPhoebeDataBulk(concepts)
  } else {
    phoebeData <- NULL
  }

  phoebeData$conceptSetTarget <- phoebeData$conceptName
  return(phoebeData)
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
  phoebeData <- unique(dplyr::bind_rows(phoebeData))
  if(nrow(phoebeData) > 0) {
    phoebeData <- phoebeData[!is.na(phoebeData$conceptId),]
  }
  cat("\n")
  return(phoebeData)
}

.getPhoebeDataBulk <- function(concepts) {
  phoebeUrlstring <- "https://hecate.pantheon-hds.com/api/concepts/phoebe/bulk"

  phoebeData <- list()

  start <- 1
  batchSize <- 500
  while (start <= length(concepts)) {
    end <- min(start + batchSize - 1, length(concepts))
    cat(paste0("\r--Searching PHOEBE - Analyzing ", start, " to ", end, " out of ", length(concepts)))
    ids <- concepts[start:end]
    response <- httr::POST(phoebeUrlstring, body = list(ids = as.integer(ids)), encode = "json" )

    if (httr::status_code(response) == 200) {
      contextText <- httr::content(response, "text", encoding = "UTF-8")
      if (contextText == "[]") {
        data <- NULL
      } else {
        data <- jsonlite::fromJSON(contextText)
        if(ncol(data) < 5) {#fix the data if there is an error
          fulldata <- NULL
          for(rowUp in 1:nrow(data)){
            if(length(data$results[[rowUp]])) { #only add rows where phoebe data was found
              fulldata <- rbind(fulldata, data$results[[rowUp]])
            }
          }
          data <- fulldata
          if(!is.null(data)) {
            data <- data |>
              SqlRender::snakeCaseToCamelCaseNames()
          }
        }
        phoebeData[[length(phoebeData) + 1]] <- data
      }
    } else {
      stop(sprintf(
        "Error in phoebe search for concept %s: %s",
        conceptUp,
        httr::status_code(response)
      ))
    }
    start <- end + 1
  }
  phoebeData <- unique(dplyr::bind_rows(phoebeData))
  if(nrow(phoebeData) > 0) {
    phoebeData <- phoebeData[!is.na(phoebeData$conceptId),]
  }
  cat("\n")
  return(phoebeData)
}

saveLastPrompt <- function(prompt) {
  promptDir <- file.path("./lastPrompt")
  if (!dir.exists(promptDir)) {
    success <- dir.create(promptDir, recursive = TRUE, showWarnings = FALSE)
    if (!success) stop("Failed to create directory: ", promptDir)
  }

  if(typeof(prompt) == "character") {
    con <- file(file.path(promptDir, "prompt.txt"), open = "w", encoding = "UTF-8")
    writeLines(prompt, con)
    close(con)
  }
}

.getDomain <- function(llmClient, searchString) {

  ellmerTypeObject <- ellmer::type_array(ellmer::type_object(
    term = ellmer::type_string(),
    domain = ellmer::type_enum(values = c("DRUG","CONDITION","PROCEDURE","VISIT","DEVICE", "MEASUREMENT"))
  ))

  prompt <- paste0("Determine what domain category, DRUG, CONDITION, PROCEDURE, MEASUREMENT, VISIT, or DEVICE, the following belongs to: ",
                   searchString,
                   "  [{
                            \"term\": \"Name of the search term\",
                            \"domain\": \"Domain name\"
                            }]")

  domainName <- queryLLM(llmClient = llmClient, prompt = prompt, ellmerTypeObject = ellmerTypeObject)

  return(domainName)
}

queryLLM <- function(llmClient, prompt, systemPrompt = NULL, silent = TRUE, output = "data frame", array = F, ellmerTypeObject = NULL) {
  if(!silent) {ParallelLogger::logInfo("\n--Querying LLM...")}

  retry_limit <- 10  # Maximum number of retries
  attempt <- 0      # Initial attempt counter
  success <- FALSE  # Flag to indicate success

  while (attempt <= retry_limit && !success) { #llm will mislabel column headers occasionally - usually fixed with a re-try
    tryCatch({
      attempt <- attempt + 1  # Increment the attempt count
      saveLastPrompt(prompt)

      if(output == "text") { #simple text return
        text <- llmClient$chat(prompt,
                               echo = "none")

        results <- text
      } else { #json return
        text <- llmClient$chat_structured(prompt,
                                          echo = "none",
                                          type = ellmerTypeObject
        )

        if(!array) {fromLLM <- jsonlite::fromJSON(jsonlite::toJSON(text))
        } else {fromLLM <- jsonlite::fromJSON(text)
        }

        fromLLM$cost <- sprintf("%.4f", llmClient$get_cost(include = "last"))

        if(output == "data frame") {
          results <- data.frame(fromLLM)
        } else {
          results <- fromLLM
        }
      }
      success <- TRUE
    },
    error = function(e) {
      # Handle the error: print a message and increment the attempt counter
      message(paste("Attempt", attempt, "failed:", e$message))

      if(grepl("abort", e$message, ignore.case=TRUE)) {
        cat("Stopping the run as requested.\n")
        stop("Execution stopped by user.")
      }
      if(attempt >= retry_limit) {
        message(paste("Reached attempt limit."))
      }
      return(NULL)  # Return NULL in case of error
    })
  }

  return(results)
}
