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

.fullConceptSetCreation <- function(conceptSetTarget,
                                    originalConceptList,
                                    excludedConcepts = "none",
                                    llmClient,
                                    connectionDetails,
                                    cdmDatabaseSchema,
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                    minCount = 0,
                                    belowMinimumCountApproach,
                                    outputDirectory,
                                    clinicalDefinition = "",
                                    excludedVocabularies = c("ICDO3"),
                                    clinicalContext = "any clinical context",
                                    bucketSize = 1,
                                    domain = "ALL",
                                    conditionForFiles,
                                    tryNumber,
                                    phoebeExclusions = phoebeExclusions,
                                    phoebeRepository,
                                    quickRun = FALSE) {
  message("Testing concepts and descendants. ")

  connection3 <- suppressMessages(DatabaseConnector::connect(connectionDetails = connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection3))

  if (file.exists(file.path(outputDirectory, paste0("firstPart_", conditionForFiles, tryNumber, ".csv")))) {
    # found the first half but not the full analysis, skip the first part and go to the second part
    message(
      "File ",
      file.path(outputDirectory, paste0("firstPart_", conditionForFiles, tryNumber, ".csv")),
      " exists...skipping to next part of analysis."
    )
    llmResults <- utils::read.csv(file.path(outputDirectory, paste0("firstPart_", conditionForFiles, tryNumber, ".csv")))
  } else {
    llmResults <- .createRecommendListViaLlmFromConceptList(
      query = conceptSetTarget,
      closestConditionConcept = conceptSetTarget,
      conceptList = originalConceptList,
      llmClient = llmClient,
      connection = connection,
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      type = "phoebe",
      minCount = minCount,
      previousResults = NULL,
      excludedConcepts = excludedConcepts,
      belowMinimumCountApproach,
      clinicalDefinition = clinicalDefinition,
      clinicalContext = clinicalContext,
      excludedVocabularies = c(excludedVocabularies),
      domain = domain,
      phoebeExclusions = phoebeExclusions,
      bucketSize = bucketSize,
      phoebeRepository = phoebeRepository,
      conditionForFiles = conditionForFiles,
      outputDirectory = outputDirectory
    )
    if(is.null(llmResults)) {
      return(NULL)
    } else {
      utils::write.csv(llmResults, file.path(outputDirectory, paste0("firstPart_", conditionForFiles, tryNumber, ".csv")), row.names = F)
    }
  }

  previousResults <- llmResults

  included <- unique(c(as.integer(llmResults$conceptId[llmResults$finalAnswer == "YES"])))

  conceptList <- unique(c(originalConceptList, included))

  message("Testing final set of included concepts.")

  # remove ancestors of the original concept set list from the list (don't want to include their descendants)
  sqlFilename <- "RemoveAncestors.sql"
  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = sqlFilename,
    packageName = "Phenelope",
    dbms = connectionDetails$dbms,
    cdm_database_schema = cdmDatabaseSchema,
    concepts_to_use = paste(originalConceptList, collapse = ", ")
  )

  ancestorList <- DatabaseConnector::querySql(connection = connection3, sql, snakeCaseToCamelCase = TRUE)

  conceptList <- conceptList[!(conceptList %in% c(unlist(ancestorList)))]

  #second pass for the included concept descendants
  llmResults <- .createRecommendListViaLlmFromConceptList(
    query = conceptSetTarget,
    closestConditionConcept = conceptSetTarget,
    conceptList = conceptList,
    llmClient = llmClient,
    connection = connection,
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    type = "included",
    minCount = minCount,
    previousResults = previousResults,
    excludedConcepts = excludedConcepts,
    belowMinimumCountApproach,
    clinicalDefinition = clinicalDefinition,
    clinicalContext = clinicalContext,
    excludedVocabularies = c(excludedVocabularies),
    domain = domain,
    bucketSize = bucketSize,
    phoebeRepository = phoebeRepository,
    conditionForFiles = conditionForFiles,
    outputDirectory = outputDirectory
  )

  return(llmResults)
}

.getFullConceptSet <- function(conceptSetTarget,
                               conceptIds,
                               domains,
                               conceptClasses = "",
                               connection,
                               connectionDetails,
                               cdmDatabaseSchema,
                               quickRun = FALSE,
                               llmClient,
                               minCount,
                               belowMinimumCountApproach,
                               exclusions = "TEST", #default is to test for exclusions - call with set to "none" if no testing wanted
                               clinicalDefinition = "",
                               excludedVocabularies = c("ICDO3"),
                               excludedConcepts,
                               clinicalContext =  "",
                               outputDirectory = outputDirectory,
                               tryNumber,
                               conditionForFiles,
                               phoebeExclusions = c(),
                               phoebeRepository,
                               bucketSize = 20) {
  if(!is.null(getOption("databaseConnectorInteger64AsNumeric"))) {
    if(!getOption("databaseConnectorInteger64AsNumeric")) { #if set to false, change to true and put back at the end
      options(databaseConnectorInteger64AsNumeric = TRUE)
      on.exit(options(databaseConnectorInteger64AsNumeric = FALSE))
    }
  }


  connection4 <- suppressMessages(DatabaseConnector::connect(connectionDetails = connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection4))

  #test for likely very large (> 5K) concept sets
  sql <- paste0("select count(distinct c.concept_id) ",
                "from ", cdmDatabaseSchema, ".concept_ancestor ca ",
                "join ", cdmDatabaseSchema, ".concept c ",
                "on ca.descendant_concept_id  = c.concept_id ",
                "where ca.ancestor_concept_id in (", paste0(c(conceptIds), collapse = ","), ") ",
                "and c.domain_id in (", paste0("'", domains, "'", collapse = "," ), ") ",
                "and c.vocabulary_id not in (", paste0("'", excludedVocabularies, "'", collapse = "," ), ") ",
                # "and c.concept_class_id in (", paste0("'", conceptClasses, "'", collapse = "," ), ") ",
                "and c.standard_concept = \"S\";")

  recordCount <- DatabaseConnector:: querySql(connection = connection4, sql, snakeCaseToCamelCase = TRUE)

  if(recordCount <= 5000) { # reasonable size concept set to test
    #test to see if any concepts should be explicitly excluded
    if(exclusions == "TEST") {
      exclusions <- .checkForExclusions(term = conditionForFiles, llmClient = llmClient)
      if(exclusions$yesNo == "YES") {
        excludedConditions <- exclusions$exclusions
      } else {
        excludedConditions <- "none"
      }
    } else {
      excludedConditions <- exclusions
    }

    #create the concept set
    finalConceptSet <- .fullConceptSetCreation(conceptSetTarget = conceptSetTarget,
                                               originalConceptList = conceptIds,
                                               excludedConcepts = excludedConcepts,
                                               llmClient = llmClient,
                                               connectionDetails = connectionDetails,
                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                               tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                               minCount = minCount,
                                               belowMinimumCountApproach = belowMinimumCountApproach,
                                               outputDirectory = outputDirectory,
                                               clinicalDefinition = clinicalDefinition,
                                               excludedVocabularies = c("ICDO3"),
                                               clinicalContext = clinicalContext,
                                               bucketSize = bucketSize,
                                               conditionForFiles = conditionForFiles,
                                               tryNumber = tryNumber,
                                               phoebeExclusions = phoebeExclusions,
                                               phoebeRepository = phoebeRepository,
                                               quickRun = FALSE)

  } else { #too large to test - send message and use descendants only
    message(paste0("The number of concepts to test (", recordCount, ") exceeds the threshold of 5000.  Producing a concept set of concepts plus descendants"))

    finalConceptSet <- NULL
    finalConceptSet$conceptId <- conceptIds
    finalConceptSet$suggestedConcept <- ""
    finalConceptSet$mainCondition <- conceptSetTarget
    finalConceptSet$finalAnswer <- "YES"
    finalConceptSet$rationaleForAnswer <- ""
    finalConceptSet$confidenceLevel <- ""
    finalConceptSet$excludedConcepts <- ""
    finalConceptSet$proposedInExcluded <- ""
    finalConceptSet$tested <- TRUE
    finalConceptSet$model <- ""
    finalConceptSet$cost <- ""
    finalConceptSet$recordCount <- 0

    finalConceptSet <- data.frame(finalConceptSet)
  }

  return(finalConceptSet)
}

.checkForExclusions <- function(term, llmClient) {
  ellmerTypeObject <- ellmer::type_array(ellmer::type_object(
    term = ellmer::type_string(),
    yesNo = ellmer::type_enum(values = c("YES","NO")),
    rationale = ellmer::type_string(),
    exclusions = ellmer::type_string()
  ))

  prompt <- paste0("Does the term: ", term, " EXPLICITLY suggest there should be exclusions? ",
                   "Examples: 'psoriasis without arthritis' explicitly excludes arthritis whereas 'psoriatic arthritis' does not ",
                   "'nonrheumatic valve disorder' exclude rheumatic valve disorder ",
                   "If yes, create a COMPREHENSIVE set of Excluded terms - DO NOT include any associated billing codes, e.g., ICD10 codes",
                   "If yes, what are the exclusions? ",
                   "  {
                            \"term\": \"Name of term\",
                            \"yesNo\": \"YES or NO\",
                            \"rationale\": \"Rationale for exclusion\",
                            \"exclusions\": \"a COMPREHENSIVE set of Excluded terms\"
                            }")

  exclusions <- queryLLM(llmClient = llmClient, prompt = prompt, ellmerTypeObject = ellmerTypeObject)
  return(exclusions)
}

.createJsonforConceptsPlusDescendants <- function(conceptIds,
                                                  connectionDetails,
                                                  cdmDatabaseSchema) {
  # --- Params ----
  connectionDetails <- connectionDetails
  vocab_schema <- cdmDatabaseSchema
  concept_ids <- c(conceptIds)  # replace with your list

  # --- Fetch metadata for the concept_ids (only valid concepts) ---
  sql <- "
SELECT concept_id,
       concept_name,
       concept_code,
       domain_id,
       vocabulary_id,
       concept_class_id,
       standard_concept
FROM @vocab_schema.concept
WHERE concept_id IN (@concept_ids)
  AND (invalid_reason IS NULL OR invalid_reason = '')
"
  # SqlRender: pass the comma-separated list of ids
  sql <- SqlRender::render(sql,
                           vocab_schema = vocab_schema,
                           concept_ids = paste(concept_ids, collapse = ","))
  # Optional: translate to target dialect (if needed)
  # sql <- SqlRender::translate(sql, targetDialect = "postgresql")

  conn <- DatabaseConnector::connect(connectionDetails)
  meta <- DatabaseConnector::querySql(conn, sql)
  DatabaseConnector::disconnect(conn)

  # --- Build items: one item per input concept id, includeDescendants = TRUE ---
  items <- lapply(concept_ids, function(cid) {
    row <- meta[meta$concept_id == cid, , drop = FALSE]
    if (nrow(row) == 1) {
      concept_meta <- list(
        CONCEPT_ID = as.integer(row$concept_id),
        CONCEPT_NAME = row$concept_name,
        CONCEPT_CODE = row$concept_code,
        DOMAIN_ID = row$domain_id,
        VOCABULARY_ID = row$vocabulary_id,
        CONCEPT_CLASS_ID = row$concept_class_id,
        STANDARD_CONCEPT = row$standard_concept,
        STANDARD_CONCEPT_CAPTION = "Standard"
      )
    } else {
      # fallback if metadata missing (still valid for ATLAS import)
      concept_meta <- list(CONCEPT_ID = as.integer(cid),
                           CONCEPT_NAME = paste0("concept ", cid))
    }

    list(
      concept = concept_meta,
      isExcluded = FALSE,
      includeDescendants = TRUE,
      includeMapped = FALSE
    )
  })

  concept_set <- list(
    items = items
  )

  # convert to JSON (NA -> null)
  json_string <- jsonlite::toJSON(concept_set, pretty = TRUE, simplifyVector = FALSE, auto_unbox = TRUE)


  # return(json_string)
  return(concept_set)
}

.vectorSearchStandard <- function(term,
                                  domains = NULL,
                                  conceptClasses = NULL,
                                  limit = 10,
                                  maxRetries = 3,
                                  waitTime = 2) {
  #domains: Condition, Procedure, Drug, Visit, Measurement
  #conceptClasses: (c: Disorder, HCPCS, 	Clinical Observation), (p: Procedure, CPT4), (d: Ingredient, Clinical Drug, Branded Drug),
  # (v: Visit) (m: Clinical Observation, CPT4)

  conceptExclusionList <- c(4040400, 4040390) #exclude very broad terms
  term <- gsub("_",  " ", term)
  params <- list(
    q = term,
    #domain_id = paste(domains, collapse = ","),
    limit = limit
  )

  if (!is.null(domains)) {
    params$domain_id <- paste(domains, collapse = ",")
  }

  if (!is.null(conceptClasses)) {
    params$concept_class_id <- paste(conceptClasses, collapse = ",")
  }
  url <- "https://hecate.pantheon-hds.com/api/search_standard"

  for (attempt in 1:maxRetries) {
    response <- tryCatch(
      {
        httr::GET(url, query = params)
      },
      error = function(e) {
        message(paste("Attempt", attempt, "failed with error:", e$message))
        return(NULL)
      }
    )

    if (!is.null(response) && httr::status_code(response) == 200) {
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content_text)

      if(length(data) > 0) {
        data <- dplyr::bind_rows(data$concepts) |>
          SqlRender::snakeCaseToCamelCaseNames()
        data <- data[!(data$conceptId %in% conceptExclusionList),]
        return(cbind(data.frame(searchTerm = term, data)))
      } else {
        return(data.frame())
      }
    }
    if (attempt < maxRetries) {
      message(sprintf(
        "Search failed for '%s' (Status: %s). Retrying in %s seconds...",
        term,
        if (is.null(response)) "Connection Error" else httr::status_code(response),
        waitTime
      ))
      Sys.sleep(waitTime)
    }
  }
  stop(sprintf("All %s attempts failed for term '%s'.", maxRetries, term))
}

.vectorSearch <- function(term,
                          domains,
                          conceptClasses = NULL,
                          vocabularyId = NULL,
                          standardConcept = NULL,
                          limit = 10,
                          maxRetries = 3,
                          waitTime = 2) {
  params <- list(
    q = term,
    domain_id = paste(domains, collapse = ","),
    vocabulary_id = vocabularyId,
    standard_concept = standardConcept,
    limit = limit
  )
  if (!is.null(conceptClasses)) {
    params$concept_class_id <- paste(conceptClasses, collapse = ",")
  }
  url <- "https://hecate.pantheon-hds.com/api/search"

  for (attempt in 1:maxRetries) {
    response <- tryCatch(
      {
        httr::GET(url, query = params)
      },
      error = function(e) {
        message(paste("Attempt", attempt, "failed with error:", e$message))
        return(NULL)
      }
    )


    if (!is.null(response) && httr::status_code(response) == 200) {
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content_text)

      if(length(data) > 0) {
        scoreData <- data[,c("concept_name", "score")]
        data <- dplyr::bind_rows(data$concepts)
        data <- merge(data, scoreData)

        data <- SqlRender::snakeCaseToCamelCaseNames(data)
      }

      return(data)
    }

    if (attempt < maxRetries) {
      message(sprintf(
        "Search failed for '%s' (Status: %s). Retrying in %s seconds...",
        term,
        if (is.null(response)) "Connection Error" else httr::status_code(response),
        waitTime
      ))
      Sys.sleep(waitTime)
    }
  }
  stop(sprintf("All %s attempts failed for term '%s'.", maxRetries, term))
}

.vectorSearchAtc <- function(term,
                             domains,
                             vocabularyId,
                             limit = 10,
                             maxRetries = 3,
                             waitTime = 2) {
  params <- list(
    q = term,
    domain_id = paste(domains, collapse = ","),
    vocabulary_id = vocabularyId,
    standard_concept = "C",
    limit = limit
  )
  url <- "https://hecate.pantheon-hds.com/api/search"

  for (attempt in 1:maxRetries) {
    response <- tryCatch(
      {
        httr::GET(url, query = params)
      },
      error = function(e) {
        message(paste("Attempt", attempt, "failed with error:", e$message))
        return(NULL)
      }
    )


    if (!is.null(response) && httr::status_code(response) == 200) {
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content_text)

      if(length(data) > 0) {
        scoreData <- data[,c("concept_name", "score")]
        data <- dplyr::bind_rows(data$concepts)
        data <- merge(data, scoreData)

        data <- SqlRender::snakeCaseToCamelCaseNames(data)
      }

      return(data)
    }

    if (attempt < maxRetries) {
      message(sprintf(
        "Search failed for '%s' (Status: %s). Retrying in %s seconds...",
        term,
        if (is.null(response)) "Connection Error" else httr::status_code(response),
        waitTime
      ))
      Sys.sleep(waitTime)
    }
  }
  stop(sprintf("All %s attempts failed for term '%s'.", maxRetries, term))
}

.grabConcepts <- function(searchString,
                          originalConceptList = originalConceptList,
                          llmClient,
                          domains,
                          classes,
                          excludedConcepts,
                          vectorSearchSize,
                          connectionDetails,
                          connection,
                          cdmDatabaseSchema,
                          clinicalDefinition,
                          clinicalContext,
                          minCount = 0,
                          belowMinimumCountApproach = "TEST ALL",
                          conditionForFiles,
                          tryNumber = 1,
                          outputDirectory,
                          phoebeExclusions,
                          standardOnly,
                          bucketSize,
                          phoebeRepository) {

  results <- getClinicalSynonyms(searchString)
  if(!is.null(results)) {
    hecateSearchString <- results$synonymousBucketNames
  } else {
    hecateSearchString <- searchString
  }

  if (file.exists(file.path(outputDirectory, paste0(conditionForFiles,
                                                    "_from_embVectors.csv")))) {
    # skip finding seeds if already exists
    message(paste0(
      "File ",
      file.path(outputDirectory, paste0(conditionForFiles,
                                        "_from_embVectors.csv")),
      " exists...skipping to next part of process."))
    prelimLlmResults <- utils::read.csv(file.path(outputDirectory, paste0(conditionForFiles,
                                                                          "_from_embVectors.csv")))
    conceptList <- as.numeric(c(prelimLlmResults$conceptId[prelimLlmResults$finalAnswer == "YES"]))

  } else {
    if(length(originalConceptList) != 0) { #use provided concept list
      conceptList <- originalConceptList

    } else { #get concept list from hecate
      #get seed concepts from hecate
      message(paste0("Searching Hecate for the following: ", hecateSearchString))

      seeds <- getHecateSearchList(hecateSearchString,
                                   domains = domains,
                                   conceptClasses = classes,
                                   vectorSearchSize = vectorSearchSize,
                                   standardOnly = standardOnly)

      seeds <- seeds[!duplicated(seeds$conceptName), ]

      # if(standardOnly == TRUE) {
      #   seeds <- .vectorSearchStandard(term = hecateSearchString,
      #                                  domains = domains,
      #                                  conceptClasses = classes,
      #                                  limit = vectorSearchSize)
      # } else {
      #   seeds <- .vectorSearch(term = hecateSearchString,
      #                          domains = domains,
      #                          # conceptClasses = classes,
      #                          limit = vectorSearchSize)
      # }
      conceptList <- seeds[1:vectorSearchSize,]

      promptToUse <- system.file("prompts", "LLM_Prompt_for_PHOEBE_generic_sensitive.txt", package = "Phenelope")
      #quick first pass - adjudicate hecate list to screen out bad fits
      message("\nTesting initial list for viable concept candidates...")
      prelimLlmResults <- .createRecommendListFromConcepts(query = searchString,
                                                           conceptList = conceptList$conceptId,
                                                           llmClient = llmClient,
                                                           prompt = promptToUse,
                                                           connectionDetails = connectionDetails,
                                                           connection = connection,
                                                           cdmDatabaseSchema = cdmDatabaseSchema,
                                                           excludedConcepts = "none",
                                                           clinicalDefinition = clinicalDefinition,
                                                           clinicalContext = clinicalContext,
                                                           conditionForFiles = conditionForFiles,
                                                           bucketSize = bucketSize)

      if(!is.null(prelimLlmResults)) {
        utils::write.csv(prelimLlmResults, file.path(outputDirectory, paste0(conditionForFiles,
                                                                             "_from_embVectors.csv")), row.names = F)
      }
      conceptList <- as.numeric(c(prelimLlmResults$conceptId[prelimLlmResults$finalAnswer == "YES"]))
    }
  }

  #create full concept set using Phenelope (includes Phoebe and descendants)
  message("\nTesting viable concept candidates, their descendants, and PHOEBE recommendations...")

  if(length(conceptList) > 0) {
    llmResults <- .getFullConceptSet(conceptSetTarget = searchString,
                                     conceptIds = conceptList,
                                     llmClient = llmClient,
                                     minCount = minCount,
                                     belowMinimumCountApproach = belowMinimumCountApproach,
                                     domains = domains,
                                     excludedConcepts = excludedConcepts,
                                     conceptClasses = classes,
                                     clinicalDefinition = clinicalDefinition,
                                     clinicalContext = clinicalContext,
                                     connection = connection,
                                     connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     conditionForFiles = conditionForFiles,
                                     tryNumber = tryNumber,
                                     outputDirectory = outputDirectory,
                                     phoebeExclusions = phoebeExclusions,
                                     phoebeRepository = phoebeRepository,
                                     bucketSize = bucketSize)
  } else {
    message("There are no viable concept candidates for this concept set.")
    return(NULL)
  }
}

#' Provide the class of a drug object from a string appropriate for building a concept set
#'
#' @description
#' Provide the class of a drug object from a string appropriate for building a concept set
#'
#' @details
#' Provide the class of a drug object from a string appropriate for building a concept set
#'
#' @param drugName    a string that represents the drug of interest
#' @return a dataframe with various elements including the vocabulary class for the drug associated with the given string
#' @export
getDrugClass <- function(drugName) {
  ellmerTypeObject <- ellmer::type_array(ellmer::type_object(
    drugName = ellmer::type_string(),
    routeOfAdministrationYesNo = ellmer::type_enum(values = c("YES","NO")),
    routeOfAdministrationName = ellmer::type_string(),
    dosageFormYesNo = ellmer::type_enum(values = c("YES","NO")),
    dosageFormName = ellmer::type_string(),
    drugStrengthYesNo = ellmer::type_enum(values = c("YES","NO")),
    drugStrengthName = ellmer::type_string(),
    drugBrandYesNo = ellmer::type_enum(values = c("YES","NO")),
    drugBrandName = ellmer::type_string(),
    multipleDrugYesNo = ellmer::type_enum(values = c("YES","NO")),
    multipleDrugNames = ellmer::type_string(),
    singleGroupDrug = ellmer::type_enum(values = c("SINGLE","GROUP")),
    singleGroupName = ellmer::type_string(),
    indicationSpecificYesNo = ellmer::type_enum(values = c("YES","NO")),
    indication = ellmer::type_string()
  ))

  promptUp <- system.file("prompts", "DetermineDrugCharacteristics.txt", package = "Phenelope")
  originalLines <- readLines(promptUp)

  drugString <- drugName

  updatedLines <- gsub("DRUG_NAME", drugString, originalLines)

  prompt <- paste(updatedLines, collapse = "\n")

  drugNameInformation <- queryLLM(llmClient = llmClient, prompt = prompt, ellmerTypeObject = ellmerTypeObject)

  if(drugNameInformation$routeOfAdministrationYesNo == "NO" &
     drugNameInformation$dosageFormYesNo == "NO" &
     drugNameInformation$drugStrengthYesNo ==  "NO" &
     drugNameInformation$drugBrandYesNo ==  "NO" &
     drugNameInformation$multipleDrugYesNo == "NO") {
    class <- "Ingredient"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "YES" &
            drugNameInformation$dosageFormYesNo == "NO" &
            drugNameInformation$drugStrengthYesNo ==  "NO" &
            drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Clinical Drug Form"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "YES" &
            drugNameInformation$dosageFormYesNo == "YES" &
            drugNameInformation$drugStrengthYesNo ==  "NO" &
            drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Clinical Drug Form"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "NO" &
            drugNameInformation$dosageFormYesNo == "NO" &
            drugNameInformation$drugStrengthYesNo ==  "YES" &
            drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Clinical Drug Comp"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "YES" &
            # drugNameInformation$dosageFormYesNo == "YES" &
            drugNameInformation$drugStrengthYesNo ==  "YES" &
            drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Clinical Drug"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "YES" &
            drugNameInformation$dosageFormYesNo == "YES" &
            drugNameInformation$drugStrengthYesNo ==  "NO" &
            drugNameInformation$drugBrandYesNo ==  "YES" ) {
    class <- "Branded Drug Form"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "NO" &
            drugNameInformation$dosageFormYesNo == "NO" &
            drugNameInformation$drugStrengthYesNo ==  "YES" &
            drugNameInformation$drugBrandYesNo ==  "YES" ) {
    class <- "Branded Drug Comp"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "YES" &
            drugNameInformation$dosageFormYesNo == "YES" &
            drugNameInformation$drugStrengthYesNo ==  "YES" &
            drugNameInformation$drugBrandYesNo ==  "YES" ) {
    class <- "Branded Drug"
  } else if(drugNameInformation$multipleDrugYesNo == "YES")  { #best fit for multiple active ingredients
    class <- "Clinical Drug Form"
  } else {
    class <- "Unknown"
  }

  drugNameInformation <- data.frame(drugClass = class, drugNameInformation)

  return(drugNameInformation)
}

.getDrugConceptSet <- function(searchString,
                               originalConceptList = originalConceptList,
                               llmClientReasoning,
                               llmClientNonReasoning,
                               connectionDetails,
                               cdmDatabaseSchema,
                               clinicalDefinition = "",
                               outputDirectory,
                               clinicalContext = "",
                               bucketSize) {

  connection3 <- suppressMessages(DatabaseConnector::connect(connectionDetails = connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection3))

  #get drug class, e.g., ingredient, drug product, etc
  domains <- c("Drug")
  drugClass <- getDrugClass(searchString)

  if (file.exists(file.path(outputDirectory, paste0(searchString, "_from_LLM.csv")))) {
    # found the llm derived concepts, skip this part
    message(
      "File ",
      file.path(outputDirectory, paste0(searchString, "_from_LLM.csv")),
      " exists...skipping to next part of analysis."
    )
    fullDrugList <- utils::read.csv(file.path(outputDirectory, paste0(searchString, "_from_LLM.csv")))
  } else { #get the drugs from llm
    promptUp <- system.file("prompts", "createDrugList.txt", package = "Phenelope")
    originalLines <- readLines(promptUp)

    ellmerTypeObject <- ellmer::type_array(ellmer::type_object(
      term = ellmer::type_string(),
      drugName = ellmer::type_string()))

    passes <- 5
    startNumber <- 100
    nextNumber <- 50
    llmSearchString <- searchString

    if(drugClass$singleGroupDrug == "SINGLE") { #can reduce level of search for a single drug search
      passes <- 3
      startNumber <- 5
      nextNumber <- 5
      llmSearchString <- drugClass$singleGroupName
    }

    fullDrugList <- NULL
    cat(paste0("Finding list of drugs for ", searchString, "\n"))
    for(passUp in 1:passes) { # 3 passes for completeness sake
      updatedLines <- gsub("SEARCH_TERM", llmSearchString, originalLines)

      updatedLines <- gsub("CURRENT_LIST", paste0(fullDrugList$drugName, collapse = "; "), updatedLines)
      updatedLines <- gsub("ADDITIONAL_INFORMATION", clinicalDefinition, updatedLines)
      updatedLines <- gsub("START_NUMBER", startNumber, updatedLines)
      updatedLines <- gsub("NEXT_NUMBER", nextNumber, updatedLines)

      prompt <- paste(updatedLines, collapse = "\n")

      start <- Sys.time()
      drugList <- queryLLM(llmClient = llmClientReasoning, prompt = prompt, ellmerTypeObject = ellmerTypeObject)
      end <- Sys.time()
      elapsedSecs <- as.numeric(difftime(end, start, units = "secs"))
      if(passUp == 1) {initTime <- elapsedSecs}

      if(ncol(drugList) > 1) {
        drugList$passNumber <- passUp
        fullDrugList <- rbind(fullDrugList, drugList[, names(drugList) != "cost"])
      }

      fullDrugList <- unique(fullDrugList[, names(fullDrugList) != "cost"])
      # message (paste0("Search ", passUp, " of ", passes, " - Total drugs: ", nrow(fullDrugList)))
      cat(paste0("\rSearch ", passUp, " of ", passes, " - Total drugs: ", nrow(fullDrugList), "..."))

    }
    cat(paste0("done\n"))

    fullDrugList <- fullDrugList[, names(fullDrugList) != "cost"]

    utils::write.csv(fullDrugList, file.path(outputDirectory, paste0(searchString, "_from_LLM.csv")), row.names = F)
  }

  drugList <- unique(fullDrugList)

  limit <- 200
  standardConceptCode <- "S"

  if(drugClass$drugClass %in% c("Clinical Dose Group", "Branded Dose Group")) {
    standardConceptCode <- "C"
  }

  if(drugClass$drugClass %in% c("Ingredient")) {
    limit <- 1
  }

  if (file.exists(file.path(outputDirectory, paste0(searchString, "_from_embVectors.csv")))) {
    # found the hecate derived concepts, skip this part
    message(
      "File ",
      file.path(outputDirectory, paste0(searchString, "_from_embVectors.csv")),
      " exists...skipping to next part of analysis."
    )
    conceptList <- utils::read.csv(file.path(outputDirectory, paste0(searchString, "_from_embVectors.csv")))
  } else { #get the drugs concept id from hecate
    conceptList <- NULL
    for(drugUp in seq_len(nrow(drugList))) {
      cat(paste0("\rFinding concept Ids for drug ", drugUp, " of ", nrow(drugList)))

      seeds <- .vectorSearch(term = drugList$drugName[[drugUp]],
                             domains = domains,
                             conceptClasses = drugClass$drugClass,
                             vocabularyId = "RxNorm, RxNorm Extension",
                             standardConcept = standardConceptCode,
                             limit = limit,
                             maxRetries = 3,
                             waitTime = 2)

      if(length(seeds)) {
        conceptList <- rbind(conceptList, seeds)
      }
    }
    conceptList$conceptSetTarget <- conceptList$conceptName
    if(length(conceptList)) {
      utils::write.csv(conceptList, file.path(outputDirectory, paste0(searchString, "_from_embVectors.csv")), row.names = F)
    }
  }


  if (file.exists(file.path(outputDirectory, paste0(searchString, "1.csv")))) {
    # found the llm adjudicated concepts, skip this part
    message(
      "File ",
      file.path(outputDirectory, paste0(searchString, "1.csv")),
      " exists...skipping to next part of analysis."
    )
    llmConceptSet <- utils::read.csv(file.path(outputDirectory, paste0(searchString, "1.csv")))
  } else { #get llm to adjudicate the concepts
    conceptList <- conceptList[,c("conceptId", "conceptSetTarget", "domainId", "vocabularyId", "conceptClassId", "standardConcept",
                                  "conceptCode")]
    conceptList <- unique(conceptList)
    conceptList <- conceptList[!is.null(conceptList$conceptId),]

    promptToUse <- system.file("prompts", "LLM_Prompt_for_PHOEBE_drug.txt", package = "Phenelope")

    #double check the list by passing to the llm using Phenelope
    if(length(conceptList) > 0) {
      llmConceptSet <- .createRecommendListFromConcepts(query = searchString,
                                                        conceptList = conceptList$conceptId,
                                                        llmClient = llmClientReasoning,
                                                        prompt = promptToUse,
                                                        connectionDetails = connectionDetails,
                                                        connection = connection3,
                                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                                        excludedConcepts = "none",
                                                        clinicalDefinition = clinicalDefinition,
                                                        clinicalContext = clinicalContext,
                                                        conditionForFiles = conditionForFiles,
                                                        bucketSize = bucketSize)
    } else {
      message(paste0("No concept Ids found for ", searchString, " perhaps try a different drug name."))
      return(NULL)
    }
  }

  llmApprovedConcepts <- llmConceptSet[llmConceptSet$finalAnswer == "YES",]

  if(length(llmApprovedConcepts) == 0) {
    message(paste0("No LLM approved concept Ids found for ", searchString, " perhaps try a different drug name."))
    return(NULL)
  }

  return(llmConceptSet)
}

.getAllDomains <- function (conceptList,
                            connectionDetails,
                            cdmDatabaseSchema,
                            excludedVocabularies) {

  if(length(conceptList) == 0) {return(NULL)}

  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

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

  conceptList <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)
  conceptList$conceptSetTarget <- conceptList$conceptName

  domains <- unique(conceptList$domainId)

  return(domains)
}

#' Turn a concept set in json list form into a vector of all included concepts
#'
#' @description
#' Create a vector of all included concept ids from a json list object.
#'
#' @details
#' This function will create a vector of all included concept ids from a json list object.
#'
#' @param conceptSet    JSON object in list form
#' @param connectionDetails An R object of type connectionDetails created using the function createConnectionDetails in the
#'                          DatabaseConnector package.
#' @param cdmDatabaseSchema The name of the database schema that contains the OMOP CDM
#'                                   instance. Requires read permissions to this database. On SQL
#'                                   Server, this should specify both the database and the
#'                                   schema, so for example 'cdm_instance.dbo'.
#' @return a vector of concept ids
#' @export

resolveConceptSet <- function(conceptSet, #in list form
                              connectionDetails,
                              cdmDatabaseSchema) {

  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  json_txt <- paste(jsonlite::toJSON(conceptSet,
                                     pretty = TRUE,
                                     simplifyVector = FALSE,
                                     auto_unbox = TRUE), collapse = "\n")

  sql <- CirceR::buildConceptSetQuery(json_txt)
  sql <- SqlRender::render(sql = sql, vocabulary_database_schema = cdmDatabaseSchema)
  conceptIdList <- DatabaseConnector::querySql(connection = connection, sql, snakeCaseToCamelCase = TRUE)

  return(conceptIdList)
}

# log_call_params: log the parameters of the calling function to a CSV file
logCallParams <- function(output_dir,
                          filename_prefix = NULL,
                          exclude = character(),                      # names to always exclude
                          redact_patterns = c("password", "pass", "pwd",
                                              "secret", "token", "key",
                                              "connection", "cred", "private"),
                          redact_replace = "<REDACTED>",
                          parent = parent.frame()) {
  # Basic validation / ensure output dir
  if (missing(output_dir) || is.null(output_dir) || output_dir == "") {
    stop("Please provide a valid output_dir (e.g. outputDirectory from your function).")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Determine caller function and its formal args
  caller_index <- sys.parent()           # frame index of the caller
  # In rare circumstances sys.parent() can be 0; guard:
  if (caller_index == 0) caller_index <- 1
  caller_func <- tryCatch(sys.function(caller_index), error = function(e) NULL)
  formal_names <- if (!is.null(caller_func)) names(formals(caller_func)) else character(0)

  # Capture caller environment as a list and keep only formal args
  caller_env_list <- as.list(parent, all.names = TRUE)
  params <- caller_env_list[names(caller_env_list) %in% formal_names]

  # If no formal parameters were found, fall back to attempting to examine the call
  if (length(params) == 0 && !is.null(caller_func)) {
    # Attempt to evaluate formals in parent.frame() in case names exist but not bound as locals
    params <- list()
    for (n in formal_names) {
      if (exists(n, envir = parent, inherits = FALSE)) {
        params[[n]] <- get(n, envir = parent)
      } else {
        # try to capture default value from formals (not evaluated)
        default_val <- formals(caller_func)[[n]]
        if (!is.null(default_val)) {
          # attempt to evaluate default in the function's enclosing environment
          params[[n]] <- tryCatch(eval(default_val, envir = parent), error = function(e) "<default_not_evaluable>")
        } else {
          params[[n]] <- NULL
        }
      }
    }
  }

  # Apply explicit exclusions
  for (nm in intersect(names(params), exclude)) {
    params[[nm]] <- redact_replace
  }

  # Apply pattern-based redaction
  for (nm in names(params)) {
    if (any(vapply(redact_patterns, function(p) grepl(p, nm, ignore.case = TRUE), logical(1)))) {
      params[[nm]] <- redact_replace
    }
  }

  # Prepare human-readable string values for CSV
  param_to_string <- function(x) {
    if (is.null(x)) return("NULL")
    if (length(x) == 0) return("empty")
    # small atomic vectors -> join with semicolon
    if (is.atomic(x) && length(x) <= 10 && (is.numeric(x) || is.character(x) || is.logical(x))) {
      return(paste(as.character(x), collapse = ";"))
    }
    # for other objects, use dput capture (single-line)
    paste(utils::capture.output(dput(x)), collapse = " ")
  }

  param_strings <- vapply(params, param_to_string, FUN.VALUE = character(1), USE.NAMES = TRUE)

  df <- data.frame(
    parameter = names(param_strings),
    value = unname(param_strings),
    stringsAsFactors = FALSE
  )

  # Build filename: use caller name if available
  caller_name <- tryCatch({
    call_expr <- sys.call(caller_index)
    if (!is.null(call_expr)) as.character(call_expr[[1]]) else "caller"
  }, error = function(e) "caller")

  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  if (is.null(filename_prefix)) filename_prefix <- paste0(caller_name, "_params")
  filename <- file.path(output_dir, paste0(filename_prefix, "_", ts, ".csv"))

  # Write CSV
  write.csv(df, file = filename, row.names = FALSE, na = "")

  invisible(filename)
}

removeClearNo <- function(query,
                          conceptList,
                          llmClient,
                          clinicalDefinition = "",
                          clinicalContext = "",
                          bucketSize = 200) {

  promptUp <- system.file("prompts", "LLM_Prompt_for_PHOEBE_generic_clear_no.txt", package = "Phenelope")
  originalLines <- readLines(promptUp)

  results <- NULL
  startPoint <- 1
  endPoint <- min(bucketSize, nrow(conceptList))
  while(startPoint <= nrow(conceptList)) {
    cat(paste0("--Querying LLM for ", query, " - Analyzing concepts ", startPoint, " through ", endPoint, " of ", nrow(conceptList), "  \r"))
    conceptList$aboveMin[1] <- T # always test the first concept

    testCondition <- conceptList[startPoint:endPoint, c("conceptId", "conceptSetTarget")]
    baseCondition <- query

    updatedLines <- gsub("MAIN_CONCEPT", baseCondition, originalLines)

    json_all <- jsonlite::toJSON(testCondition)
    updatedLines <- gsub("SUGGESTED_CONCEPT", json_all, updatedLines)
    updatedLines <- gsub("CLINICAL_CONTEXT", clinicalContext, updatedLines)
    updatedLines <- gsub("ADDITIONAL_INFORMATION", clinicalDefinition, updatedLines)

    prompt <- paste(updatedLines, collapse = "\n")
    lastPrompt <- prompt
    Phenelope:::saveLastPrompt(prompt)

    systemPrompt <- "You are an expert medical doctor specializing in healthcare data analysis. Your primary function is to analyze healthcare data, including electronic health records, to infer causal relationships between exposures and health outcomes."

    llmClient$set_system_prompt(systemPrompt)

    ellmerTypeObject <- ellmer::type_array(ellmer::type_object(
      conceptId = ellmer::type_string(),
      suggestedConcept = ellmer::type_string(),
      rationale = ellmer::type_string()
    ))

    newConceptList <- queryLLM(llmClient = llmClient,
                               prompt,
                               systemPrompt = systemPrompt,
                               ellmerTypeObject = ellmerTypeObject)

    llmClient$set_turns(list()) # Reset the chat
    startPoint <- endPoint + 1
    endPoint <- min(startPoint + bucketSize, nrow(conceptList))

    if(nrow(newConceptList) > 0 & ncol(newConceptList) == 4) {
      newConceptList$tested <- T

      newConceptList$mainCondition <- baseCondition
      newConceptList$model <- llmClient$get_model()
      results <- rbind(results, newConceptList)
    }
  }


  return(results)
}

getClinicalSynonyms <- function(concept) {
  promptUp <- system.file("prompts", "clinicalSynonyms.txt", package = "Phenelope")
  originalLines <- readLines(promptUp)

  updatedLines <- gsub("MAIN_CONCEPT", concept, originalLines)

  prompt <- paste(updatedLines, collapse = "\n")
  lastPrompt <- prompt
  Phenelope:::saveLastPrompt(prompt)

  systemPrompt <- "You are an expert medical doctor specializing in healthcare data analysis. Your primary function is to analyze healthcare data, including electronic health records, to infer causal relationships between exposures and health outcomes."

  llmClient$set_system_prompt(systemPrompt)

  ellmerTypeObject <- ellmer::type_object(
    mainConcept = ellmer::type_string(),
    synonymousBucketNames = ellmer::type_string())

  results <- queryLLM(llmClient = llmClient,
                      prompt,
                      systemPrompt = systemPrompt,
                      ellmerTypeObject = ellmerTypeObject)

  if(nrow(results) > 0) {
    return(results)
  } else {
    return(NULL)
  }
}

getHecateSearchList <- function(hecateSearchString,
                                domains,
                                conceptClasses,
                                vectorSearchSize,
                                standardOnly) {
  #convert semi-colon separated search string into a vector
  searchVector <- trimws(strsplit(hecateSearchString, ";")[[1]])

  allSeeds <- NULL
  for(searchUp in 1:length(searchVector)) {
    if(standardOnly == TRUE) {
      seeds <- .vectorSearchStandard(term = searchVector[[searchUp]],
                                     domains = domains,
                                     conceptClasses = conceptClasses,
                                     limit = vectorSearchSize)
    } else {
      seeds <- .vectorSearch(term = searchVector[[searchUp]],
                             domains = domains,
                             # conceptClasses = classes,
                             limit = vectorSearchSize)
    }
    seeds <- cbind(rn = seq_len(nrow(seeds)), seeds)

    if(nrow(seeds) > 0){
      allSeeds <- rbind(allSeeds, seeds)
    }
  }
  allSeeds <- allSeeds[order(allSeeds$rn), ]
  return(allSeeds)
}
