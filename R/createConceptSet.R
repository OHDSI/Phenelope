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

#' Create concept set by calling the LLM
#'
#' @description
#' Build a concept set from a clinical condition and an initial list of concept ids.
#'
#' @details
#' This function will create a concept set starting from a clinical condition and a concept id.
#'
#' @param conceptName Character. Name of the concept pointing to the clinical condition.
#' @param originalConceptList Integer or character vector. List of concept ids to use as a starting point.
#' @param excludedConditions Character. Names of conditions to be excluded from the concept set.
#' @param llmClient connection object for the LLM client (see ellmer package for object details)
#' @param connectionDetails An R object of type connectionDetails created using the function createConnectionDetails in the
#'                          DatabaseConnector package.
#' @param cdmDatabaseSchema The name of the database schema that contains the OMOP CDM
#'                                   instance. Requires read permissions to this database. On SQL
#'                                   Server, this should specify both the database and the
#'                                   schema, so for example 'cdm_instance.dbo'.
#' @param tempEmulationSchema	 Some database platforms like Oracle and Impala do not truly support temp tables. To
#'                             emulate temp tables, provide a schema with write privileges where temp tables can be
#'                             created.
#' @param minCount Integer. Minimum cell subject count to use for concepts.
#' @param belowMinimumCountApproach Character. How to treat concepts below the minimum count. One of "TEST ALL", "TEST PHOEBE",
#'                                  "EXCLUDE ALL", "INCLUDE ALL". "TEST ALL" = test all the concepts below the minimum count;
#'                                  "TEST PHOEBE" = only test the concepts below minimum count that were recommended by PHOEBE;
#'                                  "EXCLUDE ALL" = exclude from the concept set any concept below the minimum count;
#'                                  "INCLUDE ALL" = include all the concepts below the minimum count.
#' @param outputDirectory Character. Directory to save output artifacts.
#' @param tries Integer. Number of attempts to try for each concept. The package allows for multiple runs of the same concept to get a
#'              consensus vote from multiple LLM iterations.
#' @param successes Integer. How many successes required to include a concept. The package allows for multiple runs of the same concept to
#'                  get a consensus vote from multiple LLM iterations.
#' @param additionalInformation Character. Additional information for concept development.  This may include any specific details that
#'                              are desired for the concepts, for example, "only in women"
#' @param clinicalContext Character. Optional clinical context for the LLM to determine appropriateness of a concept, for example,
#'                                  "following surgery" would include concepts whose name indicates it happened post-surgery.
#' @param excludedVocabularies      Vocabularies not to be included in the condensing function
#' @return Final results set as a list of two elements 1) a data frame of the LLM results for each tested concept
#'                                                     and 2) a JSON object ready for porting into ATLAS if successful, FALSE if unsuccessful.
#' @export
createConceptSet <- function(conceptName,
                             originalConceptList,
                             excludedConditions = "none",
                             llmClient,
                             connectionDetails,
                             cdmDatabaseSchema,
                             tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                             minCount = 0,
                             belowMinimumCountApproach = "TEST ALL", # "TEST PHOEBE", "EXCLUDE ALL", "INCLUDE ALL"
                             outputDirectory,
                             tries = 1,
                             successes = 1,
                             additionalInformation = "",
                             excludedVocabularies = c("ICDO3"),
                             clinicalContext = "") {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertCharacter(cdmDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertNumeric(minCount, add = errorMessages)
  checkmate::assertNumeric(tries, add = errorMessages)
  checkmate::assertNumeric(successes, add = errorMessages)

  checkmate::assertCharacter(excludedConditions, len = 1, add = errorMessages)

  checkmate::assertCharacter(conceptName, len = 1, add = errorMessages)
  checkmate::assertIntegerish(originalConceptList, min.len = 1, add = errorMessages)
  checkmate::assertCharacter(belowMinimumCountApproach, len = 1, add = errorMessages)
  checkmate::assertChoice(belowMinimumCountApproach,
    choices = c(
      "TEST ALL",
      "TEST PHOEBE",
      "EXCLUDE ALL",
      "INCLUDE ALL"
    ),
    add = errorMessages
  )
  checkmate::assertCharacter(outputDirectory, len = 1, add = errorMessages)
  checkmate::assertCharacter(additionalInformation, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(clinicalContext, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(clinicalContext, len = 1, null.ok = TRUE, add = errorMessages)

  checkmate::reportAssertions(collection = errorMessages)

  DatabaseConnector::assertTempEmulationSchemaSet(
    dbms = connectionDetails$dbms,
    tempEmulationSchema = tempEmulationSchema
  )
  message("\nDeveloping a concept set for: ", conceptName, "\n")
  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  if (!dir.exists(outputDirectory)) {
    success <- dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)
    if (!success) stop("Failed to create directory: ", outputDirectory)
  }

  conditionForFiles <- gsub("/", "-", conceptName) # remove slashes
  conditionForFiles <- paste(utils::head(unlist(strsplit(conditionForFiles, " ")), 3), collapse = " ")
  if (excludedConditions == "") {
    excludedConditions <- "None"
  }

  # create recommended concept set  list(s) based on number of iterations requested
  message("\nUsing model: ", llmClient$get_model(), "\n")
  for (tryNumber in 1:tries) {
    message("Try = ", tryNumber, " out of ", tries)
    if (file.exists(file.path(outputDirectory, paste0(conditionForFiles, tryNumber, ".csv")))) {
      # skip to next iteration if output file exists
      message(
        "File ",
        file.path(outputDirectory, paste0(conditionForFiles, tryNumber, ".csv")),
        " exists...skipping to next iteration."
      )
      phoebeResults <- utils::read.csv(file.path(outputDirectory, paste0(conditionForFiles, tryNumber, ".csv")))
      next
    }

    message("Testing concepts and descendants. ")

    if (file.exists(file.path(outputDirectory, paste0("firstPart_", conditionForFiles, tryNumber, ".csv")))) {
      # found the first half but not the full analysis, skip the first part and go to the second part
      message(
        "File ",
        file.path(outputDirectory, paste0("firstPart_", conditionForFiles, tryNumber, ".csv")),
        " exists...skipping to next part of analysis."
      )
      phoebeResults <- utils::read.csv(file.path(outputDirectory, paste0("firstPart_", conditionForFiles, tryNumber, ".csv")))
    } else {
      phoebeResults <- .createRecommendListViaLlmFromConceptList(
        query = conceptName,
        closestConditionConcept = conceptName,
        conceptList = originalConceptList,
        llmClient,
        connection = connection,
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        type = "phoebe",
        minCount = minCount,
        previousResults = NULL,
        excludedConditions = excludedConditions,
        belowMinimumCountApproach,
        additionalInformation = additionalInformation,
        clinicalContext = clinicalContext,
        excludedVocabularies = c(excludedVocabularies)
      )

      utils::write.csv(phoebeResults, file.path(outputDirectory, paste0("firstPart_", conditionForFiles, tryNumber, ".csv")), row.names = F)
    }

    previousResults <- phoebeResults

    included <- unique(c(as.integer(phoebeResults$conceptId[phoebeResults$finalAnswer == "YES"])))

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

    ancestorList <- querySql(connection, sql, snakeCaseToCamelCase = TRUE)

    conceptList <- conceptList[!(conceptList %in% c(unlist(ancestorList)))]

    phoebeResults <- .createRecommendListViaLlmFromConceptList(
      query = conceptName,
      closestConditionConcept = conceptName,
      conceptList = conceptList,
      llmClient,
      connection = connection,
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      type = "included",
      minCount = minCount,
      previousResults = previousResults,
      excludedConditions = excludedConditions,
      belowMinimumCountApproach,
      additionalInformation = additionalInformation,
      clinicalContext = clinicalContext,
      excludedVocabularies = c(excludedVocabularies)
    )

    # save to dataframe as a csv
    utils::write.csv(phoebeResults, file.path(outputDirectory, paste0(conditionForFiles, tryNumber, ".csv")), row.names = F)
  }

  # create a master concept set based on the requested number of required successes
  # read first iteration
  joinedDf <- utils::read.csv(file.path(outputDirectory, paste0(conditionForFiles, tryNumber, ".csv")))
  joinedDfAll <- joinedDf
  joinedDf <- joinedDf[joinedDf$finalAnswer == "YES", ]

  if (tries > 1) {
    for (joinUp in 2:tries) {
      nextData <- utils::read.csv(file.path(outputDirectory, paste0(conditionForFiles, joinUp, ".csv")))
      nextDataAll <- nextData
      nextData <- nextData[nextData$finalAnswer == "YES", ]

      joinedDf <- joinedDf |> full_join(nextData, by = "conceptId")
      joinedDfAll <- joinedDfAll |> full_join(nextDataAll, by = "conceptId")
    }
  }

  # Prefixes to bring to the head
  prefixes <- c("suggestedCondition", "suggestedCondition.x", "conceptId", "finalAnswer")

  # Create a regex pattern for the prefixes
  pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")")

  # Get the column names that match any of the prefixes
  matchedColumns <- grep(pattern, names(joinedDfAll), value = TRUE)

  # Get the remaining columns

  remainingColumns <- setdiff(names(joinedDfAll), matchedColumns)
  # Sort the remaining columns alphabetically
  remainingColumnsSorted <- remainingColumns[order(remainingColumns)]

  # Reorder the columns
  joinedDfAll <- joinedDfAll[, c(matchedColumns, remainingColumnsSorted)]

  if (tries > 1) {
    utils::write.csv(joinedDfAll,
      file.path(outputDirectory, paste0(conditionForFiles, "_all_results.csv")),
      row.names = F
    )
  }

  # Combine responses into one column and count "YES" responses
  countDf <- joinedDf |>
    tidyr::pivot_longer(cols = starts_with("finalAnswer"), names_to = "Source", values_to = "finalAnswer") |>
    group_by(.data$conceptId) |>
    summarize(Yes_Count = sum(.data$finalAnswer == "YES", na.rm = TRUE), .groups = "drop")

  finalSet <- c(countDf$conceptId[countDf$Yes_Count >= successes])
  if (length(finalSet) == 0) { # zero yes values in assessment
    message("NOTE: There were no concepts included in the concept set.")
    return(NULL)
  }
  conceptSet <- cs(as.integer(unlist(finalSet)), name = conditionForFiles)

  conceptSet <- Capr::getConceptSetDetails(conceptSet, connection, vocabularyDatabaseSchema = cdmDatabaseSchema)
  conceptSet <- jsonlite::fromJSON(as.json(conceptSet))

  retryLimit <- 10 # Maximum number of retries
  attempt <- 0 # Initial attempt counter
  success <- FALSE # Flag to indicate success

  # initial write of code list
  write(
    jsonlite::toJSON(conceptSet,
      simplifyVector = FALSE,
      auto_unbox = TRUE
    ),
    file = file.path(outputDirectory, paste0(conditionForFiles, ".json"))
  )

  while (attempt <= retryLimit && !success) { # llm with mislabel column headers occasionally - usually fixed with a re-try
    tryCatch(
      {
        attempt <- attempt + 1 # Increment the attempt count
        # Fetch data for concept set
        conceptSetData <- fetchCondenserConceptSetData(
          conceptSetExpression = conceptSet,
          connection = connection,
          cdmDatabaseSchema = cdmDatabaseSchema,
          tempEmulationSchema = tempEmulationSchema,
          excludedVocabularies = excludedVocabularies
        )

        # Main condenser function ------------------------------------------------------
        condensedConceptSet <- condenseConceptSet(conceptSetData)
        write(jsonlite::toJSON(condensedConceptSet, pretty = TRUE, simplifyVector = FALSE, auto_unbox = TRUE),
          file = file.path(outputDirectory, paste0(conditionForFiles, ".json"))
        )
        message("The artifacts from the process may be found at: ", file.path(outputDirectory))

        success <- TRUE
      },
      error = function(e) {
        # Handle the error: print a message and increment the attempt counter
        message(paste("Attempt", attempt, "failed:", e$message))
        if (grepl("abort", e$message, ignore.case = TRUE)) {
          cat("Stopping the run as requested.\n")
          stop("Execution stopped by user.")
        }
        if (attempt >= retryLimit) {
          message(paste("Reached attempt limit."))
          cat("Stopping the run as requested.\n")

          message("The artifacts from the process may be found at: ", file.path(outputDirectory))

          stop("Execution stopped by user.")
        }
        return(FALSE) # Return FALSE in case of error
      }
    )
  }

  if (exists("phoebeResults")) {
    return(list(testedConcepts = phoebeResults, conceptSet = condensedConceptSet))
  } else {
    return(NULL)
  }
}
