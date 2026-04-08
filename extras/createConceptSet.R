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
#' @param vocabularyDatabaseSchema Character. The database vocabulary to use.
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
#' @param vocabularies Character vector. Vocabularies to consider (e.g. c('SNOMED','HCPCS')).
#' @param quickRun Logical. Quick run flag. Used to get a quick assessment of certain concepts.  The only concepts that will be tested are
#'                 the ones passed in the concept list (above).
#'
#' @return Final results set as a data frame if successful, FALSE if unsuccessful.
#' @export
createConceptSet <- function(conceptName,
                                    originalConceptList,
                                    excludedConditions = "none",
                                    llmClient,
                                    connectionDetails,
                                    cdmDatabaseSchema,
                                    vocabularyDatabaseSchema,
                                    minCount = 0,
                                    belowMinimumCountApproach = "TEST ALL", # "TEST PHOEBE", "EXCLUDE ALL", "INCLUDE ALL"
                                    outputDirectory,
                                    tries = 1,
                                    successes = 1,
                                    additionalInformation = "",
                                    clinicalContext = "",
                                    vocabularies = c('SNOMED', 'HCPCS','OMOP Extension'),
                                    quickRun = F) {

  if(!(belowMinimumCountApproach %in% c("TEST ALL", "TEST PHOEBE", "EXCLUDE ALL", "INCLUDE ALL"))) {
    ParallelLogger::logInfo("belowMinimumCountApproach must be one of the following: TEST ALL, TEST PHOEBE, EXCLUDE ALL, INCLUDE ALL")
    stop("...correct and restart.")
  }

  if (!dir.exists(outputDirectory)) {
    success <- dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)
    if (!success) stop("Failed to create directory: ", outputDirectory)
  }

  conditionForFiles <- gsub("/", "-", conceptName) #remove slashes
  conditionForFiles <- paste(utils::head(unlist(strsplit(conditionForFiles, " ")), 3), collapse = " ")
  if(excludedConditions == "") {excludedConditions <- "None"}

  if(quickRun == F) {
    #step 0 - create or read clinical description
    ParallelLogger::logInfo("Creating clinical description from LLM")
    clinicalDescription <- createClinicalDescription(condition = conceptName, excludedConditions, outputDirectory, llmClient)
    fullDefinition <- paste(officer::docx_summary(officer::read_docx(clinicalDescription))$text, collapse = "\n")
  }

  #create recommended concept set  list(s) based on number of iterations requested
  ParallelLogger::logInfo("\nUsing model: ", llmClient$get_model(), "\n")
  for(tryNumber in 1:tries) {
    tryUp <- tryNumber
    ParallelLogger::logInfo("Try = ", tryNumber, " out of ", tries)
    if(file.exists(file.path(outputDirectory,paste0(conditionForFiles,tryNumber,".csv"))) & quickRun == F) { #skip to next iteration if output file exists
      ParallelLogger::logInfo("File ", file.path(outputDirectory,paste0(conditionForFiles,tryNumber,".csv")), " exists...skipping to next iteration.")
      next
    }

    if(quickRun == T) {
      conceptList <- originalConceptList #will skip the first 2 passes if only want a quick run (to test a few concepts)
      previousResults <- NULL
    } else {
      ParallelLogger::logInfo("Testing concepts and descendants. ")
      phoebeResults <- .createRecommendListViaLlmFromConceptList(query = conceptName,
                                                                 closestConditionConcept = conceptName,
                                                                 conceptList = originalConceptList,
                                                                 llmClient,
                                                                 connectionDetails = connectionDetails,
                                                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                                                 type = "phoebe",
                                                                 minCount = minCount,
                                                                 previousResults = NULL,
                                                                 excludedConditions = excludedConditions,
                                                                 belowMinimumCountApproach,
                                                                 additionalInformation = additionalInformation,
                                                                 clinicalContext = clinicalContext,
                                                                 vocabularies,
                                                                 quickRun = quickRun)

      previousResults <- phoebeResults

      included <- unique(c(as.integer(phoebeResults$conceptId[phoebeResults$finalAnswer == 'YES'])))

      conceptList <- unique(c(originalConceptList, included))
    }
    ParallelLogger::logInfo("Testing final set of included concepts.")

    #remove ancestors of the original concept set list from the list (don't want to include their descendants)
    connection <- suppressMessages(connect(connectionDetails = connectionDetails))
    sql <- paste0("select distinct ancestor_concept_id ",
                  "from ", cdmDatabaseSchema, ".concept_ancestor ca ",
                  "where descendant_concept_id in (", paste(originalConceptList, collapse = ", "), ") ",
                  "and ancestor_concept_id not in (", paste(originalConceptList, collapse = ", "), ") ")

    ancestorList <- querySql(connection, sql, snakeCaseToCamelCase = TRUE)
    conceptList <- conceptList[!(conceptList %in% c(unlist(ancestorList)))]

    phoebeResults <- .createRecommendListViaLlmFromConceptList(query = conceptName,
                                                               closestConditionConcept = conceptName,
                                                               conceptList = conceptList,
                                                               llmClient,
                                                               connectionDetails = connectionDetails,
                                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                                               type = "included",
                                                               minCount = minCount,
                                                               previousResults = previousResults,
                                                               excludedConditions = excludedConditions,
                                                               belowMinimumCountApproach,
                                                               additionalInformation = additionalInformation,
                                                               clinicalContext = clinicalContext,
                                                               vocabularies,
                                                               quickRun = quickRun)

    # save to dataframe as a csv
    utils::write.csv(phoebeResults, file.path(outputDirectory,paste0(conditionForFiles,tryNumber,".csv")), row.names = F)
  }

  if(quickRun == F) {
    #create a master concept set based on the requested number of required successes
    #read first iteration
    joined_df <- utils::read.csv(file.path(outputDirectory,paste0(conditionForFiles, tryNumber,".csv")))
    joined_df_all <- joined_df
    joined_df <- joined_df[joined_df$finalAnswer == "YES",]

    if(tries > 1) {
      for(joinUp in 2:tries) {
        nextData <- utils::read.csv(file.path(outputDirectory,paste0(conditionForFiles,joinUp,".csv")))
        nextDataAll <- nextData
        nextData <- nextData[nextData$finalAnswer == "YES",]

        joined_df <- joined_df |> full_join(nextData, by = "conceptId")
        joined_df_all <- joined_df_all |> full_join(nextDataAll, by = "conceptId")
      }
    }

    # Prefixes to bring to the head
    prefixes <- c("suggestedCondition","suggestedCondition.x", "conceptId", "finalAnswer")

    # Create a regex pattern for the prefixes
    pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")")

    # Get the column names that match any of the prefixes
    matched_columns <- grep(pattern, names(joined_df_all), value = TRUE)

    # Get the remaining columns

    remaining_columns <- setdiff(names(joined_df_all), matched_columns)
    # Sort the remaining columns alphabetically
    remaining_columns_sorted <- remaining_columns[order(remaining_columns)]

    # Reorder the columns
    joined_df_all <- joined_df_all[, c(matched_columns, remaining_columns_sorted)]

    if(tries > 1) {
      utils::write.csv(joined_df_all, file.path(outputDirectory,paste0(conditionForFiles,"_all_results.csv")), row.names = F)
    }

    # Combine responses into one column and count "YES" responses
    count_df <- joined_df |>
      tidyr::pivot_longer(cols = starts_with("finalAnswer"), names_to = "Source", values_to = "finalAnswer") |>
      group_by(.data$conceptId) |>
      summarize(Yes_Count = sum(.data$finalAnswer == "YES", na.rm = TRUE), .groups = 'drop')

    finalSet <- c(count_df$conceptId[count_df$Yes_Count >= successes])
    conceptSet <- cs(as.integer(unlist(finalSet)), name = conditionForFiles)

    connection <- suppressMessages(connect(connectionDetails = connectionDetails))
    conceptSet <- getConceptSetDetails(conceptSet, connection, vocabularyDatabaseSchema = vocabularyDatabaseSchema)
    disconnect(connection)

    conceptSet <- jsonlite::fromJSON(as.json(conceptSet))

    retry_limit <- 5  # Maximum number of retries
    attempt <- 1      # Initial attempt counter
    success <- FALSE  # Flag to indicate success

    #initial write of code list
    write(jsonlite::toJSON(conceptSet, pretty = TRUE), file = file.path(outputDirectory,paste0(conditionForFiles,".json")))

    while (attempt <= retry_limit && !success) { #llm with mislabel column headers occasionally - usually fixed with a re-try
      tryCatch({
        connection <- suppressMessages(connect(connectionDetails = connectionDetails))

        # Fetch data for concept set
        conceptSetData <- fetchCondenserConceptSetData(
          conceptSetExpression = conceptSet,
          connection = connection,
          cdmDatabaseSchema = cdmDatabaseSchema
        )

        # Main condenser function ------------------------------------------------------
        condensedConceptSet <- condenseConceptSet(conceptSetData)
        condensedConceptSet$items <- Filter(
          function(el) el$concept$VOCABULARY_ID %in% vocabularies,
          condensedConceptSet$items
        )
        write(jsonlite::toJSON(condensedConceptSet, pretty = TRUE), file = file.path(outputDirectory,paste0(conditionForFiles,".json")))
        ParallelLogger::logInfo("The artifacts from the process may be found at: ", file.path(outputDirectory))

        success <- TRUE
      },
      error = function(e) {
        # Handle the error: print a message and increment the attempt counter
        message(paste("Attempt", attempt, "failed:", e$message))
        if(grepl("abort", e$message, ignore.case=TRUE)) {
          cat("Stopping the run as requested.\n")
          stop("Execution stopped by user.")
        }
        attempt <- attempt + 1  # Increment the attempt count
        if(attempt >= retry_limit) {
          message(paste("Reached attempt limit."))
          cat("Stopping the run as requested.\n")

          ParallelLogger::logInfo("The artifacts from the process may be found at: ", file.path(outputDirectory))

          stop("Execution stopped by user.")
        }
        return(FALSE)  # Return FALSE in case of error
      })
    }
  }
  if(exists("phoebeResults")) {
    return(phoebeResults)
  } else {
    return(NULL)
  }
}
