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
#' @param conceptSetTarget Character. Name of the concept pointing to the clinical condition.
#' @param originalConceptList Integer or character vector. List of concept ids to use as a starting point.
#' @param excludedConcepts Character. Names of concepts to be excluded from the concept set.
#' @param llmClientReasoning connection object for the LLM client (see ellmer package for object details) for a reasoning model such as OpenAI o3
#' @param llmClientNonReasoning connection object for the LLM client (see ellmer package for object details) for a non-reasoning model such as OpenAI 4o
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
#' @param clinicalDefinition Character. Clinical definition for concept setdevelopment.  This may include any specific details that
#'                              are desired for the concepts, for example, "only in women"
#' @param clinicalContext Character. Optional clinical context for the LLM to determine appropriateness of a concept, for example,
#'                                  "following surgery" would include concepts whose name indicates it happened post-surgery.
#' @param excludedVocabularies      Vocabularies not to be included in the condensing function
#' @param condenseConceptSet      True/False to perform condenser function
#' @param bucketSize          Number of concepts for LLM to analyze in one pass - Note: larger number may reduce accuracy of evaluation
#' @param phoebeRepository    Location of the phoebe repository - either HECATE or DATABASE
#' @param standardOnly        T/F - if true, only allow standard concepts, if false, any concepts
#' @param quickRun    T/F - if true, will simply test the concepts in the concept list, i.e., no PHOEBE, descendants
#' @return Final results set as a list of two elements 1) a data frame of the LLM results for each tested concept
#'                                                     and 2) a JSON object ready for porting into ATLAS if successful, FALSE if unsuccessful.
#' @export
createConceptSet <- function(conceptSetTarget,
                             originalConceptList = c(),
                             excludedConcepts = "none",
                             llmClientReasoning,
                             llmClientNonReasoning = llmClientReasoning,
                             connectionDetails,
                             cdmDatabaseSchema,
                             tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                             minCount = 0,
                             belowMinimumCountApproach = "TEST ALL", # "TEST PHOEBE", "EXCLUDE ALL", "INCLUDE ALL"
                             outputDirectory,
                             tries = 1,
                             successes = 1,
                             clinicalDefinition = "",
                             excludedVocabularies = c("ICDO3"),
                             condenseConceptSet = TRUE,
                             clinicalContext = "any clinical context",
                             bucketSize = 20,
                             phoebeRepository = "HECATE",
                             standardOnly = TRUE,
                             quickRun = FALSE) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertR6(llmClientReasoning, "Chat", add = errorMessages)
  checkmate::assertR6(llmClientNonReasoning, "Chat", add = errorMessages)
  checkmate::assertCharacter(cdmDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertNumeric(minCount, add = errorMessages)
  checkmate::assertNumeric(tries, add = errorMessages)
  checkmate::assertNumeric(successes, add = errorMessages)

  checkmate::assertCharacter(excludedConcepts, len = 1, add = errorMessages)

  checkmate::assertCharacter(conceptSetTarget, len = 1, add = errorMessages)
  if(length(originalConceptList) > 0) {
    checkmate::assertIntegerish(originalConceptList, min.len = 0, add = errorMessages)
  }
  checkmate::assertCharacter(belowMinimumCountApproach, len = 1, add = errorMessages)
  checkmate::assertChoice(belowMinimumCountApproach,
                          choices = c(
                            "TEST ALL",
                            "TEST PHOEBE",
                            "EXCLUDE ALL",
                            "INCLUDE ALL"
                          ),
                          add = errorMessages)
  checkmate::assertCharacter(outputDirectory, len = 1, add = errorMessages)
  checkmate::assertCharacter(clinicalDefinition, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(clinicalContext, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(clinicalContext, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertLogical(condenseConceptSet, add = errorMessages)
  checkmate::assertNumeric(bucketSize, add = errorMessages)
  checkmate::assertChoice(phoebeRepository,
                          choices = c(
                            "HECATE",
                            "DATABASE"
                          ),
                          add = errorMessages)
  checkmate::assertLogical(standardOnly, add = errorMessages)
  checkmate::assertLogical(quickRun, add = errorMessages)

  checkmate::reportAssertions(collection = errorMessages)

  # log params, explicitly exclude connectionDetails if you want (extra safety)
  logCallParams(output_dir = outputDirectory,
                  exclude = c("connectionDetails"),
                  filename_prefix = "createConceptSet")

  DatabaseConnector::assertTempEmulationSchemaSet(
    dbms = connectionDetails$dbms,
    tempEmulationSchema = tempEmulationSchema
  )
  llmClient <- llmClientReasoning #use the reasoning model for most instances

  message("\nDeveloping a concept set for: ", conceptSetTarget, "\n")
  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  if (!dir.exists(outputDirectory)) {
    success <- dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)
    if (!success) stop("Failed to create directory: ", outputDirectory)
  }

  conditionForFiles <- gsub("/", "-", conceptSetTarget) # remove slashes
  conditionForFiles <- paste(utils::head(unlist(strsplit(conditionForFiles, " ")), 100), collapse = " ")
  if (excludedConcepts == "") {
    excludedConcepts <- "None"
  }

  #get domain to determine analysis
  domainToUse <- .getDomain(llmClient = llmClient, searchString = conceptSetTarget)$domain[[1]]

  if(domainToUse %in% c("DRUG")) { #concept set for drugs
    condenseConceptSet <- FALSE #don't need to do this for drugs
    csConceptPlusDescendants <- TRUE #final concept set for drugs will be all concepts plus descendants
  } else {
    condenseConceptSet <- TRUE
    csConceptPlusDescendants <- FALSE
  }

  # create recommended concept set  list(s) based on number of iterations requested
  for (tryNumber in 1:tries) {
    message("Try = ", tryNumber, " out of ", tries)
    if (file.exists(file.path(outputDirectory, paste0(conditionForFiles, tryNumber, ".csv")))) {
      # skip to next iteration if output file exists
      message(
        "File ",
        file.path(outputDirectory, paste0(conditionForFiles, tryNumber, ".csv")),
        " exists...skipping to next iteration."
      )
      llmResults <- utils::read.csv(file.path(outputDirectory, paste0(conditionForFiles, tryNumber, ".csv")))
      next
    }

    searchString <- gsub(" codes", "", conceptSetTarget) #strip off the suffix

    if(quickRun == FALSE) {#need to go through the multi-stage process rather than a simple test
      #NOTE: currently not using classes - leaving them in as placeholder for future
      if(domainToUse %in% c("CONDITION")) { #full analysis with phoebe, descendants for conditions and observations
        #get seed concept ids from hecate
        domains <- c("Condition", "Observation")
        classes <- c("Disorder", "HCPCS", 	"Clinical Observation", "Clinical Finding")
        phoebeExclusions <- c() #no exclusions for conditions
        vectorSearchSize <- 25

      } else if(domainToUse %in% c("PROCEDURE")) {
        domains <- c("Procedure","Device", "Observation")
        classes <- c("Procedure", "CPT4", "Clinical Observation")
        phoebeExclusions <- c("Ontology-parent") #not valuable for procedures
        vectorSearchSize <- 200

      } else if(domainToUse %in% c("MEASUREMENT")) {
        #get seed concept ids from hecate
        domains <- c("Measurement", "Observation")
        classes <- c("CPT4", "Clinical Observation", "Procedure", "Lab Test")
        phoebeExclusions <- c("Ontology-parent") #not valuable for measurements
        vectorSearchSize <- 200

      } else if(domainToUse %in% c("VISIT")) {
        #get seed concept ids from hecate
        domains <- c("Visit", "Provider", "Procedure", "Observation")
        classes <- c("Visit")
        phoebeExclusions <- c("Ontology-parent") #not valuable for visits
        vectorSearchSize <- 200

        #test to see if it is for a specialty visit
        ellmerTypeObject <- ellmer::type_array(ellmer::type_object(
          visitName = ellmer::type_string(),
          yesNo = ellmer::type_enum(values = c("YES","NO")),
          specialtyName = ellmer::type_string()
        ))

        prompt <- paste0("Does ", searchString, " imply a visit involving a clinical specialist? ",
                         "If yes, what is the name of the clinical specialty? ",
                         "NOTE: any mention of a specific health condition for a visit infers a specialist is present",
                         "  {
                            \"visitName\": \"Name of visit code in question\",
                            \"yesNo\": \"YES or NO\",
                            \"specialtyName\": \"Name of specialty\"
                            }")

        specialty <- queryLLM(llmClient = llmClient, prompt = prompt, ellmerTypeObject = ellmerTypeObject)

        if(specialty$yesNo == "YES") { #this will ensure that the specialty provider and the visit type is included
          searchString <- paste0(searchString, " (specialty)")
          standardOnly <- FALSE #special for visits

        }

      } else if(domainToUse %in% c("DEVICE")) {
        #get seed concept ids from hecate
        domains <- c("Procedure", "Device", "Observation")
        classes <- c("Physical Object")
        phoebeExclusions <- c("Ontology-parent") #not valuable for devices
        vectorSearchSize <- 200

      } else if(domainToUse %in% c("DRUG")) {
        #get seed concept ids from hecate
        domains <- c("DRUG")
      }

      if(domainToUse %in% c("DRUG")) { #concept set for drugs
        llmResults <- .getDrugConceptSet(searchString = searchString,
                                         originalConceptList = originalConceptList,
                                         connectionDetails = connectionDetails,
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         llmClientReasoning,
                                         llmClientNonReasoning,
                                         clinicalDefinition = clinicalDefinition,
                                         outputDirectory = outputDirectory,
                                         clinicalContext = clinicalContext,
                                         bucketSize = bucketSize)

      } else { #concept set for all others
        #create the concept sets for the item
        llmResults <- .grabConcepts(searchString = searchString,
                                    originalConceptList = originalConceptList,
                                    llmClient = llmClient,
                                    domains = domains,
                                    classes = NULL, #classes,
                                    excludedConcepts = excludedConcepts,
                                    vectorSearchSize = vectorSearchSize,
                                    connectionDetails = connectionDetails,
                                    connection = connection,
                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                    clinicalDefinition = clinicalDefinition,
                                    clinicalContext = clinicalContext,
                                    minCount = minCount,
                                    belowMinimumCountApproach = belowMinimumCountApproach,
                                    conditionForFiles = conditionForFiles,
                                    tryNumber = tryNumber,
                                    outputDirectory = outputDirectory,
                                    phoebeExclusions = phoebeExclusions,
                                    standardOnly = standardOnly,
                                    bucketSize = bucketSize,
                                    phoebeRepository = phoebeRepository)

      }

    } else { #quick run - just test a set of concepts
      if(domainToUse %in% c("DRUG")) { #concept set for drugs
        if(bucketSize > 1) {
          promptToUse <- system.file("prompts", "LLM_Prompt_for_PHOEBE_drug.txt", package = "Phenelope")
        } else {
          promptToUse <- system.file("prompts", "LLM_Prompt_for_PHOEBE_single_drug.txt", package = "Phenelope")
        }
      } else {
        if(bucketSize > 1) {
          promptToUse <- system.file("prompts", "LLM_Prompt_for_PHOEBE_generic.txt", package = "Phenelope")
        } else {
          promptToUse <- system.file("prompts", "LLM_Prompt_for_PHOEBE_single_generic.txt", package = "Phenelope")
        }
      }

      llmResults <- .createRecommendListFromConcepts(query = conceptSetTarget,
                                                     conceptList = originalConceptList,
                                                     prompt = promptToUse,
                                                     llmClient = llmClientNonReasoning,
                                                     connection = connection,
                                                     connectionDetails = connectionDetails,
                                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                                     excludedConcepts = excludedConcepts,
                                                     clinicalDefinition = clinicalDefinition,
                                                     clinicalContext = clinicalContext,
                                                     conditionForFiles = conditionForFiles,
                                                     bucketSize = bucketSize)
      return(llmResults)
    }

    # save to dataframe as a csv
    if(!is.null(llmResults)) {
      utils::write.csv(llmResults, file.path(outputDirectory, paste0(conditionForFiles, tryNumber, ".csv")), row.names = F)
    } else { #no concepts are appropriate
      return(NULL)
    }
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
  prefixes <- c("suggestedConcept", "suggestedConcept.x", "conceptId", "finalAnswer")

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

  #get a list of the domains included in the concept set
  allDomains <- .getAllDomains(conceptList = c(joinedDfAll$conceptId[joinedDfAll$finalAnswer == "YES"]),
                               connectionDetails = connectionDetails,
                               cdmDatabaseSchema = cdmDatabaseSchema,
                               excludedVocabularies = excludedVocabularies)

  domainList <- data.frame(domainId = allDomains, stringsAsFactors = FALSE)
  write.csv(domainList, file.path(outputDirectory, "domains.csv"), row.names = FALSE, quote = TRUE)

  # Combine responses into one column and count "YES" responses
  countDf <- joinedDf |>
    tidyr::pivot_longer(cols = starts_with("finalAnswer"), names_to = "Source", values_to = "finalAnswer") |>
    dplyr::group_by(.data$conceptId) |>
    dplyr::summarize(Yes_Count = sum(.data$finalAnswer == "YES", na.rm = TRUE), .groups = "drop")

  finalSet <- c(countDf$conceptId[countDf$Yes_Count >= successes])
  if (length(finalSet) == 0) { # zero yes values in assessment
    message("NOTE: There were no concepts included in the concept set.")
    return(NULL)
  }
  tmp <- suppressWarnings(as.integer(unlist(finalSet)))
  finalSet <- tmp[!is.na(tmp)]
  conceptSet <- Capr::cs(as.integer(unlist(finalSet)), name = conditionForFiles)

  conceptSet <- Capr::getConceptSetDetails(conceptSet, connection, vocabularyDatabaseSchema = cdmDatabaseSchema)
  conceptSet <- jsonlite::fromJSON(Capr::as.json(conceptSet))
  finalConceptSet <- conceptSet #set this as the the final if no condensing is successfully performed

  if (file.exists(file.path(outputDirectory, paste0(conditionForFiles, ".json")))) {
    # skip condensing concept set if json file exists
    message(
      "File ",
      file.path(outputDirectory, paste0(conditionForFiles, ".json")),
      " exists...skipping condensing concept set."
    )
  } else { #condense concept set, if needed
    if(csConceptPlusDescendants == FALSE) { #produce full concept set
      # initial write of code list
      write(
        jsonlite::toJSON(conceptSet,
                         simplifyVector = FALSE,
                         auto_unbox = TRUE
        ),
        file = file.path(outputDirectory, paste0(conditionForFiles, ".json"))
      )
    } else { #produce the final concept set of all concepts plus descendants
      if(length(finalConceptSet) > 0) {
        finalConceptSet <- .createJsonforConceptsPlusDescendants(conceptIds = finalSet,
                                                                 connectionDetails = connectionDetails,
                                                                 cdmDatabaseSchema = cdmDatabaseSchema)

        write(jsonlite::toJSON(finalConceptSet, pretty = TRUE, simplifyVector = FALSE, auto_unbox = TRUE),
              file = file.path(outputDirectory, paste0(conditionForFiles, ".json"))
        )
        message("The artifacts from the process may be found at: ", file.path(outputDirectory))

      }
    }

    if(condenseConceptSet == TRUE) { #only condense concept set if requested
      retryLimit <- 10 # Maximum number of retries
      attempt <- 0 # Initial attempt counter
      success <- FALSE # Flag to indicate success

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
            finalConceptSet <- condensedConceptSet #set this as final if condensing was successfully performed
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
    }
  }

  if (exists("llmResults")) {
    return(list(testedConcepts = llmResults, conceptSet = finalConceptSet))
  } else {
    return(NULL)
  }
}
