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
# See the License for the specific language governing permissions andD
# limitations under the License.

# source("R/HelperFunctions.R")

#' Create a drug concept set
#'
#' @param name                      The name of the concept set to create. This should be an informative name reflecting
#'                                  the concept set target (the idea expressed by the concept set).
#' @template ClinicalDefinition
#' @template LlmClient
#' @template ConnectionDetails
#' @template VocabDatabaseSchema
#' @template TempEmulationSchema
#' @param cacheFolder               Optional: Folder where intermediary results can be stored.
#' @param fuzzyVocabSearchType         Type of fuzzy vocabulary search. Currently only 'HECATE' is supported.
#'
#' @returns
#' A concept set expression
#'
#' @export
createDrugConceptSet <- function(
    name,
    clinicalDefinition = NULL,
    llmClient,
    connectionDetails,
    vocabDatabaseSchema,
    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
    cacheFolder = NULL,
    fuzzyVocabSearchType = "HECATE") {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(name, len = 1, add = errorMessages)
  checkmate::assertCharacter(clinicalDefinition, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(vocabDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(cacheFolder, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertChoice(fuzzyVocabSearchType, choices = c("HECATE"), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  start <- Sys.time()

  if (!is.null(cacheFolder) && !dir.exists(cacheFolder)) {
    dir.create(cacheFolder, recursive = TRUE)
  }

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  costTracker <- new.env()
  costTracker$amount <- 0

  message("Establishing drug concept class")
  drugClass <- withCache({
    getDrugClass(name, llmClient, costTracker)
  },
  cacheFolder = cacheFolder,
  fileName = "DrugClass.csv"
  )

  message("Coming up with search terms")
  searchTerms <- withCache({
    getDrugSearchTerms(drugClass = drugClass,
                       clinicalDefinition = clinicalDefinition,
                       llmClient = llmClient,
                       costTracker = costTracker)
  },
  cacheFolder = cacheFolder,
  fileName = "SearchTerms.txt"
  )

  message("Getting recommendations")
  concepts <- withCache({
    getDrugConcepts(drugClass = drugClass,
                    searchTerms = searchTerms,
                    fuzzyVocabSearchType = fuzzyVocabSearchType)
  },
  cacheFolder = cacheFolder,
  fileName = "RecommendedConcepts.csv"
  )

  message("Adjudicating recommendations")
  promptFile <- system.file("prompts", "DrugAdjudication.txt", package = "Phenelope")
  prompt <- paste(readLines(promptFile), collapse = "\n")
  systemPromptFile <- system.file("prompts", "DrugAdjudicationSystem.txt", package = "Phenelope")
  systemPrompt <- paste(readLines(systemPromptFile), collapse = "\n")
  drugConceptAdjudicator <- DefaultConceptAdjudicator$new(batchSize = 20,
                                                          prompt = prompt,
                                                          systemPrompt = systemPrompt,
                                                          nForQuickScreen = .Machine$integer.max)
  concepts <- withCache({
    drugConceptAdjudicator$adjudicateConcepts(concepts = concepts,
                                              name = name,
                                              clinicalDefinition = clinicalDefinition,
                                              llmClient = llmClient,
                                              costTracker = costTracker)
  },
  cacheFolder = cacheFolder,
  fileName = sprintf("FinalConcepts.csv")
  )
  approvedCount <- concepts |>
    filter(.data$status == "APPROVED") |>
    count() |>
    pull()
  message("- Approved ", approvedCount, " of ", nrow(concepts), " new concepts")
  if (approvedCount == 0) {
    error <- errorCondition(
      message = "No concepts were approved in adjudication. Broaden either the set of seed concepts or the clinical definition",
      class = "NoApprovedConceptsError"
    )
    stop(error)
  }
  conceptSetExpression <- withCache({
    asDrugConceptSetExpression(concepts = concepts,
                               name = name,
                               connection = connection,
                               vocabDatabaseSchema = vocabDatabaseSchema)
  },
  cacheFolder = cacheFolder,
  fileName = "ConceptSetExpression.json"
  )

  delta <- Sys.time() - start
  message("Creating concept set took ", signif(delta, 3), " ", attr(delta, "units"), " and cost $", costTracker$amount, ".")
  return(conceptSetExpression)
}
