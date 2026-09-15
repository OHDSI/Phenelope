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

#' Create a concept set
#'
#' @param name                      The name of the concept set to create. This should be an informative name reflecting
#'                                  the concept set target (the idea expressed by the concept set).
#' @param seedConceptIds            Optional: a set of one or more concept IDs that resemble the concept set target.
#' @template ClinicalDefinition
#' @template LlmClient
#' @template ConnectionDetails
#' @template VocabDatabaseSchema
#' @template TempEmulationSchema
#' @param cacheFolder               Optional: Folder where intermediary results can be stored.
#' @template ExcludedVocabularyIds
#' @param seedConceptFinder         An object of class `SeedConceptFinder` for finding seed concepts when not provided.
#' @param conceptRecommender        An object of class `ConceptRecommender` for recommending additional concepts.
#' @param conceptAdjudicator        An object of class `ConceptAdjudicator` for adjudicating recommended concepts.
#' @param condenseConceptSet        Condense the resulting concept set?
#'
#' @returns
#' A concept set expression
#'
#' @export
createConceptSet <- function(
    name,
    seedConceptIds = NULL,
    clinicalDefinition = NULL,
    llmClient,
    connectionDetails,
    vocabDatabaseSchema,
    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
    cacheFolder = NULL,
    excludedVocabularyIds = c("ICDO3"),
    seedConceptFinder = DefaultSeedConceptFinder$new(),
    conceptRecommender = HecateConceptRecomender$new(),
    conceptAdjudicator = DefaultConceptAdjudicator$new(),
    condenseConceptSet = TRUE) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(name, len = 1, add = errorMessages)
  checkmate::assertCharacter(clinicalDefinition, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertIntegerish(seedConceptIds, null.ok = TRUE, add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(vocabDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(cacheFolder, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(excludedVocabularyIds, add = errorMessages)
  checkmate::assertR6(seedConceptFinder, "SeedConceptFinder", add = errorMessages)
  checkmate::assertR6(conceptRecommender, "ConceptRecommender", add = errorMessages)
  checkmate::assertR6(conceptAdjudicator, "ConceptAdjudicator", add = errorMessages)
  checkmate::assertLogical(condenseConceptSet, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  start <- Sys.time()

  if (!is.null(cacheFolder) && !dir.exists(cacheFolder)) {
    dir.create(cacheFolder, recursive = TRUE)
  }

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  costTracker <- new.env()
  costTracker$amount <- 0

  # Establish domain ---------------------------------------------------------------------------------------------------
  message("Determining concept set domain")
  domain <- withCache({
    getDomain(name, llmClient, costTracker)
  },
  cacheFolder = cacheFolder,
  fileName = "Domain.txt"
  )
  domainSettings <- getDomainSettings(domain)
  message("- Domain: ", domain)

  # Seed concepts ------------------------------------------------------------------------------------------------------
  if (!is.null(seedConceptIds) && length(seedConceptIds) > 0) {
    concepts <- withCache({
      getConceptsFromIds(conceptIds = seedConceptIds,
                         origin = "SEED",
                         connection = connection,
                         vocabDatabaseSchema = vocabDatabaseSchema)
    },
    cacheFolder = cacheFolder,
    fileName = "SeedConcepts.csv"
    )
  } else {
    message("Finding seed concepts")
    concepts <- withCache({
      seedConceptFinder$findSeedConcepts(name = name,
                                         clinicalDefinition = clinicalDefinition,
                                         llmClient = llmClient,
                                         costTracker = costTracker,
                                         domainSettings = domainSettings,
                                         excludedVocabularyIds = excludedVocabularyIds)
    },
    cacheFolder = cacheFolder,
    fileName = "SeedConcepts.csv"
    )
    message("- Found total of ", nrow(concepts), " seed concept IDs")
  }

  # Recommend - adjudicate iterations ----------------------------------------------------------------------------------
  for (iteration in 1:2) {
    message("Starting iteration ", iteration)

    message("Getting recommendations")
    if (iteration == 1) {
      conceptIds <- concepts |>
        pull(.data$conceptId)
    } else {
      conceptIds <- concepts |>
        filter(.data$status %in% c("APPROVED")) |>
        pull(.data$conceptId)
    }
    recommendedConcepts <- withCache({
      recommendedConcepts <- conceptRecommender$recommendConcepts(conceptIds = conceptIds,
                                                                  domainSettings = domainSettings,
                                                                  excludedVocabularyIds = excludedVocabularyIds,
                                                                  connection = connection,
                                                                  vocabDatabaseSchema = vocabDatabaseSchema)
      recommendedConcepts |>
        filter(!.data$conceptId %in% concepts$conceptId)
    },
    cacheFolder = cacheFolder,
    fileName = sprintf("RecommendConcepts_%d.csv", iteration)
    )
    concepts <- concepts |>
      bind_rows(recommendedConcepts)
    message("- ", nrow(recommendedConcepts), " concepts added by recommender")

    message("Adjudicating recommendations")
    conceptsToAdjudicate <- concepts |>
      filter(.data$status == "UNADJUDICATED")
    adjudicatedConcepts <- withCache({
      conceptAdjudicator$adjudicateConcepts(concepts = conceptsToAdjudicate,
                                            name = name,
                                            clinicalDefinition = clinicalDefinition,
                                            llmClient = llmClient,
                                            costTracker = costTracker)
    },
    cacheFolder = cacheFolder,
    fileName = sprintf("AdjudicatedConcepts_%d.csv", iteration)
    )
    message("- Approved ", sum(adjudicatedConcepts$status == "APPROVED"), " of ", nrow(adjudicatedConcepts), " new concepts")

    concepts <- concepts |>
      filter(!.data$conceptId %in% adjudicatedConcepts$conceptId) |>
      bind_rows(adjudicatedConcepts)
  }

  # Convert to (condensed) concept set expression ----------------------------------------------------------------------
  if (!is.null(cacheFolder)) {
    readr::write_csv(concepts, file.path(cacheFolder, "FinalConcepts.csv"))
  }
  conceptSetExpression <- withCache({
    conceptSetExpression <- asConceptSetExpression(concepts = concepts,
                                                   name = name,
                                                   connection = connection,
                                                   vocabDatabaseSchema = vocabDatabaseSchema)
    if (condenseConceptSet) {
      message("Condensing concept set expression")
      conceptSetExpression <- doCondense(conceptSetExpression = conceptSetExpression,
                                         connection = connection,
                                         vocabDatabaseSchema = vocabDatabaseSchema,
                                         tempEmulationSchema = tempEmulationSchema,
                                         excludedVocabularyIds = excludedVocabularyIds)
    }
    conceptSetExpression
  },
  cacheFolder = cacheFolder,
  fileName = "ConceptSetExpression.json"
  )

  delta <- Sys.time() - start
  message("Creating concept set took ", signif(delta, 3), " ", attr(delta, "units"), " and cost $", costTracker$amount, ".")
  return(conceptSetExpression)
}

doCondense <- function(conceptSetExpression, connection, vocabDatabaseSchema, tempEmulationSchema, excludedVocabularyIds) {
  conceptSetData <- fetchCondenserConceptSetData(
    conceptSetExpression = conceptSetExpression,
    connection = connection,
    cdmDatabaseSchema = vocabDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    excludedVocabularies = excludedVocabularyIds
  )
  condensedConceptSet <- condenseConceptSet(conceptSetData)
  condensedConceptSet <- jsonlite::toJSON(condensedConceptSet, pretty = TRUE, auto_unbox = TRUE)
  return(condensedConceptSet)
}
