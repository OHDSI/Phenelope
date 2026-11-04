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
#' @param fuzzyVocabSearchType      For drugs: type of fuzzy vocabulary search. Currently only 'HECATE' is supported.
#' @param batchSize                 For drugs: The size of the batches fed to the LLM for adjudication.
#'
#' @description
#' This is a convenience function that calls the LLM to determine the domain of the concept set target, and then either
#' calls `createNonDrugConceptSet()` or `createDrugConceptSet()`, depending on the domain.
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
    condenseConceptSet = TRUE,
    fuzzyVocabSearchType = "HECATE",
    batchSize = 20) {
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
  checkmate::assertChoice(fuzzyVocabSearchType, choices = c("HECATE"), add = errorMessages)
  checkmate::assertInt(batchSize, lower = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!is.null(cacheFolder) && !dir.exists(cacheFolder)) {
    dir.create(cacheFolder, recursive = TRUE)
  }

  # Establish domain ---------------------------------------------------------------------------------------------------
  message("Determining concept set domain")
  domain <- withCache({
    getDomain(name, llmClient, NULL)
  },
  cacheFolder = cacheFolder,
  fileName = "Domain.txt"
  )
  message("- Domain: ", domain)

  # Call appropriate function ------------------------------------------------------------------------------------------
  if (domain == "DRUG") {
    conceptSet <- createDrugConceptSet(
    name = name,
    clinicalDefinition = clinicalDefinition,
    llmClient = llmClient,
    connectionDetails = connectionDetails,
    vocabDatabaseSchema = vocabDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    cacheFolder = cacheFolder,
    fuzzyVocabSearchType = fuzzyVocabSearchType)
  } else {
    conceptSet <- createNonDrugConceptSet(
    name = name,
    seedConceptIds = seedConceptIds,
    clinicalDefinition = clinicalDefinition,
    llmClient = llmClient,
    connectionDetails = connectionDetails,
    vocabDatabaseSchema = vocabDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    cacheFolder = cacheFolder,
    excludedVocabularyIds = excludedVocabularyIds,
    seedConceptFinder = seedConceptFinder,
    conceptRecommender = conceptRecommender,
    conceptAdjudicator = conceptAdjudicator,
    condenseConceptSet = condenseConceptSet)
  }
  return(conceptSet)
}
