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

#' An abstract class for a seed concept finder.
#'
#` @export
SeedConceptFinder <- R6::R6Class(
  "SeedConceptFinder",
  public = list(
    #' Find seed concepts
    #'
    #' @param name The name to use to find concepts.
    #' @template ClinicalDefinition
    #' @template LlmClient
    #' @template CostTracker
    #' @template DomainSettings
    #' @template ExcludedVocabularyIds
    #'
    #' @description
    #' Return seed concepts given a concept set name and optionally a clinical definition.
    #' The `llmClient` is only used when `addSynonyms` and/or `adjudicateFuzzySearchResults` is `TRUE` in the arguments.
    #'
    #' @returns
    #' Returns an object of type Concepts with the seed concepts.
    #'
    #' @export
    findSeedConcepts = function(name,
                                clinicalDefinition = NULL,
                                llmClient = NULL,
                                costTracker = NULL,
                                domainSettings = NULL,
                                excludedVocabularyIds = NULL) {}
  )
)

#' The default seed concept finder
#'
#` @export
DefaultSeedConceptFinder <- R6::R6Class(
  "DefaultSeedConceptFinder",
  inherit = SeedConceptFinder,
  public = list(
    #' DefaultSeedConceptFinder constructor
    #'
    #' @param addSynonyms                  Generate synonyms of the concept target name before search for matching
    #'                                     concepts?
    #' @param maxN                         Maximum number of seed concepts to return.
    #' @param minCount                     Minimum number of record counts for a concept to be included.
    #' @param fuzzyVocabSearchType         Type of fuzzy vocabulary search. Currently only 'HECATE' is supported.
    #' @param adjudicateFuzzySearchResults Perform a first pass adjudication on the fuzzy vocabulary search results to
    #'                                     remove concepts that are obviously wrong?
    #'
    #' @returns
    #' An object of type `DefaultSeedConceptFinder`
    #'
    #' @export
    initialize = function(addSynonyms = TRUE,
                          maxN = 25,
                          minCount = 0,
                          fuzzyVocabSearchType = "HECATE",
                          adjudicateFuzzySearchResults = TRUE) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertLogical(addSynonyms, len = 1, add = errorMessages)
      checkmate::assertIntegerish(maxN, len = 1, lower = 1, add = errorMessages)
      checkmate::assertIntegerish(minCount, len = 1, lower = 0, add = errorMessages)
      checkmate::assertCharacter(fuzzyVocabSearchType, len = 1, add = errorMessages)
      checkmate::assertChoice(fuzzyVocabSearchType, choices = c("HECATE"), add = errorMessages)
      checkmate::assertLogical(adjudicateFuzzySearchResults, len = 1, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      private$addSynonyms <- addSynonyms
      private$maxN <- maxN
      private$minCount <- minCount
      private$fuzzyVocabSearchType <- fuzzyVocabSearchType
      private$adjudicateFuzzySearchResults <- adjudicateFuzzySearchResults
    },
    #' @description
    #' Default implementation of the concept finder.
    findSeedConcepts = function(name,
                                clinicalDefinition = NULL,
                                llmClient = NULL,
                                costTracker = NULL,
                                domainSettings = NULL,
                                excludedVocabularyIds = NULL) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(name, len = 1, add = errorMessages)
      checkmate::assertCharacter(clinicalDefinition, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertR6(llmClient, "Chat", null.ok = TRUE, add = errorMessages)
      checkmate::assertEnvironment(costTracker, null.ok = TRUE, add = errorMessages)
      checkmate::assertClass(domainSettings, "DomainSettings", null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(excludedVocabularyIds, null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (private$addSynonyms) {
        message("  Adding synonyms to name")
        names <- unique(c(name, getSynonyms(name, llmClient, costTracker)))
        message("  - added ", length(names) - 1, " synonyms")
      } else {
        names <- name
      }

      message("  Searching for seed concepts")
      searchResults <- list()
      for (i in seq_along(names)) {
        searchResults[[i]] <- searchConcepts(
          term = names[i],
          domainSettings = domainSettings,
          excludedVocabularyIds = excludedVocabularyIds,
          maxN = private$maxN,
          fuzzyVocabSearchType = private$fuzzyVocabSearchType
        ) |>
          filter(.data$recordCount >= private$minCount)
      }
      searchResults <- mergeRankings(searchResults)
      seedConcepts <- searchResults |>
        slice_head(n = private$maxN)
      message("  - Found ", nrow(seedConcepts), " seed concepts through fuzzy vocab search")

      if (private$adjudicateFuzzySearchResults) {
        message("  Adjudicating fuzzy vocab search results")
        seedConcepts = adjudicateSeedConcepts(seedConcepts,
                                              name = name,
                                              clinicalDefinition = clinicalDefinition,
                                              llmClient = llmClient,
                                              costTracker = costTracker)
        message("  - Kept ", nrow(seedConcepts), " seed concepts")
      }

      concepts <- asConcepts(seedConcepts, origin = "SEED", status = "UNADJUDICATED")
      return(concepts)
    }
  ),
  private = list(
    addSynonyms = NULL,
    maxN = NULL,
    minCount = NULL,
    fuzzyVocabSearchType = NULL,
    adjudicateFuzzySearchResults = NULL
  )
)

getSynonyms <- function(name, llmClient, costTracker) {
  # promptFile <- "inst/prompts/Synonyms.txt"
  promptFile <- system.file("prompts", "Synonyms.txt", package = "Phenelope")
  prompt <- paste(readLines(promptFile), collapse = "\n")
  prompt <- gsub("%name%", name, prompt)
  outputType <- ellmer::type_object(synonyms = ellmer::type_array(ellmer::type_string()))
  synonyms <- queryLlm(
    prompt = prompt,
    llmClient = llmClient,
    costTracker = costTracker,
    outputType = outputType
  )
  synonyms <- synonyms$synonyms
  return(synonyms)
}

mergeRankings <- function(searchResults) {
  scoredSearchResults <- lapply(searchResults, function(df) {
    df |>
      mutate(
        rank = row_number(),
        score = n() - .data$rank + 1
      )
  })

  overallRanking <- bind_rows(scoredSearchResults) |>
    group_by(.data$conceptId, .data$conceptName, .data$vocabularyId, .data$domainId, .data$conceptClassId) |>
    summarise(
      totalScore = sum(.data$score),
      avgerageRank = mean(.data$rank),
      .groups = "drop"
    ) |>
    arrange(desc(.data$totalScore), .data$avgerageRank) |>
    select("conceptId", "conceptName", "vocabularyId","domainId", "conceptClassId")
  return(overallRanking)
}

adjudicateSeedConcepts <- function(concepts,
                                   name,
                                   clinicalDefinition,
                                   llmClient,
                                   costTracker) {
  # promptFile <- "inst/prompts/SeedAdjudication.txt"
  promptFile <- system.file("prompts", "SeedAdjudication.txt", package = "Phenelope")
  prompt <- paste(readLines(promptFile), collapse = "\n")
  # systemPromptFile <- "inst/prompts/SeedAdjudicationSystem.txt"
  systemPromptFile <- system.file("prompts", "SeedAdjudicationSystem.txt", package = "Phenelope")
  systemPrompt <- paste(readLines(systemPromptFile), collapse = "\n")
  instantiatedPrompt <- instantiatePrompt(prompt = prompt,
                                          name = name,
                                          clinicalDefinition = clinicalDefinition,
                                          concepts = concepts)
  instantiatedSystemPrompt <- instantiatePrompt(prompt = systemPrompt,
                                                name = name,
                                                clinicalDefinition = clinicalDefinition,
                                                concepts = concepts)
  outputType <- ellmer::type_array(
    ellmer::type_object(
      conceptId = ellmer::type_integer()
    )
  )
  conceptsToRemove <- queryLlm(prompt = instantiatedPrompt,
                               systemPrompt = instantiatedSystemPrompt,
                               llmClient = llmClient,
                               costTracker = costTracker,
                               outputType = outputType)
  concepts <- concepts |>
    filter(!.data$conceptId %in% conceptsToRemove$conceptId)
  return(concepts)
}

