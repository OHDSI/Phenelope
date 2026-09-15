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

#' An abstract class for a concept adjudicator.
#'
#` @export
ConceptAdjudicator <- R6::R6Class(
  "ConceptAdjudicator",
  public = list(
    #' Adjudicate concepts
    #'
    #' @template Concepts
    #' @param name The name of the concept set.
    #' @template ClinicalDefinition
    #' @template LlmClient
    #' @template CostTracker
    #'
    #' @returns
    #' The same concepts as provided in the input, but with their `status` tag set to either 'APPROVED' or 'REJECTED',
    #' and an additional `rationale` column capturing the LLM rationale.
    #'
    #' @export
    adjudicateConcepts = function(concepts, name, clinicalDefinition = NULL, llmClient, costTracker = NULL) {}
  )
)

#' The default concept adjudicator using the LLM and prompts.
#'
#` @export
DefaultConceptAdjudicator <- R6::R6Class(
  "DefaultConceptAdjudicator",
  inherit = ConceptAdjudicator,
  public = list(
    #' DefaultConceptAdjudicator constructor
    #'
    #' @param batchSize               The number of concepts to adjudicate at once.
    #' @param nForQuickScreen         The minimum number of concepts to trigger the quick screening.
    #' @param quickScreenBatchSize    The number of concepts to enter quick screening at once.
    #' @param maxN                    If the number of concepts to adjudicate is greater than this number, an error of
    #'                                class `TooManyConceptsError` is thrown.
    #' @param prompt                  The prompt for the main concept adjudication. See Details for requirements
    #' @param systemPrompt            The system prompt for the main concept adjudication. See Details for requirements
    #' @param quickScreenPrompt       The prompt for the quick screening. See Details for requirements
    #' @param quickScreenSystemPrompt The system prompt for the quick screening. See Details for requirements
    #'
    #' @details
    #' This adjudicator implements a 2-stage concept adjudication process using prompts. If the number of input concepts
    #' is greater than or equal to `nForQuickScreen`, the quickscreen stage is entered. In this stage,
    #' `quickScreenBatchSize` concepts at a time are fed to the LLM using the `quickScreenPrompt` and
    #' `quickScreenSystemPrompt` prompts. Any remaining concepts go on to stage 2, where `batchSize` concepts are fed to
    #' the LLM using the `prompt` and
    #' `systemPrompt` prompts.
    #'
    #' The following placeholders in the prompts will be replaced with their actual values when adjudicating:
    #'
    #' - **%name%**: The name of the concept set (the concept set target).
    #' - **%definition%**: The clinical definition of the concept set. If not provided during adjudication, the entire line with this placeholder will be deleted.
    #' - **%concepts%**: A JSON string representing the candidate concepts.
    #'
    #' The following JSON output formats is expected for the main concept adjudication. This should include **all
    #' concepts** from the input:
    #'
    #' ```json
    #' [
    #'   {
    #'     "conceptId": <Candidate Concept id>,
    #'     "conceptName": "<Candidate Concept name>",
    #'     "decision": "<KEEP or REMOVE>",
    #'     "rationale": "<Brief rational for the decision>"
    #'   }
    #' ]
    #' ```
    #'
    #' The following JSON output formats is expected for the quick screening. This should include *only concepts to
    #' remove**:
    #'
    #' ```json
    #' [
    #'   {
    #'     "conceptId": <Candidate Concept id>,
    #'     "conceptName": "<Candidate Concept name>",
    #'     "rationale": "Brief explanation of why the concept does not imply the Target Term"
    #'   }
    #' ]
    #' ```
    #'
    #' If a prompt is not provided in this constructor it will assume the default value included in this package.
    #' To disable the quick screen stage, set `nForQuickScreen = 999999`.
    #'
    #' @returns
    #' This is the constructor.
    #'
    #' @export
    initialize = function(batchSize = 20,
                          nForQuickScreen = 500,
                          quickScreenBatchSize = 200,
                          maxN = 5000,
                          prompt = NULL,
                          systemPrompt = NULL,
                          quickScreenPrompt = NULL,
                          quickScreenSystemPrompt = NULL) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertIntegerish(batchSize, len = 1, lower = 1, add = errorMessages)
      checkmate::assertIntegerish(nForQuickScreen, len = 1, lower = 1, add = errorMessages)
      checkmate::assertIntegerish(quickScreenBatchSize, len = 1, lower = 1, add = errorMessages)
      checkmate::assertIntegerish(maxN, len = 1, lower = 1, add = errorMessages)
      checkmate::assertCharacter(prompt, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(systemPrompt, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(quickScreenPrompt, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(quickScreenSystemPrompt, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      private$batchSize <- batchSize
      private$nForQuickScreen <- nForQuickScreen
      private$quickScreenBatchSize <- quickScreenBatchSize
      private$maxN <- maxN

      if (is.null(prompt)) {
        # promptFile <- "inst/prompts/Adjudication.txt"
        promptFile <- system.file("prompts", "Adjudication.txt", package = "Phenelope")
        prompt <- paste(readLines(promptFile), collapse = "\n")
      }
      if (is.null(systemPrompt)) {
        # systemPromptFile <- "inst/prompts/AdjudicationSystem.txt"
        systemPromptFile <- system.file("prompts", "AdjudicationSystem.txt", package = "Phenelope")
        systemPrompt <- paste(readLines(systemPromptFile), collapse = "\n")
      }
      if (is.null(quickScreenPrompt)) {
        # quickScreenPromptFile <- "inst/prompts/QuickScreen.txt"
        quickScreenPromptFile <- system.file("prompts", "QuickScreen.txt", package = "Phenelope")
        quickScreenPrompt <- paste(readLines(quickScreenPromptFile), collapse = "\n")
      }
      if (is.null(quickScreenSystemPrompt)) {
        # quickScreenSystemPromptFile <- "inst/prompts/QuickScreenSystem.txt"
        quickScreenSystemPromptFile <- system.file("prompts", "QuickScreenSystem.txt", package = "Phenelope")
        quickScreenSystemPrompt <- paste(readLines(quickScreenSystemPromptFile), collapse = "\n")
      }

      private$prompt <- prompt
      private$systemPrompt <- systemPrompt
      private$quickScreenPrompt <- quickScreenPrompt
      private$quickScreenSystemPrompt <- quickScreenSystemPrompt
    },
    #' @description
    #' Adjudicates concepts using the default LLM implementation.
    adjudicateConcepts = function(concepts, name, clinicalDefinition = NULL, llmClient, costTracker = NULL) {
      validateConcepts(concepts)
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(name, len = 1, add = errorMessages)
      checkmate::assertCharacter(clinicalDefinition, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertR6(llmClient, "Chat", add = errorMessages)
      checkmate::assertEnvironment(costTracker, null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      if (nrow(concepts) > private$maxN) {
        error <- errorCondition(
          message = sprintf("Number of concepts (%d) exceeds maximum allowed (%d)", nrow(concepts), private$maxN),
          class = "TooManyConceptsError"
        )
        stop(error)
      }

      if (nrow(concepts) >= private$nForQuickScreen) {
        concepts <- quickScreen(concepts = concepts,
                                name = name,
                                clinicalDefinition = clinicalDefinition,
                                batchSize = private$quickScreenBatchSize,
                                quickScreenPrompt = private$quickScreenPrompt,
                                quickScreenSystemPrompt = private$quickScreenSystemPrompt,
                                llmClient = llmClient,
                                costTracker = costTracker)
      }
      remainingConcepts <- concepts |>
        filter(.data$status != "REJECTED")

      if (nrow(remainingConcepts) > 0) {
        remainingConcepts <- adjudicate(concepts = remainingConcepts,
                                        name = name,
                                        clinicalDefinition = clinicalDefinition,
                                        batchSize = private$batchSize,
                                        prompt = private$prompt,
                                        systemPrompt = private$systemPrompt,
                                        llmClient = llmClient,
                                        costTracker = costTracker)
        concepts <- bind_rows(
          concepts |>
            filter(.data$status == "REJECTED"),
          remainingConcepts
        )
      }
      return(concepts)
    }
  ),
  private = list(
    batchSize = NULL,
    nForQuickScreen = NULL,
    quickScreenBatchSize = NULL,
    maxN = NULL,
    prompt = NULL,
    systemPrompt = NULL,
    quickScreenPrompt = NULL,
    quickScreenSystemPrompt = NULL
  )
)

quickScreen <- function(concepts,
                        name,
                        clinicalDefinition,
                        batchSize,
                        quickScreenPrompt,
                        quickScreenSystemPrompt,
                        llmClient,
                        costTracker) {
  outputType <- ellmer::type_array(
    ellmer::type_object(
      conceptId = ellmer::type_integer(),
      conceptName = ellmer::type_string(),
      rationale = ellmer::type_string()
    )
  )

  conceptsToRemove <- list()
  start <- 1
  while (start <= nrow(concepts)) {
    end <- min(start + batchSize - 1, nrow(concepts))
    message("  Quick screen for concepts ", start, " to ", end, " out of ", nrow(concepts))
    batch <- concepts[start:end, ]
    instantiatedPrompt <- instantiatePrompt(prompt = quickScreenPrompt,
                                            name = name,
                                            clinicalDefinition = clinicalDefinition,
                                            concepts = batch)
    instantiatedSystemPrompt <- instantiatePrompt(prompt = quickScreenSystemPrompt,
                                                  name = name,
                                                  clinicalDefinition = clinicalDefinition,
                                                  concepts = batch)
    conceptsToRemove[[length(conceptsToRemove) + 1]] <- queryLlm(prompt = instantiatedPrompt,
                                                                 systemPrompt = instantiatedSystemPrompt,
                                                                 llmClient = llmClient,
                                                                 costTracker = costTracker,
                                                                 outputType = outputType)
    start <- end + 1
  }
  conceptsToRemove <- bind_rows(conceptsToRemove)
  # If LLM duplicates a concept, just pick the first one:
  conceptsToRemove <- conceptsToRemove |>
    filter(!duplicated(.data$conceptId))

  results <- concepts |>
    filter(!.data$conceptId %in% conceptsToRemove$conceptId)

  if ("rationale" %in% colnames(concepts)) {
    concepts <- concepts |>
      select(-"rationale")
  }
  results <- bind_rows(
    results,
    concepts |>
      inner_join(conceptsToRemove |>
                   select("conceptId", "rationale"),
                 by = join_by("conceptId")) |>
      mutate(rationale = paste("Quick screen:", .data$rationale),
             status = "REJECTED")
  )
  return(results)
}

adjudicate <- function(concepts,
                       name,
                       clinicalDefinition,
                       batchSize,
                       prompt,
                       systemPrompt,
                       llmClient,
                       costTracker) {
  outputType <- ellmer::type_array(
    ellmer::type_object(
      conceptId = ellmer::type_integer(),
      conceptName = ellmer::type_string(),
      decision = ellmer::type_enum(c("KEEP", "REMOVE")),
      rationale = ellmer::type_string()
    )
  )

  adjudicatedConcepts <- list()
  start <- 1
  while (start <= nrow(concepts)) {
    end <- min(start + batchSize - 1, nrow(concepts))
    message("  Adjudicating concepts ", start, " to ", end, " out of ", nrow(concepts))
    batch <- concepts[start:end, ]
    instantiatedPrompt <- instantiatePrompt(prompt = prompt,
                                            name = name,
                                            clinicalDefinition = clinicalDefinition,
                                            concepts = batch)
    instantiatedSystemPrompt <- instantiatePrompt(prompt = systemPrompt,
                                                  name = name,
                                                  clinicalDefinition = clinicalDefinition,
                                                  concepts = batch)
    maxAttempts <- 5
    for (attempt in seq_len(maxAttempts)) {
      adjudicationResults <- queryLlm(prompt = instantiatedPrompt,
                                      systemPrompt = instantiatedSystemPrompt,
                                      llmClient = llmClient,
                                      costTracker = costTracker,
                                      outputType = outputType)
      if (nrow(batch) == nrow(adjudicationResults) &&
          all(sort(batch$conceptId) == sort(adjudicationResults$conceptId))) {
        break
      } else {
        if (attempt == maxAttempts) {
          stop("During adjudication the LLM failed to return all input concepts ", maxAttempts, " times")
        }
        message("  LLM did not return all concepts. Retrying")
      }
    }

    adjudicatedConcepts[[length(adjudicatedConcepts) + 1]] <- adjudicationResults
    start <- end + 1
  }
  adjudicatedConcepts <- bind_rows(adjudicatedConcepts)

  if ("rationale" %in% colnames(concepts)) {
    concepts <- concepts |>
      select(-"rationale")
  }
  concepts <- concepts |>
    inner_join(adjudicatedConcepts |>
                 select("conceptId", "decision", "rationale"),
               by = join_by("conceptId")) |>
    mutate(status = if_else(.data$decision == "KEEP", "APPROVED", "REJECTED")) |>
    select(-"decision")

  return(concepts)
}

instantiatePrompt <- function(prompt, name, clinicalDefinition, concepts) {
  instantiatedPrompt <- gsub("%name%", name, prompt)
  if (is.null(clinicalDefinition) || clinicalDefinition == "") {
    # Delete entire line with %definition%
    instantiatedPrompt <- gsub("\n[^\n]*%definition%[^\n]*\n", "", instantiatedPrompt)
  } else {
    instantiatedPrompt <- gsub("%definition%", clinicalDefinition, instantiatedPrompt)
  }
  json <- concepts |>
    select("conceptId", "conceptName") |>
    jsonlite::toJSON()
  instantiatedPrompt <- gsub("%concepts%", json, instantiatedPrompt)
  return(instantiatedPrompt)
}
