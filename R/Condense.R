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

# library(dplyr)

#' Condense concept set
#'
#' @param conceptSetData An object of type `ConceptData` as created by `fetchConceptSetData`.
#'
#' @returns
#' A tibble with a representation of the optimal concept set expression
#'
#' @export
condenseConceptSet <- function(conceptSetData) {
  candidateConcepts <- conceptSetData$conceptsToDescendants |>
    group_by(.data$conceptId) |>
    group_split()
  # group = candidateConcepts[[1]]
  createCandidateConcept <- function(group) {
    rJava::.jnew(
      "org.ohdsi.conceptSetCondenser.CandidateConcept",
      as.integer(group$conceptId[1]),
      rJava::.jarray(as.integer(group$descendantConceptId))
    )
  }
  candidateConcepts <- rJava::.jarray(lapply(candidateConcepts, createCandidateConcept),
    contents.class = "org.ohdsi.conceptSetCondenser.CandidateConcept"
  )
  condenser <- rJava::.jnew(
    "org.ohdsi.conceptSetCondenser.ConceptSetCondenser",
    rJava::.jarray(as.integer(conceptSetData$includedConceptIds)),
    candidateConcepts
  )
  condenser$condense()
  solution <- condenser$getConceptSetExpression()

  # Format concept set expression using Circe format:
  conceptSetExpression <- lapply(
    solution,
    function(x) tibble(conceptId = x$conceptId, exclude = x$exclude, descendants = x$descendants)
  )
  conceptSetExpression <- bind_rows(conceptSetExpression)
  conceptSetExpression <- conceptSetExpression |>
    inner_join(conceptSetData$conceptMetaData, by = join_by("conceptId"))

  ####### jns added code
  if (nrow(conceptSetExpression) > 0) {
    conceptSetExpression$standardConceptCaption <- "Unknown"
    conceptSetExpression$standardConceptCaption[conceptSetExpression$standardConcept == "N"] <- "Non-Standard"
    conceptSetExpression$standardConceptCaption[conceptSetExpression$standardConcept == "S"] <- "Standard"
    conceptSetExpression$standardConceptCaption[conceptSetExpression$standardConcept == "C"] <- "Classification"
  }
  ###########

  # row = rows[[1]]
  toCirceConcept <- function(row) {
    concept <- as.list(select(row, -"exclude", -"descendants"))
    names(concept) <- toupper(SqlRender::camelCaseToSnakeCase(names(concept)))
    concept <- list(
      concept = concept,
      isExcluded = row$exclude,
      includeDescendants = row$descendants,
      includeMapped = FALSE
    )
    return(concept)
  }
  rows <- split(conceptSetExpression, seq_len(nrow(conceptSetExpression)))
  conceptSetExpression <- lapply(rows, toCirceConcept)
  names(conceptSetExpression) <- NULL
  conceptSetExpression <- list(items = conceptSetExpression)
  message(sprintf(
    "Concept set has %d concepts, optimal expression has %d concepts.",
    length(conceptSetData$includedConceptIds),
    length(solution)
  ))
  return(conceptSetExpression)
}
