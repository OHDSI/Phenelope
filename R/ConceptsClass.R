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

#' Convert data frame to Concepts
#'
#' @param dataFrame Input data frame
#' @param origin    Optional: the origin to use for all concepts in the input. Can be 'SEED', 'DESCENDANT', or
#'                  'RECOMMENDED'.
#' @param status    Optional: the status to use for all concepts in the input. Can be 'UNADJUDICATED', 'APPROVED',
#'                  'REJECTED', or 'BELOW_MIN_COUNT`.`
#'
#' @description
#' The `Concepts` class enforces several properties of a data frame. First of all, it *must* have columns:
#'
#' - conceptId
#' - conceptName
#' - vocabularyId
#' - domainId
#' - conceptClassId
#' - origin
#' - status
#'
#' Second, the `origin` column *must* be one of these values:
#'
#' - SEED
#' - DESCENDANT
#' - RECOMMENDED
#'
#' Third, the `status` column *must* be one of these values:
#'
#' - UNADJUDICATED
#' - APPROVED
#' - REJECTED
#' - BELOW_MIN_COUNT
#'
#' Finally, the `conceptId` column cannot have duplicates.
#'
#' @seealso [validateConcepts()]
#'
#' @returns
#' An object of type `Concepts`.
#'
#' @export
asConcepts <- function(dataFrame, origin = NULL, status = NULL) {
  if (!is.null(origin)) {
    dataFrame <- dataFrame |>
      mutate(origin = !!origin)
  }
  if (!is.null(status)) {
    dataFrame <- dataFrame |>
      mutate(status = !!status)
  }
  class(dataFrame) <- c("Concepts", class(dataFrame))
  validateConcepts(dataFrame)
  dataFrame <- dataFrame |>
    mutate(conceptId = as.integer(.data$conceptId),
           conceptName = as.character(.data$conceptName),
           vocabularyId = as.character(.data$vocabularyId),
           domainId = as.character(.data$domainId),
           conceptClassId = as.character(.data$conceptClassId),
           origin = as.character(.data$origin),
           status = as.character(.data$status))
  return(dataFrame)
}

#' Validate a Concepts object
#'
#' @param concepts An object of type 'Concepts'.
#'
#' @description
#' Throws an error if the object is not a valid `Concepts` object.
#'
#' @seealso [asConcepts()]
#'
#' @returns
#' Returns nothing. Is called for the side effect of not throwing an error.
validateConcepts <- function(concepts) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(concepts, "Concepts", add = errorMessages)
  checkmate::assertNames(names(concepts),
                         must.include = c("conceptId",
                                          "conceptName",
                                          "vocabularyId",
                                          "domainId",
                                          "conceptClassId",
                                          "origin",
                                          "status"),
                         add = errorMessages)
  checkmate::assertSubset(concepts$origin,
                          choices = c("SEED",
                                      "DESCENDANT",
                                      "RECOMMENDED"),
                          add = errorMessages)
  checkmate::assertSubset(concepts$status,
                          choices = c("UNADJUDICATED",
                                      "APPROVED",
                                      "REJECTED",
                                      "BELOW_MIN_COUNT"),
                          add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (any(duplicated(concepts$conceptId))) {
    stop("Cannot have duplicate concepts in a Concepts object.")
  }
}

asConceptSetExpression <- function(concepts, name, connection, vocabDatabaseSchema) {
  validateConcepts(concepts)

  approvedConceptIds <- concepts |>
    filter(.data$status == "APPROVED") |>
    pull(.data$conceptId)

  belowMinCountConceptIds <- concepts |>
    filter(.data$status == "BELOW_MIN_COUNT") |>
    pull(.data$conceptId)

  if (length(belowMinCountConceptIds) > 0) {
    # Keep concepts with record counts below the specified minimum only if they are descendant of an already included
    # concept:
    sql <- "
      SELECT descendant_concept_id AS concept_id
      FROM @vocab_database_schema.concept_ancestor
      WHERE ancestor_concept_id IN (@ancestor_concept_ids)
        AND descendant_concept_id IN (@descendant_concept_ids);
    "
    keepConceptIds <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql,
      vocab_database_schema = vocabDatabaseSchema,
      ancestor_concept_ids = approvedConceptIds,
      descendant_concept_ids = belowMinCountConceptIds,
      snakeCaseToCamelCase = TRUE
    )
    approvedConceptIds <- c(approvedConceptIds, keepConceptIds$conceptId)
  }

  conceptSet <- Capr::cs(approvedConceptIds, name = name)

  conceptSet <- Capr::getConceptSetDetails(conceptSet, connection, vocabularyDatabaseSchema = vocabDatabaseSchema)
  json <- Capr::toConceptSetJson(conceptSet)
  return(json)
}

getConceptsFromIds <- function(conceptIds, origin = "SEED", status = "UNADJUDICATED", connection, vocabDatabaseSchema) {
  if (length(conceptIds) == 0) {
    concepts <- tibble(
      conceptId = 1,
      conceptName = "",
      vocabularyId = "",
      domainId = "",
      conceptClassId = ""
    ) |>
      filter(.data$conceptId == 2)
  } else {
    sql <- "
    SELECT concept_id,
      concept_name,
      vocabulary_id,
      domain_id,
      concept_class_id
    FROM @cdm_database_schema.concept
    WHERE concept_id IN (@concept_ids)
      AND invalid_reason IS NULL;
  "
    concepts <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql,
      cdm_database_schema = vocabDatabaseSchema,
      concept_ids = conceptIds,
      snakeCaseToCamelCase = TRUE
    )
  }
  concepts <- asConcepts(concepts, origin = origin, status = status)
  return(concepts)
}
