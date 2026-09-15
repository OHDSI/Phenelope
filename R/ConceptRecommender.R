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

#' The abstract concept recommender class
#` @export
ConceptRecommender <- R6::R6Class(
  "ConceptRecommender",
  public = list(

    #' Recommend concepts
    #'
    #' @param conceptIds A set of concept IDs to find related concepts for.
    #' @template DomainSettings
    #' @template Connection
    #' @template VocabDatabaseSchema
    #' @template ExcludedVocabularyIds
    #'
    #' @returns
    #' An object of type `Concepts` containing the recommended concepts. Overlap with the input concepts is *not*
    #' removed.
    #'
    #' @export
    recommendConcepts = function(conceptIds, domainSettings, excludedVocabularyIds = NULL, connection, vocabDatabaseSchema) {}
  )
)

#' Concept recommender implementation using the Hecate Phoebe search.
#'
#` @export
HecateConceptRecomender <- R6::R6Class(
  "HecateConceptRecomender",
  inherit = ConceptRecommender,
  public = list(

    #' HecateConceptRecomender constructor
    #'
    #' @param minCount             Minimum record count for concepts to be considered for adjudication.
    #' @param keepLowCountConcepts If TRUE concepts are kepts with status BELOW_MIN_COUNT. These concepts will be
    #'                             included in the final concept set only if they are descendants of concepts that are
    #'                             explicitly included in the concept set.
    #'
    #' @returns
    #' An object of type `HecateConceptRecomender`.
    #'
    #' @export
    initialize = function(minCount = 0,
                          keepLowCountConcepts = TRUE) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertIntegerish(minCount, len = 1, lower = 0, add = errorMessages)
      checkmate::assertLogical(keepLowCountConcepts, len = 1, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      private$minCount <- minCount
      private$keepLowCountConcepts <- keepLowCountConcepts
    },
    #' @description
    #' Recommends concepts using the Hecate Phoebe implementation.
    recommendConcepts = function(conceptIds, domainSettings, excludedVocabularyIds = NULL, connection, vocabDatabaseSchema) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertIntegerish(conceptIds, min.len = 1, add = errorMessages)
      checkmate::assertClass(domainSettings, "DomainSettings", add = errorMessages)
      checkmate::assertCharacter(excludedVocabularyIds, null.ok = TRUE, add = errorMessages)
      checkmate::assertClass(connection, "DatabaseConnectorConnection", add = errorMessages)
      checkmate::assertCharacter(vocabDatabaseSchema, len = 1, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      message("  Adding descendants")
      # Note: getDescendants returns an object of type Concepts:
      descendants <- getDescendants(conceptIds = conceptIds,
                                    domainSettings = domainSettings,
                                    excludedVocabularyIds = excludedVocabularyIds,
                                    connection = connection,
                                    vocabDatabaseSchema = vocabDatabaseSchema)
      descendants <- descendants |>
        removeGenericConcepts() |>
        filter(!.data$conceptId %in% conceptIds)
      if (private$minCount > 0) {
        descendants <- descendants |>
          addHecateRecordCounts() |>
          mutate(status = if_else(.data$recordCount < private$minCount, "BELOW_MIN_COUNT", "UNADJUDICATED")) |>
          select(-"recordCount")
      }
      message("  - Found ", nrow(descendants), " additional concepts through descendants")

      message("  Getting Phoebe recommendations")
      recommendations <- getHecatePhoebeRecommendations(c(conceptIds, descendants$conceptId))
      if (!is.null(domainSettings) && !is.null(domainSettings$phoebeExclusions)) {
        recommendations <- recommendations |>
          filter(!.data$relationshipId %in% domainSettings$phoebeExclusions)
      }
      belowMinCountConceptIds <- recommendations |>
        filter(.data$recordCount < private$minCount) |>
        pull(.data$conceptId) |>
        unique()

      # Concept information (domain, valid) in Hecate may be outdated, so fetch from vocab server:
      recommendations <- getConceptsFromIds(
        conceptIds = recommendations$conceptId,
        origin = "RECOMMENDED",
        connection = connection,
        vocabDatabaseSchema = vocabDatabaseSchema) |>
        mutate(status = if_else(.data$conceptId %in% belowMinCountConceptIds, "BELOW_MIN_COUNT", "UNADJUDICATED"))

      if (!is.null(domainSettings)) {
        recommendations <- recommendations |>
          filter(.data$domainId %in% domainSettings$domainId)
      }
      if (!is.null(excludedVocabularyIds)) {
        recommendations <- recommendations |>
          filter(!.data$vocabularyId %in% excludedVocabularyIds)
      }
      recommendations <- recommendations |>
        removeGenericConcepts() |>
        filter(!.data$conceptId %in% c(conceptIds, descendants$conceptId))
      message("  - Found ", nrow(recommendations), " additional concepts through Phoebe recommendations")

      concepts <- bind_rows(descendants, recommendations)
      if (!private$keepLowCountConcepts) {
        concepts <- concepts |>
          filter(status != "BELOW_MIN_COUNT")
      }
      validateConcepts(concepts)
      return(concepts)
    }
  ),
  private = list(
    minCount = NULL,
    keepLowCountConcepts = NULL
  )
)

getHecatePhoebeRecommendations <- function(conceptIds) {
  # Calling https://hecate.pantheon-hds.com/api/concepts/phoebe/bulk with 1 concept ID throws an error
  if (length(conceptIds) == 1) {
    return(getHecatePhoebeRecommendationsSingle(conceptIds))
  } else {
    return(getHecatePhoebeRecommendationsBulk(conceptIds))
  }
}

getHecatePhoebeRecommendationsSingle <- function(conceptId) {
  urlTemplate <- "https://hecate.pantheon-hds.com/api/concepts/%d/phoebe"

  message("  Searching Phoebe for concepts ", conceptId)

  response <- httr::GET(sprintf(urlTemplate, conceptId))

  if (httr::status_code(response) == 200) {
    contentText <- httr::content(response, "text", encoding = "UTF-8")
    if (contentText == "[]") {
      data <- createEmptyPhoebeData()
    } else {
      data <- jsonlite::fromJSON(contentText)
    }
  } else {
    stop("Error in phoebe search for concept ", conceptId, " with HTML repsonse ", httr::status_code(response))
  }
  data <- data |>
    SqlRender::snakeCaseToCamelCaseNames()
  return(data)
}

getHecatePhoebeRecommendationsBulk <- function(conceptIds) {
  phoebeUrlstring <- "https://hecate.pantheon-hds.com/api/concepts/phoebe/bulk"

  phoebeData <- list()

  start <- 1
  batchSize <- 500
  while (start <= length(conceptIds)) {
    end <- min(start + batchSize - 1, length(conceptIds))
    message("  Searching Phoebe for concepts ", start, " to ", end, " out of ", length(conceptIds))

    batch <- conceptIds[start:end]
    response <- httr::POST(phoebeUrlstring, body = list(ids = as.integer(batch)), encode = "json" )

    if (httr::status_code(response) == 200) {
      contentText <- httr::content(response, "text", encoding = "UTF-8")
      if (contentText == "[]") {
        data <- createEmptyPhoebeData()
      } else {
        data <- jsonlite::fromJSON(contentText)
        data <- bind_rows(data$results)
        phoebeData[[length(phoebeData) + 1]] <- data
      }
    } else {
      stop(sprintf(
        "Error in phoebe search for concepts %s: %s",
        paste(batch, collapse = ", "),
        httr::status_code(response)
      ))
    }
    start <- end + 1
  }
  phoebeData <- bind_rows(phoebeData) |>
    SqlRender::snakeCaseToCamelCaseNames()
  return(phoebeData)
}

createEmptyPhoebeData <- function() {
  data <- tibble(
    relationship_id = "",
    concept_id = 1,
    concept_name = "",
    vocabulary_id = "",
    record_count = 0
  ) |>
    filter(.data$concept_id == 2)
  return(data)
}

addHecateRecordCounts <- function(concepts) {
  baseUrl <- "https://hecate.pantheon-hds.com/api/concepts/"
  concepts <- concepts |>
    mutate(recordCount = 0)
  for (i in seq_len(nrow(concepts))) {
    response <- httr::GET(paste0(baseUrl, as.integer(concepts$conceptId[i])))

    if (httr::status_code(response) == 200) {
      contentText <- httr::content(response, "text", encoding = "UTF-8")
      if (contentText != "[]") {
        data <- jsonlite::fromJSON(contentText)
        concepts$recordCount[i] <- data$record_count
      }
    } else {
      stop(sprintf(
        "Error in Hecate search for concept %s: %s",
        concepts$conceptId[i],
        httr::status_code(response)
      ))
    }
  }
  return(concepts)
}

getDescendants <- function(conceptIds, domainSettings, excludedVocabularyIds, connection, vocabDatabaseSchema) {
  sql <- "
    SELECT DISTINCT concept_id,
      concept_name,
      vocabulary_id,
      domain_id,
      concept_class_id
    FROM @cdm_database_schema.concept
    INNER JOIN @cdm_database_schema.concept_ancestor
      ON descendant_concept_id = concept_id
    WHERE ancestor_concept_id IN (@concept_ids)
    {@domain_ids != ''} ? {  AND domain_id IN (@domain_ids)}
    {@excluded_vocabulary_ids != ''} ? {  AND vocabulary_id NOT IN (@excluded_vocabulary_ids)}
      AND invalid_reason IS NULL;
  "
  descendants <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    cdm_database_schema = vocabDatabaseSchema,
    concept_ids = conceptIds,
    domain_ids = paste(sprintf("'%s'", domainSettings$domainIds), collapse = ", "),
    excluded_vocabulary_ids = paste(sprintf("'%s'", excludedVocabularyIds), collapse = ", "),
    snakeCaseToCamelCase = TRUE
  )
  descendants <- asConcepts(descendants, origin = "DESCENDANT", status = "UNADJUDICATED")
  return(descendants)
}

removeGenericConcepts <- function(concepts) {
  excludeWords <- c(
    "finding$",
    "^Disorder of",
    "^Finding of",
    "^Disease of",
    "Injury of",
    "by site$",
    "by body site$",
    "by mechanism$",
    "of body region$",
    "of anatomical site$",
    "of specific body structure$"
  )

  exceptionWords <- c(
    "due to",
    "caused by"
  )
  concepts <- concepts |>
    filter(!grepl(paste(excludeWords, collapse = "|"), .data$conceptName) |
             grepl(paste(exceptionWords, collapse = "|"), .data$conceptName))
  return(concepts)
}
