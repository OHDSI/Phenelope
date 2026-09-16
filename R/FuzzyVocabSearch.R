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

searchConcepts = function(term, domainSettings, excludedVocabularyIds, maxN = 10, fuzzyVocabSearchType) {
  if (fuzzyVocabSearchType == "HECATE") {
    searchResults <- searchConceptsHecate(
      term = term,
      domainSettings = domainSettings,
      excludedVocabularyIds = excludedVocabularyIds,
      maxN = maxN
    )
    if (nrow(searchResults) == 0) {
      searchResults <- getEmptySearchResult()
    }
  } else {
    stop("Unknown fuzzyVocabSearchType: ", fuzzyVocabSearchType)
  }
  return(searchResults)

}

getEmptySearchResult <- function() {
  result <- tibble(
    conceptId = NA_integer_,
    conceptName = NA_character_,
    domainId = NA_character_,
    vocabularyId = NA_character_,
    conceptClassId = NA_character_,
    standardConcept = NA_character_,
    conceptCode = NA_character_,
    invalidReason = NA_character_,
    validStartDate = NA_character_,
    validEndDate = NA_character_,
    recordCount = NA_integer_
  ) |>
    filter(.data$conceptId == 1)
}

searchConceptsHecate = function(term, domainSettings, excludedVocabularyIds, maxN = 10) {
  maxRetries <- 10
  waitTime <- 3
  params <- list(
    q = term,
    limit = maxN
  )
  if (!is.null(domainSettings)) {
    params$domain_id <- paste(domainSettings$domainIds, collapse = ",")
  }
  if (!is.null(excludedVocabularyIds)) {
    params$exclude_vocabulary_id <- paste(excludedVocabularyIds, collapse = ",")
  }
  url <- "https://hecate.pantheon-hds.com/api/search_standard"

  for (attempt in 1:maxRetries) {
    response <- tryCatch(
      {
        httr::GET(url, query = params)
      },
      error = function(e) {
        message(paste("Attempt", attempt, "failed with error:", e$message))
        return(NULL)
      }
    )
    if (!is.null(response) && httr::status_code(response) == 200) {
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content_text)
      data <- bind_rows(data$concepts) |>
        SqlRender::snakeCaseToCamelCaseNames() |>
        as_tibble()
      return(data)
    }
    if (attempt < maxRetries) {
      message(sprintf(
        "Search failed for '%s' (Status: %s). Retrying in %s seconds...",
        term,
        if (is.null(response)) "Connection Error" else httr::status_code(response),
        waitTime
      ))
      Sys.sleep(waitTime)
    }
  }
  stop(sprintf("All %s attempts failed for term '%s'.", maxRetries, term))
}
