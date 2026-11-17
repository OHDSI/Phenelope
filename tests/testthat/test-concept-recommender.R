library(testthat)
library(Phenelope)

domainSettings <- structure(list(), class = "DomainSettings")
connection <- structure(list(), class = "DatabaseConnectorConnection")

recommendConcepts <- function(recommender, ...) {
  arguments <- list(
    conceptIds = 1L,
    domainSettings = domainSettings,
    excludedVocabularyIds = NULL,
    connection = connection,
    vocabDatabaseSchema = "main",
    iteration = 1L
  )
  replacements <- list(...)
  arguments[names(replacements)] <- replacements
  do.call(recommender$recommendConcepts, arguments)
}

test_that("HecateConceptRecommender validates constructor inputs", {
  expect_no_error(HecateConceptRecommender$new())
  expect_no_error(HecateConceptRecommender$new(minCount = 1))
  expect_error(HecateConceptRecommender$new(minCount = -1), "minCount")
  expect_error(HecateConceptRecommender$new(minCount = 1.5), "minCount")
  expect_error(HecateConceptRecommender$new(minCount = c(1, 2)), "minCount")
})

test_that("HecateConceptRecommender validates recommendation inputs", {
  recommender <- HecateConceptRecommender$new()

  expect_warning(recommendConcepts(recommender, conceptIds = integer()), "concepts")
  expect_error(recommendConcepts(recommender, conceptIds = "1"), "conceptIds")
  expect_error(recommendConcepts(recommender, domainSettings = list()), "DomainSettings")
  expect_error(recommendConcepts(recommender, excludedVocabularyIds = 1), "excludedVocabularyIds")
  expect_error(recommendConcepts(recommender, connection = list()), "DatabaseConnectorConnection")
  expect_error(recommendConcepts(recommender, vocabDatabaseSchema = character()), "vocabDatabaseSchema")
  expect_error(recommendConcepts(recommender, vocabDatabaseSchema = c("main", "other")), "vocabDatabaseSchema")
})
