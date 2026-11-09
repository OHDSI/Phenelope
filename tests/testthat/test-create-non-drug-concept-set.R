library(testthat)
library(Phenelope)
library(dplyr)

createTestConcepts <- function(conceptIds, status = "UNADJUDICATED") {
  asConcepts(
    tibble(
      conceptId = conceptIds,
      conceptName = paste("Concept", conceptIds),
      vocabularyId = rep("SNOMED", length(conceptIds)),
      domainId = rep("Condition", length(conceptIds)),
      conceptClassId = rep("Clinical Finding", length(conceptIds)),
      origin = rep("SEED", length(conceptIds)),
      status = rep(status, length(conceptIds))
    )
  )
}

TestSeedConceptFinder <- R6::R6Class(
  "TestSeedConceptFinder",
  inherit = SeedConceptFinder,
  public = list(
    initialize = function(concepts) {
      private$concepts <- concepts
    },
    findSeedConcepts = function(...) {
      private$concepts
    }
  ),
  private = list(concepts = NULL)
)

TestConceptRecommender <- R6::R6Class(
  "TestConceptRecommender",
  inherit = ConceptRecommender,
  public = list(
    initialize = function(concepts) {
      private$concepts <- concepts
    },
    recommendConcepts = function(...) {
      private$concepts
    }
  ),
  private = list(concepts = NULL)
)

TestConceptAdjudicator <- R6::R6Class(
  "TestConceptAdjudicator",
  inherit = ConceptAdjudicator,
  public = list(
    initialize = function(status) {
      private$status <- status
    },
    adjudicateConcepts = function(concepts, ...) {
      concepts$status <- private$status
      concepts
    }
  ),
  private = list(status = NULL)
)

test_that("createNonDrugConceptSet warns and returns an empty expression when no concepts are approved", {
  seedConcepts <- createTestConcepts(201826)
  emptyRecommendations <- createTestConcepts(integer())
  cacheFolder <- withr::local_tempdir()
  writeLines("CONDITION", file.path(cacheFolder, "Domain.txt"))

  result <- expect_warning(
    createNonDrugConceptSet(
      name = "test condition",
      llmClient = client,
      connectionDetails = connectionDetails,
      vocabDatabaseSchema = "main",
      cacheFolder = cacheFolder,
      seedConceptFinder = TestSeedConceptFinder$new(seedConcepts),
      conceptRecommender = TestConceptRecommender$new(emptyRecommendations),
      conceptAdjudicator = TestConceptAdjudicator$new("REJECTED")
    ),
    class = "NoApprovedConceptsWarning"
  )

  result <- jsonlite::fromJSON(result)
  expect_length(result$items, 0)
})

test_that("createNonDrugConceptSet warns and returns the first seed concept when too many concepts need adjudication", {
  seedConcepts <- createTestConcepts(80502)
  recommendations <- createTestConcepts(201827)
  cacheFolder <- withr::local_tempdir()
  writeLines("CONDITION", file.path(cacheFolder, "Domain.txt"))

  result <- expect_warning(
    createNonDrugConceptSet(
      name = "test condition",
      llmClient = client,
      connectionDetails = connectionDetails,
      vocabDatabaseSchema = "main",
      cacheFolder = cacheFolder,
      seedConceptFinder = TestSeedConceptFinder$new(seedConcepts),
      conceptRecommender = TestConceptRecommender$new(recommendations),
      maxNforAdjudication = 1,
      conceptAdjudicator = TestConceptAdjudicator$new("REJECTED"),
      condenseConceptSet = FALSE
    ),
    class = "TooManyConceptsWarning"
  )

  result <- jsonlite::fromJSON(result)
  conceptIds <- result$items$concept$CONCEPT_ID
  expect_equal(conceptIds, 80502)
  expect_equal(result$items$includeDescendants, TRUE)
})
