library(testthat)
library(Phenelope)

test_that("condenseConceptSet", {
  includedConceptIds <- c(1, 2, 3, 4)
  conceptsToDescendants <- data.frame(
    conceptId = c(1, 1, 1, 1, 2, 2, 3, 4, 5),
    descendantConceptId = c(1, 2, 3, 5, 2, 3, 3, 4, 5)
  )
  conceptMetaData <- data.frame(conceptId = c(1, 2, 3, 4, 5), standardConcept = c("S", "S", "S", "S", "S"))
  expression <- condenseConceptSet(list(
    includedConceptIds = includedConceptIds,
    conceptsToDescendants = conceptsToDescendants,
    conceptMetaData = conceptMetaData
  ))
  expect_equal(length(expression$items), 3)
})
