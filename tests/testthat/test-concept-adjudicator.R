library(testthat)
library(Phenelope)

prompts <- list(
  prompt = "%concepts%",
  systemPrompt = "System prompt",
  quickScreenPrompt = "%concepts%",
  quickScreenSystemPrompt = "System prompt"
)

createAdjudicator <- function(...) {
  do.call(
    DefaultConceptAdjudicator$new,
    utils::modifyList(prompts, list(...))
  )
}

emptyConcepts <- structure(
  data.frame(
    conceptId = integer(),
    conceptName = character(),
    vocabularyId = character(),
    domainId = character(),
    conceptClassId = character(),
    origin = character(),
    status = character()
  ),
  class = c("Concepts", "data.frame")
)

test_that("DefaultConceptAdjudicator validates constructor inputs", {
  expect_no_error(createAdjudicator())
  expect_error(createAdjudicator(batchSize = 0), "batchSize")
  expect_error(createAdjudicator(nForQuickScreen = 0), "nForQuickScreen")
  expect_error(createAdjudicator(quickScreenBatchSize = 1.5), "quickScreenBatchSize")
  expect_error(createAdjudicator(prompt = 1), "prompt")
  expect_error(createAdjudicator(systemPrompt = character()), "systemPrompt")
  expect_error(createAdjudicator(quickScreenPrompt = TRUE), "quickScreenPrompt")
  expect_error(createAdjudicator(quickScreenSystemPrompt = c("one", "two")), "quickScreenSystemPrompt")
})
