library(testthat)
library(Phenelope)

test_that("getDomain validates inputs", {
  expect_error(getDomain(name = 1, llmClient = client), "name")
  expect_error(getDomain(name = character(), llmClient = client), "name")
  expect_error(getDomain(name = c("Condition", "Procedure"), llmClient = client), "name")
  expect_error(getDomain(name = "Condition", llmClient = environment()), "R6")
  expect_error(getDomain(name = "Condition", llmClient = client, costTracker = 1), "environment")
})
