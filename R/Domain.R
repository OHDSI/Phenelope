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

#' Get the domain of a concept set target
#'
#' @param name The name of the concept set.
#' @template LlmClient
#' @template CostTracker
#'
#' @returns
#' An uppercase character string with the name of the domain (e.g. 'CONDITION').
#'
#' @export
getDomain <- function(name, llmClient, costTracker = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(name, len = 1, add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertEnvironment(costTracker, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # promptFile <- "inst/prompts/Domain.txt"
  promptFile <- system.file("prompts", "Domain.txt", package = "Phenelope")
  prompt <- paste(readLines(promptFile), collapse = "\n")
  prompt <- gsub("%name%", name, prompt)
  outputType <- ellmer::type_object(
    domain = ellmer::type_enum(values = c("DRUG","CONDITION","PROCEDURE","VISIT","DEVICE", "MEASUREMENT"))
  )
  domain <- queryLlm(prompt,
                     llmClient = llmClient,
                     costTracker = costTracker,
                     outputType = outputType)
  domain <- domain$domain
  return(domain)
}

#' Get the settings for a specific domain.
#'
#' @param domain The name of a domain (all caps), e.g. 'CONDITION'.
#'
#' @seealso [getDomain()]
#'
#' @returns
#' An object of type `DomainSettings`.
#'
#' @export
getDomainSettings <- function(domain) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(domain, len = 1, add = errorMessages)
  checkmate::assertChoice(domain, choices = c("CONDITION",
                                              "PROCEDURE",
                                              "MEASUREMENT",
                                              "VISIT",
                                              "DEVICE",
                                              "DRUG"), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (domain == "CONDITION") {
    domainSettings <- list(
      domainIds = c("Condition", "Observation"),
      phoebeExclusions = c()
    )
  } else if (domain == "PROCEDURE") {
    domainSettings <- list(
      domainIds = c("Procedure","Device", "Observation"),
      phoebeExclusions = c("Ontology-parent")
    )
  } else if (domain == "MEASUREMENT") {
    domainSettings <- list(
      domainIds = c("Measurement", "Observation"),
      phoebeExclusions = c("Ontology-parent")
    )
  } else if (domain == "VISIT") {
    domainSettings <- list(
      domainIds = c("Visit", "Provider", "Procedure", "Observation"),
      phoebeExclusions = c("Ontology-parent")
    )
  } else if (domain == "DEVICE") {
    domainSettings <- list(
      domainIds = c("Procedure", "Device", "Observation"),
      phoebeExclusions = c("Ontology-parent")
    )
  } else if (domain == "DRUG") {
    stop("The DRUG domain is currently not supported")
  }
  class(domainSettings) <- "DomainSettings"
  return(domainSettings)
}
