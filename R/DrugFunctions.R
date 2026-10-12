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

getDrugClass <- function(drugName, llmClient, costTracker = NULL) {
  outputType <- ellmer::type_array(ellmer::type_object(
    drugName = ellmer::type_string(),
    routeOfAdministrationYesNo = ellmer::type_enum(values = c("YES","NO")),
    routeOfAdministrationName = ellmer::type_string(),
    dosageFormYesNo = ellmer::type_enum(values = c("YES","NO")),
    dosageFormName = ellmer::type_string(),
    drugStrengthYesNo = ellmer::type_enum(values = c("YES","NO")),
    drugStrengthName = ellmer::type_string(),
    drugBrandYesNo = ellmer::type_enum(values = c("YES","NO")),
    drugBrandName = ellmer::type_string(),
    multipleDrugYesNo = ellmer::type_enum(values = c("YES","NO")),
    multipleDrugNames = ellmer::type_string(),
    singleGroupDrug = ellmer::type_enum(values = c("SINGLE","GROUP")),
    singleGroupName = ellmer::type_string(),
    indicationSpecificYesNo = ellmer::type_enum(values = c("YES","NO")),
    indication = ellmer::type_string()
  ))

  promptFile <- system.file("prompts", "DetermineDrugCharacteristics.txt", package = "Phenelope")
  prompt <- readLines(promptFile)
  instantiatedPrompt <- gsub("%drug_name%", drugName, prompt)
  instantiatedPrompt <- paste(instantiatedPrompt, collapse = "\n")

  drugNameInformation <- queryLlm(llmClient = llmClient,
                                  prompt = instantiatedPrompt,
                                  costTracker = costTracker,
                                  outputType = outputType)

  if (drugNameInformation$routeOfAdministrationYesNo == "NO" &
      drugNameInformation$dosageFormYesNo == "NO" &
      drugNameInformation$drugStrengthYesNo ==  "NO" &
      drugNameInformation$drugBrandYesNo ==  "NO" &
      drugNameInformation$multipleDrugYesNo == "NO") {
    class <- "Ingredient"
  } else if (drugNameInformation$routeOfAdministrationYesNo == "YES" &
             drugNameInformation$dosageFormYesNo == "NO" &
             drugNameInformation$drugStrengthYesNo ==  "NO" &
             drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Clinical Drug Form"
  } else if (drugNameInformation$routeOfAdministrationYesNo == "YES" &
             drugNameInformation$dosageFormYesNo == "YES" &
             drugNameInformation$drugStrengthYesNo ==  "NO" &
             drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Clinical Drug Form"
  } else if (drugNameInformation$routeOfAdministrationYesNo == "NO" &
             drugNameInformation$dosageFormYesNo == "NO" &
             drugNameInformation$drugStrengthYesNo ==  "YES" &
             drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Clinical Drug Comp"
  } else if (drugNameInformation$routeOfAdministrationYesNo == "YES" &
             # drugNameInformation$dosageFormYesNo == "YES" &
             drugNameInformation$drugStrengthYesNo ==  "YES" &
             drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Clinical Drug"
  } else if (drugNameInformation$routeOfAdministrationYesNo == "YES" &
             drugNameInformation$dosageFormYesNo == "YES" &
             drugNameInformation$drugStrengthYesNo ==  "NO" &
             drugNameInformation$drugBrandYesNo ==  "YES" ) {
    class <- "Branded Drug Form"
  } else if (drugNameInformation$routeOfAdministrationYesNo == "NO" &
             drugNameInformation$dosageFormYesNo == "NO" &
             drugNameInformation$drugStrengthYesNo ==  "YES" &
             drugNameInformation$drugBrandYesNo ==  "YES" ) {
    class <- "Branded Drug Comp"
  } else if (drugNameInformation$routeOfAdministrationYesNo == "YES" &
             drugNameInformation$dosageFormYesNo == "YES" &
             drugNameInformation$drugStrengthYesNo ==  "YES" &
             drugNameInformation$drugBrandYesNo ==  "YES" ) {
    class <- "Branded Drug"
  } else if (drugNameInformation$multipleDrugYesNo == "YES")  { #best fit for multiple active ingredients
    class <- "Clinical Drug Form"
  } else {
    class <- "Unknown"
  }

  drugNameInformation <- data.frame(drugClass = class, drugNameInformation)
  return(drugNameInformation)
}

getDrugConcepts <- function(drugClass,
                            searchTerms,
                            fuzzyVocabSearchType) {
  if (drugClass$drugClass %in% c("Clinical Dose Group", "Branded Dose Group")) {
    standardConceptCode <- "C"
  } else {
    standardConceptCode <- "S"
  }
  if (drugClass$drugClass %in% c("Ingredient")) {
    limit <- 1
  } else {
    limit <- 200
  }

  concepts <- list()
  for (i in seq_along(searchTerms)) {
    concepts[[i]] <- searchConcepts(term = searchTerms[i],
                                    domainIds = "Drug",
                                    conceptClassIds = drugClass$drugClass,
                                    vocabularyIds = c("RxNorm", "RxNorm Extension"),
                                    standardConcept = standardConceptCode,
                                    maxN = limit,
                                    fuzzyVocabSearchType = fuzzyVocabSearchType)
  }
  concepts <- bind_rows(concepts) |>
    filter(!duplicated(.data$conceptId))
  concepts <- asConcepts(concepts,
                         origin = "RECOMMENDED",
                         status = "UNADJUDICATED")
  return(concepts)
}

getDrugSearchTerms <- function(drugClass, clinicalDefinition, llmClient, costTracker) {
  promptFile <- system.file("prompts", "CreateDrugList.txt", package = "Phenelope")
  prompt <- readLines(promptFile)
  systemPromptFile <- system.file("prompts", "CreateDrugListSystem.txt", package = "Phenelope")
  systemPrompt <- paste(readLines(systemPromptFile), collapse = "\n")

  outputType <- ellmer::type_object(
    drugName = ellmer::type_array(ellmer::type_string())
  )

  if (drugClass$singleGroupDrug == "SINGLE") { #can reduce level of search for a single drug search
    passes <- 3
    startNumber <- 5
    nextNumber <- 5
    llmSearchString <- drugClass$singleGroupName
  } else {
    passes <- 5
    startNumber <- 100
    nextNumber <- 50
    llmSearchString <- drugClass$drugName
  }

  searchTerms <- c()
  for (i in 1:passes) {
    instantiatedPrompt <- gsub("%search_term%", llmSearchString, prompt)
    instantiatedPrompt <- gsub("%current_list%", paste0(searchTerms, collapse = "; "), instantiatedPrompt)
    if (is.null(clinicalDefinition) || clinicalDefinition == "") {
      # Delete entire line with %definition%
      instantiatedPrompt <- gsub("\n[^\n]*%definition%[^\n]*\n", "", instantiatedPrompt)
    } else {
      instantiatedPrompt <- gsub("%definition%", clinicalDefinition, instantiatedPrompt)
    }
    instantiatedPrompt <- gsub("%start_number%", startNumber, instantiatedPrompt)
    instantiatedPrompt <- gsub("%next_number%", nextNumber, instantiatedPrompt)
    instantiatedPrompt <- paste(instantiatedPrompt, collapse = "\n")

    newSearchTerms <- queryLlm(prompt = instantiatedPrompt,
                               systemPrompt = systemPrompt,
                               llmClient = llmClient,
                               costTracker = costTracker,
                               outputType = outputType)

    searchTerms <- unique(c(searchTerms, newSearchTerms$drugName))
    message("- Round ", i, " of ", passes, " - Total unique terms: ", length(searchTerms))
  }
  return(searchTerms)
}

asDrugConceptSetExpression <- function(concepts, name, connection, vocabDatabaseSchema) {
  validateConcepts(concepts)

  approvedConceptIds <- concepts |>
    filter(.data$status == "APPROVED") |>
    pull(.data$conceptId)

  conceptSet <- Capr::cs(Capr::descendants(approvedConceptIds), name = name)

  conceptSet <- Capr::getConceptSetDetails(conceptSet, connection, vocabularyDatabaseSchema = vocabDatabaseSchema)
  json <- Capr::as.json(conceptSet)
  return(json)
}
