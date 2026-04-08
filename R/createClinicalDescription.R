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

#' Create clinical description
#'
#' @description
#' Creates a clinical description using LLM
#'
#' @details
#' This function will create a concept set starting from a clinical condition and a concept id
#'
#' @param condition                  name of the clinical condition to describe
#' @param llmClient                  connection object for the LLM client (see ellmer package for object details)
#' @param excludedConditions         list of excluded conditions (the clinical description will cite these)
#' @param outputToWord               write output to a Word document T or F
#' @param wordFileName               name of the Word file to create when `outputToWord = TRUE`
#'
#' @return
#' Invisibly returns the clinical description as a text string.
#'
#' @export
createClinicalDescription <- function(condition,
                                      llmClient,
                                      excludedConditions = NULL,
                                      outputToWord = TRUE,
                                      wordFileName = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(condition, len = 1, add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertCharacter(excludedConditions, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertLogical(outputToWord, len = 1, add = errorMessages)
  checkmate::assertCharacter(wordFileName, null.ok = TRUE, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  promptUp <- system.file("prompts", "clinicalDescriptionPromptBriefWithExclusions.txt", package = "Phenelope")
  getPrompt <- readr::read_file(file = promptUp) # can change to other prompt text file

  writeLines(paste0("\nCreating clinical description for: ", condition))

  openPrompt <- paste("For the condition:", condition)

  if (length(excludedConditions) > 0) {
    writeLines(paste0("\nExcluding conditions: ", excludedConditions))
    openPrompt <- paste(openPrompt, ", Excluding: ", excludedConditions)
  }

  prompt <- paste(openPrompt, getPrompt)

  response <- llmClient$chat(prompt, echo = "none")

  # LLM output
  lines <- strsplit(response, "\n")[[1]]

  if (outputToWord) {
    # Create a new Word document
    doc <- officer::read_docx()

    # Add LLM output to the document
    for (line in lines) {
      if (gregexpr("~~~~", line)[[1]][1] >= 1) {
        line <- gsub("\\~", "", line)
        # Define bold text styling
        boldText <- officer::fp_text(bold = TRUE, font.size = 18, color = "black")

        # Create a paragraph with the bold text
        line <- officer::fpar(officer::ftext(line, prop = boldText))

        # Add the paragraph to the document
        doc <- officer::body_add_fpar(doc, line)
      } else if (gregexpr("\\~\\~\\~", line)[[1]][1] >= 1) {
        line <- gsub("\\~", "", line)
        # Define bold text styling
        boldText <- officer::fp_text(bold = TRUE, font.size = 14, color = "black", underlined = TRUE)

        # Create a paragraph with the bold text
        line <- officer::fpar(officer::ftext(line, prop = boldText))

        # Add the paragraph to the document
        doc <- officer::body_add_fpar(doc, line)
      } else if (gregexpr("\\~\\~", line)[[1]][1] >= 1) {
        line <- gsub("\\~", "", line)
        # Define bold text styling
        boldText <- officer::fp_text(bold = TRUE, font.size = 12, color = "black")

        # Create a paragraph with the bold text
        line <- officer::fpar(officer::ftext(line, prop = boldText))

        # Add the paragraph to the document
        doc <- officer::body_add_fpar(doc, line)
      } else {
        line <- gsub("\\~", "", line)
        doc <- officer::body_add_par(doc, line, style = "Normal")
      }
    }

    # Save the document
    outputDirectory <- dirname(wordFileName)
    if (!dir.exists(outputDirectory)) {
      success <- dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)
      if (!success) stop("Failed to create directory: ", outputDirectory)
    }

    print(doc, target = wordFileName)
    message("Clinical description for ", condition, " created at ", wordFileName)
  }

  lines <- trimws(lines)
  lines <- paste(lines, collapse = "\n\n")
  if (outputToWord) {
    invisible(lines)
  } else {
    return(lines)
  }
}
