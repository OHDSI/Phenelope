# GPT-o3 running on Azure with additional timing reminder, removing narrative request.
client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_gpt4o_endpoint")),
  api_version = "2023-03-15-preview",
  model = "gpt-4o",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

prompt <- "who was the first president of the US?"
response <- client$chat(prompt, echo = "output")

client$get_cost()

client$get_model()
token_usage()$price

chat <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

capital <- function(chat, country) {
  chat$chat(interpolate("What's the capital of {{country}}"))
}
capital(chat, "New Zealand")
#> Wellington
capital(chat, "France")
#> Paris


url <- sprintf("https://hecate.pantheon-hds.com/api/concepts/%d/phoebe", 440372)
response <- httr::GET(url)

contextText <- as.data.frame(httr::content(response, "text", encoding = "UTF-8"))
data <- jsonlite::fromJSON(contextText)

url <- sprintf("https://hecate.pantheon-hds.com/api/concepts/%d/phoebe", conceptId)
response <- httr::GET(url)

if (httr::status_code(response) == 200) {
  contextText <- httr::content(response, "text", encoding = "UTF-8")
  if (contextText == "[]") {
    return(NULL)
  }
  data <- jsonlite::fromJSON(contextText)
  data <- data |>
    SqlRender::snakeCaseToCamelCaseNames()
  return(data)
} else {
  stop(sprintf("Error in phoebe search for concept %s: %s",
               conceptId,
               httr::status_code(response)))
}


HELLPsyndrome <- list(conceptList = c(4316372), #list of seed concepts to start the analysis
                      condition = "HELLP syndrome", #important - this will determine what llm uses as the main condition
                      excludedConditions = "none", #text list of conditions that should be excluded
                      excludeCauses = F, #set to true if you don't want to include causes in the concepts (usually left as F)
                      belowMinimumCountApproach = "EXCLUDE ALL", #how to test/not test concepts below minimum counts - important for cancers
                      #choose: "TEST ALL" to test all the concepts below the minimum count
                      #"TEST PHOEBE" to test only the ones recommended by PHOEBE and set all the others (descendants) to Ubiquitous
                      #"EXCLUDE ALL" to set all the ones below the minimum count to Rare (won't be in concept set)
                      #"INCLUDE ALL" to set all the ones below the minimum count to Ubiquitous (will be in concept set)
                      minCount = 0, #the threshold for testing (see above)
                      conceptSetName = "[LLM Test] HELLP syndrome", #what you want the concept set in ATLAS to be called
                      outputFolder = "p:/shared/llm/HELLP syndrome", #where you want the final artifacts to be saved
                      clinicalContext = "patients in general population", #if you want a concept set specific to a prior condition, add it here
                      additionalInformation = "", #added information for the prompt
                      tries = 1, #the number of times you want llm to go through the list if you are concerned about consistency
                      successes = 1) #the number of successes (Ubiquitous) responses that must be achieved to include concept

agitation <- list(conceptList = c(4086746), #list of seed concepts to start the analysis
                  condition = "agitation", #important - this will determine what llm uses as the main condition
                  excludedConditions = "none", #text list of conditions that should be excluded
                  excludeCauses = F, #set to true if you don't want to include causes in the concepts (usually left as F)
                  belowMinimumCountApproach = "TEST ALL", #how to test/not test concepts below minimum counts - important for cancers
                  minCount = 0, #the threshold for testing (see above)
                  conceptSetName = "[Epi1314] agitation 2", #what you want the concept set in ATLAS to be called
                  outputFolder = "p:/shared/llm/Epi1314/agitation 2", #where you want the final artifacts to be saved
                  clinicalContext = "patients with dementia", #if you want a concept set specific to a prior condition, add it here
                  additionalInformation = "", #added information for the prompt
                  tries = 1, #the number of times you want llm to go through the list if you are concerned about consistency
                  successes = 1) #the number of successes (Ubiquitous) responses that must be achieved to include concept

psychosis <- list(conceptList = c(436073), #list of seed concepts to start the analysis
                  condition = "psychosis", #important - this will determine what llm uses as the main condition
                  excludedConditions = "schizophrenia, vascular dementia, bipolar disorder, schizoaffective disorder, major depressive disorder", #text list of conditions that should be excluded
                  excludeCauses = F, #set to true if you don't want to include causes in the concepts (usually left as F)
                  belowMinimumCountApproach = "TEST ALL", #how to test/not test concepts below minimum counts - important for cancers
                  minCount = 0, #the threshold for testing (see above)
                  conceptSetName = "[Epi1314] Psychosis 2", #what you want the concept set in ATLAS to be called
                  outputFolder = "p:/shared/llm/Epi1314/psychosis 2", #where you want the final artifacts to be saved
                  clinicalContext = "patients with dementia", #if you want a concept set specific to a prior condition, add it here
                  additionalInformation = "", #added information for the prompt
                  tries = 1, #the number of times you want llm to go through the list if you are concerned about consistency
                  successes = 1) #the number of successes (Ubiquitous) responses that must be achieved to include concept

CopdExacerbation <- list(conceptList = c(257004, 4110056, 43530693, 46269701, 4115044),
                         condition = "COPD Exacerbation",
                         excludedConditions = "none",
                         excludeCauses = F,
                         belowMinimumCountApproach = "TEST ALL",
                         minCount = 0,
                         conceptSetName = "[LLM] COPD Exacerbation",
                         outputFolder = "p:/shared/llm/COPD Exacerbation",
                         clinicalContext = "in general population",
                         additionalInformation = "",
                         tries = 1,
                         successes = 1)

acuteAbdominalPain <- list(conceptList = c(200219),
                         condition = "Acute Abdominal Pain",
                         excludedConditions = "none",
                         excludeCauses = F,
                         belowMinimumCountApproach = "TEST ALL",
                         minCount = 0,
                         conceptSetName = "[LLM] Acute Abdominal Pain",
                         outputFolder = "p:/shared/llm/Lower Respiratory Tract Infection",
                         clinicalContext = "in general population",
                         additionalInformation = "",
                         tries = 1,
                         successes = 1)

malignantSolidTumors <- list(conceptList = c(439392),
                           condition = "malignant solid tumors",
                           excludedConditions = "none",
                           excludeCauses = F,
                           belowMinimumCountApproach = "TEST ALL",
                           minCount = 0,
                           conceptSetName = "[LLM] malignant solid tumors",
                           outputFolder = "p:/shared/llm/Evanette/malignant solid tumors",
                           clinicalContext = "in general population",
                           additionalInformation = "",
                           tries = 1,
                           successes = 1)

conditionList <- list(malignantSolidTumors)

#running rapid tests
#allows for fast multiple testing of concepts to assess consistency of llm answers
psychosis <- list(conceptList = c(rep(4286201, times = 10)),
                  condition = "psychosis",
                  excludedConditions = "schizophrenia, vascular dementia, bipolar disorder, schizoaffective disorder, major depressive disorder", #text list of conditions that should be excluded
                  conceptSetName = "[LLM Test] psychosis",
                  clinicalContext = "patients with dementia", #if you want a concept set specific to a prior condition, add it here
                  outputFolder = "p:/shared/llm/test",
                  tries = 1,
                  successes = 1)

conditionList <- list(psychosis)

for(conditionUp in 1:length(conditionList)) {
  ROhdsiWebApi::authorizeWebApi(baseUrl, "windows") # Windows
  finalSet <- executeLlmConceptCreate(conceptName = conditionList[[conditionUp]]$condition,
                                      originalConceptList = conditionList[[conditionUp]]$conceptList,
                                      excludedConditions = conditionList[[conditionUp]]$excludedConditions,
                                      tries = conditionList[[conditionUp]]$tries,
                                      successes = conditionList[[conditionUp]]$successes,
                                      database = database,
                                      vocabulary = database$vocabulary,
                                      minCount = 0,
                                      belowMinimumCountApproach = "TEST ALL",
                                      clinicalContext = conditionList[[conditionUp]]$clinicalContext,
                                      conceptSetName = conditionList[[conditionUp]]$conceptSetName,
                                      baseUrl = "https://epi.jnj.com:8443/WebAPI",
                                      postConceptSet = F,
                                      outputDirectory = "p:/shared/llm/test", #change to a directory on your machine
                                      quickRun = T,
                                      model = "4o") #set to T to NOT go through all the steps for an "official" run (i.e., for testing)

  View(get(paste0(conditionList[[conditionUp]]$condition, "Df")), paste0(conditionList[[conditionUp]]$condition, " Results"))
}



library(Eunomia)
connectionDetails <- getEunomiaConnectionDetails()
connection <- DatabaseConnector::connect(connectionDetails)
DatabaseConnector::querySql(connection, "SELECT * FROM concept_ancestor where ancestor_concept_id = 4244662;")
#  COUNT(*)
#1     2694

getTableNames(connection,databaseSchema = 'main')
disconnect(connection)
