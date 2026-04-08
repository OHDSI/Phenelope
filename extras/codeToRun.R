library(Phenelope)
#set the option below if needed
# options(sqlRenderTempEmulationSchema = "some_schema")

baseUrl <- "https://epi.jnj.com:8443/WebAPI"
conceptSetNamePrefix <- "[LLM Concept Set]"

#set up your LLM client - create the LLM client for one model below
llmClient <- ellmer::chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

# The 4o model is recommended
llmClient <- ellmer::chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_gpt4o_endpoint")),
  api_version = "2023-03-15-preview",
  model = "gpt-4o",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

#adjust the code below for your database info - specific database doesn't matter - just a route to get to the vocab
currentCcaeVersion <- 3789
dbConnectionString <- paste("jdbc:databricks://",
                            Sys.getenv("DATABRICKS_HOST"),
                            ":443/default;transportMode=http;ssl=1;AuthMech=3;httpPath=",
                            Sys.getenv("DATABRICKS_HTTP_PATH"),
                            ";EnableArrow=1;",sep='')

ccaeConnection <- list(cdmDatabaseSchema = paste0("merative_ccae.cdm_merative_ccae_v", currentCcaeVersion),
                       connectionDetails = DatabaseConnector::createConnectionDetails(dbms = "spark",
                                                                                      connectionString =  dbConnectionString,
                                                                                      user = keyring::key_get(service = "dBuserName"),
                                                                                      password = keyring::key_get(service = "dBpassword")))
database <- ccaeConnection

#example - these are the parameters you would use to create a real concept set (below are parameters for testing functions)
#HELLPsyndrome is a good one to test BUT change the conceptSetName and outputFolder first
HELLPsyndrome <- list(conceptList = c(4316372), #list of seed concepts to start the analysis
                      condition = "HELLP syndrome", #important - this will determine what llm uses as the main condition
                      excludedConditions = "none", #text list of conditions that should be excluded
                      excludeCauses = F, #set to true if you don't want to include causes in the concepts (usually left as F)
                      belowMinimumCountApproach = "TEST ALL", #how to test/not test concepts below minimum counts - important for cancers
                      #choose: "TEST ALL" to test all the concepts below the minimum count
                      #"TEST PHOEBE" to test only the ones below the minimum count AND recommended by PHOEBE
                      #   and automatically set all the others (descendants) to Yes
                      #"EXCLUDE ALL" to automatically set all the ones below the minimum count to No (won't be in concept set)
                      #"INCLUDE ALL" to automatically set all the ones below the minimum count to Yes (will be in concept set)
                      minCount = 0, #the threshold for testing (see above)
                      outputFolder = "p:/shared/llm/HELLP syndrome", #where you want the final artifacts to be saved
                      clinicalContext = "patients in general population", #if you want a concept set specific to a prior condition, add it here
                      additionalInformation = "", #added information for the prompt
                      tries = 1, #the number of times you want llm to go through the list if you are concerned about consistency
                      successes = 1) #the number of successes (Ubiquitous) responses that must be achieved to include concept

conditionList <- list(HELLPsyndrome) #can put multiple concept set specs in this list

################################################################################################
for(conditionUp in 1:length(conditionList)) {
  #produces clinical description
  clinicalDescription <- Phenelope::createClinicalDescription(condition= conditionList[[conditionUp]]$condition,
                                                              excludedConditions = conditionList[[conditionUp]]$excludedConditions,
                                                              llmClient = llmClient,
                                                              outputToWord = TRUE,
                                                              wordFileName = file.path(conditionList[[conditionUp]]$outputFolder,
                                                                                   "clinical description.docx"))

  #create concept set
  finalSet <- Phenelope::createConceptSet(conceptName = conditionList[[conditionUp]]$condition,
                                          originalConceptList = conditionList[[conditionUp]]$conceptList,
                                          excludedConditions = conditionList[[conditionUp]]$excludedConditions,
                                          tries = conditionList[[conditionUp]]$tries,
                                          successes = conditionList[[conditionUp]]$successes,
                                          llmClient = llmClient,
                                          connectionDetails = database$connectionDetails,
                                          cdmDatabaseSchema = database$cdmDatabaseSchema,
                                          minCount = conditionList[[conditionUp]]$minCount,
                                          belowMinimumCountApproach = conditionList[[conditionUp]]$belowMinimumCountApproach,
                                          clinicalContext = conditionList[[conditionUp]]$clinicalContext,
                                          additionalInformation = conditionList[[conditionUp]]$additionalInformation,
                                          outputDirectory = conditionList[[conditionUp]]$outputFolder)

  #after job extras
  if(!is.null(finalSet)) {
    #view the results
    (View(finalSet[[1]], paste0(conditionList[[conditionUp]]$condition, " Results")))

    #post to ATLAS
    ROhdsiWebApi::authorizeWebApi(baseUrl, "windows") # Windows
    conceptSetName <- paste(conceptSetNamePrefix, conditionList[[conditionUp]]$condition)
    post <- ROhdsiWebApi::postConceptSetDefinition(name = conceptSetName,
                                                   conceptSetDefinition = finalSet[[2]], baseUrl)
  }
}

################################################################################################
#create clinical description stand-alone (the full process above will automatically produce a clinical
#description along with the concept set)
clinicalDescription <- Phenelope::createClinicalDescription(condition= "agitation in Alzheimer's disease",
                                                            excludedConditions = "none",
                                                            wordFileName = "p:/shared/llm/agitation in Alzheimer's disease/clinicalDescription.docx",
                                                            llmClient = llmClient,
                                                            outputToWord = TRUE)





