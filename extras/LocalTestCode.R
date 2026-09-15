library(Phenelope)

llmClient <- ellmer::chat_lmstudio(
  model = "qwen/qwen3.6-35b-a3b"
)
options(forceUnstructured = TRUE)

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = Sys.getenv("LOCAL_POSTGRES_SERVER"),
  user = Sys.getenv("LOCAL_POSTGRES_USER"),
  password = Sys.getenv("LOCAL_POSTGRES_PASSWORD")
)
vocabDatabaseSchema <- "vocab_feb2026"
name <- "Acute liver failure"
clinicalDefinition <- "Acute liver failure is a rare but life-threatening syndrome characterized by the rapid deterioration of hepatocellular function, manifesting as significant coagulopathy and hepatic encephalopathy of any grade, developing within 28 days of the onset of jaundice or initial hepatic symptoms in an individual without evidence of pre-existing chronic liver disease or cirrhosis. The syndrome arises from a direct, primary insult to hepatocytes — including viral, toxic, drug-induced, autoimmune, metabolic, or indeterminate causes — and is conceptually distinct from liver dysfunction occurring as a secondary consequence of hemodynamic compromise (e.g., ischemic hepatitis, shock liver), systemic sepsis, or passive hepatic congestion from right-sided heart failure, all of which are explicitly excluded."

conceptSet <- createConceptSet(
  name = name,
  clinicalDefinition = clinicalDefinition,
  llmClient = llmClient,
  connectionDetails = connectionDetails,
  vocabDatabaseSchema = vocabDatabaseSchema,
  cacheFolder = "cacheAlf"
)


# Test recommender separately:
conceptRecommender <- HecateConceptRecomender$new(minCount = 0)
newConcepts <- conceptRecommender$recommendConcepts(conceptIds = conceptIds,
                                                    domainSettings = getDomainSettings("CONDITION"),
                                                    connection = connection,
                                                    vocabDatabaseSchema = vocabDatabaseSchema)
