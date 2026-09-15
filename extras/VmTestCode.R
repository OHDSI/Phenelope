library(Phenelope)

llmClientO3 <- ellmer::chat_azure_openai(
  endpoint = keyring::key_get("genai_openai_endpoint"),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

cdmDatabaseSchema <- "merative_ccae.cdm_merative_ccae_v3789"

cd <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
# options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")
emulationSchema = "scratch.scratch_mschuemi"

targetName <- "Acute liver failure"
targetClinicalDefinition <- "Acute liver failure is a rare but life-threatening syndrome characterized by the rapid deterioration of hepatocellular function, manifesting as significant coagulopathy and hepatic encephalopathy of any grade, developing within 28 days of the onset of jaundice or initial hepatic symptoms in an individual without evidence of pre-existing chronic liver disease or cirrhosis. The syndrome arises from a direct, primary insult to hepatocytes — including viral, toxic, drug-induced, autoimmune, metabolic, or indeterminate causes — and is conceptually distinct from liver dysfunction occurring as a secondary consequence of hemodynamic compromise (e.g., ischemic hepatitis, shock liver), systemic sepsis, or passive hepatic congestion from right-sided heart failure, all of which are explicitly excluded."

targetName <- "Type B lactic acidosis"
targetClinicalDefinition <- "Type B lactic acidosis is a metabolic disorder characterized by an elevated blood L-lactate concentration of 5 mmol/L or greater accompanied by a systemic arterial pH below 7.35, arising from pathophysiological mechanisms that are not solely attributable to generalized tissue hypoperfusion or systemic hypoxia. The condition encompasses all recognized Type B etiological subtypes: Type B1, in which lactic acidosis occurs in association with systemic diseases such as malignancy, hepatic failure, or non-shock sepsis; Type B2, in which lactic acidosis results from exposure to drugs or exogenous toxins that impair mitochondrial function or cellular lactate metabolism (e.g., metformin, linezolid, or nucleoside reverse-transcriptase inhibitors); and Type B3, in which lactic acidosis arises from inborn errors of metabolism, including mitochondrial myopathies and inherited enzyme deficiencies affecting the pyruvate-lactate pathway. Cases in which a Type B mechanism is identifiable and contributory are included even when concurrent tissue hypoperfusion is present as a co-occurring process. Lactic acidosis solely caused by tissue hypoperfusion or systemic hypoxia (Type A) is excluded, as is D-lactic acidosis â€” a biochemically distinct syndrome produced by intestinal bacterial fermentation that generates D-lactate undetectable by standard clinical L-lactate assays."

conceptSet <- createConceptSet(
  name = targetName,
  clinicalDefinition = targetClinicalDefinition,
  llmClient = llmClientO3,
  connectionDetails = cd,
  vocabDatabaseSchema = cdmDatabaseSchema,
  tempEmulationSchema = emulationSchema,
  cacheFolder = "cacheTypeBlacticAcidosis"
)
writeLines(conceptSet)

# Using mincount and no cache folder:
conceptSet <- createConceptSet(
  name = targetName,
  clinicalDefinition = targetClinicalDefinition,
  llmClient = llmClientO3,
  connectionDetails = cd,
  vocabDatabaseSchema = cdmDatabaseSchema,
  seedConceptFinder = DefaultSeedConceptFinder$new(minCount = 100),
  conceptRecommender = HecateConceptRecomender$new(minCount = 100)
)
writeLines(conceptSet)

# Procedure, no clinical definition and no cache folder:
conceptSet <- createConceptSet(
  name = "Appendectomy",
  llmClient = llmClientO3,
  connectionDetails = cd,
  vocabDatabaseSchema = cdmDatabaseSchema
)
writeLines(conceptSet)
