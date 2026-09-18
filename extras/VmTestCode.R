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
seedConceptId <- NULL
targetClinicalDefinition <- "Acute liver failure is a rare but life-threatening syndrome characterized by the rapid deterioration of hepatocellular function, manifesting as significant coagulopathy and hepatic encephalopathy of any grade, developing within 28 days of the onset of jaundice or initial hepatic symptoms in an individual without evidence of pre-existing chronic liver disease or cirrhosis. The syndrome arises from a direct, primary insult to hepatocytes — including viral, toxic, drug-induced, autoimmune, metabolic, or indeterminate causes — and is conceptually distinct from liver dysfunction occurring as a secondary consequence of hemodynamic compromise (e.g., ischemic hepatitis, shock liver), systemic sepsis, or passive hepatic congestion from right-sided heart failure, all of which are explicitly excluded."

targetName <- "Type B lactic acidosis"
seedConceptId <- NULL
targetClinicalDefinition <- "Type B lactic acidosis is a metabolic disorder characterized by an elevated blood L-lactate concentration of 5 mmol/L or greater accompanied by a systemic arterial pH below 7.35, arising from pathophysiological mechanisms that are not solely attributable to generalized tissue hypoperfusion or systemic hypoxia. The condition encompasses all recognized Type B etiological subtypes: Type B1, in which lactic acidosis occurs in association with systemic diseases such as malignancy, hepatic failure, or non-shock sepsis; Type B2, in which lactic acidosis results from exposure to drugs or exogenous toxins that impair mitochondrial function or cellular lactate metabolism (e.g., metformin, linezolid, or nucleoside reverse-transcriptase inhibitors); and Type B3, in which lactic acidosis arises from inborn errors of metabolism, including mitochondrial myopathies and inherited enzyme deficiencies affecting the pyruvate-lactate pathway. Cases in which a Type B mechanism is identifiable and contributory are included even when concurrent tissue hypoperfusion is present as a co-occurring process. Lactic acidosis solely caused by tissue hypoperfusion or systemic hypoxia (Type A) is excluded, as is D-lactic acidosis â€” a biochemically distinct syndrome produced by intestinal bacterial fermentation that generates D-lactate undetectable by standard clinical L-lactate assays."

targetName <- "acute hepatic impairment"
seedConceptId <- NULL
targetClinicalDefinition <- "Acute hepatic impairment is a clinical state characterized by the sudden or subacute onset of measurable reduction in hepatic functional capacity, evidenced by one or more of the following: new-onset hyperbilirubinemia, prolongation of coagulation parameters, or hypoalbuminemia not fully attributable to a non-hepatic cause. The condition encompasses both de novo impairment in individuals with no prior hepatic disease and acute functional deterioration superimposed on pre-existing chronic liver disease (acute-on-chronic presentations). All medically or physiologically driven etiologies qualify, including but not limited to toxic, drug-induced, infectious, autoimmune, ischemic, and congestive mechanisms. Structural hepatic injury resulting from physical trauma â€” such as hepatic laceration from blunt abdominal injury â€” is explicitly excluded, as this represents a mechanistically distinct process of structural disruption rather than intrinsic hepatic functional impairment."

targetName <- "Age-related macular degeneration"
seedConceptId <- NULL
targetClinicalDefinition <- "Age-related macular degeneration (AMD) is a chronic, progressive, idiopathic degenerative disease of the central retina arising from age-driven deterioration of the retinal pigment epithelium, Bruch's membrane, and choriocapillaris, leading to dysfunction and loss of macular photoreceptors. The phenotype encompasses both the non-neovascular (dry/atrophic) form â€” characterized by drusen accumulation, retinal pigment epithelium atrophy, and in advanced cases geographic atrophy â€” and the neovascular (wet) form, characterized by pathological choroidal neovascularization through a disrupted Bruch's membrane, resulting in subretinal or intraretinal fluid, exudation, or hemorrhage. The definition spans the full disease continuum from early AMD (small to medium drusen, no visual impairment) through intermediate AMD (large drusen and/or pigmentary abnormalities) to late AMD (geographic atrophy or active neovascular disease). This phenotype is strictly restricted to idiopathic, age-driven pathophysiology and explicitly excludes inherited macular dystrophies (including Stargardt disease, Best vitelliform dystrophy, and other genetically determined retinal degenerations), as well as macular pathology secondary to a primary systemic or ocular condition such as diabetic macular edema, macular edema from retinal vein occlusion, or uveitis-associated macular disease."

targetName <- "Hypertriglyceridaemia associated acute pancreatitis"
seedConceptId <- NULL
targetClinicalDefinition <- "Hypertriglyceridaemia associated acute pancreatitis is an acute inflammatory condition of the pancreas in which markedly elevated serum triglycerides, at a concentration of 1,000 mg/dL (11.3 mmol/L) or greater measured at or near the time of the acute event, serve as the primary causative mechanism of acinar cell injury, mediated through the toxic effects of free fatty acids liberated from triglyceride-rich lipoproteins within the pancreatic microcirculation. The phenotype encompasses all cases meeting this triglyceride threshold irrespective of the underlying aetiology of the hypertriglyceridaemia itself, including both primary genetic disorders of lipid metabolism (such as familial hypertriglyceridaemia or lipoprotein lipase deficiency) and secondary causes (such as poorly controlled diabetes mellitus, hypothyroidism, obesity, or lipid-altering medications). Cases in which a biliary aetiology â€” including cholelithiasis or biliary obstruction â€” or significant alcohol use is identified as a concurrent precipitating cause of the pancreatitis episode are excluded, as these represent distinct pathophysiological pathways that preclude unambiguous attribution of the acute event to hypertriglyceridaemia alone."
cacheFolder <- "cacheHaap"

targetName <- "Cigarette smoker"
seedConceptId <- 903657
targetClinicalDefinition <- "A cigarette smoker is an individual with documented, habitual use of combusted tobacco in the form of manufactured or hand-rolled cigarettes, characterized by the purposeful inhalation of smoke into the respiratory tract. The designation encompasses both current smokers (those who have smoked on ≥1 day in the past 30 days) and past or former smokers with a confirmed lifetime consumption of ≥100 cigarettes, as this level establishes a clinically meaningful exposure associated with long-term physiologic effects. It includes daily, non-daily, and heavy or light patterns of cigarette consumption, regardless of concurrent use of other tobacco products or nicotine replacement therapies. The term explicitly excludes individuals who have never smoked, those solely exposed to second-hand smoke, users of non-combustible nicotine products (e-cigarettes, heated tobacco, chewing tobacco, snuff), and those whose only tobacco use involves cigars, pipes, hookahs, or other non-cigarette forms."
cacheFolder <- "cacheCigaretteSmoker"

targetName <- "Cerebral hemorrhage"
seedConceptId <- 376713
targetClinicalDefinition <- "Cerebral hemorrhage is an acute or subacute pathological accumulation of blood within the brain parenchyma and/or ventricular system caused by rupture of intracerebral or intraventricular blood vessels. It encompasses both primary (e.g., hypertensive, amyloid-related) and secondary (e.g., vascular malformation, tumor, coagulopathy, hemorrhagic transformation of ischemic tissue) non-traumatic intracerebral and intraventricular bleeds that produce mass effect, tissue destruction, or neurotoxic sequelae and are typically confirmed by neuroimaging or neuropathology. The term excludes hemorrhage confined to the epidural, subdural, or subarachnoid spaces, isolated microhemorrhages or petechial hemorrhages without space-occupying effect, and hemorrhages resulting directly from penetrating or blunt head trauma. Clinically, cerebral hemorrhage is characterized by sudden focal or global neurological deficits, impaired consciousness, or signs of increased intracranial pressure, and constitutes a subtype of hemorrhagic stroke distinct from other intracranial bleeding syndromes."
cacheFolder <- "cacheCerebralHemorrhage"

targetName <- "Non-arteritic Anterior Ischemic Optic Neuropathy"
seedConceptId <- NULL
targetClinicalDefinition <- "Non-arteritic Anterior Ischemic Optic Neuropathy (NAION) is an acute ischemic injury to the anterior segment of the optic nerve, arising from interruption of perfusion through the short posterior ciliary arteries supplying the optic nerve head, in the absence of a vasculitic etiology such as giant cell arteritis. It presents as a sudden-onset, unilateral visual disturbance ranging across the full severity spectrum â€” from a mild, sectoral or altitudinal visual field defect to severe reduction in visual acuity â€” accompanied by optic disc edema during the acute phase. The phenotype encompasses both the spontaneous form, characterized by structural optic disc vulnerability (crowded disc, small cup-to-disc ratio) in the setting of chronic microvascular risk factors such as hypertension, diabetes mellitus, or obstructive sleep apnea, and cases precipitated by acute systemic hemodynamic compromise including perioperative hypotension or significant blood loss. Explicitly excluded are: arteritic anterior ischemic optic neuropathy associated with giant cell arteritis; posterior ischemic optic neuropathy, which involves the retrobulbar optic nerve without optic disc edema at onset; and demyelinating optic neuritis, which involves immune-mediated inflammatory injury to the optic nerve rather than ischemic vascular occlusion."
cacheFolder <- "cacheNaion"



conceptSet <- createConceptSet(
  name = targetName,
  seedConceptIds = seedConceptId,
  clinicalDefinition = targetClinicalDefinition,
  llmClient = llmClientO3,
  connectionDetails = cd,
  vocabDatabaseSchema = cdmDatabaseSchema,
  tempEmulationSchema = emulationSchema,
  cacheFolder = cacheFolder
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
