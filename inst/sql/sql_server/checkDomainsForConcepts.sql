select concept_id, concept_name, domain_id, vocabulary_id
from @cdm_database_schema.concept c
where concept_id in (@concept_list)
  and upper(domain_id) in ('CONDITION', 'OBSERVATION', 'PROCEDURE', 'MEASUREMENT', 'VISIT', 'DRUG')
  and vocabulary_id not in (@excludedVocabularies)
  and invalid_reason is NULL;
