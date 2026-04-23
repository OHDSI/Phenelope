select distinct descendant_concept_id as concept_id, concept_name,  domain_id, vocabulary_id
from @cdm_database_schema.concept_ancestor ca
join @cdm_database_schema.concept c
on descendant_concept_id = concept_id
where ancestor_concept_id in (@concept_list)
  and upper(domain_id) in ('CONDITION', 'OBSERVATION', 'PROCEDURE', 'MEASUREMENT', 'VISIT', 'DRUG', 'PROVIDER')
  AND concept_name NOT LIKE '%finding'
  AND concept_name NOT LIKE 'Disorder of%'
  AND concept_name NOT LIKE 'Finding of%'
  AND concept_name NOT LIKE 'Disease of%'
  AND concept_name NOT LIKE 'Injury of%'
  AND concept_name NOT LIKE '%by site'
  AND concept_name NOT LIKE '%by body site'
  AND concept_name NOT LIKE '%by mechanism'
  AND concept_name NOT LIKE '%of body region'
  AND concept_name NOT LIKE '%of anatomical site'
  AND concept_name NOT LIKE '%of specific body structure%'
  and vocabulary_id not in (@excludedVocabularies)
  and invalid_reason is NULL;

