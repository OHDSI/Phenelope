select concept_id as concept_id, concept_name
from @cdm_database_schema.concept c
where concept_id in (@concept_list)
and upper(domain_id) in ('CONDITION', 'OBSERVATION')
and invalid_reason is NULL;


