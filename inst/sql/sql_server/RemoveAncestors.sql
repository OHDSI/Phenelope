select distinct ancestor_concept_id
from  @cdm_database_schema.concept_ancestor ca
where descendant_concept_id in (@concepts_to_use)
 and ancestor_concept_id not in (@concepts_to_use);
