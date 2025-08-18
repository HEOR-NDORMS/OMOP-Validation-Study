# Frequency drug_concept_id
drug_concept_id_freq <- cdm[["drug_exposure"]] %>%
  group_by(drug_concept_id) %>%
  summarise(n = n()) %>%
  left_join(cdm$concept, by = c("drug_concept_id" = "concept_id")) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  select(drug_concept_id, concept_name, n, percentage) %>%
  collect() %>%                               
  arrange(desc(n)) 
# %>%                        
#   slice_head(n = 20)     

rio::export(drug_concept_id_freq, here::here("OMOP Analysis", "DrugExposure", "drug_concept_id_frequency.xlsx"))

# Frequency drug_type_concept_id
drug_type_concept_id_freq <- cdm[["drug_exposure"]] %>%
  group_by(drug_type_concept_id) %>%
  summarise(n = n()) %>%
  left_join(cdm$concept, by = c("drug_type_concept_id" = "concept_id")) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  select(drug_type_concept_id, concept_name, n, percentage) %>%
  collect() %>%                               
  arrange(desc(n)) 
# %>%                        
#   slice_head(n = 20)
rio::export(drug_type_concept_id_freq, here::here("OMOP Analysis", "DrugExposure", "drug_type_concept_id_frequency.xlsx"))

# Frequency drug_source_concept_id
drug_source_concept_id_freq <- cdm[["drug_exposure"]] %>%
  group_by(drug_source_concept_id) %>%
  summarise(n = n()) %>%
  left_join(cdm$concept, by = c("drug_source_concept_id" = "concept_id")) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  select(drug_source_concept_id, concept_name, n, percentage) %>%
  collect() %>%                               
  arrange(desc(n)) 
# %>%                        
#   slice_head(n = 20)
rio::export(drug_source_concept_id_freq, here::here("OMOP Analysis", "DrugExposure", "drug_source_concept_id_frequency.xlsx"))
