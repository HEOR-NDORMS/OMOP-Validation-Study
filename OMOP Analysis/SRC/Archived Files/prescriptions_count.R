# Remove the visits that are not clinically relevance ----
# the costs are to be updated
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.4_omop.xlsx") 

# Join visit_detail and provider tables (visit occurrence does not have provider_id to be used for linkage)
joined_visit_provider_tables <- cdm$visit_detail %>% 
  dplyr::inner_join(cdm$provider) 


drug_exposure_table <-joined_visit_provider_tables %>%
  dplyr::left_join(provider_cost_inputs %>% dplyr::filter(Include_standard == "1"),
                   by = "specialty_concept_id", copy=TRUE) %>% # Filter for only meaningful specialties
  dplyr::select(person_id, provider_id, specialty_concept_id, concept_name, specialty_concept_summarised, 
                visit_detail_start_date, visit_detail_id, unit_cost, visit_detail_concept_id) %>%
  dplyr::filter(visit_detail_start_date >= study_start_date & # include only visits in the study period
                  visit_detail_start_date <= study_end_date) %>%
  # Filter for only primary care visits
  dplyr::filter(visit_detail_concept_id == 581477) %>% 
  dplyr::rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  dplyr::distinct(visit_detail_start_date, subject_id, specialty_concept_id, .keep_all=TRUE) %>% # Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
  dplyr::rename(specialty = concept_name,
                specialty_reduced =  specialty_concept_summarised) %>% 
  dplyr::left_join(cdm[["drug_exposure"]], 
                                         by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::filter(drug_exposure_start_date >= study_start_date & drug_exposure_end_date	<= study_end_date) %>% 
  dplyr::group_by(drug_source_value) %>% 
  dplyr::summarise(n_drugs = n()) %>% 
  dplyr::slice_min(n = 20, order_by = n_drugs) %>% 
  computeQuery()
