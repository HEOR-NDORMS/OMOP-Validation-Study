
##Join visit_detail and provider tables
joined_visit_provider_tables <- cdm$visit_detail %>% 
  dplyr::inner_join(cdm$provider) 

# 1. Identify the possible values for staff roles

visit_specialty_source_value <- joined_visit_provider_tables %>% 
  group_by(specialty_source_value) %>%
  summarise(
    number_visit = n(),
    number_distinct_provider  = n_distinct(provider_id))%>%
  mutate(freq_visit = (number_visit / sum (number_visit)))%>%
  mutate(freq_provider = (number_distinct_provider / sum (number_distinct_provider)))


# 2. Prepare visit data for health economics analysis


  
  ## Sub-setting the visit data
  cdm[["visit_data"]] <- joined_visit_provider_tables %>%
    dplyr::inner_join(provider_cost_inputs %>% dplyr::filter(Include == "1"), by = "specialty_source_value", copy=TRUE) %>% # Filter for only meaningful specialties
    dplyr::select(person_id, provider_id, specialty_source_value, visit_detail_start_date, visit_detail_id, unit_cost, visit_detail_concept_id) %>%
    dplyr::filter(visit_detail_start_date >= study_start_date & # include only visits in the study period
                    visit_detail_start_date <= study_end_date) %>%
    dplyr::filter(visit_detail_concept_id == 581477) %>% # including only primary care
    dplyr::rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
    dplyr::distinct(visit_detail_start_date, subject_id, specialty_source_value, .keep_all=TRUE) %>% # Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
    dplyr::rename(specialty = specialty_source_value) %>%  # renaming for consistency with other countries as this variable is used for the HCRU and costs too
    computeQuery()
  info(logger, "PRODUCING VISIT DATA FOR UK IS DONE")