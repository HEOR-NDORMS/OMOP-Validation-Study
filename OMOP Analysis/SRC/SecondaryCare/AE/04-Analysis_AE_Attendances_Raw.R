# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: AE Analysis of the patients' AE attendances
# Date: 31.10.2025
# Last date edited: 31.10.2025
# ****************************************************************************** #

# Prepare the AE attendances data ----

## Filter visit_detail for outpatient visits within study period ----
cdm[["ae_attendances"]] <- cdm[["visit_detail"]] %>%
  dplyr::filter(
    visit_detail_concept_id == 9203,
    visit_detail_start_date >= study_start_date,
    visit_detail_start_date <= study_end_date
  ) %>%
  dplyr::select(
    visit_detail_id, person_id, provider_id,
    visit_detail_start_date, visit_detail_concept_id, preceding_visit_detail_id
  ) %>%
  dplyr::compute()

# Add the patient table ----
cdm[["ae_attendances_with_patient_info"]] <- cdm[["ae_attendances"]] %>%
  dplyr::left_join(
    cdm[["person"]] %>%
      dplyr::select(person_id, year_of_birth),
    by = "person_id"
  ) %>%
  dplyr::compute()

## Merge cohort with visit data ----
cohort_ae_data <- cohort_table %>%
  dplyr::left_join(
    cdm[["ae_attendances_with_patient_info"]], 
    by = c("subject_id" = "person_id"),
    copy = TRUE
  ) %>%
  dplyr::filter(
    visit_detail_start_date >= index_date &
      visit_detail_start_date <= follow_up_end
  ) %>% 
  dplyr::collect()


