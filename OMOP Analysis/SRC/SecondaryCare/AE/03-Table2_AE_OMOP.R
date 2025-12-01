# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: AE Table 2: description of the patients' AE attendances
# Date: 16.07.2025
# Last date edited: 01.12.2025
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


# Merge back to cohort data, so that we can have the number of the cohort pts
# This will allow us to have users and non-users
table_two_ae_omop <- left_join(cohort_table %>% 
                                      select(subject_id, index_date, follow_up_end), 
                               cohort_ae_data, 
                                    by = c("subject_id", "index_date", "follow_up_end"))

# Create the users status
table_two_ae_omop <- table_two_ae_omop %>% 
  mutate(
    user_status = if_else(is.na(visit_detail_start_date), "Non-User", "User")
  ) %>% 
  # select columns of interest
  select(!c(cohort_start_date, cohort_end_date, fracture_site, 
            two_yrs_after_index, cancer_date, mbd_date, fracture_after_index,
            cancer_date, mbd_date, fracture_after_index, death_date,
            study_end, follow_up_reason))


