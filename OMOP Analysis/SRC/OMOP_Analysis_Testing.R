# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki, Gianluca Fabiano
# Do file name: Primary care analysis	- OMOP data
# Date: 10.02.2025
# Last date edited: 26.03.2025
# ****************************************************************************** #

library(tidylog)

# Importing the cohort
cohort_data_omop <- table_one_omop
# Importing provider specialties and their unit cost
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.3.xlsx") 
# Join visit_detail and provider tables to link each visit with a provider
joined_visit_provider_tables <- cdm$visit_detail %>% 
  dplyr::inner_join(cdm$provider) 

## Sub-setting the visit data during study period and for primary care
cdm[["visit_data"]] <- joined_visit_provider_tables %>%
  dplyr::inner_join(
    provider_cost_inputs %>%
      dplyr::filter(Include == "1") %>% # Keep only visits with meaningful specialties
      dplyr::select(specialty_source_value, specialty_source_value_summarised, unit_cost),
    by = "specialty_source_value", copy = TRUE
  ) %>% 
  dplyr::filter(
    visit_detail_start_date >= study_start_date &
      visit_detail_start_date <= study_end_date,  # Include only visits in the study period
    visit_detail_concept_id == 581477  # Keep only primary care visits
  ) %>%
  dplyr::select(
    person_id, provider_id, specialty_source_value, specialty_source_value_summarised,
    visit_detail_start_date, visit_detail_id, unit_cost, visit_detail_concept_id
  ) %>%
  dplyr::rename(
    subject_id = person_id,
    specialty = specialty_source_value,
    specialty_reduced = specialty_source_value_summarised
  ) %>% 
  dplyr::distinct( # Remove duplicate visits on the same day with the same specialist for the same patient
    visit_detail_start_date, subject_id, specialty, .keep_all = TRUE
  ) 

visit_data <- cdm[["visit_data"]] %>% collect() # just for testing we collect but should be avoided

## Filtering to keep only visits during exposed time and derive count per patient
filtered_visits <- visit_data %>%
  dplyr::left_join(
    cohort_data_omop %>% dplyr::select(subject_id, index_date, follow_up_end), 
    by = "subject_id"
  ) %>%
  dplyr::filter(visit_detail_start_date >= index_date & 
                  visit_detail_start_date <= follow_up_end) %>%
  dplyr::group_by(subject_id, specialty) %>%
  dplyr::summarise(visit_count = n(), .groups = "drop") %>%
  dplyr::ungroup() 

## Merge cohort with filtered visits and assign zero to visit count for non users
cohort_visit_data <- cohort_data_omop %>%
  dplyr::left_join(filtered_visits, by = "subject_id") %>%
  dplyr::mutate(
    visit_count = case_when(
      is.na(visit_count) ~ 0,      # Assign 0 if visit_count is NA
      TRUE ~ visit_count           # Keep existing value otherwise
    ),
    exposed_yrs = as.numeric(follow_up_end - index_date) / 365.25,  # Calculate years of exposure
    visits_per_yr = visit_count / exposed_yrs                       # Calculate visits per year
  ) %>%
  dplyr::select(subject_id, index_date, follow_up_end, specialty, visit_count, exposed_yrs, visits_per_yr)

# Count of non-service users (i.e., subjects with visit_count = 0)
non_service_users_count <- cohort_visit_data %>%
  dplyr::filter(visit_count == 0) %>%
  dplyr::distinct(subject_id) %>%
  dplyr::count() %>%
  dplyr::pull(n)

# Compute summary statistics for service users
user_only_summary_statistics <- cohort_visit_data %>%
  dplyr::filter(visit_count > 0) %>% 
  dplyr::group_by(subject_id) %>%  # Ensure each patient exposed time is used once
  dplyr::summarise(
    total_visits = sum(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_yr = total_visits / total_exposed_yrs
  ) %>%
  dplyr::summarise(
    mean_visit_per_year = round(sum(total_visits) / sum(total_exposed_yrs), 2),
    min_visit_per_year = round(min(visits_per_yr, na.rm = TRUE), 2),
    max_visit_per_year = round(max(visits_per_yr, na.rm = TRUE), 2),
    sd_visit_per_year = round(sd(visits_per_yr, na.rm = TRUE), 2),
    median_visit_per_year = round(quantile(visits_per_yr, probs = 0.5, na.rm = TRUE), 2),
    lower_q_visits_per_year = round(quantile(visits_per_yr, probs = 0.25, na.rm = TRUE), 2),
    upper_q_visits_per_year = round(quantile(visits_per_yr, probs = 0.75, na.rm = TRUE), 2)
  )

# Compute summary statistics for all patients (service & non service users)
all_summary_statistics <- cohort_visit_data %>%
  dplyr::group_by(subject_id) %>%  # Ensure each patient exposed time is used once
  dplyr::summarise(
    total_visits = sum(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_yr = total_visits / total_exposed_yrs
  ) %>%
  dplyr::summarise(
    mean_visit_per_year = round(sum(total_visits) / sum(total_exposed_yrs), 2),
    min_visit_per_year = round(min(visits_per_yr, na.rm = TRUE), 2),
    max_visit_per_year = round(max(visits_per_yr, na.rm = TRUE), 2),
    sd_visit_per_year = round(sd(visits_per_yr, na.rm = TRUE), 2),
    median_visit_per_year = round(quantile(visits_per_yr, probs = 0.5, na.rm = TRUE), 2),
    lower_q_visits_per_year = round(quantile(visits_per_yr, probs = 0.25, na.rm = TRUE), 2),
    upper_q_visits_per_year = round(quantile(visits_per_yr, probs = 0.75, na.rm = TRUE), 2)
  )

# Compute summary statistics for service users - by specialty
user_only_summary_statistics_specialty <- cohort_visit_data %>%
  dplyr::filter(visit_count > 0) %>% 
  dplyr::group_by(subject_id, specialty) %>%  # Ensure each patient exposed time is used once
  dplyr::summarise(
    total_visits = sum(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_yr = total_visits / total_exposed_yrs
  ) %>%
  group_by(specialty) %>%
  dplyr::summarise(
    mean_visit_per_year = round(sum(total_visits) / sum(total_exposed_yrs), 2),
    min_visit_per_year = round(min(visits_per_yr, na.rm = TRUE), 2),
    max_visit_per_year = round(max(visits_per_yr, na.rm = TRUE), 2),
    sd_visit_per_year = round(sd(visits_per_yr, na.rm = TRUE), 2),
    median_visit_per_year = round(quantile(visits_per_yr, probs = 0.5, na.rm = TRUE), 2),
    lower_q_visits_per_year = round(quantile(visits_per_yr, probs = 0.25, na.rm = TRUE), 2),
    upper_q_visits_per_year = round(quantile(visits_per_yr, probs = 0.75, na.rm = TRUE), 2)
  )

# Compute summary statistics for all patients - by specialty
all_summary_statistics_specialty <- cohort_visit_data %>%
  dplyr::group_by(subject_id, specialty) %>%  # Ensure each patient exposed time is used once
  dplyr::summarise(
    total_visits = sum(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_yr = total_visits / total_exposed_yrs
  ) %>%
  group_by(specialty) %>%
  dplyr::summarise(
    mean_visit_per_year = round(sum(total_visits) / sum(total_exposed_yrs), 2),
    min_visit_per_year = round(min(visits_per_yr, na.rm = TRUE), 2),
    max_visit_per_year = round(max(visits_per_yr, na.rm = TRUE), 2),
    sd_visit_per_year = round(sd(visits_per_yr, na.rm = TRUE), 2),
    median_visit_per_year = round(quantile(visits_per_yr, probs = 0.5, na.rm = TRUE), 2),
    lower_q_visits_per_year = round(quantile(visits_per_yr, probs = 0.25, na.rm = TRUE), 2),
    upper_q_visits_per_year = round(quantile(visits_per_yr, probs = 0.75, na.rm = TRUE), 2)
  )
