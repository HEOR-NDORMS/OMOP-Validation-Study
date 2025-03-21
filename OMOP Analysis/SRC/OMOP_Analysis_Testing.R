# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Primary care and Secondary care analysis	
# Date: 10.02.2025
# Last date edited: 10.02.2025
# ****************************************************************************** #

# Service users are defined as patients with records of healthcare encounters 
# with the NHS and their resource use and associated costs will be estimated 
# separately for primary care (hence excluding patients with no record of primary care visits with any relevant healthcare staff),

# Table 5 “validation_staff” contains the requested fields from aurum.staff

library(tidylog)

# Starting with table_one_omop

cohort_data_omop <- table_one_omop


# Remove the visits that are not clinically relevance ----
# the costs are to be updated
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.3.xlsx") 

# Join visit_detail and provider tables
joined_visit_provider_tables <- cdm$visit_detail %>% 
  dplyr::inner_join(cdm$provider) 

## Sub-setting the visit data ----
# cdm[["visit_data"]] <- NULL, to remove the visit_data table from the cdm object
cdm[["visit_data"]] <- joined_visit_provider_tables %>%
  dplyr::left_join(provider_cost_inputs %>% dplyr::filter(Include == "1"),
                   by = "specialty_source_value", copy=TRUE) %>% # Filter for only meaningful specialties
  dplyr::select(person_id, provider_id, specialty_source_value, specialty_source_value_summarised, 
                visit_detail_start_date, visit_detail_id, unit_cost, visit_detail_concept_id) %>%
  dplyr::filter(visit_detail_start_date >= study_start_date & # include only visits in the study period
                  visit_detail_start_date <= study_end_date) %>%
  # Filter for only primary care visits
  dplyr::filter(visit_detail_concept_id == 581477) %>% 
  dplyr::rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  dplyr::distinct(visit_detail_start_date, subject_id, specialty_source_value, .keep_all=TRUE) %>% # Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
  dplyr::rename(specialty = specialty_source_value,
                specialty_reduced =  specialty_source_value_summarised) %>%  # renaming for consistency with other countries as this variable is used for the HCRU and costs too
  computeQuery()

visit_data_df <- cdm[["visit_data"]] %>% dplyr::collect()

# merge the cohort data to this visit_data_df
visit_data <- left_join(cohort_data_omop %>% select(subject_id, index_date,
                                                    follow_up_end, follow_up_time),
                        visit_data_df, 
                        by = "subject_id") %>% 
  # in order to count all the visits from index date until follow-up end date
  filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end) %>% 
  mutate(exposed_yrs = as.numeric(follow_up_end - index_date) / 365.25)

## Clinically Relevant Consultations ----
provider_cost_inputs_relev <- provider_cost_inputs %>% 
  filter(Include == 1)
specialty_source_value_ids <- unique(provider_cost_inputs_relev$specialty_source_value)


# should be the same as the summary of users by specialty
visit_count_user <- visit_data %>%
  filter(specialty %in% specialty_source_value_ids) %>% 
  group_by(subject_id, specialty) %>% 
  summarise(
    exposed_yrs = first(exposed_yrs),
    visit_count = n(),
            .groups = "drop") %>% 
  mutate(visits_per_yr = visit_count / exposed_yrs)

user_only_summary_statistics_overall <- tibble(
  mean_visit_per_year = round(sum(visit_count_user$visit_count) / sum(visit_count_user$exposed_yrs), 2),
  min_visit_per_year = round(min(visit_count_user$visits_per_yr), 2),
  max_visit_per_year = round(max(visit_count_user$visits_per_yr), 2),
  sd_visit_per_year = round(sd(visit_count_user$visits_per_yr), 2),
  median_visit_per_year = round(quantile(visit_count_user$visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(visit_count_user$visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(visit_count_user$visits_per_yr, probs = (.75)), 2)
)

visit_count_user_by_sp <- visit_data %>%
  filter(specialty %in% specialty_source_value_ids) %>% 
  group_by(specialty) %>% 
  summarise(
    visit_count = n(),
    .groups = "drop")

visit_count_all <- cohort_data_omop %>% 
  left_join(visit_count_user, by = c("subject_id")) %>% 
  select(subject_id, index_date, follow_up_end, exposed_yrs.x, exposed_yrs.y,
         specialty, visit_count) %>% 
  mutate(visit_count = replace_na(visit_count, 0)) %>% 
  mutate(visits_per_yr = visit_count/ exposed_yrs) %>% 
  mutate(visits_per_yr = replace_na(visits_per_yr, 0))

user_only_summary_by_sp <- visit_data %>%
  filter(specialty %in% specialty_source_value_ids) %>% 
  group_by(subject_id, specialty) %>% 
  summarise(
    visit_count = n(),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = visit_count / total_exposed_yrs,
    .groups = "drop") %>% 
  # group_by(subject_id, specialty) %>%
  # summarise(total_patient_visits = first(visit_count),
  #           total_exposed_yrs = first(exposed_yrs),
  #           visits_per_year = total_patient_visits / total_exposed_yrs,
  #           .groups = "drop") %>%
  group_by(specialty) %>%
  summarise(tot_visits = round(sum(visit_count, na.rm = TRUE), 2),
            mean_visits_per_year = round(sum(visit_count, na.rm = TRUE) / sum(total_exposed_yrs, na.rm = TRUE), 2),
            sd_visits_per_year = round(sd(visits_per_year, na.rm = TRUE), 2),
            min_visits_per_year = round(min(visits_per_year, na.rm = TRUE), 2),
            max_visits_per_year = round(max(visits_per_year, na.rm = TRUE), 2),
            num_subjects_visited = n_distinct(subject_id),
            .groups = "drop")

all_summary_statistics_overall <- tibble(
  mean_visit_per_year = round(sum(visit_count_all$visit_count) / sum(visit_count_all$exposed_yrs), 2),
  min_visit_per_year = round(min(visit_count_all$visits_per_yr), 2),
  max_visit_per_year = round(max(visit_count_all$visits_per_yr), 2),
  sd_visit_per_year = round(sd(visit_count_all$visits_per_yr), 2),
  median_visit_per_year = round(quantile(visit_count_all$visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(visit_count_all$visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(visit_count_all$visits_per_yr, probs = (.75)), 2)
)


all_summary <- visit_data %>%
  group_by(subject_id, specialty) %>%
  summarise(total_patient_visits = first(visit_count),
            total_exposed_yrs = first(exposed_yrs),
            visits_per_year = total_patient_visits / total_exposed_yrs,
            user_status = first(user_status),
            .groups = "drop") %>%
  group_by(specialty) %>%
  summarise(tot_visits = round(sum(total_patient_visits, na.rm = TRUE), 2),
            mean_visits_per_year = round(sum(total_patient_visits, na.rm = TRUE) / sum(total_exposed_yrs, na.rm = TRUE), 2),
            sd_visits_per_year = round(sd(visits_per_year, na.rm = TRUE), 2),
            min_visits_per_year = round(min(visits_per_year, na.rm = TRUE), 2),
            max_visits_per_year = round(max(visits_per_year, na.rm = TRUE), 2),
            num_subjects_visited = n_distinct(subject_id),
            num_non_users = sum(user_status == "Non-User"),
            .groups = "drop") %>%
  mutate(num_subjects_visited = ifelse(specialty == "Not Clinically Relevant", num_non_users, num_subjects_visited)) %>%
  select(-num_non_users)



