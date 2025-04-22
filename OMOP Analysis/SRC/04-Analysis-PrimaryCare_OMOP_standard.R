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
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.4.xlsx") 

# Join visit_detail and provider tables (visit occurrence does not have provider_id to be used for linkage)
joined_visit_provider_tables <- cdm$visit_detail %>% 
  dplyr::inner_join(cdm$provider) 

## Sub-setting the visit data ----
# cdm[["visit_data"]] <- NULL, to remove the visit_data table from the cdm object
cdm[["visit_data"]] <- joined_visit_provider_tables %>%
  dplyr::left_join(provider_cost_inputs %>% dplyr::filter(Include_standard == "1"),
                   by = "specialty_concept_id", copy=TRUE) %>% # Filter for only meaningful specialties
  dplyr::select(person_id, provider_id, specialty_concept_id, concept_name, specialty_source_value_summarised, 
                visit_detail_start_date, visit_detail_id, unit_cost, visit_detail_concept_id) %>%
  dplyr::filter(visit_detail_start_date >= study_start_date & # include only visits in the study period
                  visit_detail_start_date <= study_end_date) %>%
  # Filter for only primary care visits
  dplyr::filter(visit_detail_concept_id == 581477) %>% 
  dplyr::rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  dplyr::distinct(visit_detail_start_date, subject_id, specialty_concept_id, .keep_all=TRUE) %>% # Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
  dplyr::rename(specialty = concept_name,
                specialty_reduced =  specialty_source_value_summarised) %>%  # renaming for consistency with other countries as this variable is used for the HCRU and costs too
  computeQuery()

visit_data_df <- cdm[["visit_data"]] %>% dplyr::collect()

# merge the cohort data to this visit_data_df
visit_data <- left_join(cohort_data_omop %>% select(subject_id, index_date,
                                                    follow_up_end, follow_up_time),
                        visit_data_df, 
                        by = "subject_id") %>% 
  # in order to count all the visits from index date until follow-up end date
  filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end)

## Clinically Relevant Consultations ----
provider_cost_inputs_relev <- provider_cost_inputs %>% 
  filter(Include_standard == 1)
specialty_source_value_ids <- unique(provider_cost_inputs_relev$concept_name)

### Filter the data with these clinically relevant ----
# This would make it easier to identify users/non-users as well. Bc technically, if a patient had only visits before index but not during the exposure time, that is a non-user. 
# So it is simpler if you prepare the visit data first, with visits in the exposure time and then join back to the list of patients to identify users as those who have at last one visit during that time (and with a relevant staff) and non-users are all the others (i.e., zero visits or only non-relevant visits).
visit_data <- visit_data %>% 
  mutate(
    specialty = case_when(
      specialty %in% specialty_source_value_ids ~ specialty,
          TRUE ~ "Not Clinically Relevant"
    ),
    is_clinically_relevant = if_else(specialty == "Not Clinically Relevant", 0, 1),
    in_exposure_time = if_else(
      !is.na(visit_detail_start_date), 1, 0
    )
  ) %>% 
  mutate(specialty_reduced = ifelse(
    is.na(specialty_reduced), 
    "Not Clinically Relevant",
    specialty_reduced)) %>%
  group_by(subject_id) %>%
  mutate(user_status = if_else(
    any(is_clinically_relevant == 1 & in_exposure_time == 1, na.rm = TRUE),
    "User", "Non-User"
  )) %>%
  ungroup()

### Create a new df so that we can create visits based on these reduced specialty ----
visit_data_reduced <- visit_data

# Count of users/non-users, maybe as a sense chack, should be the same as table 1
(u_n_summary <- visit_data %>%
    group_by(user_status) %>%
    summarise(count_of_pts = n_distinct(subject_id)) %>%
    ungroup())

### Create summary visits ----
summary_visits <- visit_data %>%
  group_by(subject_id, index_date, specialty) %>%
  summarise(visit_count = n(), .groups = "drop")

# Merge the summary visits back to the visits_data
# Merge back to the visits_data
visit_data <- left_join(visit_data, summary_visits,
                        by = c("subject_id", "index_date", "specialty")) %>%
  group_by(subject_id) %>%
  mutate(
    visit_count = if_else(
      specialty == "Not Clinically Relevant", 0, visit_count),
    unit_cost = if_else(
      specialty == "Not Clinically Relevant", 0, unit_cost),
    exposed_yrs = as.numeric(follow_up_end - index_date) / 365.25,
    visits_per_year = visit_count / exposed_yrs,
    visits_per_year = replace_na(visits_per_year, 0),
    visit_cost = visit_count * unit_cost,
    visits_costs_per_year = visit_cost / exposed_yrs,
    visits_costs_per_year = replace_na(visits_costs_per_year, 0)
  ) %>%
  ungroup()

### Create summary visits based on the reduced specialties -----
summary_visits_reduced <- visit_data_reduced %>%
  group_by(subject_id, index_date, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# merge it back to visit data
visit_data_reduced <- left_join(visit_data_reduced, summary_visits_reduced,
                                by = c("subject_id", "index_date", "specialty_reduced")) %>%
  group_by(subject_id) %>%
  mutate(
    visit_count = if_else(
      specialty_reduced == "Not Clinically Relevant", 
      0, 
      visit_count),
    unit_cost = if_else(
      specialty_reduced == "Not Clinically Relevant", 
      0, 
      unit_cost),
    exposed_yrs = as.numeric(follow_up_end - index_date) / 365.25,
    visits_per_year = visit_count / exposed_yrs,
    visits_per_year = replace_na(visits_per_year, 0),
    visit_cost = visit_count * unit_cost,
    visits_costs_per_year = visit_cost / exposed_yrs,
    visits_costs_per_year = replace_na(visits_costs_per_year, 0)
  ) %>%
  ungroup()

## Visits - Primary care ----
# NOTE:
# users as those with at least one visit, regardless of who they had it with.
# So if one had a GP visit and another patient had one with cardiologist they are both considered users.
# Non-user would be someone with a overall count = 0
visits_results_omop <- analyse_visits(
  visit_data = visit_data
)

# Get the output
user_only_summary <- visits_results_omop$user_only_summary
all_summary <- visits_results_omop$all_summary
(non_service_users <- visits_results_omop$non_service_users)
(tot_exposed_yrs_all <- visits_results_omop$tot_exposed_yrs_all)
(tot_exposed_yrs_user <- visits_results_omop$tot_exposed_yrs_user)

# Visits with reduced specialties
visits_results_omop_reduced <- analyse_visits_reduced(
  visit_data_reduced = visit_data_reduced
)

# Get the output
user_only_summary_reduced <- visits_results_omop_reduced$user_only_summary_reduced
all_summary_reduced <- visits_results_omop_reduced$all_summary_reduced
(non_service_users_reduced <- visits_results_omop_reduced$non_service_users_reduced)
(tot_exposed_yrs_all <- visits_results_omop_reduced$tot_exposed_yrs_all)
(tot_exposed_yrs_user <- visits_results_omop_reduced$tot_exposed_yrs_user)


### Overall visits summaries ----
# User-only summary statistics
# Filter user visits and relevant specialties
user_visits <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  # mutate(
  #   raw_exposure_interval = interval(index_date, pmin(visit_detail_start_date, follow_up_end)),
  #   month_after_index = interval(index_date, visit_detail_start_date) %/% months(1),
  #   year_after_index = case_when(
  #     month_after_index %in% c(1:12) ~ 1,
  #     TRUE ~ 2
  #   )
  # )
  mutate(
    # suppose if the ptient has 1.5 years, so the one is 1 year is in yr1 and 0.5 yr2
    # Exposure interval from index_date to follow_up_end
    raw_exposure_interval = interval(index_date, follow_up_end),
    
    # Months since index_date, ensuring it doesn’t exceed follow-up end
    month_after_index = interval(index_date, follow_up_end) %/% months(1),
    
    # Assign Year 1 or Year 2 based on the corrected month index
    year_after_index = case_when(
      month_after_index <= 12 ~ 1,
      TRUE ~ 2
    )
  )

# We then compute visits per year per patient
user_yearly_visits <- user_visits %>%
  group_by(subject_id, specialty) %>%
  summarise(visits_count = first(visit_count),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(subject_id, year_after_index) %>%
  summarise(
    total_patient_visits = sum(visits_count, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / year_after_index,
    .groups = "drop"
  )

# Then,compute total visits over full exposure period
user_total_visits <- visit_data %>%
  filter(user_status == "User",
         specialty != "Not Clinically Relevant") %>%
  group_by(subject_id, specialty) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(subject_id) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop"
  )

# Merge yearly and overall exposure stats
user_summary_df <- user_yearly_visits %>%
  left_join(user_total_visits, by = "subject_id") %>% 
  distinct()

user_only_summary_statistics_overall <- tibble(
  mean_visit_per_year = round(sum(user_summary_df$tot_visits) / sum(user_summary_df$tot_exposed_yrs), 2),
  min_visit_per_year = round(min(user_summary_df$tot_visits_per_yr), 2),
  max_visit_per_year = round(max(user_summary_df$tot_visits_per_yr), 2),
  sd_visit_per_year = round(sd(user_summary_df$tot_visits_per_yr), 2),
  median_visit_per_year = round(quantile(user_summary_df$tot_visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(user_summary_df$tot_visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(user_summary_df$tot_visits_per_yr, probs = (.75)), 2)
)

user_summary_df_yr1 <- user_summary_df %>% 
  filter(year_after_index == 1)

user_only_summary_statistics_yr1 <- tibble(
  mean_visit_per_year = round(sum(user_summary_df_yr1$visits_per_year) / sum(user_summary_df_yr1$year_after_index), 2),
  min_visit_per_year = round(min(user_summary_df_yr1$visits_per_year), 2),
  max_visit_per_year = round(max(user_summary_df_yr1$visits_per_year), 2),
  sd_visit_per_year = round(sd(user_summary_df_yr1$visits_per_year), 2),
  median_visit_per_year = round(quantile(user_summary_df_yr1$visits_per_year, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(user_summary_df_yr1$visits_per_year, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(user_summary_df_yr1$visits_per_year, probs = (.75)), 2)
)

# Sense check
max_users_df <- user_summary_df %>% 
  group_by(subject_id, year_after_index) %>% 
  summarise(
    max_visits = max(total_patient_visits)
  )

# Regarding the max in year 1 with 209 visits, indeed the patient (1156291120220)
# has these number of visits

user_summary_df_yr2 <- user_summary_df %>% 
  filter(year_after_index == 2)

user_only_summary_statistics_yr2 <- tibble(
  mean_visit_per_year = round(sum(user_summary_df_yr2$visits_per_year) / sum(user_summary_df_yr2$year_after_index), 2),
  min_visit_per_year = round(min(user_summary_df_yr2$visits_per_year), 2),
  max_visit_per_year = round(max(user_summary_df_yr2$visits_per_year), 2),
  sd_visit_per_year = round(sd(user_summary_df_yr2$visits_per_year), 2),
  median_visit_per_year = round(quantile(user_summary_df_yr2$visits_per_year, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(user_summary_df_yr2$visits_per_year, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(user_summary_df_yr2$visits_per_year, probs = (.75)), 2)
)


# All patients
all_visits <- visit_data %>%
  # mutate(
  #   raw_exposure_interval = interval(index_date, pmin(visit_detail_start_date, follow_up_end)),
  #   month_after_index = interval(index_date, visit_detail_start_date) %/% months(1),
  #   year_after_index = case_when(
  #     month_after_index %in% c(1:12) ~ 1,
  #     TRUE ~ 2
  #   )
  # )
  mutate(
    # Exposure interval from index_date to follow_up_end
    raw_exposure_interval = interval(index_date, follow_up_end),
    
    # Months since index_date, ensuring it doesn’t exceed follow-up end
    month_after_index = interval(index_date, follow_up_end) %/% months(1),
    
    # Assign Year 1 or Year 2 based on the corrected month index
    year_after_index = case_when(
      month_after_index <= 12 ~ 1,
      TRUE ~ 2
    )
  )

# We then compute visits per year per patient
all_yearly_visits <- all_visits %>%
  group_by(subject_id, specialty) %>%
  summarise(visits_count = first(visit_count),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(subject_id, year_after_index) %>%
  summarise(
    total_patient_visits = sum(visits_count, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / year_after_index,
    .groups = "drop"
  )

# Then,compute total visits over full exposure period
all_total_visits <- visit_data %>%
  group_by(subject_id, specialty) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(subject_id) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop"
  )

# Merge yearly and overall exposure stats
all_summary_df <- all_yearly_visits %>%
  left_join(all_total_visits, by = "subject_id") %>% 
  distinct()

all_only_summary_statistics_overall <- tibble(
  mean_visit_per_year = round(sum(all_summary_df$tot_visits) / sum(all_summary_df$tot_exposed_yrs), 2),
  min_visit_per_year = round(min(all_summary_df$tot_visits_per_yr), 2),
  max_visit_per_year = round(max(all_summary_df$tot_visits_per_yr), 2),
  sd_visit_per_year = round(sd(all_summary_df$tot_visits_per_yr), 2),
  median_visit_per_year = round(quantile(all_summary_df$tot_visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(all_summary_df$tot_visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(all_summary_df$tot_visits_per_yr, probs = (.75)), 2)
)

all_summary_df_yr1 <- all_summary_df %>% 
  filter(year_after_index == 1)

all_only_summary_statistics_yr1 <- tibble(
  mean_visit_per_year = round(sum(all_summary_df_yr1$visits_per_year) / sum(all_summary_df_yr1$year_after_index), 2),
  min_visit_per_year = round(min(all_summary_df_yr1$visits_per_year), 2),
  max_visit_per_year = round(max(all_summary_df_yr1$visits_per_year), 2),
  sd_visit_per_year = round(sd(all_summary_df_yr1$visits_per_year), 2),
  median_visit_per_year = round(quantile(all_summary_df_yr1$visits_per_year, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(all_summary_df_yr1$visits_per_year, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(all_summary_df_yr1$visits_per_year, probs = (.75)), 2)
)

# Sense check
max_all_df <- all_summary_df %>% 
  group_by(subject_id, year_after_index) %>% 
  summarise(
    max_visits = max(total_patient_visits)
  )

all_summary_df_yr2 <- all_summary_df %>% 
  filter(year_after_index == 2)

all_only_summary_statistics_yr2 <- tibble(
  mean_visit_per_year = round(sum(all_summary_df_yr2$visits_per_year) / sum(all_summary_df_yr2$year_after_index), 2),
  min_visit_per_year = round(min(all_summary_df_yr2$visits_per_year), 2),
  max_visit_per_year = round(max(all_summary_df_yr2$visits_per_year), 2),
  sd_visit_per_year = round(sd(all_summary_df_yr2$visits_per_year), 2),
  median_visit_per_year = round(quantile(all_summary_df_yr2$visits_per_year, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(all_summary_df_yr2$visits_per_year, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(all_summary_df_yr2$visits_per_year, probs = (.75)), 2)
)



# Export the output
library(writexl)

visits_list <- list(
  overall_user_visits_only = user_only_summary_statistics_overall,
  yr1_user_visits_only = user_only_summary_statistics_yr1,
  yr2_user_visits_only = user_only_summary_statistics_yr2,
  overall_all_visits_only = all_only_summary_statistics_overall,
  yr1_all_visits_only = all_only_summary_statistics_yr1,
  yr2_all_visits_only = all_only_summary_statistics_yr2,
  user_only_summary = user_only_summary,
  all_summary = all_summary,
  user_only_summary_reduced = user_only_summary_reduced,
  all_summary_reduced = all_summary_reduced
)

# Write to an Excel file
# write_xlsx(visits_list, here::here("OMOP Analysis", "Results", "Output_visits_OMOP.xlsx"))

## Cost - Primary care ----
cost_results_omop <- analyse_visits_cost(
  visit_data = visit_data
)

# reducing the specialty
cost_results_omop_reduced <- analyse_visits_cost_reduced(
  visit_data_reduced = visit_data_reduced
)
### Get the output grouped by specialty ----
user_only_cost_summary <- cost_results_omop$user_only_cost_summary
all_cost_summary <- cost_results_omop$all_cost_summary
(tot_exposed_yrs_all <- cost_results_omop$tot_exposed_yrs_all)
(tot_exposed_yrs_user <- cost_results_omop$tot_exposed_yrs_user)
user_only_cost_summary_reduced <- cost_results_omop_reduced$user_only_cost_summary_reduced
all_cost_summary_reduced <- cost_results_omop_reduced$all_cost_summary_reduced

### Overall cost summaries ----
## Users
user_costs <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    # Exposure interval from index_date to follow_up_end
    raw_exposure_interval = interval(index_date, follow_up_end),
    
    # Months since index_date, ensuring it doesn’t exceed follow-up end
    month_after_index = interval(index_date, follow_up_end) %/% months(1),
    
    # Assign Year 1 or Year 2 based on the corrected month index
    year_after_index = case_when(
      month_after_index <= 12 ~ 1,
      TRUE ~ 2
    )
  )
# user_costs <- visit_data %>%
#   filter(user_status == "user", specialty != "Not Clinicusery Relevant") %>%
#   mutate(
#     raw_exposure_interval = interval(index_date, pmin(consdate, follow_up_end)),
#     month_after_index = interval(index_date, consdate) %/% months(1),
#     year_after_index = case_when(
#       month_after_index %in% c(1:12) ~ 1,
#       TRUE ~ 2
#     )
#   )
# We then compute visits per year per patient
user_yearly_costs <- user_costs %>%
  group_by(subject_id, specialty) %>%
  summarise(visit_cost = first(visit_cost),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(subject_id, year_after_index) %>%
  summarise(
    total_patient_visits_cost = sum(visit_cost, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_cost_per_year = total_patient_visits_cost / year_after_index,
    .groups = "drop"
  )

# Then,compute total visits over full exposure period
user_total_costs <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  group_by(subject_id, specialty) %>%
  summarise(
    total_patient_visits_costs = first(visit_cost),
    total_exposed_yrs = first(exposed_yrs),
    visits_costs_per_year = total_patient_visits_costs / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(subject_id) %>%
  summarise(
    tot_visits_cost = sum(total_patient_visits_costs, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_cost_per_yr = tot_visits_cost / tot_exposed_yrs,
    .groups = "drop"
  )

# Merge yearly and overuser exposure stats
user_costs_summary_df <- user_yearly_costs %>%
  left_join(user_total_costs, by = "subject_id") %>% 
  distinct()

user_only_costs_summary_statistics_overall <- tibble(
  mean_visit_cost_per_year = round(sum(user_costs_summary_df$tot_visits_cost) / sum(user_costs_summary_df$tot_exposed_yrs), 2),
  min_visit_cost_per_year = round(min(user_costs_summary_df$tot_visits_cost_per_yr), 2),
  max_visit_cost_per_year = round(max(user_costs_summary_df$tot_visits_cost_per_yr), 2),
  sd_visit_cost_per_year = round(sd(user_costs_summary_df$tot_visits_cost_per_yr), 2),
  median_visit_cost_per_year = round(quantile(user_costs_summary_df$tot_visits_cost_per_yr, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(user_costs_summary_df$tot_visits_cost_per_yr, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(user_costs_summary_df$tot_visits_cost_per_yr, probs = (.75)), 2)
)

user_costs_summary_df_yr1 <- user_costs_summary_df %>% 
  filter(year_after_index == 1)

user_only_costs_summary_statistics_yr1 <- tibble(
  mean_visit_cost_per_year = round(sum(user_costs_summary_df_yr1$visits_cost_per_year) / sum(user_costs_summary_df_yr1$year_after_index), 2),
  min_visit_cost_per_year = round(min(user_costs_summary_df_yr1$visits_cost_per_year), 2),
  max_visit_cost_per_year = round(max(user_costs_summary_df_yr1$visits_cost_per_year), 2),
  sd_visit_cost_per_year = round(sd(user_costs_summary_df_yr1$visits_cost_per_year), 2),
  median_visit_cost_per_year = round(quantile(user_costs_summary_df_yr1$visits_cost_per_year, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(user_costs_summary_df_yr1$visits_cost_per_year, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(user_costs_summary_df_yr1$visits_cost_per_year, probs = (.75)), 2)
)

# Sense check
max_users_cost_costs_df <- user_costs_summary_df_yr1 %>% 
  group_by(subject_id, year_after_index) %>% 
  summarise(
    max_visits_cost = max(total_patient_visits_cost)
  )

# Regarding the max in year 1 with 209 visits, indeed the patient (1156291120220)
# has these number of visits

user_costs_summary_df_yr2 <- user_costs_summary_df %>% 
  filter(year_after_index == 2)

user_only_costs_summary_statistics_yr2 <- tibble(
  mean_visit_cost_per_year = round(sum(user_costs_summary_df_yr2$visits_cost_per_year) / sum(user_costs_summary_df_yr2$year_after_index), 2),
  min_visit_cost_per_year = round(min(user_costs_summary_df_yr2$visits_cost_per_year), 2),
  max_visit_cost_per_year = round(max(user_costs_summary_df_yr2$visits_cost_per_year), 2),
  sd_visit_cost_per_year = round(sd(user_costs_summary_df_yr2$visits_cost_per_year), 2),
  median_visit_cost_per_year = round(quantile(user_costs_summary_df_yr2$visits_cost_per_year, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(user_costs_summary_df_yr2$visits_cost_per_year, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(user_costs_summary_df_yr2$visits_cost_per_year, probs = (.75)), 2)
)

## All Patients
all_costs <- visit_data %>%
  mutate(
    # Exposure interval from index_date to follow_up_end
    raw_exposure_interval = interval(index_date, follow_up_end),
    
    # Months since index_date, ensuring it doesn’t exceed follow-up end
    month_after_index = interval(index_date, follow_up_end) %/% months(1),
    
    # Assign Year 1 or Year 2 based on the corrected month index
    year_after_index = case_when(
      month_after_index <= 12 ~ 1,
      TRUE ~ 2
    )
  )
# all_costs <- visit_data %>%
#   filter(all_status == "all", specialty != "Not Clinically Relevant") %>%
#   mutate(
#     raw_exposure_interval = interval(index_date, pmin(consdate, follow_up_end)),
#     month_after_index = interval(index_date, consdate) %/% months(1),
#     year_after_index = case_when(
#       month_after_index %in% c(1:12) ~ 1,
#       TRUE ~ 2
#     )
#   )
# We then compute visits per year per patient
all_yearly_costs <- all_costs %>%
  group_by(subject_id, specialty) %>%
  summarise(visit_cost = first(visit_cost),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(subject_id, year_after_index) %>%
  summarise(
    total_patient_visits_cost = sum(visit_cost, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_cost_per_year = total_patient_visits_cost / year_after_index,
    .groups = "drop"
  )

# Then,compute total visits over full exposure period
all_total_costs <- visit_data %>%
  group_by(subject_id, specialty) %>%
  summarise(
    total_patient_visits_costs = first(visit_cost),
    total_exposed_yrs = first(exposed_yrs),
    visits_costs_per_year = total_patient_visits_costs / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(subject_id) %>%
  summarise(
    tot_visits_cost = sum(total_patient_visits_costs, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_cost_per_yr = tot_visits_cost / tot_exposed_yrs,
    .groups = "drop"
  )

# Merge yearly and overall exposure stats
all_costs_summary_df <- all_yearly_costs %>%
  left_join(all_total_costs, by = "subject_id") %>% 
  distinct()

all_only_costs_summary_statistics_overall <- tibble(
  mean_visit_cost_per_year = round(sum(all_costs_summary_df$tot_visits_cost) / sum(all_costs_summary_df$tot_exposed_yrs), 2),
  min_visit_cost_per_year = round(min(all_costs_summary_df$tot_visits_cost_per_yr), 2),
  max_visit_cost_per_year = round(max(all_costs_summary_df$tot_visits_cost_per_yr), 2),
  sd_visit_cost_per_year = round(sd(all_costs_summary_df$tot_visits_cost_per_yr), 2),
  median_visit_cost_per_year = round(quantile(all_costs_summary_df$tot_visits_cost_per_yr, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(all_costs_summary_df$tot_visits_cost_per_yr, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(all_costs_summary_df$tot_visits_cost_per_yr, probs = (.75)), 2)
)

all_costs_summary_df_yr1 <- all_costs_summary_df %>% 
  filter(year_after_index == 1)

all_only_costs_summary_statistics_yr1 <- tibble(
  mean_visit_cost_per_year = round(sum(all_costs_summary_df_yr1$visits_cost_per_year) / sum(all_costs_summary_df_yr1$year_after_index), 2),
  min_visit_cost_per_year = round(min(all_costs_summary_df_yr1$visits_cost_per_year), 2),
  max_visit_cost_per_year = round(max(all_costs_summary_df_yr1$visits_cost_per_year), 2),
  sd_visit_cost_per_year = round(sd(all_costs_summary_df_yr1$visits_cost_per_year), 2),
  median_visit_cost_per_year = round(quantile(all_costs_summary_df_yr1$visits_cost_per_year, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(all_costs_summary_df_yr1$visits_cost_per_year, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(all_costs_summary_df_yr1$visits_cost_per_year, probs = (.75)), 2)
)

# Sense check
max_alls_cost_costs_df <- all_costs_summary_df_yr1 %>% 
  group_by(subject_id, year_after_index) %>% 
  summarise(
    max_visits_cost = max(total_patient_visits_cost)
  )

# Regarding the max in year 1 with 209 visits, indeed the patient (1156291120220)
# has these number of visits

all_costs_summary_df_yr2 <- all_costs_summary_df %>% 
  filter(year_after_index == 2)

all_only_costs_summary_statistics_yr2 <- tibble(
  mean_visit_cost_per_year = round(sum(all_costs_summary_df_yr2$visits_cost_per_year) / sum(all_costs_summary_df_yr2$year_after_index), 2),
  min_visit_cost_per_year = round(min(all_costs_summary_df_yr2$visits_cost_per_year), 2),
  max_visit_cost_per_year = round(max(all_costs_summary_df_yr2$visits_cost_per_year), 2),
  sd_visit_cost_per_year = round(sd(all_costs_summary_df_yr2$visits_cost_per_year), 2),
  median_visit_cost_per_year = round(quantile(all_costs_summary_df_yr2$visits_cost_per_year, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(all_costs_summary_df_yr2$visits_cost_per_year, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(all_costs_summary_df_yr2$visits_cost_per_year, probs = (.75)), 2)
)


# List of the output of costs
costs_list <- list(
  overall_user_only_costs = user_only_costs_summary_statistics_overall,
  yr1_user_only_costs = user_only_costs_summary_statistics_yr1,
  yr2_user_only_costs = user_only_costs_summary_statistics_yr2,
  overall_all_only_costs = all_only_costs_summary_statistics_overall,
  yr1_all_only_costs = all_only_costs_summary_statistics_yr1,
  yr2_all_only_costs = all_only_costs_summary_statistics_yr2,
  user_only_cost_summary = user_only_cost_summary,
  all_cost_summary = all_cost_summary,
  user_only_cost_summary_reduced = user_only_cost_summary_reduced,
  all_cost_summary_reduced = all_cost_summary_reduced
)

# Write to an Excel file
# write_xlsx(costs_list, here::here("OMOP Analysis", "Results", "Output_costs_OMOP.xlsx"))

## Cohort summary ----
cohort_summary_omop <- cohort_summary(visit_data)

# Write to an Excel file
# rio::export(cohort_summary_omop, here::here("OMOP Analysis", "Results", "cohort_summary_OMOP.xlsx"))
