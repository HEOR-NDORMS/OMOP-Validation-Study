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

specialty_names <- unique(provider_cost_inputs$concept_name)

# Merge the cohort data to this visit_data_df ----
visit_data <- left_join(cohort_data_omop, visit_data_df, 
                                       by = "subject_id") %>% 
  # in order to count all the visits from index date until follow-up end date
  filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end) %>% 
  filter(specialty %in% specialty_names)

### Filter the data with these clinically relevant
# This would make it easier to identify users/non-users as well. Bc technically, if a patient had only visits before index but not during the exposure time, that is a non-user. 
# So it is simpler if you prepare the visit data first, with visits in the exposure time and then join back to the list of patients to identify users as those who have at last one visit during that time (and with a relevant staff) and non-users are all the others (i.e., zero visits or only non-relevant visits).
visit_data <- cohort_data_omop %>% 
  left_join(visit_data, by = c("subject_id", "cohort_start_date", 
                               "cohort_end_date", "condition_concept_id",
                               "two_yrs_after_index", 
                               "fracture_site", "index_date", "cancer_date", 
                               "mbd_date", "fracture_after_index", "death_date",
                               "follow_up_end", "follow_up_reason", 
                               "follow_up_time", "gender_concept_id", 
                               "year_of_birth", "month_of_birth", "day_of_birth",
                               "birth_datetime", "race_concept_id",
                               "ethnicity_concept_id", "location_id", 
                               "care_site_id", "person_source_value", 
                               "gender_source_value", "gender_source_concept_id",
                               "race_source_value", "race_source_concept_id", 
                               "ethnicity_source_value", "ethnicity_source_concept_id",
                               "study_end"
  ), copy = T) %>% 
  mutate(is_clinically_relevant = case_when(
    specialty_reduced == "Administrative Non-clinical" ~ 0,
    specialty_reduced != "Administrative Non-clinical" ~ 1)) %>%
  group_by(subject_id) %>%
  mutate(user_status = case_when(
    any(is_clinically_relevant == 1, na.rm = TRUE) ~ "User",
    all(is_clinically_relevant == 0 | is.na(is_clinically_relevant)) & all(is.na(visit_detail_start_date)) ~ "Non-User",
    all(is_clinically_relevant == 0 | is.na(is_clinically_relevant)) ~ "Non-User",
    TRUE ~ "Unknown"
  )) %>%
  ungroup() 

# Remove duplication caused by merging
visit_data <- visit_data %>% 
  distinct()
  # distinct(visit_detail_start_date, subject_id, specialty_concept_id, .keep_all=TRUE)

# specialty_names <- unique(provider_cost_inputs$concept_name)
# visit_data_t <- visit_data %>% 
#   filter(specialty %in% specialty_names)
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

# Export the output
library(writexl)

visits_list <- list(
  user_only_summary = user_only_summary,
  all_summary = all_summary,
  user_only_summary_reduced = user_only_summary_reduced,
  all_summary_reduced = all_summary_reduced
)

# Write to an Excel file
write_xlsx(visits_list, here::here("OMOP Analysis", "Results", "Output_visits_OMOP.xlsx"))


### Overall visits summaries ----
# User-only summary statistics
# Filter user visits and relevant specialties
user_visits <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1)

### Unadjusted ----
# We then compute visits per year per patient
unadjusted_yearly_visits <- user_visits %>%
  group_by(subject_id, specialty) %>%
  summarise(visits_count = first(visit_count),
            exposed_yrs = first(exposed_yrs),
            .groups = "drop") %>%
  group_by(subject_id) %>%
  summarise(
    total_patient_visits = sum(visits_count, na.rm = TRUE),
    .groups = "drop"
  )

# mean(user_yearly_visits$visits_per_year)

unadjusted_user_summary_statistics_overall <- unadjusted_yearly_visits %>%
  summarise(
    mean_visit_per_year = mean(total_patient_visits),
    min_visit_per_year = min(total_patient_visits),
    max_visit_per_year = max(total_patient_visits),
    sd_visit_per_year = sd(total_patient_visits),
    median_visit_per_year = quantile(total_patient_visits, 0.5),
    lower_q_visits_per_year = quantile(total_patient_visits, 0.25),
    upper_q_visits_per_year = quantile(total_patient_visits, 0.75),
    n = n()
  ) %>%
  mutate(
    se = sd_visit_per_year / sqrt(n),
    lower = mean_visit_per_year - qt(0.975, df = n - 1) * se,
    upper = mean_visit_per_year + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))


# We then compute visits per year per patient
unadjusted_all_yearly_visits <- visit_data %>%
  group_by(subject_id, specialty) %>%
  summarise(visits_count = n(),
            .groups = "drop") %>%
  group_by(subject_id) %>%
  summarise(
    total_patient_visits = sum(visits_count, na.rm = TRUE),
    .groups = "drop"
  )

unadjusted_all_summary_statistics_overall <- unadjusted_all_yearly_visits %>%
  summarise(
    mean_visit_per_year = mean(total_patient_visits),
    min_visit_per_year = min(total_patient_visits),
    max_visit_per_year = max(total_patient_visits),
    sd_visit_per_year = sd(total_patient_visits),
    median_visit_per_year = quantile(total_patient_visits, 0.5),
    lower_q_visits_per_year = quantile(total_patient_visits, 0.25),
    upper_q_visits_per_year = quantile(total_patient_visits, 0.75),
    n = n()
  ) %>%
  mutate(
    se = sd_visit_per_year / sqrt(n),
    lower = mean_visit_per_year - qt(0.975, df = n - 1) * se,
    upper = mean_visit_per_year + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))

unadjusted_visits_list <- list(
  unadjusted_all_summary = unadjusted_all_summary_statistics_overall,
  unadjusted_user_only_summary = unadjusted_user_summary_statistics_overall
)

# Write to an Excel file
write_xlsx(unadjusted_visits_list, here::here("OMOP Analysis", "Results", "Output_visits_OMOP-Unadjusted.xlsx"))


### Adjusted ----
adjusted_users_visits_overall <- visit_data %>%
  filter(is_clinically_relevant == 1, user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
  
  # Total exposure duration
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    years_exposed = days_exposed_total / 365.25,
    died_during_followup = !is.na(death_date) & death_date <= follow_up_end
  ) %>%
  
  # Drop those with < 1 year and no death
  filter(days_exposed_total >= 365.25 | died_during_followup) %>%
  
  # Count visits per patient/specialty
  group_by(subject_id, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop") %>%
  
  # Join back exposure and death info
  left_join(
    visit_data %>%
      mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
      mutate(
        days_exposed_total = as.numeric(follow_up_end - index_date),
        years_exposed = days_exposed_total / 365.25,
        died_during_followup = !is.na(death_date) & death_date <= follow_up_end
      ) %>%
      distinct(subject_id, years_exposed, days_exposed_total, died_during_followup),
    by = "subject_id"
  ) %>%
  
  # Adjust visits based on overall period rules
  mutate(
    adjusted_visits = case_when(
      died_during_followup ~ visit_count,  # No adjustment if died
      days_exposed_total >= 365.25 ~ visit_count,  # No adjustment if full year
      days_exposed_total >= 182.625 ~ (visit_count / days_exposed_total) * 365.25,  # Adjust if ≥ half year
      TRUE ~ 0  # Ignore if < 6 months exposure
    )
  ) %>% 
  mutate(adjusted_visits = round(adjusted_visits, 2))
# 1834920545

# Sum adjusted visits per patient
# why the hell are we dropping the decimals, sometimes you can be annoying af!!
adjusted_users_visits_overall_results <- adjusted_users_visits_overall %>%
  group_by(subject_id) %>%
  summarise(
    total_patient_visits = sum(adjusted_visits, na.rm = TRUE),
    .groups = "drop"
  )

# Summary statistics
adjusted_user_only_summary_statistics_overall <- adjusted_users_visits_overall_results %>%
  summarise(
    mean_visit_per_year = mean(total_patient_visits),
    min_visit_per_year = min(total_patient_visits),
    max_visit_per_year = max(total_patient_visits),
    sd_visit_per_year = sd(total_patient_visits),
    median_visit_per_year = quantile(total_patient_visits, 0.5),
    lower_q_visits_per_year = quantile(total_patient_visits, 0.25),
    upper_q_visits_per_year = quantile(total_patient_visits, 0.75),
    n = n(),
    se = sd_visit_per_year / sqrt(n),
    lower = mean_visit_per_year - qt(0.975, df = n - 1) * se,
    upper = mean_visit_per_year + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))

# All patients
adjusted_all_visits_overall <- visit_data %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
  
  # Exposure and death status
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    years_exposed = days_exposed_total / 365.25,
    died_during_followup = !is.na(death_date) & death_date <= follow_up_end
  ) %>%
  
  # Drop those with < 1 year and no death
  filter(days_exposed_total >= 365.25 | died_during_followup) %>%
  
  # Summarise visits per patient/specialty
  group_by(subject_id, specialty) %>%
  summarise(visit_count = n(), .groups = "drop") %>%
  
  # Join back exposure info
  left_join(
    visit_data %>%
      mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
      mutate(
        days_exposed_total = as.numeric(follow_up_end - index_date),
        years_exposed = days_exposed_total / 365.25,
        died_during_followup = !is.na(death_date) & death_date <= follow_up_end
      ) %>%
      distinct(subject_id, days_exposed_total, years_exposed, died_during_followup),
    by = "subject_id"
  ) %>%
  
  # Adjust visits
  mutate(
    adjusted_visits = case_when(
      # specialty == "Not Clinically Relevant" ~ 0,
      died_during_followup ~ visit_count,
      days_exposed_total >= 365.25 ~ visit_count,
      days_exposed_total >= 182.625 ~ (visit_count / days_exposed_total) * 365.25,
      TRUE ~ 0
    )
  )

# Sum adjusted visits per patient
# why the hell are we dropping the decimals, sometimes you can be annoying af!!
adjusted_all_visits_overall_results <- adjusted_all_visits_overall %>%
  group_by(subject_id) %>%
  summarise(
    total_patient_visits = sum(adjusted_visits, na.rm = TRUE),
    .groups = "drop"
  )

# Summary statistics
adjusted_all_summary_statistics_overall <- adjusted_all_visits_overall_results %>%
  summarise(
    mean_visit_per_year = mean(total_patient_visits),
    min_visit_per_year = min(total_patient_visits),
    max_visit_per_year = max(total_patient_visits),
    sd_visit_per_year = sd(total_patient_visits),
    median_visit_per_year = quantile(total_patient_visits, 0.5),
    lower_q_visits_per_year = quantile(total_patient_visits, 0.25),
    upper_q_visits_per_year = quantile(total_patient_visits, 0.75),
    n = n(),
    se = sd_visit_per_year / sqrt(n),
    lower = mean_visit_per_year - qt(0.975, df = n - 1) * se,
    upper = mean_visit_per_year + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))

adjusted_visits_list <- list(
  adjusted_all_summary = adjusted_all_summary_statistics_overall,
  adjusted_user_only_summary = adjusted_user_only_summary_statistics_overall
)

write_xlsx(adjusted_visits_list, here::here("OMOP Analysis", "Results", "Output_visits_OMOP-Adjusted.xlsx"))


##### Boostrap ----
###### Unadjusted User ----
# Load the boot package
library(boot)  # Required for bootstrapping functions

# Define the statistic function
# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')
fc_user <- function(d, i){
  unadjusted_yearly_visits <- d[i, ]               # Sample the data with replacement
  return(mean(unadjusted_yearly_visits$total_patient_visits))   # This is your variable of interest (e.g., visits, or costs)
}

# Set a seed for reproducibility
set.seed(626)

# Run bootstrap with 1000 resamples
boot_result_user <- boot(unadjusted_yearly_visits, fc_user, R = 1000)

# Compute 95% percentile-based confidence interval
boot_ci_user <- boot.ci(boot.out = boot_result_user, type = "norm")

mean_boot_visits_user <-round(as.numeric(boot_result_user$t0),3)
lower_ci_boot_visits_user <-round(as.numeric(boot_ci_user$normal[2]), 3)
upper_ci_boot_visits_user <-round(as.numeric(boot_ci_user$normal[3]), 3)

boot_user_summary_statistics_overall <- tibble(
  mean_boot_visits_user = mean_boot_visits_user,
  lower_ci_boot_visits_user = lower_ci_boot_visits_user,
  upper_ci_boot_visits_user = upper_ci_boot_visits_user
)

###### Adjusted User ----

# Define the statistic function
# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')
fc_user_adj <- function(d, i){
  adjusted_users_visits_overall_results <- d[i, ]               # Sample the data with replacement
  return(mean(adjusted_users_visits_overall_results$total_patient_visits))   # This is your variable of interest (e.g., visits, or costs)
}

# Set a seed for reproducibility
set.seed(626)

# Run bootstrap with 1000 resamples
boot_result_user_adj <- boot(adjusted_users_visits_overall_results, fc_user_adj, R = 1000)

# Compute 95% percentile-based confidence interval
boot_ci_user_adj <- boot.ci(boot.out = boot_result_user_adj, type = "norm")

mean_boot_visits_user_adj <-round(as.numeric(boot_result_user_adj$t0),3)
lower_ci_boot_visits_user_adj <-round(as.numeric(boot_ci_user_adj$normal[2]), 3)
upper_ci_boot_visits_user_adj <-round(as.numeric(boot_ci_user_adj$normal[3]), 3)

adj_boot_user_summary_statistics_overall <- tibble(
  mean_boot_visits_user_adj = mean_boot_visits_user_adj,
  lower_ci_boot_visits_user_adj = lower_ci_boot_visits_user_adj,
  upper_ci_boot_visits_user_adj = upper_ci_boot_visits_user_adj
)

###### Unadjusted All ----
# Define the statistic function
# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')
fc_all <- function(d, i){
  unadjusted_all_yearly_visits <- d[i, ]               # Sample the data with replacement
  return(mean(unadjusted_all_yearly_visits$total_patient_visits))   # This is your variable of interest (e.g., visits, or costs)
}

# Set a seed for reproducibility
set.seed(42)

# Run bootstrap with 1000 resamples
boot_result_all <- boot(unadjusted_all_yearly_visits, fc_all, R = 1000)

# Compute 95% percentile-based confidence interval
boot_ci_all <- boot.ci(boot.out = boot_result_all, type = "norm")

mean_boot_visits_all <-round(as.numeric(boot_result_all$t0),3)
lower_ci_boot_visits_all <-round(as.numeric(boot_ci_all$normal[2]), 3)
upper_ci_boot_visits_all <-round(as.numeric(boot_ci_all$normal[3]), 3)
boot_all_summary_statistics_overall <- tibble(
  mean_boot_visits_all = mean_boot_visits_all,
  lower_ci_boot_visits_all = lower_ci_boot_visits_all,
  upper_ci_boot_visits_all = upper_ci_boot_visits_all
)


####### Adjusted All ----
fc_all_adj <- function(d, i){
  adjusted_all_visits_overall_results <- d[i, ]               # Sample the data with replacement
  return(mean(adjusted_all_visits_overall_results$total_patient_visits))   # This is your variable of interest (e.g., visits, or costs)
}

# Set a seed for reproducibility
set.seed(42)

# Run bootstrap with 1000 resamples
boot_result_all_adj <- boot(adjusted_all_visits_overall_results, fc_all_adj, R = 1000)

# Compute 95% percentile-based confidence interval
boot_ci_all_adj <- boot.ci(boot.out = boot_result_all_adj, type = "norm")

mean_boot_visits_all_adj <-round(as.numeric(boot_result_all_adj$t0),3)
lower_ci_boot_visits_all_adj <-round(as.numeric(boot_ci_all_adj$normal[2]), 3)
upper_ci_boot_visits_all_adj <-round(as.numeric(boot_ci_all_adj$normal[3]), 3)

adj_boot_all_summary_statistics_overall <- tibble(
  mean_boot_visits_all_adj = mean_boot_visits_all_adj,
  lower_ci_boot_visits_all_adj = lower_ci_boot_visits_all_adj,
  upper_ci_boot_visits_all_adj = upper_ci_boot_visits_all_adj
)


boot_visits_list <- list(
  unadj_boot_all_summary_stats = boot_all_summary_statistics_overall,
  unadj_boot_user_summary_stats = boot_user_summary_statistics_overall,
  adj_boot_all_summary_stats = adj_boot_all_summary_statistics_overall,
  adj_boot_user_summary_stats = adj_boot_user_summary_statistics_overall
)

write_xlsx(boot_visits_list, here::here("OMOP Analysis", "Results", "Output_visits_OMOP-Boot.xlsx"))


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

# List of the output of costs
costs_list <- list(
  user_only_cost_summary = user_only_cost_summary,
  all_cost_summary = all_cost_summary,
  user_only_cost_summary_reduced = user_only_cost_summary_reduced,
  all_cost_summary_reduced = all_cost_summary_reduced
)

# Write to an Excel file
write_xlsx(costs_list, here::here("OMOP Analysis", "Results", "Output_costs_OMOP.xlsx"))


### Overall cost summaries ----
## Users
user_costs <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1)

### Unadjusted ----

# We then compute visits per year per patient
unadjusted_user_yearly_costs <- user_costs %>%
  group_by(subject_id, specialty) %>%
  summarise(
    visit_count = n(),
    unit_cost = unit_cost,
    visits_cost = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct() %>% 
  group_by(subject_id) %>%
  summarise(
    total_patient_visits_cost = sum(visits_cost, na.rm = TRUE),
    # total_exposed_yrs = first(exposed_yrs),
    # visits_cost_per_year = total_patient_visits_cost / total_exposed_yrs,
    .groups = "drop"
  )


# Summary stats for user cost per year
unadjusted_user_only_costs_summary_statistics_overall <- unadjusted_user_yearly_costs %>%
  # group_by(year_after_index) %>%
  summarise(
    mean_visits_cost = round(mean(total_patient_visits_cost, na.rm = TRUE), 2),
    min_visits_cost = round(min(total_patient_visits_cost, na.rm = TRUE), 2),
    max_visits_cost = round(max(total_patient_visits_cost, na.rm = TRUE), 2),
    sd_visits_cost = round(sd(total_patient_visits_cost, na.rm = TRUE), 2),
    median_visits_cost = round(median(total_patient_visits_cost, na.rm = TRUE), 2),
    q1_visits_cost = round(quantile(total_patient_visits_cost, 0.25, na.rm = TRUE), 2),
    q3_visits_cost = round(quantile(total_patient_visits_cost, 0.75, na.rm = TRUE), 2),
    n = n()
  ) %>%
  mutate(
    se = sd_visits_cost / sqrt(n),
    lower = mean_visits_cost - qt(0.975, df = n - 1) * se,
    upper = mean_visits_cost + qt(0.975, df = n - 1) * se
  )

unadjusted_all_yearly_costs <- visit_data %>%
  group_by(subject_id, specialty) %>%
  summarise(
    visit_count = n(),
    unit_cost = unit_cost,
    visits_cost = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct() %>%
  group_by(subject_id) %>%
  summarise(
    total_patient_visits_cost = sum(visits_cost, na.rm = TRUE),
    # total_exposed_yrs = first(exposed_yrs),
    # visits_cost_per_year = total_patient_visits_cost / total_exposed_yrs,
    .groups = "drop"
  )



unadjusted_all_costs_summary_statistics_overall <- unadjusted_all_yearly_costs %>%
  summarise(
    mean_visits_cost = round(mean(total_patient_visits_cost, na.rm = TRUE), 2),
    min_visits_cost = round(min(total_patient_visits_cost, na.rm = TRUE), 2),
    max_visits_cost = round(max(total_patient_visits_cost, na.rm = TRUE), 2),
    sd_visits_cost = round(sd(total_patient_visits_cost, na.rm = TRUE), 2),
    median_visits_cost = round(median(total_patient_visits_cost, na.rm = TRUE), 2),
    q1_visits_cost = round(quantile(total_patient_visits_cost, 0.25, na.rm = TRUE), 2),
    q3_visits_cost = round(quantile(total_patient_visits_cost, 0.75, na.rm = TRUE), 2),
    n = n()
  ) %>%
  mutate(
    se = sd_visits_cost / sqrt(n),
    lower = mean_visits_cost - qt(0.975, df = n - 1) * se,
    upper = mean_visits_cost + qt(0.975, df = n - 1) * se
  )

unadjusted_costs_list <- list(
  unadjusted_all_costs_summary = unadjusted_all_costs_summary_statistics_overall,
  unadjusted_user_only_costs = unadjusted_user_only_costs_summary_statistics_overall
)


write_xlsx(unadjusted_costs_list, here::here("OMOP Analysis", "Results", "Output_costs_OMOP-Unadjusted.xlsx"))

### Adjusted ----
adjusted_users_costs_overall <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
  
  # Total exposure duration
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    years_exposed = days_exposed_total / 365.25,
    died_during_followup = !is.na(death_date) & death_date <= follow_up_end
  ) %>%
  # Drop those with < 1 year and no death
  filter(days_exposed_total >= 365.25 | died_during_followup) %>%
  
  # Count visits per patient/specialty
  group_by(subject_id, specialty) %>%
  summarise(visit_count = n(), 
            unit_cost = unit_cost,
            .groups = "drop") %>%
  distinct() %>% 
  
  # Join back exposure and death info
  left_join(
    visit_data %>%
      mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
      mutate(
        days_exposed_total = as.numeric(follow_up_end - index_date),
        years_exposed = days_exposed_total / 365.25,
        died_during_followup = !is.na(death_date) & death_date <= follow_up_end
      ) %>%
      distinct(subject_id, years_exposed, days_exposed_total, died_during_followup),
    by = "subject_id"
  ) %>%
  
  # Adjust visits based on overall period rules
  mutate(
    adjusted_visits = case_when(
      died_during_followup ~ visit_count,  # No adjustment if died
      days_exposed_total >= 365.25 ~ visit_count,  # No adjustment if full year
      days_exposed_total >= 182.625 ~ (visit_count / days_exposed_total) * 365.25,  # Adjust if ≥ half year
      TRUE ~ 0  # Ignore if < 6 months exposure
    )
  ) %>% 
  
  mutate(adjusted_visit_costs = adjusted_visits * unit_cost) 

# Sum adjusted visit costs per patient
adjusted_users_costs_overall_results <- adjusted_users_costs_overall %>%
  group_by(subject_id) %>%
  summarise(
    total_patient_visits_cost = sum(adjusted_visit_costs, na.rm = TRUE),
    .groups = "drop"
  )

# summary stats
adjusted_user_costs_summary_statistics_overall <- adjusted_users_costs_overall_results %>%
  summarise(
    mean_visits_cost = round(mean(total_patient_visits_cost, na.rm = TRUE), 2),
    min_visits_cost = round(min(total_patient_visits_cost, na.rm = TRUE), 2),
    max_visits_cost = round(max(total_patient_visits_cost, na.rm = TRUE), 2),
    sd_visits_cost = round(sd(total_patient_visits_cost, na.rm = TRUE), 2),
    median_visits_cost = round(median(total_patient_visits_cost, na.rm = TRUE), 2),
    q1_visits_cost = round(quantile(total_patient_visits_cost, 0.25, na.rm = TRUE), 2),
    q3_visits_cost = round(quantile(total_patient_visits_cost, 0.75, na.rm = TRUE), 2),
    n = n()
  ) %>%
  mutate(
    se = sd_visits_cost / sqrt(n),
    lower = mean_visits_cost - qt(0.975, df = n - 1) * se,
    upper = mean_visits_cost + qt(0.975, df = n - 1) * se
  )

# All patients
adjusted_all_costs_overall <- visit_data %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
  
  # Exposure and death status
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    years_exposed = days_exposed_total / 365.25,
    died_during_followup = !is.na(death_date) & death_date <= follow_up_end
  ) %>%
  
  # Drop those with < 1 year and no death
  filter(days_exposed_total >= 365.25 | died_during_followup) %>%
  
  # Summarise visits per patient/specialty
  group_by(subject_id, specialty) %>%
  summarise(visit_count = sum(visit_count),
            unit_cost = unit_cost,
            .groups = "drop") %>%
  
  # Remove duplicated rows
  distinct() %>% 
  
  # Join back exposure info
  left_join(
    visit_data %>%
      mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
      mutate(
        days_exposed_total = as.numeric(follow_up_end - index_date),
        years_exposed = days_exposed_total / 365.25,
        died_during_followup = !is.na(death_date) & death_date <= follow_up_end
      ) %>%
      distinct(subject_id, days_exposed_total, years_exposed, died_during_followup),
    by = "subject_id"
  ) %>%
  
  # Adjust visits
  mutate(
    adjusted_visits = case_when(
      # specialty == "Not Clinically Relevant" ~ 0,
      died_during_followup ~ visit_count,
      days_exposed_total >= 365.25 ~ visit_count,
      days_exposed_total >= 182.625 ~ (visit_count / days_exposed_total) * 365.25,
      TRUE ~ 0
    )
  ) %>% 
  mutate(adjusted_visit_costs = adjusted_visits * unit_cost) 


# Sum adjusted visits per patient
# why the hell are we dropping the decimals, sometimes you can be annoying af!!
adjusted_all_costs_overall_results <- adjusted_all_costs_overall %>%
  group_by(subject_id) %>%
  summarise(
    total_patient_visits_cost = sum(adjusted_visit_costs, na.rm = TRUE),
    .groups = "drop"
  )

adjusted_all_costs_summary_statistics_overall <- adjusted_all_costs_overall_results %>%
  summarise(
    mean_visits_cost = round(mean(total_patient_visits_cost, na.rm = TRUE), 2),
    min_visits_cost = round(min(total_patient_visits_cost, na.rm = TRUE), 2),
    max_visits_cost = round(max(total_patient_visits_cost, na.rm = TRUE), 2),
    sd_visits_cost = round(sd(total_patient_visits_cost, na.rm = TRUE), 2),
    median_visits_cost = round(median(total_patient_visits_cost, na.rm = TRUE), 2),
    q1_visits_cost = round(quantile(total_patient_visits_cost, 0.25, na.rm = TRUE), 2),
    q3_visits_cost = round(quantile(total_patient_visits_cost, 0.75, na.rm = TRUE), 2),
    n = n()
  ) %>%
  mutate(
    se = sd_visits_cost / sqrt(n),
    lower = mean_visits_cost - qt(0.975, df = n - 1) * se,
    upper = mean_visits_cost + qt(0.975, df = n - 1) * se
  )

adjusted_costs_list <- list(
  adjusted_all_costs_summary = adjusted_all_costs_summary_statistics_overall,
  adjusted_user_only_costs = adjusted_user_costs_summary_statistics_overall
)

write_xlsx(adjusted_costs_list, here::here("OMOP Analysis", "Results", "Output_costs_OMOP-Adjusted.xlsx"))


#### Boostrap ----
###### Unadjusted User ----

# Define the statistic function
# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')
fc_user_costs <- function(d, i){
  unadjusted_user_yearly_costs <- d[i, ]               # Sample the data with replacement
  return(mean(unadjusted_user_yearly_costs$total_patient_visits_cost))   # This is your variable of interest (e.g., visits, or costs)
}

# Set a seed for reproducibility
set.seed(626)

# Run bootstrap with 1000 resamples
boot_result_user_costs <- boot(unadjusted_user_yearly_costs, fc_user_costs, R = 1000)

# Compute 95% percentile-based confidence interval
boot_ci_user_costs <- boot.ci(boot.out = boot_result_user_costs, type = "norm")

mean_boot_visits_user_costs <-round(as.numeric(boot_result_user_costs$t0),3)
lower_ci_boot_visits_user_costs <- round(as.numeric(boot_ci_user_costs$normal[2]), 3)
upper_ci_boot_visits_user_costs <- round(as.numeric(boot_ci_user_costs$normal[3]), 3)


boot_user_costs_summary_statistics_overall <- tibble(
  mean_boot_visits_user_costs = mean_boot_visits_user_costs,
  lower_ci_boot_visits_user_costs = lower_ci_boot_visits_user_costs,
  upper_ci_boot_visits_user_costs = upper_ci_boot_visits_user_costs
)

###### Adjusted User ----
# Define the statistic function
# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')
fc_user_costs_adj <- function(d, i){
  adjusted_users_costs_overall_results <- d[i, ]               # Sample the data with replacement
  return(mean(adjusted_users_costs_overall_results$total_patient_visits_cost))   # This is your variable of interest (e.g., visits, or costs)
}

# Set a seed for reproducibility
set.seed(626)

# Run bootstrap with 1000 resamples
boot_result_user_costs_adj <- boot(adjusted_users_costs_overall_results, fc_user_costs_adj, R = 1000)

# Compute 95% percentile-based confidence interval
boot_ci_user_costs_adj <- boot.ci(boot.out = boot_result_user_costs_adj, type = "norm")

mean_boot_visits_user_costs_adj <-round(as.numeric(boot_result_user_costs_adj$t0),3)
lower_ci_boot_visits_user_costs_adj <- round(as.numeric(boot_ci_user_costs_adj$normal[2]), 3)
upper_ci_boot_visits_user_costs_adj <- round(as.numeric(boot_ci_user_costs_adj$normal[3]), 3)


adj_boot_user_costs_summary_statistics_overall <- tibble(
  mean_boot_visits_user_costs_adj = mean_boot_visits_user_costs_adj,
  lower_ci_boot_visits_user_costs_adj = lower_ci_boot_visits_user_costs_adj,
  upper_ci_boot_visits_user_costs_adj = upper_ci_boot_visits_user_costs_adj
)

###### Unadjusted All ----
# Define the statistic function
# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')
fc_all_costs <- function(d, i){
  unadjusted_all_yearly_costs <- d[i, ]               # Sample the data with replacement
  return(mean(unadjusted_all_yearly_costs$total_patient_visits_cost))   # This is your variable of interest (e.g., visits, or costs)
}

# Set a seed for reproducibility
set.seed(42)

# Run bootstrap with 1000 resamples
boot_result_all_costs <- boot(unadjusted_all_yearly_costs, fc_all_costs, R = 1000)

# Compute 95% percentile-based confidence interval
boot_ci_all_costs <- boot.ci(boot.out = boot_result_all_costs, type = "norm")

mean_boot_visits_all_costs <-round(as.numeric(boot_result_all_costs$t0),3)
lower_ci_boot_visits_all_costs <-round(as.numeric(boot_ci_all_costs$normal[2]), 3)
upper_ci_boot_visits_all_costs <-round(as.numeric(boot_ci_all_costs$normal[3]), 3)

boot_all_costs_summary_statistics_overall <- tibble(
  mean_boot_visits_all_costs = mean_boot_visits_all_costs,
  lower_ci_boot_visits_all_costs = lower_ci_boot_visits_all_costs,
  upper_ci_boot_visits_all_costs = upper_ci_boot_visits_all_costs
)

boot_costs_list <- list(
  boot_all_summary_stats = boot_all_costs_summary_statistics_overall,
  boot_user_summary_stats = boot_user_costs_summary_statistics_overall
)

write_xlsx(boot_costs_list, here::here("OMOP Analysis", "Results", "Output_costs_OMOP-Unadjusted_Boot.xlsx"))

###### Adjusted All ----
# Define the statistic function
# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')
fc_all_costs_adj <- function(d, i){
  adjusted_all_costs_overall_results <- d[i, ]               # Sample the data with replacement
  return(mean(adjusted_all_costs_overall_results$total_patient_visits_cost))   # This is your variable of interest (e.g., visits, or costs)
}

# Set a seed for reproducibility
set.seed(42)

# Run bootstrap with 1000 resamples
boot_result_all_costs_adj <- boot(adjusted_all_costs_overall_results, fc_all_costs_adj, R = 1000)

# Compute 95% percentile-based confidence interval
boot_ci_all_costs_adj <- boot.ci(boot.out = boot_result_all_costs_adj, type = "norm")

mean_boot_visits_all_costs_adj <-round(as.numeric(boot_result_all_costs_adj$t0),3)
lower_ci_boot_visits_all_costs_adj <-round(as.numeric(boot_ci_all_costs_adj$normal[2]), 3)
upper_ci_boot_visits_all_costs_adj <-round(as.numeric(boot_ci_all_costs_adj$normal[3]), 3)

adj_boot_all_costs_summary_statistics_overall <- tibble(
  mean_boot_visits_all_costs_adj = mean_boot_visits_all_costs_adj,
  lower_ci_boot_visits_all_costs_adj = lower_ci_boot_visits_all_costs_adj,
  upper_ci_boot_visits_all_costs_adj = upper_ci_boot_visits_all_costs_adj
)

boot_costs_list <- list(
  unadj_boot_all_stats = boot_all_costs_summary_statistics_overall,
  unadj_boot_user_stats = boot_user_costs_summary_statistics_overall,
  adj_boot_all_costs_stats = adj_boot_all_costs_summary_statistics_overall,
  adj_boot_user_costs_stats = adj_boot_user_costs_summary_statistics_overall
)

write_xlsx(boot_costs_list, here::here("CPRD Raw Analysis", "Results", "Output_costs_Raw-Boot.xlsx"))

## Cohort summary ----
cohort_summary_omop <- cohort_summary(visit_data)

# Write to an Excel file
rio::export(cohort_summary_omop, here::here("OMOP Analysis", "Results", "cohort_summary_OMOP.xlsx"))


## Specialty count ----
overall_specific_role_count <- visit_data %>% 
  group_by(specialty_reduced, specialty) %>% 
  summarise(count_specific_role = n(),
            .groups = "drop") %>% 
  mutate(percent_specific_role = count_specific_role / sum(count_specific_role) * 100) 

overall_category_count <- visit_data %>% 
  group_by(specialty_reduced) %>% 
  summarise(count_category = n(),
            .groups = "drop") %>% 
  mutate(percent_category = round(count_category / sum(count_category) * 100, 2))

overall_healthcare_role_list <- list(
  overall_category_count = overall_category_count,
  overall_specific_role_count = overall_specific_role_count
)

write_xlsx(overall_healthcare_role_list, here::here("OMOP Analysis", "Results", "Output_overall_healthcare_roles-OMOP.xlsx"))

library(gtsummary)

(gt_overall_healthcare_roles_count <- visit_data %>%
    tbl_summary(
      include = c(specialty_reduced, specialty),
      label = list(
        specialty_reduced = "Healthcare Category", 
        specialty = "Healthcare Specific Role"
      ),
      statistic = all_categorical() ~ "{n} ({p}%)",
      sort = all_categorical() ~ "frequency"
    ) %>%
    modify_header(label ~ "**Table of Healthcare Roles**") %>%
    bold_labels())

gt_overall_healthcare_roles <- as_tibble(gt_overall_healthcare_roles_count) %>% 
  rename("Table of Healthcare Roles" = "**Table of Healthcare Roles**",
         "N = 3,816,955" = "**N = 3,816,955**") 

rio::export(gt_overall_healthcare_roles, here::here("OMOP Analysis", "Results", "Output_GT_overall_healthcare_roles-OMOP.xlsx"))
