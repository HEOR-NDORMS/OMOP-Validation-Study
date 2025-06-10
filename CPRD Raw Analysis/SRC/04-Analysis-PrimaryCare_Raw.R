# **************************************************************************** #
# 	Project name: Validation Study
# 	Author: Njoki Njuki
#   Do file name: Primary care analysis
#   Date: 10.02.2025
#   Last date edited: 17.03.2025
# **************************************************************************** #

# Service users are defined as patients with records of healthcare encounters
# with the NHS and their resource use and associated costs will be estimated
# separately for primary care (hence excluding patients with no record of 
# primary care visits with any relevant healthcare staff),

# Table 5 “validation_staff” contains the requested fields from aurum.staff

# Starting with table_one_raw

cohort_table_raw <- table_one_raw

# HRCU & Cost Analysis ----
## Import the staff and consultation tables ----
# this is to be used for the specialty
validation_staff <- my_dm$`validation_staff` %>% collect()
validation_consultation <- my_dm$`validation_consultation` %>% collect() %>% 
  # Filter such that we have the visits between the study start and study end dates
  filter(consdate >= study_start_date & consdate <= study_end_date)

## Merge with validation_consultation ----
# Filter from index date to follow up end date
visit_data <- inner_join(cohort_table_raw, validation_consultation,
                         by = "patid"
) %>%  # in order to count all the visits  from index date until follow-up end date.
  filter(consdate >= index_date & consdate <= follow_up_end)
# left_join it back to the cohort data - so this 13 data will be non-users
# NOTE: 13 patients are being dropped when we filter to have consultation date be
# after or on index date and before or on follow-up end date.

# Merge cohort data
# merge back to cohort data, inorder to have patients have been filtered out 
# in the previous step
visit_data <- left_join(
  cohort_table_raw, visit_data,
  by = c(
    "patid", "death", "death_date",
    "aurum_min_accept_date", "aurum_max_accept_date", "site",
    "cancer_fa_index_dt", "paget_fa_index_dt", "index_date",
    "fracture_after_index", "follow_up_end",
    "follow_up_reason", "follow_up_time", "two_yrs_after_index",
    "study_end"
  )
)

## Merge with validation_staff
visit_data <- left_join(visit_data, validation_staff, by = "staffid") %>%
  ## Keep only distinct rows
  distinct(consdate, patid, description, .keep_all = TRUE) %>%
  ## Deselect columns not needed
  select(!c(
    cancer_fa_index_dt, paget_fa_index_dt,aurum_min_accept_date,
    aurum_max_accept_date
  )) %>%
  rename("fracture_site" = "site")

# Provider unit costs
# the costs are to be updated
# file changed to include the specialty source values that have been grouped 
# into 3: GP, Nurse, Nurse
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.4_omop.xlsx") %>%
  janitor::clean_names() %>%
  # include those with 1 which are clinically relevant
  filter(include_source == 1) %>%
  select(!notes)

# Remove the visits that are not clinically relevant
specialty_source_value_ids <- unique(provider_cost_inputs$specialty_source_value)

# Create the user_status variable
visit_data <- visit_data %>%
  rename("specialty_source_value" = "description") %>%
  mutate(specialty_source_value = case_when(
    specialty_source_value %in% specialty_source_value_ids ~ specialty_source_value,
    TRUE ~ "Not Clinically Relevant"
  )) %>%
  left_join(provider_cost_inputs, by = "specialty_source_value") %>%
  mutate(is_clinically_relevant = if_else(
    specialty_source_value == "Not Clinically Relevant", 0, 1)) %>%
  mutate(specialty_source_value_summarised = ifelse(
    is.na(specialty_source_value_summarised), 
    "Not Clinically Relevant",
    specialty_source_value_summarised)) %>%
  group_by(patid) %>%
  mutate(user_status = if_else(
    any(is_clinically_relevant == 1, na.rm = TRUE), 
    "User", "Non-User")) %>%
  mutate(not_users_status = case_when(
    user_status == "Non-User" & is.na(consdate) ~ "Unknown Visits",
    user_status == "Non-User" ~ "Visits with Non-clinically Relevant Staff"
  )) %>% 
  ungroup()

# Create a new df so that we can create visits based on these reduced specialty
visit_data_reduced <- visit_data

# Count of users/non-users
(u_n_summary <- visit_data %>%
    group_by(user_status) %>%
    summarise(count_of_pts = n_distinct(patid)) %>%
    ungroup())

# Create summary visits
summary_visits <- visit_data %>%
  group_by(patid, index_date, specialty_source_value) %>%
  summarise(visit_count = n(), .groups = "drop")

# Merge back to the visits_data
visit_data <- left_join(visit_data, summary_visits,
                        by = c("patid", "index_date", "specialty_source_value")
) %>%
  group_by(patid) %>%
  mutate(
    visit_count = if_else(
      specialty_source_value == "Not Clinically Relevant", 0, visit_count),
    unit_cost = if_else(
      specialty_source_value == "Not Clinically Relevant", 0, unit_cost),
    exposed_yrs = as.numeric(follow_up_end - index_date) / 365.25,
    visits_per_year = visit_count / exposed_yrs,
    visits_per_year = replace_na(visits_per_year, 0),
    visit_cost = visit_count * unit_cost,
    visits_costs_per_year = visit_cost / exposed_yrs,
    visits_costs_per_year = replace_na(visits_costs_per_year, 0)
  ) %>%
  rename(
    "specialty" = "specialty_source_value",
    "specialty_reduced" = "specialty_source_value_summarised"
  ) %>%
  ungroup()

# Create summary visits based on the reduced specialties
summary_visits_reduced <- visit_data_reduced %>%
  group_by(patid, index_date, specialty_source_value_summarised) %>%
  summarise(visit_count = n(), .groups = "drop")

# merge it back to visit data
visit_data_reduced <- left_join(visit_data_reduced, summary_visits_reduced,
                                by = c("patid", "index_date", "specialty_source_value_summarised")
) %>%
  group_by(patid) %>%
  mutate(
    visit_count = if_else(
      specialty_source_value_summarised == "Not Clinically Relevant", 
      0, 
      visit_count),
    unit_cost = if_else(
      specialty_source_value_summarised == "Not Clinically Relevant", 
      0, 
      unit_cost),
    exposed_yrs = as.numeric(follow_up_end - index_date) / 365.25,
    visits_per_year = visit_count / exposed_yrs,
    visits_per_year = replace_na(visits_per_year, 0),
    visit_cost = visit_count * unit_cost,
    visits_costs_per_year = visit_cost / exposed_yrs,
    visits_costs_per_year = replace_na(visits_costs_per_year, 0)
  ) %>%
  rename(
    "specialty" = "specialty_source_value",
    "specialty_reduced" = "specialty_source_value_summarised"
  ) %>%
  ungroup()

## Visits - Primary care ----
# NOTE:
# users as those with at least one visit, regardless of who they had it with.
# So if one had a GP visit and another patient had one with cardiologist they are both considered users.
# Non-user would be someone with a overall count = 0
visits_results_raw <- analyse_visits(
  visit_data = visit_data
)

# Get the output
user_only_summary <- visits_results_raw$user_only_summary
all_summary <- visits_results_raw$all_summary
(non_service_users <- visits_results_raw$non_service_users)
(tot_exposed_yrs_all <- visits_results_raw$tot_exposed_yrs_all)
(tot_exposed_yrs_user <- visits_results_raw$tot_exposed_yrs_user)

# Visits with reduced specialties
visits_results_raw_reduced <- analyse_visits_reduced(
  visit_data_reduced = visit_data_reduced
)

# Get the output
user_only_summary_reduced <- visits_results_raw_reduced$user_only_summary_reduced
all_summary_reduced <- visits_results_raw_reduced$all_summary_reduced
(non_service_users_reduced <- visits_results_raw_reduced$non_service_users_reduced)
(tot_exposed_yrs_all <- visits_results_raw_reduced$tot_exposed_yrs_all)
(tot_exposed_yrs_user <- visits_results_raw_reduced$tot_exposed_yrs_user)


### Overall visits summaries ----
# User-only summary statistics
# Filter user visits and relevant specialties
user_visits <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant")

# Export the output
library(writexl)

visits_list <- list(
  user_only_summary = user_only_summary,
  all_summary = all_summary,
  user_only_summary_reduced = user_only_summary_reduced,
  all_summary_reduced = all_summary_reduced
)

# Write to an Excel file
write_xlsx(visits_list, here::here("CPRD Raw Analysis", "Results", "Output_visits_Raw.xlsx"))

#### Unadjusted ----

# We then compute visits per year per patient
unadjusted_yearly_visits <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  group_by(patid, specialty) %>%
  summarise(visits_count = n(),
            .groups = "drop") %>%
  group_by(patid) %>%
  summarise(
    total_patient_visits = sum(visits_count, na.rm = TRUE),
    # total_exposed_yrs = first(exposed_yrs),
    # visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  )

# mean(user_yearly_visits$visits_per_year)


unadjusted_user_only_summary_statistics_overall <- unadjusted_yearly_visits %>%
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

# We then compute visits per year per patient
unadjusted_all_yearly_visits <- visit_data %>%
  group_by(patid, specialty) %>%
  summarise(visits_count = first(visit_count),
            # exposed_yrs = first(exposed_yrs),
            .groups = "drop") %>%
  group_by(patid) %>%
  summarise(
    total_patient_visits = sum(visits_count, na.rm = TRUE),
    # total_exposed_yrs = first(exposed_yrs),
    # visits_per_year = total_patient_visits / total_exposed_yrs,
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
    n = n(),
    se = sd_visit_per_year / sqrt(n),
    lower = mean_visit_per_year - qt(0.975, df = n - 1) * se,
    upper = mean_visit_per_year + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))


unadjusted_visits_list <- list(
  unadjusted_all_summary = unadjusted_all_summary_statistics_overall,
  unadjusted_user_only_summary = unadjusted_user_only_summary_statistics_overall
 )

write_xlsx(unadjusted_visits_list, here::here("CPRD Raw Analysis", "Results", "Output_visits_Raw-Unadjusted.xlsx"))

#### Adjusted ----
adjusted_users_visits_overall <- visit_data %>%
  filter(specialty != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
  
  # Total exposure duration
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    years_exposed = days_exposed_total / 365.25,
    died_during_followup = !is.na(death_date) & death_date <= follow_up_end
  ) %>%
  
  # Drop those with < 1 year and no death
  filter(days_exposed_total >= 365.25 | died_during_followup) %>%
  
  # Count visits per patient/specialty
  group_by(patid, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop") %>%
  
  # Join back exposure and death info
  left_join(
    visit_data %>%
      mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
      mutate(
        days_exposed_total = as.numeric(follow_up_end - index_date),
        years_exposed = days_exposed_total / 365.25,
        died_during_followup = !is.na(death_date) & death_date <= follow_up_end
      ) %>%
      distinct(patid, years_exposed, days_exposed_total, died_during_followup),
    by = "patid"
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
  group_by(patid) %>%
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
  mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
  
  # Exposure and death status
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    years_exposed = days_exposed_total / 365.25,
    died_during_followup = !is.na(death_date) & death_date <= follow_up_end
  ) %>%
  
  # Drop those with < 1 year and no death
  filter(days_exposed_total >= 365.25 | died_during_followup) %>%
  
  # Set visit count
  mutate(
    visit_count = if_else(specialty == "Not Clinically Relevant", 0L, 1L)
  ) %>%
  
  # Summarise visits per patient/specialty
  group_by(patid, specialty) %>%
  summarise(visit_count = sum(visit_count), .groups = "drop") %>%
  
  # Join back exposure info
  left_join(
    visit_data %>%
      mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
      mutate(
        days_exposed_total = as.numeric(follow_up_end - index_date),
        years_exposed = days_exposed_total / 365.25,
        died_during_followup = !is.na(death_date) & death_date <= follow_up_end
      ) %>%
      distinct(patid, days_exposed_total, years_exposed, died_during_followup),
    by = "patid"
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
  group_by(patid) %>%
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

write_xlsx(adjusted_visits_list, here::here("CPRD Raw Analysis", "Results", "Output_visits_Raw-Adjusted.xlsx"))

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

###### Adjusted All ----
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

write_xlsx(boot_visits_list, here::here("CPRD Raw Analysis", "Results", "Output_visits_Raw-Boot.xlsx"))

## Cost - Primary care ----
cost_results_raw <- analyse_visits_cost(
  visit_data = visit_data
)

# reducing the specialty
cost_results_raw_reduced <- analyse_visits_cost_reduced(
  visit_data_reduced = visit_data_reduced
)
### Get the output grouped by specialty ----
user_only_cost_summary <- cost_results_raw$user_only_cost_summary
all_cost_summary <- cost_results_raw$all_cost_summary
(tot_exposed_yrs_all <- cost_results_raw$tot_exposed_yrs_all)
(tot_exposed_yrs_user <- cost_results_raw$tot_exposed_yrs_user)
user_only_cost_summary_reduced <- cost_results_raw_reduced$user_only_cost_summary_reduced
all_cost_summary_reduced <- cost_results_raw_reduced$all_cost_summary_reduced

### Overall cost summaries ----
user_costs <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant")

# We then compute visits per year per patient
user_yearly_costs <- user_costs %>%
  group_by(patid, specialty) %>%
  summarise(visit_cost = first(visit_cost),
            exposed_yrs = first(exposed_yrs),
            .groups = "drop") %>%
  group_by(patid) %>%
  summarise(
    total_patient_visits_cost = sum(visit_cost, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_cost_per_year = total_patient_visits_cost / total_exposed_yrs,
    .groups = "drop"
  )

user_only_costs_summary_statistics_overall <- tibble(
  mean_visit_cost_per_year = round(sum(user_yearly_costs$total_patient_visits_cost) / sum(user_yearly_costs$total_exposed_yrs), 2),
  min_visit_cost_per_year = round(min(user_yearly_costs$visits_cost_per_year), 2),
  max_visit_cost_per_year = round(max(user_yearly_costs$visits_cost_per_year), 2),
  sd_visit_cost_per_year = round(sd(user_yearly_costs$visits_cost_per_year), 2),
  median_visit_cost_per_year = round(quantile(user_yearly_costs$visits_cost_per_year, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(user_yearly_costs$visits_cost_per_year, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(user_yearly_costs$visits_cost_per_year, probs = (.75)), 2)
)

# All patients
# We then compute visit costs per year per patient
all_yearly_costs <- visit_data %>%
  group_by(patid, specialty) %>%
  summarise(visit_cost = first(visit_cost),
            exposed_yrs = first(exposed_yrs),
            .groups = "drop") %>%
  group_by(patid) %>%
  summarise(
    total_patient_visits_cost = sum(visit_cost, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_cost_per_year = total_patient_visits_cost / total_exposed_yrs,
    .groups = "drop"
  )

all_costs_summary_statistics_overall <- tibble(
  mean_visit_cost_per_year = round(sum(all_yearly_costs$total_patient_visits_cost) / sum(all_yearly_costs$total_exposed_yrs), 2),
  min_visit_cost_per_year = round(min(all_yearly_costs$visits_cost_per_year), 2),
  max_visit_cost_per_year = round(max(all_yearly_costs$visits_cost_per_year), 2),
  sd_visit_cost_per_year = round(sd(all_yearly_costs$visits_cost_per_year), 2),
  median_visit_cost_per_year = round(quantile(all_yearly_costs$visits_cost_per_year, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(all_yearly_costs$visits_cost_per_year, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(all_yearly_costs$visits_cost_per_year, probs = (.75)), 2)
)

# List of the output of costs
costs_list <- list(
  overall_user_only_costs = user_only_costs_summary_statistics_overall,
  overall_all_only_costs = all_costs_summary_statistics_overall,
  user_only_cost_summary = user_only_cost_summary,
  all_cost_summary = all_cost_summary,
  user_only_cost_summary_reduced = user_only_cost_summary_reduced,
  all_cost_summary_reduced = all_cost_summary_reduced
)

# Write to an Excel file
# write_xlsx(costs_list, here::here("CPRD Raw Analysis", "Results", "Output_costs_Raw.xlsx"))

#### Unadjusted ----
# We then compute visits per year per patient
unadjusted_user_yearly_costs <- user_costs %>%
  group_by(patid, specialty) %>%
  summarise(
    visit_count = n(),
    unit_cost = unit_cost,
    visits_cost = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct() %>% 
  group_by(patid) %>%
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
  group_by(patid, specialty) %>%
  summarise(visit_cost = first(visit_cost),
            # exposed_yrs = first(exposed_yrs),
            .groups = "drop") %>%
  group_by(patid) %>%
  summarise(
    total_patient_visits_cost = sum(visit_cost, na.rm = TRUE),
    # total_exposed_yrs = first(exposed_yrs),
    # visits_cost_per_year = total_patient_visits_cost / total_exposed_yrs,
    .groups = "drop"
  )

unadjusted_all_costs_summary_statistics_overall <- unadjusted_all_yearly_costs %>%
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


unadjusted_costs_list <- list(
  unadjusted_all_costs_summary = unadjusted_all_costs_summary_statistics_overall,
  unadjusted_user_only_costs = unadjusted_user_only_costs_summary_statistics_overall
)
write_xlsx(unadjusted_costs_list, here::here("CPRD Raw Analysis", "Results", "Output_costs_Raw-Unadjusted.xlsx"))

#### Adjusted ----
adjusted_users_costs_overall <- visit_data %>%
  filter(specialty != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
  
  # Total exposure duration
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    years_exposed = days_exposed_total / 365.25,
    died_during_followup = !is.na(death_date) & death_date <= follow_up_end
  ) %>%
  # Drop those with < 1 year and no death
  filter(days_exposed_total >= 365.25 | died_during_followup) %>%
  
  # Count visits per patient/specialty
  group_by(patid, specialty) %>%
  summarise(visit_count = n(), 
            unit_cost = unit_cost,
            .groups = "drop") %>%
  distinct() %>% 
  
  # Join back exposure and death info
  left_join(
    visit_data %>%
      mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
      mutate(
        days_exposed_total = as.numeric(follow_up_end - index_date),
        years_exposed = days_exposed_total / 365.25,
        died_during_followup = !is.na(death_date) & death_date <= follow_up_end
      ) %>%
      distinct(patid, years_exposed, days_exposed_total, died_during_followup),
    by = "patid"
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
  group_by(patid) %>%
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
  mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
  
  # Exposure and death status
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    years_exposed = days_exposed_total / 365.25,
    died_during_followup = !is.na(death_date) & death_date <= follow_up_end
  ) %>%
  
  # Drop those with < 1 year and no death
  filter(days_exposed_total >= 365.25 | died_during_followup) %>%
  
  # Set visit count
  mutate(
    visit_count = if_else(specialty == "Not Clinically Relevant", 0L, 1L)
  ) %>%
  
  # Summarise visits per patient/specialty
  group_by(patid, specialty) %>%
  summarise(visit_count = sum(visit_count),
            unit_cost = unit_cost,
            .groups = "drop") %>%
  
  # Remove duplicated rows
  distinct() %>% 
  
  # Join back exposure info
  left_join(
    visit_data %>%
      mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
      mutate(
        days_exposed_total = as.numeric(follow_up_end - index_date),
        years_exposed = days_exposed_total / 365.25,
        died_during_followup = !is.na(death_date) & death_date <= follow_up_end
      ) %>%
      distinct(patid, days_exposed_total, years_exposed, died_during_followup),
    by = "patid"
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
  group_by(patid) %>%
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

write_xlsx(adjusted_costs_list, here::here("CPRD Raw Analysis", "Results", "Output_costs_Raw-Adjusted.xlsx"))


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
cohort_summary_raw <- cohort_summary(visit_data)

# Write to an Excel file
rio::export(cohort_summary_raw, here::here("CPRD Raw Analysis", "Results", "cohort_summary_raw.xlsx"))



