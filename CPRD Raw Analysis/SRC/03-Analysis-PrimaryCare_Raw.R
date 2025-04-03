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
validation_consultation <- my_dm$`validation_consultation` %>% collect()

## Merge with validation_consultation ----
# Filter from index date to follow up end date
visit_data <- inner_join(cohort_table_raw, validation_consultation,
  by = "patid"
) %>%
  # in order to count all the visits from index date until follow-up end date.
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
    "patid", "gender", "yob", "death", "death_date",
    "aurum_min_accept_date", "aurum_max_accept_date", "site",
    "fx_date", "days_prior_obs", "days_after_obs", "age_fracture",
    "type", "cancer_fa_index_dt", "paget_fa_index_dt", "index_date",
    "after_index", "fracture_after_index", "follow_up_end",
    "follow_up_reason", "follow_up_time", "exposed_days", "excluded_flag"
  )
)

## Merge with validation_staff
visit_data <- left_join(visit_data, validation_staff, by = "staffid") %>%
  ## Keep only distinct rows
  distinct(consdate, patid, description, .keep_all = TRUE) %>%
  ## Deselect columns not needed
  select(!c(
    gender, yob, death, death_date,
    fx_date, cancer_fa_index_dt, paget_fa_index_dt,
    days_after_obs, days_prior_obs, aurum_min_accept_date,
    aurum_max_accept_date, type, after_index
  )) %>%
  rename("fracture_site" = "site")

# Provider unit costs
# the costs are to be updated
# file changed to include the specialty source values that have been grouped 
# into 3: GP, Nurse, Nurse
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.3.xlsx") %>%
  janitor::clean_names() %>%
  # include those with 1 which are clinically relevant
  filter(include == 1) %>%
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

# We then compute visits per year per patient
user_yearly_visits <- user_visits %>%
  group_by(patid, specialty) %>%
  summarise(visits_count = first(visit_count),
            exposed_yrs = first(exposed_yrs),
            .groups = "drop") %>%
  group_by(patid) %>%
  summarise(
    total_patient_visits = sum(visits_count, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  )

# mean(user_yearly_visits$visits_per_year)

user_only_summary_statistics_overall <- tibble(
  mean_visit_per_year = round(sum(user_yearly_visits$total_patient_visits) / sum(user_yearly_visits$total_exposed_yrs), 2),
  min_visit_per_year = round(min(user_yearly_visits$visits_per_year), 2),
  max_visit_per_year = round(max(user_yearly_visits$visits_per_year), 2),
  sd_visit_per_year = round(sd(user_yearly_visits$visits_per_year), 2),
  median_visit_per_year = round(quantile(user_yearly_visits$visits_per_year, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(user_yearly_visits$visits_per_year, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(user_yearly_visits$visits_per_year, probs = (.75)), 2)
)


# All patients

# We then compute visits per year per patient
all_yearly_visits <- visit_data %>%
  group_by(patid, specialty) %>%
  summarise(visits_count = first(visit_count),
            exposed_yrs = first(exposed_yrs),
            .groups = "drop") %>%
  group_by(patid) %>%
  summarise(
    total_patient_visits = sum(visits_count, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  )

all_summary_statistics_overall <- tibble(
  mean_visit_per_year = round(sum(all_yearly_visits$total_patient_visits) / sum(all_yearly_visits$total_exposed_yrs), 2),
  min_visit_per_year = round(min(all_yearly_visits$visits_per_year), 2),
  max_visit_per_year = round(max(all_yearly_visits$visits_per_year), 2),
  sd_visit_per_year = round(sd(all_yearly_visits$visits_per_year), 2),
  median_visit_per_year = round(quantile(all_yearly_visits$visits_per_year, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(all_yearly_visits$visits_per_year, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(all_yearly_visits$visits_per_year, probs = (.75)), 2)
)

## For years 1 and 2 users
# Identify patients who qualify for at least 30.44 days in the second year
eligible_patients <- cohort_table %>% 
  mutate(exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days"))) %>% 
  mutate(
    included_second_year = exposed_days > 365.25 & (exposed_days - 365.25) >= 30.44 & exposed_days <= 730
  ) %>%
  select(patid, included_second_year)

# Adjust year_after_index calculation and merge with eligible patients
user_visits_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    raw_interval = interval(index_date, consdate),
    year_after_index = pmin(2, pmax(1, floor(interval(index_date, consdate) / years(1)) + 1))  # Add 1, cap at 2
  ) %>%
  inner_join(eligible_patients, by = "patid") %>%  # Keep only eligible patients
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%  # Ensure valid second-year patients
  group_by(patid, year_after_index, specialty) %>%
  summarise(visit_count = n(), .groups = "drop")

# Total visits per patient per year
user_total_visits_per_patient_yr <- user_visits_yr %>%
  group_by(patid, year_after_index) %>%
  summarise(total_visits = sum(visit_count),
            visits_per_yr = total_visits/year_after_index,
            .groups = "drop") %>% 
  distinct()

# Summary statistics for visits per year
user_summary_visits_per_year <- user_total_visits_per_patient_yr %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visit_count = round(sum(total_visits) /sum(year_after_index), 2),
    min_visits = round(min(visits_per_yr), 2),
    max_visits = round(max(visits_per_yr), 2),
    sd_visits = round(sd(visits_per_yr), 2),
    median_visits = round(median(visits_per_yr), 2),
    q1_visits = round(quantile(visits_per_yr, 0.25), 2),
    q3_visits = round(quantile(visits_per_yr, 0.75), 2),
    .groups = "drop"
  )

## For all patients
# Adjust year_after_index calculation and merge with eligible patients
all_visits_yr <- visit_data %>%
  mutate(
    raw_interval = interval(index_date, consdate),
    year_after_index = pmin(2, pmax(1, floor(interval(index_date, consdate) / years(1)) + 1))  # Add 1, cap at 2
  ) %>%
  inner_join(eligible_patients, by = "patid") %>%  # Keep only eligible patients
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%  # Ensure valid second-year patients
  group_by(patid, year_after_index, specialty) %>%
  summarise(visit_count = n(), .groups = "drop") %>% 
  mutate(visit_count = if_else(
    specialty == "Not Clinically Relevant", 0, visit_count
  ))

# Total visits per patient per year
all_total_visits_per_patient_yr <- all_visits_yr %>%
  group_by(patid, year_after_index) %>%
  summarise(total_visits = sum(visit_count),
            visits_per_yr = total_visits/year_after_index,
            .groups = "drop") %>% 
  distinct()

# Summary statistics for visits per year
all_summary_visits_per_year <- all_total_visits_per_patient_yr %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visit_count = round(sum(total_visits) / n_distinct(patid), 2),
    min_visits = round(min(visits_per_yr), 2),
    max_visits = round(max(visits_per_yr), 2),
    sd_visits = round(sd(visits_per_yr), 2),
    median_visits = round(median(visits_per_yr), 2),
    q1_visits = round(quantile(visits_per_yr, 0.25), 2),
    q3_visits = round(quantile(visits_per_yr, 0.75), 2),
    .groups = "drop"
  )

# Export the output
library(writexl)

visits_list <- list(
  overall_user_visits_only = user_only_summary_statistics_overall,
  user_summary_visits_per_yr = user_summary_visits_per_year,
  overall_all_visits_only = all_summary_statistics_overall,
  all_summary_visits_per_yr = all_summary_visits_per_year,
  user_only_summary = user_only_summary,
  all_summary = all_summary,
  user_only_summary_reduced = user_only_summary_reduced,
  all_summary_reduced = all_summary_reduced
)

# Write to an Excel file
# write_xlsx(visits_list, here::here("CPRD Raw Analysis", "Results", "Output_visits_Raw.xlsx"))

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

# Count visits per patient, year, and specialty (User Visits)
user_visits_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    raw_interval = interval(index_date, consdate),
    year_after_index = pmin(2, pmax(1, floor(interval(index_date, consdate) / years(1)) + 1))  # Add 1, cap at 2
  ) %>%
  inner_join(eligible_patients, by = "patid") %>%
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%
  group_by(patid, year_after_index, specialty) %>%
  summarise(visit_count = n(), .groups = "drop")

# Visit costs based on visit count
user_visits_cost_yr <- user_visits_yr %>%
  left_join(select(visit_data, patid, specialty, unit_cost), by = c("patid", "specialty")) %>%
  mutate(visit_cost = unit_cost * visit_count) %>% 
  distinct()

# Total visit cost per patient per year
user_total_visit_cost_per_patient_yr <- user_visits_cost_yr %>%
  group_by(patid, year_after_index) %>%
  summarise(total_cost = sum(visit_cost, na.rm = TRUE), 
            visit_cost_per_yr = total_cost/year_after_index,
            .groups = "drop")

# Summary statistics for visit costs per year
user_summary_visit_cost_per_year <- user_total_visit_cost_per_patient_yr %>%
  group_by(year_after_index) %>%
  summarise(
    mean_cost = sum(total_cost) / sum(year_after_index),
    min_cost = min(visit_cost_per_yr),
    max_cost = max(visit_cost_per_yr),
    sd_cost = sd(visit_cost_per_yr),
    median_cost = median(visit_cost_per_yr),
    q1_cost = quantile(visit_cost_per_yr, 0.25),
    q3_cost = quantile(visit_cost_per_yr, 0.75),
    .groups = "drop"
  )

# Visits for all patients
all_visits_yr <- visit_data %>%
  mutate(
    raw_interval = interval(index_date, consdate),
    year_after_index = pmin(2, pmax(1, floor(interval(index_date, consdate) / years(1)) + 1))  # Add 1, cap at 2
  ) %>%
  inner_join(eligible_patients, by = "patid") %>%
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%
  group_by(patid, year_after_index, specialty) %>%
  summarise(visit_count = n(), .groups = "drop")

# Visit costs based on visit count (All Visits)
all_visits_cost_yr <- all_visits_yr %>%
  left_join(select(visit_data, patid, specialty, unit_cost), by = c("patid", "specialty")) %>%
  mutate(visit_cost = unit_cost * visit_count) %>%
  mutate(visit_cost = if_else(specialty == "Not Clinically Relevant", 0, visit_cost)) %>% 
  distinct()

# Total visit cost per patient per year (All Visits)
all_total_visit_cost_per_patient_yr <- all_visits_cost_yr %>%
  group_by(patid, year_after_index) %>%
  summarise(total_cost = sum(visit_cost, na.rm = TRUE),
            visit_cost_per_yr = total_cost/year_after_index,
            .groups = "drop")

# Summary statistics for visit costs per year (All Visits)
all_summary_visit_cost_per_year <- all_total_visit_cost_per_patient_yr %>%
  group_by(year_after_index) %>%
  summarise(
    mean_cost = sum(total_cost) / sum(year_after_index),
    min_cost = min(visit_cost_per_yr),
    max_cost = max(visit_cost_per_yr),
    sd_cost = sd(visit_cost_per_yr),
    median_cost = median(visit_cost_per_yr),
    q1_cost = quantile(visit_cost_per_yr, 0.25),
    q3_cost = quantile(visit_cost_per_yr, 0.75),
    .groups = "drop"
  )


# List of the output of costs
costs_list <- list(
  overall_user_only_costs = user_only_costs_summary_statistics_overall,
  user_summary_visit_cost_per_yr = user_summary_visit_cost_per_year,
  overall_all_only_costs = all_costs_summary_statistics_overall,
  all_summary_visit_cost_per_yr = all_summary_visit_cost_per_year,
  user_only_cost_summary = user_only_cost_summary,
  all_cost_summary = all_cost_summary,
  user_only_cost_summary_reduced = user_only_cost_summary_reduced,
  all_cost_summary_reduced = all_cost_summary_reduced
)

# Write to an Excel file
# write_xlsx(costs_list, here::here("CPRD Raw Analysis", "Results", "Output_costs_Raw.xlsx"))

## Cohort summary ----
cohort_summary_raw <- cohort_summary(visit_data)

# Write to an Excel file
# rio::export(cohort_summary_raw, here::here("CPRD Raw Analysis", "Results", "cohort_summary_raw.xlsx"))



