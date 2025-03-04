# **************************************************************************** #
# 	Project name: Validation Study
# 	Author: Njoki Njuki
#   Do file name: Primary care analysis
#   Date: 10.02.2025
#   Last date edited: 04.03.2025
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
# merge back to cohort data
visit_data <- left_join(
  cohort_table_raw, visit_data,
  by = c(
    "patid", "gender", "yob", "death", "death_date",
    "aurum_min_accept_date", "aurum_max_accept_date", "site",
    "fx_date", "days_prior_obs", "days_after_obs", "age_fracture",
    "type", "cancer_fa_index_dt", "paget_fa_index_dt", "index_date",
    "after_index", "fracture_after_index", "follow_up_end",
    "follow_up_reason", "follow_up_time"
  )
)

## Merge with validation_staff
visit_data <- left_join(visit_data, validation_staff, by = "staffid") %>%
  ## Keep only distinct rows
  distinct(consdate, patid, description, .keep_all = TRUE) %>%
  ## Select columns needed
  select(!c(
    gender, yob, death, death_date,
    fx_date, cancer_fa_index_dt, paget_fa_index_dt
  )) %>%
  rename("fracture_site" = "site")

# Provider unit costs
# the costs are to be updated
# file changed to include the specialty source values that have been grouped 
# into 3: GP, Nurse, Nurse
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.3_summarised.xlsx") %>%
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
    exposed_yrs = as.numeric(follow_up_end - index_date) / 360.25,
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
    exposed_yrs = as.numeric(follow_up_end - index_date) / 360.25,
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
user_only_summary_statistics_df <- visit_data %>%
  filter(user_status == "User",
         specialty != "Not Clinically Relevant") %>%
  group_by(patid, specialty) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop"
  )


# mean(user_only_summary_statistics_df$tot_visits_per_yr)

user_only_summary_statistics <- tibble(
  mean_visit_per_year = round(sum(user_only_summary_statistics_df$tot_visits) / sum(user_only_summary_statistics_df$tot_exposed_yrs), 2),
  min_visit_per_year = round(min(user_only_summary_statistics_df$tot_visits_per_yr), 2),
  max_visit_per_year = round(max(user_only_summary_statistics_df$tot_visits_per_yr), 2),
  sd_visit_per_year = round(sd(user_only_summary_statistics_df$tot_visits_per_yr), 2),
  median_visit_per_year = round(quantile(user_only_summary_statistics_df$tot_visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(user_only_summary_statistics_df$tot_visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(user_only_summary_statistics_df$tot_visits_per_yr, probs = (.75)), 2)
)

# reducing the specialty
user_only_summary_statistics_df_reduced <- visit_data_reduced %>%
  filter(user_status == "User", 
         specialty_reduced != "Not Clinically Relevant") %>%
  group_by(patid, specialty_reduced) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop"
  )


# mean(user_only_summary_statistics_df_reduced$tot_visits_per_yr)

user_only_summary_statistics_reduced <- tibble(
  mean_visit_per_year = round(sum(user_only_summary_statistics_df_reduced$tot_visits) / sum(user_only_summary_statistics_df_reduced$tot_exposed_yrs), 2),
  min_visit_per_year = round(min(user_only_summary_statistics_df_reduced$tot_visits_per_yr), 2),
  max_visit_per_year = round(max(user_only_summary_statistics_df_reduced$tot_visits_per_yr), 2),
  sd_visit_per_year = round(sd(user_only_summary_statistics_df_reduced$tot_visits_per_yr), 2),
  median_visit_per_year = round(quantile(user_only_summary_statistics_df_reduced$tot_visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(user_only_summary_statistics_df_reduced$tot_visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(user_only_summary_statistics_df_reduced$tot_visits_per_yr, probs = (.75)), 2)
)


# All summary statistics
all_summary_statistics_df <- visit_data %>%
  group_by(patid, specialty) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    user_status = first(user_status),
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop"
  ) %>%
  mutate(tot_visits_per_yr = replace_na(tot_visits_per_yr, 0))

# reducing the specialty
all_summary_statistics <- tibble(
  mean_visit_per_year = round(sum(all_summary_statistics_df$tot_visits) / sum(all_summary_statistics_df$tot_exposed_yrs), 2),
  min_visit_per_year = round(min(all_summary_statistics_df$tot_visits_per_yr), 2),
  max_visit_per_year = round(max(all_summary_statistics_df$tot_visits_per_yr), 2),
  sd_visit_per_year = round(sd(all_summary_statistics_df$tot_visits_per_yr), 2),
  median_visit_per_year = round(quantile(all_summary_statistics_df$tot_visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(all_summary_statistics_df$tot_visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(all_summary_statistics_df$tot_visits_per_yr, probs = (.75)), 2)
)

all_summary_statistics_df_reduced <- visit_data_reduced %>%
  group_by(patid, specialty_reduced) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    user_status = first(user_status),
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop"
  ) %>%
  mutate(tot_visits_per_yr = replace_na(tot_visits_per_yr, 0))


all_summary_statistics_reduced <- tibble(
  mean_visit_per_year = round(sum(all_summary_statistics_df_reduced$tot_visits) / sum(all_summary_statistics_df_reduced$tot_exposed_yrs), 2),
  min_visit_per_year = round(min(all_summary_statistics_df_reduced$tot_visits_per_yr), 2),
  max_visit_per_year = round(max(all_summary_statistics_df_reduced$tot_visits_per_yr), 2),
  sd_visit_per_year = round(sd(all_summary_statistics_df_reduced$tot_visits_per_yr), 2),
  median_visit_per_year = round(quantile(all_summary_statistics_df_reduced$tot_visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(all_summary_statistics_df_reduced$tot_visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(all_summary_statistics_df_reduced$tot_visits_per_yr, probs = (.75)), 2)
)


# Export the output
library(writexl)

visits_list <- list(
  user_only_summary_statistics = user_only_summary_statistics,
  all_summary_statistics = all_summary_statistics,
  user_only_summary = user_only_summary,
  all_summary = all_summary,
  user_only_summary_statistics_reduced = user_only_summary_statistics_reduced,
  all_summary_statistics_reduced = all_summary_statistics_reduced,
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
user_cost_summary_statistics_df <- visit_data %>%
  filter(user_status == "User", 
         specialty != "Not Clinically Relevant") %>%
  filter(visit_cost > 0) %>%
  group_by(patid, specialty) %>%
  summarise(
    total_patient_cost = first(visit_cost),
    total_exposed_yrs = first(exposed_yrs),
    visits_costs_per_year = total_patient_cost / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_costs = sum(total_patient_cost, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_costs_per_yr = tot_costs / tot_exposed_yrs,
    .groups = "drop"
  )

user_cost_summary_statistics <- tibble(
  mean_visit_cost_per_year = round(sum(user_cost_summary_statistics_df$tot_costs) / sum(user_cost_summary_statistics_df$tot_exposed_yrs), 2),
  min_visit_cost_per_year = round(min(user_cost_summary_statistics_df$tot_costs_per_yr), 2),
  max_visit_cost_per_year = round(max(user_cost_summary_statistics_df$tot_costs_per_yr), 2),
  sd_visit_cost_per_year = round(sd(user_cost_summary_statistics_df$tot_costs_per_yr), 2),
  median_visit_cost_per_year = round(quantile(user_cost_summary_statistics_df$tot_costs_per_yr, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(user_cost_summary_statistics_df$tot_costs_per_yr, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(user_cost_summary_statistics_df$tot_costs_per_yr, probs = (.75)), 2)
)

# reducing the specialties
user_cost_summary_statistics_df_reduced <- visit_data_reduced %>%
  filter(user_status == "User", 
         specialty_reduced != "Not Clinically Relevant") %>%
  filter(visit_cost > 0) %>%
  group_by(patid, specialty_reduced) %>%
  summarise(
    total_patient_cost = first(visit_cost),
    total_exposed_yrs = first(exposed_yrs),
    visits_costs_per_year = total_patient_cost / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_costs = sum(total_patient_cost, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_costs_per_yr = tot_costs / tot_exposed_yrs,
    .groups = "drop"
  )

user_cost_summary_statistics_reduced <- tibble(
  mean_visit_cost_per_year = round(sum(user_cost_summary_statistics_df_reduced$tot_costs) / sum(user_cost_summary_statistics_df_reduced$tot_exposed_yrs), 2),
  min_visit_cost_per_year = round(min(user_cost_summary_statistics_df_reduced$tot_costs_per_yr), 2),
  max_visit_cost_per_year = round(max(user_cost_summary_statistics_df_reduced$tot_costs_per_yr), 2),
  sd_visit_cost_per_year = round(sd(user_cost_summary_statistics_df_reduced$tot_costs_per_yr), 2),
  median_visit_cost_per_year = round(quantile(user_cost_summary_statistics_df_reduced$tot_costs_per_yr, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(user_cost_summary_statistics_df_reduced$tot_costs_per_yr, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(user_cost_summary_statistics_df_reduced$tot_costs_per_yr, probs = (.75)), 2)
)


all_cost_summary_statistics_df <- visit_data %>%
  group_by(patid, specialty) %>%
  summarise(
    total_patient_cost = first(visit_cost),
    total_exposed_yrs = first(exposed_yrs),
    visits_costs_per_year = total_patient_cost / total_exposed_yrs,
    user_status = first(user_status),
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_costs = sum(total_patient_cost, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_costs_per_yr = tot_costs / tot_exposed_yrs,
    .groups = "drop"
  ) %>%
  mutate(tot_costs_per_yr = replace_na(tot_costs_per_yr, 0))

all_cost_summary_statistics <- tibble(
  mean_visit_cost_per_year = round(sum(all_cost_summary_statistics_df$tot_costs) / sum(all_cost_summary_statistics_df$tot_exposed_yrs), 2),
  min_visit_cost_per_year = round(min(all_cost_summary_statistics_df$tot_costs_per_yr), 2),
  max_visit_cost_per_year = round(max(all_cost_summary_statistics_df$tot_costs_per_yr), 2),
  sd_visit_cost_per_year = round(sd(all_cost_summary_statistics_df$tot_costs_per_yr), 2),
  median_visit_cost_per_year = round(quantile(all_cost_summary_statistics_df$tot_costs_per_yr, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(all_cost_summary_statistics_df$tot_costs_per_yr, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(all_cost_summary_statistics_df$tot_costs_per_yr, probs = (.75)), 2)
)

# reducing the specialties
all_cost_summary_statistics_df_reduced <- visit_data_reduced %>%
  group_by(patid, specialty_reduced) %>%
  summarise(
    total_patient_cost = first(visit_cost),
    total_exposed_yrs = first(exposed_yrs),
    visits_costs_per_year = total_patient_cost / total_exposed_yrs,
    user_status = first(user_status),
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_costs = sum(total_patient_cost, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_costs_per_yr = tot_costs / tot_exposed_yrs,
    .groups = "drop"
  ) %>%
  mutate(tot_costs_per_yr = replace_na(tot_costs_per_yr, 0))

all_cost_summary_statistics_reduced <- tibble(
  mean_visit_cost_per_year = round(sum(all_cost_summary_statistics_df_reduced$tot_costs) / sum(all_cost_summary_statistics_df_reduced$tot_exposed_yrs), 2),
  min_visit_cost_per_year = round(min(all_cost_summary_statistics_df_reduced$tot_costs_per_yr), 2),
  max_visit_cost_per_year = round(max(all_cost_summary_statistics_df_reduced$tot_costs_per_yr), 2),
  sd_visit_cost_per_year = round(sd(all_cost_summary_statistics_df_reduced$tot_costs_per_yr), 2),
  median_visit_cost_per_year = round(quantile(all_cost_summary_statistics_df_reduced$tot_costs_per_yr, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(all_cost_summary_statistics_df_reduced$tot_costs_per_yr, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(all_cost_summary_statistics_df_reduced$tot_costs_per_yr, probs = (.75)), 2)
)

# List of the output of costs
costs_list <- list(
  user_cost_summary_statistics = user_cost_summary_statistics,
  all_cost_summary_statistics = all_cost_summary_statistics,
  user_only_cost_summary = user_only_cost_summary,
  all_cost_summary = all_cost_summary,
  user_cost_summary_statistics_reduced = user_cost_summary_statistics_reduced,
  all_cost_summary_statistics_reduced = all_cost_summary_statistics_reduced,
  user_only_cost_summary_reduced = user_only_cost_summary_reduced,
  all_cost_summary_reduced = all_cost_summary_reduced
)

# Write to an Excel file
# write_xlsx(costs_list, here::here("CPRD Raw Analysis", "Results", "Output_costs_Raw.xlsx"))

## Cohort summary ----
cohort_summary_raw <- cohort_summary(visit_data)

# Write to an Excel file
# export(cohort_summary_raw, here::here("CPRD Raw Analysis", "Results", "cohort_summary_raw.xlsx"))


## Plots ----
### Visits ----

visit_data_reduced_MIN <- visit_data_reduced %>% select(
  patid, index_date, consdate, user_status,
  specialty_reduced, follow_up_end
)

#### PLOT visits per month/specialty   -------

# Filter for relevant specialties and compute months since index
visit_data_reduced_user <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>%
  mutate(month_after_index = interval(
    index_date, consdate) %/% months(1)) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute the total number of visits per patient per month 
# (sum across specialties)
total_visits_per_patient_month <- visit_data_reduced_user %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute the mean number of visits per month across all patients
mean_visits_per_month <- total_visits_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_count = sum(total_visits) / n_distinct(patid),
    .groups = "drop"
  )

# Step 3: Compute the total number of visits per specialty per month
visits_per_specialty_month <- visit_data_reduced_user %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_visits_specialty = sum(visit_count), .groups = "drop")

# Step 4: Merge the total mean visits per month
plots_users_mean_visits_specialty <- visits_per_specialty_month %>%
  left_join(mean_visits_per_month, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE))

# Step 5: Plot the mean visits per month with specialty proportions
(plot1 <- ggplot(
  plots_users_mean_visits_specialty,
  aes(
    x = month_after_index,
    y = mean_visit_count * proportion,
    fill = specialty_reduced
  )
) +
  geom_bar(
    stat = "identity",
    position = "stack"
  ) +
  scale_fill_manual(values = c(
    "General Practitioner" = "#1f77b4",
    "Nurse" = "#377eb8",
    "Other" = "#253494"
  )) +
  labs(
    x = "Month After Index",
    y = "Mean Visits per User",
    fill = "Specialty"
  ) +
  theme_minimal())

ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots", 
  "Mean_visits_per_specialty_unweighted.PNG"),
  plot = plot1, 
  width = 8,
  height = 6, 
  bg = "white"
)

#### PLOT Tot Visits ------

visit_data_reduced_user2 <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = interval(
    index_date, consdate) %/% months(1)) %>%
  group_by(month_after_index) %>%
  summarise(visit_count = n(), .groups = "drop")

(plot2 <- ggplot(
  visit_data_reduced_user2,
  aes(
    x = month_after_index,
    y = visit_count
  )
) +
  geom_bar(
    stat = "identity",
    fill = "#1f77b4"
  ) + # Blue color for the bars
  labs(
    x = "Month After Index",
    y = "Total Visits"
  ) +
  theme_minimal())

ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots", "Tot_visits.PNG"),
  plot = plot2,
  width = 8, 
  height = 6, 
  bg = "white"
)


#### PLOT exposure weighted ------
visit_data_reduced_MIN <- visit_data_reduced %>% select(
  patid, index_date, consdate, user_status,
  specialty_reduced, follow_up_end, exposed_yrs
)

# Step 1: Merge `exposed_yrs` into the user-specific data
visit_data_reduced_user <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = interval(
    index_date, consdate) %/% months(1)) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    exposed_yrs = first(exposed_yrs), # Keep the exposed_yrs for each patient
    .groups = "drop"
  )

# Step 2: Compute the total number of visits per patient per month
total_visits_per_patient_month <- visit_data_reduced_user %>%
  group_by(patid, month_after_index) %>%
  summarise(
    total_visits = sum(visit_count),
    total_exposed_yrs = first(exposed_yrs), # Get exposure years for each patient
    .groups = "drop"
  )

# Step 3: Compute the exposure-weighted mean number of visits per month across all patients
mean_visits_per_month_weighted <- total_visits_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(
    weighted_visits = sum(total_visits) / sum(total_exposed_yrs),
    .groups = "drop"
  )

# Step 4: Compute the total number of visits per specialty per month
visits_per_specialty_month <- visit_data_reduced_user %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(
    total_visits_specialty = sum(visit_count),
    total_exposed_yrs = first(exposed_yrs), # Get exposure years for each patient
    .groups = "drop"
  )

# Step 5: Merge the total exposure-weighted mean visits per month
plots_users_mean_visits_specialty_weighted <- visits_per_specialty_month %>%
  left_join(mean_visits_per_month_weighted, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE))

# Step 6: Plot the exposure-weighted mean visits per month with specialty proportions
(plot3 <- ggplot(
  plots_users_mean_visits_specialty_weighted,
  aes(x = month_after_index, y = weighted_visits * proportion, fill = specialty_reduced)
) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "General Practitioner" = "#1f77b4",
    "Nurse" = "#377eb8",
    "Other" = "#253494"
  )) +
  labs(x = "Month After Index", y = "Exposure-Weighted Visits per User", fill = "Specialty") +
  theme_minimal())

ggsave(here::here("CPRD Raw Analysis", "Results", "Plots", "Mean_visits_per_specialty_unweighted.PNG"),
  plot = plot3, width = 8, height = 6, bg = "white"
)


### Cost ----

visit_data_reduced_MIN_c <- visit_data_reduced %>% 
  select(
  patid, index_date, consdate, user_status,
  specialty_reduced, follow_up_end, unit_cost
)

#### PLOT costs per month/specialty   -------

# Filter for relevant specialties and compute months since index
visit_data_reduced_user_c <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>%
  mutate(month_after_index = interval(
    index_date, consdate) %/% months(1)) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(),
            visit_costs = visit_count * unit_cost,
            .groups = "drop") %>% 
  distinct(patid, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE)

# Step 1: Compute the total number of costs per patient per month 
# (sum across specialties)
total_costs_per_patient_month <- visit_data_reduced_user_c %>%
  group_by(patid, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# Step 2: Compute the mean number of visits per month across all patients
mean_costs_per_month <- total_costs_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_count = sum(total_costs) / n_distinct(patid),
    .groups = "drop"
  )

# Step 3: Compute the total number of visits per specialty per month
costs_per_specialty_month <- visit_data_reduced_user_c %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_costs_specialty = sum(visit_costs), .groups = "drop")

# Step 4: Merge the total mean visits per month
plots_users_mean_costs_specialty <- costs_per_specialty_month %>%
  left_join(mean_costs_per_month, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_costs_specialty / sum(total_costs_specialty, na.rm = TRUE))

# Step 5: Plot the mean visits per month with specialty proportions
(plot_costs1 <- ggplot(
  plots_users_mean_costs_specialty,
  aes(
    x = month_after_index,
    y = mean_visit_count * proportion,
    fill = specialty_reduced
  )
) +
    geom_bar(
      stat = "identity",
      position = "stack"
    ) +
    scale_fill_manual(values = c(
      "General Practitioner" = "#1f77b4",
      "Nurse" = "#377eb8",
      "Other" = "#253494"
    )) +
    labs(
      x = "Month After Index",
      y = "Mean Costs per User",
      fill = "Specialty"
    ) +
    theme_minimal())

ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots", 
  "Mean_costs_per_specialty_unweighted.PNG"),
  plot = plot_costs1, 
  width = 8,
  height = 6, 
  bg = "white"
)

#### PLOT Tot Costs ------

visit_data_reduced_user_c2 <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = interval(
    index_date, consdate) %/% months(1)) %>%
  group_by(month_after_index) %>%
  summarise(visit_count = n(),
             visit_costs = visit_count * unit_cost,
             .groups = "drop") %>% 
  distinct(month_after_index, visit_count, visit_costs, 
           .keep_all = TRUE)


(plot_costs2 <- ggplot(
  visit_data_reduced_user_c2,
  aes(
    x = month_after_index,
    y = visit_costs
  )
) +
    geom_bar(
      stat = "identity",
      fill = "#1f77b4"
    ) + # Blue color for the bars
    labs(
      x = "Month After Index",
      y = "Total Costs"
    ) +
    theme_minimal())

ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots", "Tot_costs.PNG"),
  plot = plot_costs2,
  width = 8, 
  height = 6, 
  bg = "white"
)


#### PLOT exposure weighted ------
visit_data_reduced_MIN_c <- visit_data_reduced %>% select(
  patid, index_date, consdate, user_status,
  specialty_reduced, follow_up_end, exposed_yrs, unit_cost
)

# Step 1: Merge `exposed_yrs` into the user-specific data
visit_data_reduced_user_c <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = interval(
    index_date, consdate) %/% months(1)) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    exposed_yrs = first(exposed_yrs), # Keep the exposed_yrs for each patient
    .groups = "drop"
  ) %>% 
  distinct(patid, month_after_index, specialty_reduced, 
           visit_costs, exposed_yrs, 
           .keep_all = TRUE)

# Step 2: Compute the total number of visits per patient per month
total_costs_per_patient_month <- visit_data_reduced_user_c %>%
  group_by(patid, month_after_index) %>%
  summarise(
    total_costs = sum(visit_costs),
    total_exposed_yrs = first(exposed_yrs), # Get exposure years for each patient
    .groups = "drop"
  )

# Step 3: Compute the exposure-weighted mean number of visits per month across all patients
mean_costs_per_month_weighted <- total_costs_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(
    weighted_costs = sum(total_costs) / sum(total_exposed_yrs),
    .groups = "drop"
  )

# Step 4: Compute the total number of visits per specialty per month
costs_per_specialty_month <- visit_data_reduced_user_c %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(
    total_costs_specialty = sum(visit_costs),
    total_exposed_yrs = first(exposed_yrs), # Get exposure years for each patient
    .groups = "drop"
  )

# Step 5: Merge the total exposure-weighted mean visits per month
plots_users_mean_costs_specialty_weighted <- costs_per_specialty_month %>%
  left_join(mean_costs_per_month_weighted, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_costs_specialty / sum(total_costs_specialty, na.rm = TRUE))

# Step 6: Plot the exposure-weighted mean visits per month with specialty proportions
(plot_costs3 <- ggplot(
  plots_users_mean_costs_specialty_weighted,
  aes(x = month_after_index, y = weighted_costs * proportion, fill = specialty_reduced)
) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c(
      "General Practitioner" = "#1f77b4",
      "Nurse" = "#377eb8",
      "Other" = "#253494"
    )) +
    labs(x = "Month After Index", 
         y = "Exposure-Weighted Costs per User", 
         fill = "Specialty") +
    theme_minimal())

ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots", "Mean_costs_per_specialty_weighted.PNG"),
  plot = plot_costs3, 
  width = 8, 
  height = 6, 
  bg = "white"
)
