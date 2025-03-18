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
# merge back to cohort data
visit_data <- left_join(
  cohort_table_raw, visit_data,
  by = c(
    "patid", "gender", "yob", "death", "death_date",
    "aurum_min_accept_date", "aurum_max_accept_date", "site",
    "fx_date", "days_prior_obs", "days_after_obs", "age_fracture",
    "type", "cancer_fa_index_dt", "paget_fa_index_dt", "index_date",
    "after_index", "fracture_after_index", "follow_up_end",
    "follow_up_reason", "follow_up_time", "exposed_yrs", "exposed_months"
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
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  # mutate(
  #   raw_exposure_interval = interval(index_date, pmin(consdate, follow_up_end)),
  #   month_after_index = interval(index_date, consdate) %/% months(1),
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
user_yearly_visits <- user_visits %>%
  group_by(patid, specialty) %>%
  summarise(visits_count = first(visit_count),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(patid, year_after_index) %>%
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

# Merge yearly and overall exposure stats
user_summary_df <- user_yearly_visits %>%
  left_join(user_total_visits, by = "patid") %>% 
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
  group_by(patid, year_after_index) %>% 
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
  #   raw_exposure_interval = interval(index_date, pmin(consdate, follow_up_end)),
  #   month_after_index = interval(index_date, consdate) %/% months(1),
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
  group_by(patid, specialty) %>%
  summarise(visits_count = first(visit_count),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(patid, year_after_index) %>%
  summarise(
    total_patient_visits = sum(visits_count, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / year_after_index,
    .groups = "drop"
  )

# Then,compute total visits over full exposure period
all_total_visits <- visit_data %>%
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

# Merge yearly and overall exposure stats
all_summary_df <- all_yearly_visits %>%
  left_join(all_total_visits, by = "patid") %>% 
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
  group_by(patid, year_after_index) %>% 
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
#   filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
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
  group_by(patid, specialty) %>%
  summarise(visit_cost = first(visit_cost),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(patid, year_after_index) %>%
  summarise(
    total_patient_visits_cost = sum(visit_cost, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_cost_per_year = total_patient_visits_cost / year_after_index,
    .groups = "drop"
  )

# Then,compute total visits over full exposure period
user_total_costs <- visit_data %>%
  filter(user_status == "User",
         specialty != "Not Clinically Relevant") %>%
  group_by(patid, specialty) %>%
  summarise(
    total_patient_visits_costs = first(visit_cost),
    total_exposed_yrs = first(exposed_yrs),
    visits_costs_per_year = total_patient_visits_costs / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_visits_cost = sum(total_patient_visits_costs, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_cost_per_yr = tot_visits_cost / tot_exposed_yrs,
    .groups = "drop"
  )

# Merge yearly and overall exposure stats
user_costs_summary_df <- user_yearly_costs %>%
  left_join(user_total_costs, by = "patid") %>% 
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
  group_by(patid, year_after_index) %>% 
  summarise(
    max_visits_cost = max(total_patient_visits_cost)
  )


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
  group_by(patid, specialty) %>%
  summarise(visit_cost = first(visit_cost),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(patid, year_after_index) %>%
  summarise(
    total_patient_visits_cost = sum(visit_cost, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_cost_per_year = total_patient_visits_cost / year_after_index,
    .groups = "drop"
  )

# Then,compute total visits over full exposure period
all_total_costs <- visit_data %>%
  group_by(patid, specialty) %>%
  summarise(
    total_patient_visits_costs = first(visit_cost),
    total_exposed_yrs = first(exposed_yrs),
    visits_costs_per_year = total_patient_visits_costs / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_visits_cost = sum(total_patient_visits_costs, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_cost_per_yr = tot_visits_cost / tot_exposed_yrs,
    .groups = "drop"
  )

# Merge yearly and overall exposure stats
all_costs_summary_df <- all_yearly_costs %>%
  left_join(all_total_costs, by = "patid") %>% 
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
  group_by(patid, year_after_index) %>% 
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
# write_xlsx(costs_list, here::here("CPRD Raw Analysis", "Results", "Output_costs_Raw.xlsx"))

## Cohort summary ----
cohort_summary_raw <- cohort_summary(visit_data)

# Write to an Excel file
# rio::export(cohort_summary_raw, here::here("CPRD Raw Analysis", "Results", "cohort_summary_raw.xlsx"))



