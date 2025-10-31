# **************************************************************************** #
# 	Project name: Validation Study
# 	Author: Njoki Njuki, Gianluca Fabiano
#   Do file name: Primary care analysis
#   Date: 01.10.2025
#   Last date edited: 01.10.2025
# **************************************************************************** #

## This script derives yearly summary statistics for both healthcare resource use (HCRU) and costs.
# It produces estimates for two populations:
#   - All patients in the cohort
#   - Users only (i.e., patients with at least one clinically relevant encounter)

# For each population, two types of encounters are analysed:
#   (a) All encounters (regardless of provider or relevance)
#   (b) Clinically relevant encounters only

# The analysis includes partial years, i.e., years in which patients were exposed for just part of the time

# For each of the four resulting groups (population × encounter type), we compute:
#   - Observed yearly statistics: mean, SD, quartiles, etc.
#   - Bootstrapped estimates of the mean and confidence intervals (using 1000 resamples)

# Yearly cost summaries follow the same logic and structure as the encounter summaries.

# Libraries
library(dplyr)
library(tidyr)
library(lubridate)

# --- Visits ------

# Ran from the local machine
# Start by running 05-CreatingCosts_OP_omop.R
# Then from the server, save cohort_table dataset and appt_data

cohort_table <- import("H:/Projects/Validation/Data/cohort_data_omop.csv")

appt_data <- import("H:/Projects/Validation/Data/appt_data_omop.csv") |> 
  arrange(subject_id)
op_costs_final_data_recoded <- op_costs_final_data_recoded |> 
  arrange(subject_id)
op_costs_final_data_recoded_min <- op_costs_final_data_recoded |> 
  select(unit_cost)

appt_data <- cbind(appt_data, op_costs_final_data_recoded_min)

## All patients ----

# --- Preparation ---
n_all_pts <- n_distinct(cohort_table$subject_id)

# Create exposure matrix for years (partial years are included) ---
cohort_grid_omop_all_years <- cohort_table %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    total_years = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 365.25)
  ) %>%
  rowwise() %>%
  mutate(year_vector = list(0:1)) %>%  # Always up to 2 years
  unnest(year_vector) %>%
  mutate(
    year_col = paste0("year_", year_vector + 1),
    value = ifelse(year_vector < total_years, 0, NA_real_)  # mark unexposed years as NA
  ) %>%
  select(subject_id, year_col, value) %>%
  pivot_wider(names_from = year_col, values_from = value)

# Compute max exposed year per patient ---
cohort_years_all_years <- cohort_table %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_year_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 365.25)
  ) %>%
  select(subject_id, max_year_exposed)
# --- a) All patients & all encounters ------

# --- Step 1: Summarise encounters per year ---
yearly_encounters_summary <- appt_data %>%
  mutate(
    index_date = ymd(index_date),
    visit_detail_start_date = ymd(visit_detail_start_date),
    year_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25) + 1
  ) %>%
  left_join(cohort_years_all_years, by = "subject_id") %>%
  filter(year_after_index <= max_year_exposed) %>%  # keep only exposed years
  group_by(subject_id, year_after_index) %>%
  summarise(total_encounters = n(), .groups = "drop") %>%
  mutate(year_col = paste0("year_", year_after_index)) %>%
  select(subject_id, year_col, total_encounters)

# --- Step 2: Reshape to wide format and merge ---
observed_visit_matrix_year <- yearly_encounters_summary %>%
  pivot_wider(names_from = year_col, values_from = total_encounters)

year_cols <- grep("^year_", names(cohort_grid_omop_all_years), value = TRUE)

merged_matrix_encounters_year <- cohort_grid_omop_all_years %>%
  left_join(observed_visit_matrix_year, by = "subject_id", suffix = c("_exp", "_visit"))

# --- Step 3: Fill in values where exposed but no visits ---
for (col in year_cols) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  
  merged_matrix_encounters_year[[col]] <- dplyr::case_when(
    is.na(merged_matrix_encounters_year[[exp_col]]) ~ NA_real_,  # not exposed
    !is.na(merged_matrix_encounters_year[[visit_col]]) ~ merged_matrix_encounters_year[[visit_col]],
    TRUE ~ 0  # exposed but no visits
  )
}

# --- Step 4: Reshape to long format ---
final_matrix_encounters_year <- merged_matrix_encounters_year %>%
  select(subject_id, all_of(year_cols))

final_long_df_encounters_all_patients_all_encounters_all_years <- final_matrix_encounters_year %>%
  pivot_longer(
    cols = starts_with("year_"),
    names_to = "year",
    values_to = "tot_encounters"
  ) %>%
  mutate(year_after_index = as.integer(gsub("year_", "", year)))

# --- Step 5: Summarise by year ---
observed_visit_summary_all_patients_all_encounters_all_years <- final_long_df_encounters_all_patients_all_encounters_all_years %>%
  group_by(year_after_index) %>%
  summarise(
    mean_exposed       = mean(tot_encounters, na.rm = TRUE),
    min                = min(tot_encounters, na.rm = TRUE),
    max                = max(tot_encounters, na.rm = TRUE),
    sd                 = sd(tot_encounters, na.rm = TRUE),
    q1                 = quantile(tot_encounters, 0.25, na.rm = TRUE),
    median             = median(tot_encounters, na.rm = TRUE),
    q3                 = quantile(tot_encounters, 0.75, na.rm = TRUE),
    n_exposed          = sum(!is.na(tot_encounters)),
    tot_encounters     = sum(tot_encounters, na.rm = TRUE),
    mean_all_patients  = tot_encounters / n_all_pts,
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd / sqrt(n_exposed),
    lower = mean_exposed - qt(0.975, df = n_exposed - 1) * se,
    upper = mean_exposed + qt(0.975, df = n_exposed - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))


## User-only  ----
# Preparation

# Extract user cohort
cohort_table_users <- appt_data %>%
  filter(user_status == "User") %>% distinct(subject_id, index_date, follow_up_end)

(n_users <- n_distinct(cohort_table_users$subject_id))

# Create exposure matrix (including partial years)
cohort_grid_omop_all_years_users <- cohort_table_users %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    total_years = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 365.25)
  ) %>%
  rowwise() %>%
  mutate(year_vector = list(0:1)) %>%
  unnest(year_vector) %>%
  mutate(
    year_col = paste0("year_", year_vector + 1),
    value = ifelse(year_vector < total_years, 0, NA_real_)
  ) %>%
  select(subject_id, year_col, value) %>%
  pivot_wider(names_from = year_col, values_from = value)

# Max year exposed
cohort_years_all_years_users <- cohort_table_users %>%
  mutate(
    max_year_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 365.25)
  ) %>%
  select(subject_id, max_year_exposed)


# --- a) All users & all encounters ----

# Step 1: Summarise encounters per year
yearly_encounters_summary_users <- appt_data %>%
  filter(user_status == "User") %>%
  mutate(
    year_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25) + 1
  ) %>%
  left_join(cohort_years_all_years_users, by = "subject_id") %>%
  filter(year_after_index <= max_year_exposed) %>%
  group_by(subject_id, year_after_index) %>%
  summarise(total_encounters = n(), .groups = "drop") %>%
  mutate(year_col = paste0("year_", year_after_index)) %>%
  select(subject_id, year_col, total_encounters)

# Step 2: Reshape and merge
observed_visit_matrix_year_users <- yearly_encounters_summary_users %>%
  pivot_wider(names_from = year_col, values_from = total_encounters)

year_cols <- grep("^year_", names(cohort_grid_omop_all_years_users), value = TRUE)

merged_matrix_encounters_year_users <- cohort_grid_omop_all_years_users %>%
  left_join(observed_visit_matrix_year_users, by = "subject_id", suffix = c("_exp", "_visit"))

# Step 3: Fill in visit counts
for (col in year_cols) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  merged_matrix_encounters_year_users[[col]] <- dplyr::case_when(
    is.na(merged_matrix_encounters_year_users[[exp_col]]) ~ NA_real_,
    !is.na(merged_matrix_encounters_year_users[[visit_col]]) ~ merged_matrix_encounters_year_users[[visit_col]],
    TRUE ~ 0
  )
}

# Step 4: Reshape to long
final_matrix_encounters_year_users <- merged_matrix_encounters_year_users %>%
  select(subject_id, all_of(year_cols))

final_long_df_encounters_users_all_encounters_all_years <- final_matrix_encounters_year_users %>%
  pivot_longer(
    cols = starts_with("year_"),
    names_to = "year",
    values_to = "tot_encounters"
  ) %>%
  mutate(year_after_index = as.integer(gsub("year_", "", year)))

# Step 5: Summarise
observed_visit_summary_users_all_encounters_all_years <- final_long_df_encounters_users_all_encounters_all_years %>%
  group_by(year_after_index) %>%
  summarise(
    mean_exposed       = mean(tot_encounters, na.rm = TRUE),
    min                = min(tot_encounters, na.rm = TRUE),
    max                = max(tot_encounters, na.rm = TRUE),
    sd                 = sd(tot_encounters, na.rm = TRUE),
    q1                 = quantile(tot_encounters, 0.25, na.rm = TRUE),
    median             = median(tot_encounters, na.rm = TRUE),
    q3                 = quantile(tot_encounters, 0.75, na.rm = TRUE),
    n_exposed          = sum(!is.na(tot_encounters)),
    tot_encounters     = sum(tot_encounters, na.rm = TRUE),
    mean_all_patients  = tot_encounters / n_users,
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd / sqrt(n_exposed),
    lower = mean_exposed - qt(0.975, df = n_exposed - 1) * se,
    upper = mean_exposed + qt(0.975, df = n_exposed - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))

# # --- b) All users & relevant encounters -----
combined_summary_encounters_yearly <- bind_rows(
  
  observed_visit_summary_all_patients_all_encounters_all_years %>%
    mutate(group = "All Patients & all encounters"),
  
  # observed_visit_summary_all_patients_relev_encounters_all_years %>%
  #   mutate(group = "All Patients & relevant encounters"),
  
  observed_visit_summary_users_all_encounters_all_years %>%
    mutate(group = "Users & all encounters"),
  
  # observed_visit_summary_users_relev_encounters_all_years %>%
  #   mutate(group = "Users & relevant encounters")
) %>%
  select(group, everything())

combined_summary_encounters_yearly

## Bootstrap -----

bootstrap_yearly_summary <- function(data, year_var = "year_after_index", value_var = "tot_encounters", R = 1000, seed = 626) {
  
  unique_months <- sort(unique(data[[year_var]]))
  
  results <- lapply(unique_months, function(m) {
    data_m <- data %>% filter(!!sym(year_var) == m)
    
    stat_fn <- function(d, i) {
      d_sample <- d[i, ]
      mean(d_sample[[value_var]], na.rm = TRUE)
    }
    
    set.seed(seed)
    boot_res <- boot(data = data_m, statistic = stat_fn, R = R)
    boot_vals <- as.numeric(boot_res$t)
    
    ci <- boot.ci(boot_res, type = c("norm", "basic", "perc"))
    
    tibble(
      year            = m,
      mean             = round(mean(boot_vals), 3),
      variance         = round(var(boot_vals), 3),
      sd               = round(sd(boot_vals), 3),
      se               = round(sd(boot_vals) / sqrt(length(boot_vals)), 3),
      ci_lower_norm    = round(ci$normal[2], 3),
      ci_upper_norm    = round(ci$normal[3], 3),
      ci_lower_basic   = round(ci$basic[4], 3),
      ci_upper_basic   = round(ci$basic[5], 3),
      ci_lower_perc    = round(ci$perc[4], 3),
      ci_upper_perc    = round(ci$perc[5], 3)
    )
  })
  
  bind_rows(results)
}


## Bootstrap on years 
yearly_bootstrap_all_patients_all_encounters_all_years <- bootstrap_yearly_summary(final_long_df_encounters_all_patients_all_encounters_all_years)
# yearly_bootstrap_all_patients_relev_encounters_all_years <- bootstrap_yearly_summary(final_long_df_encounters_all_patients_relev_encounters_all_years)
yearly_bootstrap_users_all_encounters_all_years <- bootstrap_yearly_summary(final_long_df_encounters_users_all_encounters_all_years)
# yearly_bootstrap_users_relev_encounters_all_years <- bootstrap_yearly_summary(final_long_df_encounters_users_relev_encounters_all_years)

# Combine bootstrap summaries
combined_encounters_bootstrap_summary_yearly <- bind_rows(
  yearly_bootstrap_all_patients_all_encounters_all_years %>%
    mutate(group = "All Patients & all encounters"),
  
  # yearly_bootstrap_all_patients_relev_encounters_all_years %>%
  #   mutate(group = "All Patients & relevant encounters"),
  
  yearly_bootstrap_users_all_encounters_all_years %>%
    mutate(group = "Users & all encounters"),
  
  # yearly_bootstrap_users_relev_encounters_all_years %>%
  #   mutate(group = "Users & relevant encounters")
) %>%
  select(group, everything())


# --- Costs ------
# Ran from the local machine
# Start by running 05-CreatingCosts_OP_omop.R
# Then from the server, save cohort_table dataset and appt_data

cohort_table <- import("H:/Projects/Validation/Data/cohort_data_omop.csv")

appt_data <- import("H:/Projects/Validation/Data/appt_data_omop.csv") |> 
  arrange(subject_id)
op_costs_final_data_recoded <- op_costs_final_data_recoded |> 
  arrange(subject_id)
op_costs_final_data_recoded_min <- op_costs_final_data_recoded |> 
  select(unit_cost)

appt_data <- cbind(appt_data, op_costs_final_data_recoded_min)


# Compute max exposed year per patient ---
cohort_years_all_years <- cohort_table %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_year_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 365.25)
  ) %>%
  select(subject_id, max_year_exposed)


# --- a) All patients & all encounters ------

# --- Step 1: Summarise encounters per year ---
yearly_costs_summary <- appt_data %>%
  mutate(
    index_date = ymd(index_date),
    visit_detail_start_date = ymd(visit_detail_start_date),
    year_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25) + 1
  ) %>%
  left_join(cohort_years_all_years, by = "subject_id") %>%
  filter(year_after_index <= max_year_exposed) %>%  # keep only exposed years
  group_by(subject_id, year_after_index) %>%
  summarise(total_costs = sum(unit_cost, na.rm = TRUE)) %>%
  mutate(year_col = paste0("year_", year_after_index)) %>%
  select(subject_id, year_col, total_costs)

# --- Step 2: Reshape to wide format and merge ---
observed_costs_matrix_year <- yearly_costs_summary %>%
  pivot_wider(names_from = year_col, values_from = total_costs)

year_cols <- grep("^year_", names(cohort_grid_omop_all_years), value = TRUE)

merged_matrix_costs_year <- cohort_grid_omop_all_years %>%
  left_join(observed_costs_matrix_year, by = "subject_id", suffix = c("_exp", "_visit"))

# --- Step 3: Fill in values where exposed but no visits ---
for (col in year_cols) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  
  merged_matrix_costs_year[[col]] <- dplyr::case_when(
    is.na(merged_matrix_costs_year[[exp_col]]) ~ NA_real_,  # not exposed
    !is.na(merged_matrix_costs_year[[visit_col]]) ~ merged_matrix_costs_year[[visit_col]],
    TRUE ~ 0  # exposed but no visits
  )
}

# --- Step 4: Reshape to long format ---
final_matrix_encounters_year <- merged_matrix_costs_year %>%
  select(subject_id, all_of(year_cols))

final_long_df_costs_all_patients_all_encounters_all_years <- final_matrix_encounters_year %>%
  pivot_longer(
    cols = starts_with("year_"),
    names_to = "year",
    values_to = "total_costs"
  ) %>%
  mutate(year_after_index = as.integer(gsub("year_", "", year)))

# --- Step 5: Summarise by year ---
observed_cost_summary_all_patients_all_encounters_all_years <- final_long_df_costs_all_patients_all_encounters_all_years %>%
  group_by(year_after_index) %>%
  summarise(
    mean_exposed       = mean(total_costs, na.rm = TRUE),
    min                = min(total_costs, na.rm = TRUE),
    max                = max(total_costs, na.rm = TRUE),
    sd                 = sd(total_costs, na.rm = TRUE),
    q1                 = quantile(total_costs, 0.25, na.rm = TRUE),
    median             = median(total_costs, na.rm = TRUE),
    q3                 = quantile(total_costs, 0.75, na.rm = TRUE),
    n_exposed          = sum(!is.na(total_costs)),
    total_costs        = sum(total_costs, na.rm = TRUE),
    mean_all_patients  = total_costs / n_all_pts,
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd / sqrt(n_exposed),
    lower = mean_exposed - qt(0.975, df = n_exposed - 1) * se,
    upper = mean_exposed + qt(0.975, df = n_exposed - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))

# Extract user cohort
cohort_table_users <- appt_data %>%
  filter(user_status == "User") %>% distinct(subject_id, index_date, follow_up_end)

n_users <- n_distinct(cohort_table_users$subject_id)

# Create exposure matrix (including partial years)
cohort_grid_omop_all_years_users <- cohort_table_users %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    total_years = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 365.25)
  ) %>%
  rowwise() %>%
  mutate(year_vector = list(0:1)) %>%
  unnest(year_vector) %>%
  mutate(
    year_col = paste0("year_", year_vector + 1),
    value = ifelse(year_vector < total_years, 0, NA_real_)
  ) %>%
  select(subject_id, year_col, value) %>%
  pivot_wider(names_from = year_col, values_from = value)

# Max year exposed
cohort_years_all_years_users <- cohort_table_users %>%
  mutate(
    max_year_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 365.25)
  ) %>%
  select(subject_id, max_year_exposed)

# --- a) All users & all encounters ------

# --- Step 1: Summarise encounters per year ---
yearly_costs_summary <- appt_data %>%
  filter(user_status == "User") %>%
  mutate(
    index_date = ymd(index_date),
    visit_detail_start_date = ymd(visit_detail_start_date),
    year_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25) + 1
  ) %>%
  left_join(cohort_years_all_years, by = "subject_id") %>%
  filter(year_after_index <= max_year_exposed) %>%  # keep only exposed years
  group_by(subject_id, year_after_index) %>%
  summarise(total_costs = sum(unit_cost, na.rm = TRUE)) %>%
  mutate(year_col = paste0("year_", year_after_index)) %>%
  select(subject_id, year_col, total_costs)

# --- Step 2: Reshape to wide format and merge ---
observed_costs_matrix_year <- yearly_costs_summary %>%
  pivot_wider(names_from = year_col, values_from = total_costs)

year_cols <- grep("^year_", names(cohort_grid_omop_all_years_users), value = TRUE)

merged_matrix_costs_year <- cohort_grid_omop_all_years_users %>%
  left_join(observed_costs_matrix_year, by = "subject_id", suffix = c("_exp", "_visit"))

# --- Step 3: Fill in values where exposed but no visits ---
for (col in year_cols) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  
  merged_matrix_costs_year[[col]] <- dplyr::case_when(
    is.na(merged_matrix_costs_year[[exp_col]]) ~ NA_real_,  # not exposed
    !is.na(merged_matrix_costs_year[[visit_col]]) ~ merged_matrix_costs_year[[visit_col]],
    TRUE ~ 0  # exposed but no visits
  )
}

# --- Step 4: Reshape to long format ---
final_matrix_encounters_year <- merged_matrix_costs_year %>%
  select(subject_id, all_of(year_cols))

final_long_df_costs_users_all_encounters_all_years <- final_matrix_encounters_year %>%
  pivot_longer(
    cols = starts_with("year_"),
    names_to = "year",
    values_to = "total_costs"
  ) %>%
  mutate(year_after_index = as.integer(gsub("year_", "", year)))

# --- Step 5: Summarise by year ---
observed_cost_summary_users_all_encounters_all_years <- final_long_df_costs_users_all_encounters_all_years %>%
  group_by(year_after_index) %>%
  summarise(
    mean_exposed       = mean(total_costs, na.rm = TRUE),
    min                = min(total_costs, na.rm = TRUE),
    max                = max(total_costs, na.rm = TRUE),
    sd                 = sd(total_costs, na.rm = TRUE),
    q1                 = quantile(total_costs, 0.25, na.rm = TRUE),
    median             = median(total_costs, na.rm = TRUE),
    q3                 = quantile(total_costs, 0.75, na.rm = TRUE),
    n_exposed          = sum(!is.na(total_costs)),
    total_costs        = sum(total_costs, na.rm = TRUE),
    mean_all_patients  = total_costs / n_users,
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd / sqrt(n_exposed),
    lower = mean_exposed - qt(0.975, df = n_exposed - 1) * se,
    upper = mean_exposed + qt(0.975, df = n_exposed - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))

# # --- b) All users & relevant encounters -----
# # --- Output: Combine All Summaries 

combined_summary_costs_yearly <- bind_rows(
  
  observed_cost_summary_all_patients_all_encounters_all_years %>%
    mutate(group = "All Patients & all encounters"),
  
  # observed_cost_summary_all_patients_relev_encounters_all_years %>%
  #   mutate(group = "All Patients & relevant encounters"),
  
  observed_cost_summary_users_all_encounters_all_years %>%
    mutate(group = "Users & all encounters"),
  
  # observed_cost_summary_users_relev_encounters_all_years %>%
  #   mutate(group = "Users & relevant encounters")
) %>%
  select(group, everything())

combined_summary_costs_yearly

## Bootstrap -----

bootstrap_yearly_summary_costs <- function(data, year_var = "year_after_index", value_var = "total_costs", R = 1000, seed = 626) {
  
  unique_months <- sort(unique(data[[year_var]]))
  
  results <- lapply(unique_months, function(m) {
    data_m <- data %>% filter(!!sym(year_var) == m)
    
    stat_fn <- function(d, i) {
      d_sample <- d[i, ]
      mean(d_sample[[value_var]], na.rm = TRUE)
    }
    
    set.seed(seed)
    boot_res <- boot(data = data_m, statistic = stat_fn, R = R)
    boot_vals <- as.numeric(boot_res$t)
    
    ci <- boot.ci(boot_res, type = c("norm", "basic", "perc"))
    
    tibble(
      year            = m,
      mean             = round(mean(boot_vals), 3),
      variance         = round(var(boot_vals), 3),
      sd               = round(sd(boot_vals), 3),
      se               = round(sd(boot_vals) / sqrt(length(boot_vals)), 3),
      ci_lower_norm    = round(ci$normal[2], 3),
      ci_upper_norm    = round(ci$normal[3], 3),
      ci_lower_basic   = round(ci$basic[4], 3),
      ci_upper_basic   = round(ci$basic[5], 3),
      ci_lower_perc    = round(ci$perc[4], 3),
      ci_upper_perc    = round(ci$perc[5], 3)
    )
  })
  
  bind_rows(results)
}


## Bootstrap on years 

yearly_bootstrap_costs_all_patients_all_encounters_all_years <- bootstrap_yearly_summary_costs(final_long_df_costs_all_patients_all_encounters_all_years)
# yearly_bootstrap_costs_all_patients_relev_encounters_all_years <- bootstrap_yearly_summary_costs(final_long_df_costs_all_patients_relev_encounters_all_years)
yearly_bootstrap_costs_users_all_encounters_all_years <- bootstrap_yearly_summary_costs(final_long_df_costs_users_all_encounters_all_years)
# yearly_bootstrap_costs_users_relev_encounters_all_years <- bootstrap_yearly_summary_costs(final_long_df_costs_users_relev_encounters_all_years)

# Combine bootstrap summaries
combined_costs_bootstrap_summary_yearly <- bind_rows(
  yearly_bootstrap_costs_all_patients_all_encounters_all_years %>%
    mutate(group = "All Patients & all encounters"),
  
  # yearly_bootstrap_costs_all_patients_relev_encounters_all_years %>%
  #   mutate(group = "All Patients & relevant encounters"),
  
  yearly_bootstrap_costs_users_all_encounters_all_years %>%
    mutate(group = "Users & all encounters"),
  
  # yearly_bootstrap_costs_users_relev_encounters_all_years %>%
  #   mutate(group = "Users & relevant encounters")
) %>%
  select(group, everything())

# Export ----

write_xlsx(combined_summary_encounters_yearly, 
           here::here("OMOP Analysis", "Results", "SecondaryCareAnalysis", 
                      "OP", "Yearly", 
                      "yearly_combined_summary_encounters_SOURCE.xlsx"))

write_xlsx(combined_encounters_bootstrap_summary_yearly,
           here::here("OMOP Analysis", "Results", "SecondaryCareAnalysis", 
                      "OP", "Yearly", 
                      "yearly_combined_bootstrap_summary_encounters_SOURCE.xlsx"))

write_xlsx(combined_summary_costs_yearly, 
           here::here("OMOP Analysis", "Results", "SecondaryCareAnalysis",
                      "OP", "Yearly", 
                      "yearly_combined_summary_costs_SOURCE.xlsx"))

write_xlsx(combined_costs_bootstrap_summary_yearly, 
           here::here("OMOP Analysis", "Results", "SecondaryCareAnalysis", 
                      "OP", "Yearly",
                      "yearly_combined_bootstrap_summary_costs_SOURCE.xlsx"))
