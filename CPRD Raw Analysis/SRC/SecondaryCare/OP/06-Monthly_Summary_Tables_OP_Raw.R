# **************************************************************************** #
# 	Project name: Validation Study
# 	Author: Njoki Njuki, Gianluca Fabiano
#   Do file name: Secondary Analysis - OP: Monthly tables
#   Date: 24.09.2025
#   Last date edited: 24.09.2025
# **************************************************************************** #

## This script derives monthly summary statistics for both healthcare resource use (HCRU) and costs.
# It produces estimates for two populations:
#   - All patients in the cohort
#   - Users only (i.e., patients with at least one clinically relevant encounter)

# For each population, two types of encounters are analysed:
#   (a) All encounters (regardless of provider or relevance)
#   (b) Clinically relevant encounters only

# The analysis is repeated under two time-based definitions:
#   - Full months only: calendar months where the patient was fully exposed (i.e., final partial month excluded)
#   - All months: includes all calendar months, even if exposure was partial

# For each of the four resulting groups (population × encounter type), we compute:
#   - Observed monthly statistics: mean, SD, quartiles, etc.
#   - Bootstrapped estimates of the mean and confidence intervals (using 1000 resamples)

# Note: Bootstrap is currently applied only to the 'all months' definitions but is ready to be run for full-month definitions if needed.

# Load Packages
library(tidyverse)
library(rio)
library(boot)

# Ran from the local machine
# Start by running 05-CreatingCosts_OP_Raw.R
# Then from the server, save cohort_table dataset and table_one_op_raw_appt_recoded

cohort_table_raw <- import("H:/Projects/Validation/Data/cohort_data_raw.csv")

appt_data_raw <- import("H:/Projects/Validation/Data/appt_data_raw.csv") |> 
  arrange(patid)

### HCRU -----
## All patients & full months ----
# Preparation 
n_all_pts_raw <- n_distinct(cohort_table_raw$patid)

# Create 24-month exposure matrix
cohort_grid_raw <- cohort_table_raw %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)  # NA for unexposed months
  ) %>%
  select(patid, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# Compute calendar months with full exposure (i.e., dropping the final month if the patient was not exposed in full)
cohort_months_raw <- cohort_table_raw %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = interval(index_date, follow_up_end) %/% months(1)
  ) %>%
  select(patid, max_month_exposed)

# --- a) All patients & all encounters -----

# Filter encounters to full months only
monthly_encounters_summary_raw <- appt_data_raw %>%
  mutate(
    index_date = ymd(index_date),
    apptdate = ymd(apptdate),
    month_after_index = trunc(as.numeric(difftime(apptdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_raw, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>%  # only full months
  group_by(patid, month_after_index) %>%
  summarise(total_encounters = n(), .groups = "drop") %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  select(patid, month_col, total_encounters)

# Reshape to wide format: encounters per patient-month
observed_visit_matrix_raw <- monthly_encounters_summary_raw %>%
  pivot_wider(names_from = month_col, values_from = total_encounters)

# Merge visit counts with exposure base
month_cols_raw <- grep("^month_", names(cohort_grid_raw), value = TRUE)

merged_matrix_encounters_raw <- cohort_grid_raw %>%
  left_join(observed_visit_matrix_raw, by = "patid", suffix = c("_exp", "_visit"))

# Overwrite exposed months with visit counts
for (col in month_cols_raw) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  
  merged_matrix_encounters_raw[[col]] <- dplyr::case_when(
    is.na(merged_matrix_encounters_raw[[exp_col]]) ~ NA_real_,  # not exposed
    !is.na(merged_matrix_encounters_raw[[visit_col]]) ~ merged_matrix_encounters_raw[[visit_col]],
    TRUE ~ 0  # exposed but no encounters
  )
}

final_matrix_encounters_raw <- merged_matrix_encounters_raw %>%
  select(patid, all_of(month_cols_raw))

final_long_df_encounters_all_patients_all_encounters_raw <- final_matrix_encounters_raw %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "tot_encounters"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))|> 
  mutate(tot_encounters = if_else(is.na(tot_encounters), 0, tot_encounters))

# Summarise monthly visit statistics
observed_visit_summary_all_patients_all_encounters_raw <- final_long_df_encounters_all_patients_all_encounters_raw %>%
  group_by(month_after_index) %>%
  summarise(
    mean_encounters_exposed = mean(tot_encounters, na.rm = TRUE), # mean for patients exposed in each month (i.e. NAs for non-exposed months)
    min_encounters          = min(tot_encounters, na.rm = TRUE),
    max_encounters          = max(tot_encounters, na.rm = TRUE),
    sd_encounters           = sd(tot_encounters, na.rm = TRUE),
    q1_encounters           = quantile(tot_encounters, 0.25, na.rm = TRUE),
    median_encounters       = median(tot_encounters, na.rm = TRUE),
    q3_encounters           = quantile(tot_encounters, 0.75, na.rm = TRUE),
    n_exposed               = sum(!is.na(tot_encounters)),
    tot_encounters          = sum(tot_encounters, na.rm = TRUE),
    mean_encounters_all_patients = tot_encounters / n_all_pts_raw,  # mean for all patients, regardless of exposure (i.e. zeros for non-exposed months) - sum monthly mean = overall mean
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd_encounters / sqrt(n_exposed),
    lower = mean_encounters_exposed - qt(0.975, df = n_exposed - 1) * se,
    upper = mean_encounters_exposed + qt(0.975, df = n_exposed - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))


## User-only & full months ----

# Preparation
cohort_table_users_raw <- appt_data_raw %>% filter(user_status== "User") %>% 
  distinct(patid, index_date, site, follow_up_end, follow_up_reason, follow_up_time)

(n_users_raw <- n_distinct(cohort_table_users_raw$patid))

# Create 24-month exposure matrix
cohort_grid_raw <- cohort_table_users_raw %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)  # NA for unexposed months
  ) %>%
  select(patid, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# Compute calendar months with full exposure (i.e., dropping the final month if the patient was not exposed in full)
cohort_months_raw <- cohort_table_users_raw %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = interval(index_date, follow_up_end) %/% months(1)
  ) %>%
  select(patid, max_month_exposed)


# --- a) All users & all encounters ----

# Filter encounters to full months only
monthly_encounters_summary_raw <- appt_data_raw %>%
  mutate(
    index_date = ymd(index_date),
    apptdate = ymd(apptdate),
    month_after_index = trunc(as.numeric(difftime(apptdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_raw, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>%  # only full months
  group_by(patid, month_after_index) %>%
  summarise(total_encounters = n(), .groups = "drop") %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  select(patid, month_col, total_encounters)

# Reshape to wide format: encounters per patient-month
observed_visit_matrix_raw <- monthly_encounters_summary_raw %>%
  pivot_wider(names_from = month_col, values_from = total_encounters)

# Merge visit counts with exposure base
month_cols_raw <- grep("^month_", names(cohort_grid_raw), value = TRUE)

merged_matrix_encounters_raw <- cohort_grid_raw %>%
  left_join(observed_visit_matrix_raw, by = "patid", suffix = c("_exp", "_visit"))

# Overwrite exposed months with visit counts
for (col in month_cols_raw) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  
  merged_matrix_encounters_raw[[col]] <- dplyr::case_when(
    is.na(merged_matrix_encounters_raw[[exp_col]]) ~ NA_real_,  # not exposed
    !is.na(merged_matrix_encounters_raw[[visit_col]]) ~ merged_matrix_encounters_raw[[visit_col]],
    TRUE ~ 0  # exposed but no encounters
  )
}

final_matrix_encounters_raw <- merged_matrix_encounters_raw %>%
  select(patid, all_of(month_cols_raw))

final_long_df_encounters_users_all_encounters_raw <- final_matrix_encounters_raw %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "tot_encounters"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month))) |> 
  mutate(tot_encounters = if_else(is.na(tot_encounters), 0, tot_encounters))

# Summarise monthly visit statistics

observed_visit_summary_users_all_encounters_raw <- final_long_df_encounters_users_all_encounters_raw %>%
  group_by(month_after_index) %>%
  summarise(
    mean_encounters_exposed = mean(tot_encounters, na.rm = TRUE), # mean for users exposed in each month (i.e. NAs for non-exposed months)
    min_encounters          = min(tot_encounters, na.rm = TRUE),
    max_encounters          = max(tot_encounters, na.rm = TRUE),
    sd_encounters           = sd(tot_encounters, na.rm = TRUE),
    q1_encounters           = quantile(tot_encounters, 0.25, na.rm = TRUE),
    median_encounters       = median(tot_encounters, na.rm = TRUE),
    q3_encounters           = quantile(tot_encounters, 0.75, na.rm = TRUE),
    n_exposed           = sum(!is.na(tot_encounters)),
    tot_encounters          = sum(tot_encounters, na.rm = TRUE),
    mean_encounters_all_patients = tot_encounters / n_users_raw,  # mean for all users, regardless of exposure (i.e. zeros for non-exposed months) - sum monthly mean = overall mean
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd_encounters / sqrt(n_exposed),
    lower = mean_encounters_exposed - qt(0.975, df = n_exposed - 1) * se,
    upper = mean_encounters_exposed + qt(0.975, df = n_exposed - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))

## All patients & all months (i.e., including partial months) ----
# Preparation 
n_all_pts_raw <- n_distinct(cohort_table_raw$patid)

# Create 24-month exposure matrix 
cohort_grid_raw_all_months <- cohort_table_raw %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    # Calculate total months including partial ones
    total_months = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%  # Always up to 24 months
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < total_months, 0, NA_real_)  # Include partial months
  ) %>%
  select(patid, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)


# Compute calendar months including partial ones
cohort_months_all_months_raw <- cohort_table_raw %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  select(patid, max_month_exposed)


# --- a) All patients & all encounters -----

# Filter encounters

monthly_encounters_summary_raw <- appt_data_raw %>%
  mutate(
    index_date = ymd(index_date),
    apptdate = ymd(apptdate),
    month_after_index = trunc(as.numeric(difftime(apptdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_all_months_raw, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>% 
  group_by(patid, month_after_index) %>%
  summarise(total_encounters = n(), .groups = "drop") %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  select(patid, month_col, total_encounters)

# Reshape to wide format: encounters per patient-month

observed_visit_matrix_raw <- monthly_encounters_summary_raw %>%
  pivot_wider(names_from = month_col, values_from = total_encounters)

# Merge visit counts with exposure base

month_cols_raw <- grep("^month_", names(cohort_grid_raw_all_months), value = TRUE)

merged_matrix_encounters_raw <- cohort_grid_raw_all_months %>%
  left_join(observed_visit_matrix_raw, by = "patid", suffix = c("_exp", "_visit"))

# Overwrite exposed months with visit counts
for (col in month_cols_raw) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  
  merged_matrix_encounters_raw[[col]] <- dplyr::case_when(
    is.na(merged_matrix_encounters_raw[[exp_col]]) ~ NA_real_,  # not exposed
    !is.na(merged_matrix_encounters_raw[[visit_col]]) ~ merged_matrix_encounters_raw[[visit_col]],
    TRUE ~ 0  # exposed but no encounters
  )
}

final_matrix_encounters_raw <- merged_matrix_encounters_raw %>%
  select(patid, all_of(month_cols_raw))

final_long_df_encounters_all_patients_all_encounters_raw <- final_matrix_encounters_raw %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "tot_encounters"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))|> 
  mutate(tot_encounters = if_else(is.na(tot_encounters), 0, tot_encounters))

# Summarise monthly visit statistics

observed_visit_summary_all_patients_all_encounters_all_months_raw <- final_long_df_encounters_all_patients_all_encounters_raw %>%
  group_by(month_after_index) %>%
  summarise(
    mean_encounters_exposed = mean(tot_encounters, na.rm = TRUE), # mean for patients exposed in each month (i.e. NAs for non-exposed months)
    min_encounters          = min(tot_encounters, na.rm = TRUE),
    max_encounters          = max(tot_encounters, na.rm = TRUE),
    sd_encounters           = sd(tot_encounters, na.rm = TRUE),
    q1_encounters           = quantile(tot_encounters, 0.25, na.rm = TRUE),
    median_encounters       = median(tot_encounters, na.rm = TRUE),
    q3_encounters           = quantile(tot_encounters, 0.75, na.rm = TRUE),
    n_exposed           = sum(!is.na(tot_encounters)),
    tot_encounters          = sum(tot_encounters, na.rm = TRUE),
    mean_encounters_all_patients = tot_encounters / n_all_pts_raw,  # mean for all patients, regardless of exposure (i.e. zeros for non-exposed months) - sum monthly mean = overall mean
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd_encounters / sqrt(n_exposed),
    lower = mean_encounters_exposed - qt(0.975, df = n_exposed - 1) * se,
    upper = mean_encounters_exposed + qt(0.975, df = n_exposed - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))


## User-only & all months ----

# Preparation

cohort_table_users_raw <- appt_data_raw %>% filter(user_status== "User") %>% 
  distinct(patid, index_date, site, follow_up_end, follow_up_reason, follow_up_time)

n_users_raw <- n_distinct(cohort_table_users_raw$patid)

# Create 24-month exposure matrix
cohort_grid_raw_all_months_raw <- cohort_table_users_raw %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    # Calculate total months including partial ones
    total_months = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%  # Always up to 24 months
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < total_months, 0, NA_real_)  # Include partial months
  ) %>%
  select(patid, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)


# Compute calendar months including partial ones
cohort_months_all_months_raw <- cohort_table_users_raw %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  select(patid, max_month_exposed)


# --- a) All users & all encounters ----

# Filter encounters 

monthly_encounters_summary_raw <- appt_data_raw %>%
  mutate(
    index_date = ymd(index_date),
    apptdate = ymd(apptdate),
    month_after_index = trunc(as.numeric(difftime(apptdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_all_months_raw, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>%  
  group_by(patid, month_after_index) %>%
  summarise(total_encounters = n(), .groups = "drop") %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  select(patid, month_col, total_encounters)

# Reshape to wide format: encounters per patient-month

observed_visit_matrix_raw <- monthly_encounters_summary_raw %>%
  pivot_wider(names_from = month_col, values_from = total_encounters)

# Merge visit counts with exposure base

month_cols_raw <- grep("^month_", names(cohort_grid_raw_all_months_raw), value = TRUE)

merged_matrix_encounters_raw <- cohort_grid_raw_all_months_raw %>%
  left_join(observed_visit_matrix_raw, by = "patid", suffix = c("_exp", "_visit"))

# Overwrite exposed months with visit counts
for (col in month_cols_raw) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  
  merged_matrix_encounters_raw[[col]] <- dplyr::case_when(
    is.na(merged_matrix_encounters_raw[[exp_col]]) ~ NA_real_,  # not exposed
    !is.na(merged_matrix_encounters_raw[[visit_col]]) ~ merged_matrix_encounters_raw[[visit_col]],
    TRUE ~ 0  # exposed but no encounters
  )
}

final_matrix_encounters_raw <- merged_matrix_encounters_raw %>%
  select(patid, all_of(month_cols_raw))

final_long_df_encounters_users_all_encounters_raw <- final_matrix_encounters_raw %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "tot_encounters"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))|> 
  mutate(tot_encounters = if_else(is.na(tot_encounters), 0, tot_encounters))

# Summarise monthly visit statistics

observed_visit_summary_users_all_encounters_all_months_raw <- final_long_df_encounters_users_all_encounters_raw %>%
  group_by(month_after_index) %>%
  summarise(
    mean_encounters_exposed = mean(tot_encounters, na.rm = TRUE), # mean for users exposed in each month (i.e. NAs for non-exposed months)
    min_encounters          = min(tot_encounters, na.rm = TRUE),
    max_encounters          = max(tot_encounters, na.rm = TRUE),
    sd_encounters           = sd(tot_encounters, na.rm = TRUE),
    q1_encounters           = quantile(tot_encounters, 0.25, na.rm = TRUE),
    median_encounters       = median(tot_encounters, na.rm = TRUE),
    q3_encounters           = quantile(tot_encounters, 0.75, na.rm = TRUE),
    n_exposed           = sum(!is.na(tot_encounters)),
    tot_encounters          = sum(tot_encounters, na.rm = TRUE),
    mean_encounters_all_patients = tot_encounters / n_users_raw,  # mean for all users, regardless of exposure (i.e. zeros for non-exposed months) - sum monthly mean = overall mean
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd_encounters / sqrt(n_exposed),
    lower = mean_encounters_exposed - qt(0.975, df = n_exposed - 1) * se,
    upper = mean_encounters_exposed + qt(0.975, df = n_exposed - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))

## Bootstrap -----

bootstrap_monthly_summary <- function(data, month_var = "month_after_index", value_var = "tot_encounters", R = 1000, seed = 626) {
  
  unique_months <- sort(unique(data[[month_var]]))
  
  results <- lapply(unique_months, function(m) {
    data_m <- data %>% filter(!!sym(month_var) == m)
    
    stat_fn <- function(d, i) {
      d_sample <- d[i, ]
      mean(d_sample[[value_var]], na.rm = TRUE)
    }
    
    set.seed(seed)
    boot_res <- boot(data = data_m, statistic = stat_fn, R = R)
    boot_vals <- as.numeric(boot_res$t)
    
    ci <- boot.ci(boot_res, type = c("norm", "basic", "perc"))
    
    tibble(
      month            = m,
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

library(boot)
## Bootstrap on full months

# monthly_bootstrap_all_patients_all_encounters <- bootstrap_monthly_summary(final_long_df_encounters_all_patients_all_encounters)
# monthly_bootstrap_all_patients_relev_encounters <- bootstrap_monthly_summary(final_long_df_encounters_all_patients_relev_encounters)
# monthly_bootstrap_users_all_encounters <- bootstrap_monthly_summary(final_long_df_encounters_users_all_encounters)
# monthly_bootstrap_users_relev_encounters <- bootstrap_monthly_summary(final_long_df_encounters_users_relev_encounters)


## Bootstrap on all months (including partial)

monthly_bootstrap_all_patients_all_encounters_all_months_raw <- bootstrap_monthly_summary(final_long_df_encounters_all_patients_all_encounters_raw)
# monthly_bootstrap_all_patients_relev_encounters_all_months <- bootstrap_monthly_summary(final_long_df_encounters_all_patients_relev_encounters)
monthly_bootstrap_users_all_encounters_all_months_raw <- bootstrap_monthly_summary(final_long_df_encounters_users_all_encounters_raw)
# monthly_bootstrap_users_relev_encounters_all_months <- bootstrap_monthly_summary(final_long_df_encounters_users_relev_encounters)


# Combine bootstrap summaries
combined_encounters_bootstrap_summary_monthly_raw <- bind_rows(
  monthly_bootstrap_all_patients_all_encounters_all_months_raw %>%
    mutate(group = "All Patients & all encounters"),
  
  # monthly_bootstrap_all_patients_relev_encounters_all_months %>%
  #   mutate(group = "All Patients & relevant encounters"),
  
  monthly_bootstrap_users_all_encounters_all_months_raw %>%
    mutate(group = "Users & all encounters"),
  
  # monthly_bootstrap_users_relev_encounters_all_months %>%
  #   mutate(group = "Users & relevant encounters")
) %>%
  select(group, everything())

# Export
library(writexl)
write_xlsx(combined_encounters_bootstrap_summary_monthly_raw, 
           here::here("CPRD Raw Analysis", "Results", "SecondaryCareAnalysis", "OP", "Monthly",
                      "monthly_combined_bootstrap_summary_encounters_SOURCE.xlsx"))

### COSTS -----

## All patients & full months ----
# Preparation 
n_all_pts_raw <- n_distinct(cohort_table_raw$patid)

# Create 24-month exposure matrix
cohort_grid_raw <- cohort_table_raw %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)  # NA for unexposed months
  ) %>%
  select(patid, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# Compute calendar months with full exposure (i.e., dropping the final month if the patient was not exposed in full)

cohort_months_raw <- cohort_table_raw %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = interval(index_date, follow_up_end) %/% months(1)
  ) %>%
  select(patid, max_month_exposed)

# --- a) All patients & all encounters -----

# Filter encounters to full months only
op_costs_final_data_recoded_raw <- op_costs_final_data_recoded_raw |> 
  arrange(patid)
# op_costs_final_data_recoded_min_raw <- op_costs_final_data_recoded_raw |> 
#   select(unit_cost) 
#   

# appt_data_raw <- cbind(appt_data_raw, op_costs_final_data_recoded_min_raw)

appt_data_raw <- full_join(appt_data_raw |> group_by(patid) |> 
                             mutate(row_id = row_number()) |> ungroup(), 
                           op_costs_final_data_recoded_raw |> 
                               select(patid, unit_cost) |> 
                               group_by(patid) |> mutate(row_id = row_number()) |> ungroup(),
                             by = c("patid", "row_id")) 

merged_matrix_costs_raw <- appt_data_raw %>%
  mutate(
    index_date = ymd(index_date),
    apptdate = ymd(apptdate),
    month_after_index = trunc(as.numeric(difftime(apptdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_raw, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>%
  group_by(patid, month_after_index) %>%
  summarise(
    total_patient_cost = sum(unit_cost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  select(patid, month_col, total_patient_cost)


# Reshape to wide format: encounters per patient-month

merged_matrix_costs_raw <- merged_matrix_costs_raw %>%
  pivot_wider(names_from = month_col, values_from = total_patient_cost)

# Merge visit counts with exposure base

month_cols_raw <- grep("^month_", names(cohort_grid_raw), value = TRUE)

merged_matrix_costs_raw <- cohort_grid_raw %>%
  left_join(merged_matrix_costs_raw, by = "patid", suffix = c("_exp", "_cost"))

# Overwrite exposed months with visit counts
for (col in month_cols_raw) {
  exp_col <- paste0(col, "_exp")
  cost_col <- paste0(col, "_cost")
  
  merged_matrix_costs_raw[[col]] <- dplyr::case_when(
    is.na(merged_matrix_costs_raw[[exp_col]]) ~ NA_real_,  # not exposed
    !is.na(merged_matrix_costs_raw[[cost_col]]) ~ merged_matrix_costs_raw[[cost_col]],
    TRUE ~ 0  # exposed but no encounters
  )
}

final_matrix_costs_raw <- merged_matrix_costs_raw %>%
  select(patid, all_of(month_cols_raw))

final_long_df_costs_all_patients_all_encounters_raw <- final_matrix_costs_raw %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "total_patient_cost"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))|> 
  mutate(total_patient_cost = if_else(is.na(total_patient_cost), 0, total_patient_cost))

# Summarise monthly visit statistics

observed_cost_summary_all_patients_all_encounters_raw <- final_long_df_costs_all_patients_all_encounters_raw %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs_exposed = mean(total_patient_cost, na.rm = TRUE), # mean for patients exposed in each month (i.e. NAs for non-exposed months)
    min_costs          = min(total_patient_cost, na.rm = TRUE),
    max_costs          = max(total_patient_cost, na.rm = TRUE),
    sd_costs           = sd(total_patient_cost, na.rm = TRUE),
    q1_costs           = quantile(total_patient_cost, 0.25, na.rm = TRUE),
    median_costs       = median(total_patient_cost, na.rm = TRUE),
    q3_costs           = quantile(total_patient_cost, 0.75, na.rm = TRUE),
    n_exposed          = sum(!is.na(total_patient_cost)),
    total_patient_cost = sum(total_patient_cost, na.rm = TRUE),
    mean_costs_all_patients = total_patient_cost / n_all_pts_raw,  # mean for all patients, regardless of exposure (i.e. zeros for non-exposed months) - sum monthly mean = overall mean
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd_costs / sqrt(n_exposed),
    lower = mean_costs_exposed - qt(0.975, df = n_exposed - 1) * se,
    upper = mean_costs_exposed + qt(0.975, df = n_exposed - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))

## User-only & full months ----

# Preparation

cohort_table_users_raw <- appt_data_raw %>% 
  filter(user_status== "User") %>% 
  distinct(patid, index_date, site, follow_up_end, follow_up_reason, follow_up_time)

n_users_raw <- n_distinct(cohort_table_users_raw$patid)

# Create 24-month exposure matrix
cohort_grid_raw <- cohort_table_users_raw %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)  # NA for unexposed months
  ) %>%
  select(patid, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# Compute calendar months with full exposure (i.e., dropping the final month if the patient was not exposed in full)

cohort_months_raw <- cohort_table_users_raw %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = interval(index_date, follow_up_end) %/% months(1)
  ) %>%
  select(patid, max_month_exposed)

# --- a) All users & all encounters ----

# Filter encounters to full months only

monthly_cost_summary_raw <- appt_data_raw %>%
  filter(user_status == "User") %>%
  mutate(
    index_date = ymd(index_date),
    apptdate = ymd(apptdate),
    month_after_index = trunc(as.numeric(difftime(apptdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_raw, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>%
  group_by(patid, month_after_index) %>%
  summarise(
    total_patient_cost = sum(unit_cost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  select(patid, month_col, total_patient_cost)

# Reshape to wide format

observed_cost_matrix_raw <- monthly_cost_summary_raw %>%
  pivot_wider(names_from = month_col, values_from = total_patient_cost)

# Merge with exposure grid

month_cols_raw <- grep("^month_", names(cohort_grid_raw), value = TRUE)

merged_matrix_costs_raw <- cohort_grid_raw %>%
  left_join(observed_cost_matrix_raw, by = "patid", suffix = c("_exp", "_cost"))

# Overwrite exposed months with cost values
for (col in month_cols_raw) {
  exp_col <- paste0(col, "_exp")
  cost_col <- paste0(col, "_cost")
  
  merged_matrix_costs_raw[[col]] <- dplyr::case_when(
    is.na(merged_matrix_costs_raw[[exp_col]]) ~ NA_real_,
    !is.na(merged_matrix_costs_raw[[cost_col]]) ~ merged_matrix_costs_raw[[cost_col]],
    TRUE ~ 0
  )
}

final_matrix_costs_raw <- merged_matrix_costs_raw %>%
  select(patid, all_of(month_cols_raw))

final_long_df_costs_users_all_encounters_raw <- final_matrix_costs_raw %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "total_patient_cost"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month))) |> 
  mutate(total_patient_cost = if_else(is.na(total_patient_cost), 0, total_patient_cost))

# Summarise monthly cost statistics

observed_cost_summary_users_all_encounters_raw <- final_long_df_costs_users_all_encounters_raw %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs_exposed = mean(total_patient_cost, na.rm = TRUE),
    min_costs          = min(total_patient_cost, na.rm = TRUE),
    max_costs          = max(total_patient_cost, na.rm = TRUE),
    sd_costs           = sd(total_patient_cost, na.rm = TRUE),
    q1_costs           = quantile(total_patient_cost, 0.25, na.rm = TRUE),
    median_costs       = median(total_patient_cost, na.rm = TRUE),
    q3_costs           = quantile(total_patient_cost, 0.75, na.rm = TRUE),
    n_exposed          = sum(!is.na(total_patient_cost)),
    total_patient_cost = sum(total_patient_cost, na.rm = TRUE),
    mean_costs_all_patients = total_patient_cost / n_users_raw,
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd_costs / sqrt(n_exposed),
    lower = mean_costs_exposed - qt(0.975, df = n_exposed - 1) * se,
    upper = mean_costs_exposed + qt(0.975, df = n_exposed - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))

# --- All patients & all months (including partial months) ----
# Preparation 
n_all_pts_raw <- n_distinct(cohort_table_raw$patid)

# Create 24-month exposure matrix 
cohort_grid_raw_all_months_raw <- cohort_table_raw %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    total_months = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < total_months, 0, NA_real_)  # Include partial months
  ) %>%
  select(patid, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# Compute calendar months including partial ones
cohort_months_all_months_raw <- cohort_table_raw %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  select(patid, max_month_exposed)

# --- a) All patients & all encounters ----

monthly_costs_summary_raw <- appt_data_raw %>%
  mutate(
    index_date = ymd(index_date),
    apptdate = ymd(apptdate),
    month_after_index = trunc(as.numeric(difftime(apptdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_all_months_raw, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>%
  group_by(patid, month_after_index) %>%
  summarise(total_patient_cost = sum(unit_cost, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  select(patid, month_col, total_patient_cost)

observed_cost_matrix_raw <- monthly_costs_summary_raw %>%
  pivot_wider(names_from = month_col, values_from = total_patient_cost)

month_cols_raw <- grep("^month_", names(cohort_grid_raw_all_months), value = TRUE)

merged_matrix_costs_raw <- cohort_grid_raw_all_months %>%
  left_join(observed_cost_matrix_raw, by = "patid", suffix = c("_exp", "_cost"))

for (col in month_cols_raw) {
  exp_col <- paste0(col, "_exp")
  cost_col <- paste0(col, "_cost")
  
  merged_matrix_costs_raw[[col]] <- dplyr::case_when(
    is.na(merged_matrix_costs_raw[[exp_col]]) ~ NA_real_,
    !is.na(merged_matrix_costs_raw[[cost_col]]) ~ merged_matrix_costs_raw[[cost_col]],
    TRUE ~ 0
  )
}

final_matrix_costs_raw <- merged_matrix_costs_raw %>%
  select(patid, all_of(month_cols_raw))

final_long_df_costs_all_patients_all_encounters_raw <- final_matrix_costs_raw %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "total_patient_cost"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))|> 
  mutate(total_patient_cost = if_else(is.na(total_patient_cost), 0, total_patient_cost))

observed_cost_summary_all_patients_all_encounters_all_months_raw <- final_long_df_costs_all_patients_all_encounters_raw %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs_exposed = mean(total_patient_cost, na.rm = TRUE),
    min_costs = min(total_patient_cost, na.rm = TRUE),
    max_costs = max(total_patient_cost, na.rm = TRUE),
    sd_costs  = sd(total_patient_cost, na.rm = TRUE),
    q1_costs  = quantile(total_patient_cost, 0.25, na.rm = TRUE),
    median_costs = median(total_patient_cost, na.rm = TRUE),
    q3_costs  = quantile(total_patient_cost, 0.75, na.rm = TRUE),
    n_exposed = sum(!is.na(total_patient_cost)),
    tot_costs = sum(total_patient_cost, na.rm = TRUE),
    mean_costs_all_patients = tot_costs / n_all_pts_raw,
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd_costs / sqrt(n_exposed),
    lower = mean_costs_exposed - qt(0.975, df = n_exposed - 1) * se,
    upper = mean_costs_exposed + qt(0.975, df = n_exposed - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))



## User-only & all months ----
# Preparation

cohort_table_users_raw <- appt_data_raw %>% filter(user_status== "User") %>% 
  distinct(patid, index_date, site, follow_up_end, follow_up_reason, follow_up_time)

n_users_raw <- n_distinct(cohort_table_users_raw$patid)

# Create 24-month exposure matrix

cohort_grid_raw_all_months <- cohort_table_users_raw %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    # Calculate total months including partial ones
    total_months = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%  # Always up to 24 months
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < total_months, 0, NA_real_)  # Include partial months
  ) %>%
  select(patid, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)


# Compute calendar months including partial ones
cohort_months_all_months_raw <- cohort_table_users_raw %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  select(patid, max_month_exposed)



# --- a) All users & all encounters ----

# Filter all encounters for users and compute monthly cost
monthly_cost_summary_raw <- appt_data_raw %>%
  filter(user_status == "User") %>%
  mutate(
    index_date = ymd(index_date),
    apptdate = ymd(apptdate),
    month_after_index = trunc(as.numeric(difftime(apptdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_all_months_raw, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>%
  group_by(patid, month_after_index) %>%
  summarise(total_patient_cost = sum(unit_cost, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  select(patid, month_col, total_patient_cost)

# Wide format matrix
observed_cost_matrix_raw <- monthly_cost_summary_raw %>%
  pivot_wider(names_from = month_col, values_from = total_patient_cost)

# Merge with exposure grid
month_cols_raw <- grep("^month_", names(cohort_grid_raw_all_months), value = TRUE)

merged_matrix_costs_raw <- cohort_grid_raw_all_months %>%
  left_join(observed_cost_matrix_raw, by = "patid", suffix = c("_exp", "_cost"))

# Fill costs into exposure grid
for (col in month_cols_raw) {
  exp_col <- paste0(col, "_exp")
  cost_col <- paste0(col, "_cost")
  
  merged_matrix_costs_raw[[col]] <- case_when(
    is.na(merged_matrix_costs_raw[[exp_col]]) ~ NA_real_,
    !is.na(merged_matrix_costs_raw[[cost_col]]) ~ merged_matrix_costs_raw[[cost_col]],
    TRUE ~ 0
  )
}

# Reshape long
final_matrix_costs_raw <- merged_matrix_costs_raw %>%
  select(patid, all_of(month_cols_raw))

final_long_df_costs_users_all_encounters_raw <- final_matrix_costs_raw %>%
  pivot_longer(cols = starts_with("month_"), names_to = "month", values_to = "total_patient_cost") %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month))) |> 
  mutate(total_patient_cost = if_else(is.na(total_patient_cost), 0, total_patient_cost))

# Monthly summary
observed_cost_summary_users_all_encounters_all_months_raw <- final_long_df_costs_users_all_encounters_raw %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs_exposed      = mean(total_patient_cost, na.rm = TRUE),
    min_costs               = min(total_patient_cost, na.rm = TRUE),
    max_costs               = max(total_patient_cost, na.rm = TRUE),
    sd_costs                = sd(total_patient_cost, na.rm = TRUE),
    q1_costs                = quantile(total_patient_cost, 0.25, na.rm = TRUE),
    median_costs            = median(total_patient_cost, na.rm = TRUE),
    q3_costs                = quantile(total_patient_cost, 0.75, na.rm = TRUE),
    n_exposed               = sum(!is.na(total_patient_cost)),
    total_patient_cost      = sum(total_patient_cost, na.rm = TRUE),
    mean_costs_all_users    = total_patient_cost / n_users_raw,
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd_costs / sqrt(n_exposed),
    lower = mean_costs_exposed - qt(0.975, df = n_exposed - 1) * se,
    upper = mean_costs_exposed + qt(0.975, df = n_exposed - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))

## Bootstrap -----

bootstrap_monthly_summary_costs <- function(data, month_var = "month_after_index", value_var = "total_patient_cost", R = 1000, seed = 626) {
  
  unique_months <- sort(unique(data[[month_var]]))
  
  results <- lapply(unique_months, function(m) {
    data_m <- data %>% filter(!!sym(month_var) == m)
    
    stat_fn <- function(d, i) {
      d_sample <- d[i, ]
      mean(d_sample[[value_var]], na.rm = TRUE)
    }
    
    set.seed(seed)
    boot_res <- boot(data = data_m, statistic = stat_fn, R = R)
    boot_vals <- as.numeric(boot_res$t)
    
    ci <- boot.ci(boot_res, type = c("norm", "basic", "perc"))
    
    tibble(
      month            = m,
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

## Bootstrap on full months

# monthly_bootstrap_costs_all_patients_all_encounters <- bootstrap_monthly_summary_costs(final_long_df_costs_all_patients_all_encounters)
# monthly_bootstrap_costs_all_patients_relev_encounters <- bootstrap_monthly_summary_costs(final_long_df_costs_all_patients_relev_encounters)
# monthly_bootstrap_costs_users_all_encounters <- bootstrap_monthly_summary_costs(final_long_df_costs_users_all_encounters)
# monthly_bootstrap_costs_users_relev_encounters <- bootstrap_monthly_summary_costs(final_long_df_costs_users_relev_encounters)

## Bootstrap on all months (including partial)

monthly_bootstrap_cost_all_patients_all_encounters_all_months_raw <- bootstrap_monthly_summary_costs(final_long_df_costs_all_patients_all_encounters_raw)
# monthly_bootstrap_cost_all_patients_relev_encounters_all_months <- bootstrap_monthly_summary_costs(final_long_df_costs_all_patients_relev_encounters)
monthly_bootstrap_cost_users_all_encounters_all_months_raw <- bootstrap_monthly_summary_costs(final_long_df_costs_users_all_encounters_raw)
# monthly_bootstrap_cost_users_relev_encounters_all_months <- bootstrap_monthly_summary_costs(final_long_df_costs_users_relev_encounters)


# Combine bootstrap summaries
combined_costs_bootstrap_summary_monthly_raw <- bind_rows(
  monthly_bootstrap_cost_all_patients_all_encounters_all_months_raw %>%
    mutate(group = "All Patients & all encounters"),
  
  # monthly_bootstrap_cost_all_patients_relev_encounters_all_months %>%
  #   mutate(group = "All Patients & relevant encounters"),
  
  monthly_bootstrap_cost_users_all_encounters_all_months_raw %>%
    mutate(group = "Users & all encounters"),
  
  # monthly_bootstrap_cost_users_relev_encounters_all_months %>%
  #   mutate(group = "Users & relevant encounters")
) %>%
  select(group, everything())

# Export
library(writexl)
write_xlsx(combined_costs_bootstrap_summary_monthly_raw, 
           here::here("CPRD Raw Analysis", "Results", "SecondaryCareAnalysis", 
                      "OP", "Monthly", 
                      "monthly_combined_bootstrap_summary_costs_SOURCE.xlsx"))



## Plot for initial interpretation ----

library(ggplot2)

### OP visits plots ----
# Add group identifiers and combine into one dataframe
df_boot_means_raw <- bind_rows(
  monthly_bootstrap_all_patients_all_encounters_all_months_raw %>%
    mutate(group = "All Patients - All Encounters"),
  
  # monthly_bootstrap_all_patients_relev_encounters_all_months %>%
  #   mutate(group = "All Patients - Relevant Encounters"),
  
  monthly_bootstrap_users_all_encounters_all_months_raw %>%
    mutate(group = "Users - All Encounters"),
  
  # monthly_bootstrap_users_relev_encounters_all_months %>%
  #   mutate(group = "Users - Relevant Encounters")
)

# Convert `month` to numeric if necessary
df_boot_means_raw <- df_boot_means_raw %>%
  mutate(month = as.numeric(month),
         mean = as.numeric(mean))  # Ensure mean is numeric

# Plot
(boot_mean_visits_plot_raw <- ggplot(df_boot_means_raw, aes(x = month, y = mean, color = group)) +
    geom_line(size = 1) +
    geom_point() +
    labs(
    title = "Bootstrap Mean Monthly Encounters Over Time",
    x = "Month After Index",
    y = "Mean Number of Encounters",
    color = "Group"
  ) +
    scale_x_continuous(breaks = 1:24) +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      plot.caption = element_text(size = 10, hjust = 0)
    )
)

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis", "OP", "Plots",
  "Boot_mean_visits_plot-Raw.PNG"),
  plot = boot_mean_visits_plot_raw,
  width = 10,
  height = 7,
  bg = "white"
)

### Costs plot ----

# Combine cost bootstrap summaries
df_boot_costs_raw <- bind_rows(
  monthly_bootstrap_cost_all_patients_all_encounters_all_months_raw %>%
    mutate(group = "All Patients - All Encounters"),
  
  # monthly_bootstrap_cost_all_patients_relev_encounters_all_months %>%
  #   mutate(group = "All Patients - Relevant Encounters"),
  
  monthly_bootstrap_cost_users_all_encounters_all_months_raw %>%
    mutate(group = "Users - All Encounters"),
  
  # monthly_bootstrap_cost_users_relev_encounters_all_months %>%
  #   mutate(group = "Users - Relevant Encounters")
)

# Convert variables to numeric if needed
df_boot_costs_raw <- df_boot_costs_raw %>%
  mutate(month = as.numeric(month),
         mean = as.numeric(mean))  # Ensure mean is numeric

# Plot
(boot_mean_costs_plot_raw <- ggplot(df_boot_costs_raw, aes(x = month, y = mean, color = group)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Bootstrap Mean Monthly Costs Over Time",
    x = "Month After Index",
    y = "Mean Monthly Cost (£)",
    color = "Group"
  ) +
  scale_x_continuous(breaks = 1:24) +
  theme_minimal(base_size = 14, base_family = "Times") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 10, hjust = 0)
  )
)

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis", "OP", "Plots",
  "Boot_mean_costs_plot-Raw.PNG"),
  plot = boot_mean_costs_plot_raw,
  width = 10,
  height = 7,
  bg = "white"
)

# Cleaning environment

rm(list = setdiff(ls(), c(
  
  ## From previous codes
  # === Project metadata and core data ===
  "db", "my_dm", "study_start_date", "study_end_date",
  "table_one_op_raw_appt_recoded", "provider_cost_inputs", "cohort_table", "cohort_table_users", 
  "all_patients",
  
  # === Attrition and summaries ===
  "AttritionReportCoh", "AttritionReportFrac_Raw",
  "gt_summary_table", "gt_summary_table_df",
  "gt_overall_healthcare_roles_count", 
  
  # === Overall summary encounters ===
  "all_patients_all_encounters_counts", "summary_all_patients_all_encounters",
  "all_patients_relevant_encounters_counts", "summary_all_patients_relevant_encounters",
  "summary_users_all_encounters",
  "users_relevant_encounters_counts", "summary_users_relevant_encounters",
  "combined_summary_encounters",
  #boot
  "boot_summary_all_patients_all_encounters", "boot_summary_all_patients_relevant_encounters",
  "boot_summary_users_all_encounters", "boot_summary_users_relevant_encounters",
  "combined_bootstrap_summary_encounters",
  
  # === Overall summary costs ===
  "all_patients_all_encounters_costs", "summary_all_patients_all_encounters_costs",
  "all_patients_relevant_encounters_costs", "summary_all_patients_relevant_encounters_costs",
  "users_all_encounters_costs", "summary_users_all_encounters_costs",
  "users_relevant_encounters_costs", "summary_users_relevant_encounters_costs",
  "combined_summary_costs",
  #boot
  "boot_summary_all_patients_all_encounters_costs", "boot_summary_all_patients_relevant_encounters_costs",
  "boot_summary_users_all_encounters_costs", "boot_summary_users_relevant_encounters_costs",
  "combined_bootstrap_costs_summary",
  
  ###  From this code:
  
  # === Monthly summary encounters ===
  ## Full months
  # All patients
  "observed_visit_summary_all_patients_all_encounters", 
  "observed_visit_summary_all_patients_relev_encounters",
  # Users
  "cohort_table_users", 
  "observed_visit_summary_users_all_encounters", 
  "observed_visit_summary_users_relev_encounters", 
  
  ## All months
  # All patients
  "observed_visit_summary_all_patients_all_encounters_all_months", 
  "observed_visit_summary_all_patients_relev_encounters_all_months",
  # Users
  "observed_visit_summary_users_all_encounters_all_months", 
  "observed_visit_summary_users_relev_encounters_all_months", 
  # Bootstrap
  "monthly_bootstrap_all_patients_all_encounters_all_months", 
  "monthly_bootstrap_all_patients_relev_encounters_all_months",
  "monthly_bootstrap_users_all_encounters_all_months", 
  "monthly_bootstrap_users_relev_encounters_all_months",
  
  # === Monthly summary costs ===
  ## Full months
  # All patients
  "observed_cost_summary_all_patients_all_encounters", 
  "observed_cost_summary_all_patients_relev_encounters",
  # Users
  "observed_cost_summary_users_all_encounters", 
  "observed_cost_summary_users_relev_encounters",
  
  ## All months
  # All patients
  "observed_cost_summary_all_patients_all_encounters_all_months", 
  "observed_cost_summary_all_patients_relev_encounters_all_months",
  # Users
  "observed_cost_summary_users_all_encounters_all_months", 
  "observed_cost_summary_users_relev_encounters_all_months",
  # Bootstrap
  "monthly_bootstrap_cost_all_patients_all_encounters_all_months", 
  "monthly_bootstrap_cost_all_patients_relev_encounters_all_months",
  "monthly_bootstrap_cost_users_all_encounters_all_months", 
  "monthly_bootstrap_cost_users_relev_encounters_all_months"
  
)))

