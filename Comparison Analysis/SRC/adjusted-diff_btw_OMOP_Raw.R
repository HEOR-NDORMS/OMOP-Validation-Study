#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Differences between OMOP and Raw using visits data - Unadjusted analysis
# Date: 09.06.2025
# Last date edited: 09.06.2025
# ****************************************************************************** #

# Load packages ----
library(here)
library(dplyr)
library(ggplot2)
library(tidylog)
library(rio)
library(boot)
library(tidytable)
library(lubridate)
library(writexl)

# Import data ----
visit_data_omop <- rio::import("H:/Projects/Validation/Data/visit_data_omop.csv")
cohort_data_omop <- rio::import("H:/Projects/Validation/Data/cohort_table_omop.csv")

visits_data_raw <- rio::import("H:/Projects/Validation/Data/visit_data_raw.csv")
cohort_data_raw <- rio::import("H:/Projects/Validation/Data/cohort_table_raw.csv")


# Adjusted Visits ----
## Source ----
# Create 24-Month Exposure Matrix Per Patient
cohort_grid_raw <- cohort_data_raw %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)
  ) %>%
  select(patid, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# Adjusted Visit Count Calculation
adjusted_visits_data_r <- visits_data_raw %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(days_exposed_total / 30.44),
    month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1,
    died_in_final_month = !is.na(death_date) &
      floor(as.numeric(difftime(death_date, index_date, units = "days")) / 30.44) + 1 == final_month_after_index,
    days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
    adjusted_visit_count = case_when(
      month_after_index != final_month_after_index ~ 1,
      died_in_final_month ~ 1,
      days_into_final_month >= 30.44 ~ 1,
      days_into_final_month >= 15.22 ~ (1 / days_into_final_month) * 30.44,
      TRUE ~ 0
    )
  ) %>%
  filter(!(month_after_index == final_month_after_index & adjusted_visit_count == 0)) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(adjusted_visits = sum(adjusted_visit_count), .groups = "drop")

# adjusted_visits_data_r <- visits_data_raw %>%
#   filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
#   mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
#   mutate(
#     days_exposed_total = as.numeric(follow_up_end - index_date),
#     final_month_after_index = ceiling(days_exposed_total / 30.44),
#     month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1,
#     days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
#     died_in_final_month = !is.na(death_date) &
#       floor(as.numeric(death_date - index_date) / 30.44) + 1 == final_month_after_index
#   ) %>%
#   group_by(patid, month_after_index, specialty_reduced) %>%
#   summarise(visit_count = n(), .groups = "drop") %>%
#   left_join(
#     visits_data_raw %>%
#       mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
#       mutate(
#         days_exposed_total = as.numeric(follow_up_end - index_date),
#         final_month_after_index = ceiling(days_exposed_total / 30.44),
#         days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
#         died_in_final_month = !is.na(death_date) &
#           floor(as.numeric(death_date - index_date) / 30.44) + 1 == final_month_after_index
#       ) %>%
#       distinct(patid, final_month_after_index, days_into_final_month, died_in_final_month, days_exposed_total),
#     by = "patid"
#   ) %>%
#   mutate(
#     adjusted_visits = case_when(
#       month_after_index != final_month_after_index ~ visit_count,
#       died_in_final_month ~ visit_count,
#       days_into_final_month >= 30.44 ~ visit_count,
#       days_into_final_month >= 15.22 ~ (visit_count / days_into_final_month) * 30.44,
#       TRUE ~ 0
#     )
#   ) %>%
#   filter(adjusted_visits > 0)

# Create Adjusted Visit Matrix (Wide Format)
adjusted_visits_long_r <- adjusted_visits_data_r %>%
  group_by(patid, month_after_index) %>%
  summarise(adjusted_visits = sum(adjusted_visits), .groups = "drop") %>%
  mutate(month_col = paste0("month_", month_after_index))

adjusted_visit_matrix_r <- adjusted_visits_long_r %>%
  select(patid, month_col, adjusted_visits) %>%
  pivot_wider(names_from = month_col, values_from = adjusted_visits)

# Merge with Exposure Matrix and Coalesce
month_cols <- grep("^month_", names(cohort_grid_raw), value = TRUE)

merged_matrix_r <- cohort_grid_raw %>%
  left_join(adjusted_visit_matrix_r, by = "patid", suffix = c("_exp", "_visit"))

for (col in month_cols) {
  merged_matrix_r[[col]] <- dplyr::coalesce(
    merged_matrix_r[[paste0(col, "_visit")]],
    merged_matrix_r[[paste0(col, "_exp")]]
  )
}

final_matrix_adjusted_r <- merged_matrix_r %>%
  select(patid, all_of(month_cols))

# Convert to Long Format
final_long_df_adjusted_r <- final_matrix_adjusted_r %>%
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "adjusted_visit_count"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# Accurate Mean Adjusted Visits per Month
# Get patient-months with valid exposure
# But first, let's filter for users only
users_adj_r <- visits_data_raw %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  distinct(patid) %>% 
  pull(patid)

exposed_patient_months_r <- cohort_grid_raw %>%
  filter(patid %in% users_adj_r) %>% 
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "exposure"
  ) %>%
  filter(!is.na(exposure)) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month))) %>%
  select(patid, month_after_index)

# Sum adjusted visits per patient-month
total_visits_per_patient_month_r <- adjusted_visits_data_r %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(adjusted_visits), .groups = "drop")

# Join exposure data and fill 0s
adjusted_visits_full_r <- exposed_patient_months_r %>%
  left_join(total_visits_per_patient_month_r, by = c("patid", "month_after_index")) %>%
  mutate(total_visits = replace_na(total_visits, 0))

# Calculate mean visits using full exposed cohort
mean_visits_per_month_r <- adjusted_visits_full_r %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = mean(total_visits), .groups = "drop")

fc_grouped_visits <- function(data, indices) {
  sampled <- data[indices, ]
  mean(sampled$total_visits)
}

set.seed(12)

adj_boot_user_visits_summary_per_month_r <- adjusted_visits_full_r |>
  group_by(month_after_index) |>
  group_split() |>
  map_df(function(df_month) {
    boot_result <- boot(df_month, statistic = fc_grouped_visits, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "norm")
    
    tibble(
      month_after_index = unique(df_month$month_after_index),
      mean_visits_boot = round(boot_result$t0, 3),
      lower_ci = round(as.numeric(boot_ci$normal[2]), 3),
      upper_ci = round(as.numeric(boot_ci$normal[3]), 3)
    )
  })

mean_visits_combined_r <- mean_visits_per_month_r |>
  left_join(adj_boot_user_visits_summary_per_month_r, by = "month_after_index")


## OMOP ----
# Create 24-Month Exposure Matrix Per Patient
# Build Exposure Grid with 24 Months
cohort_grid_omop <- cohort_data_omop %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)
  ) %>%
  select(subject_id, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# Adjusted Visit Counts
adjusted_visits_data_o <- visit_data_omop %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(days_exposed_total / 30.44),
    month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1,
    died_in_final_month = !is.na(death_date) &
      floor(as.numeric(difftime(death_date, index_date, units = "days")) / 30.44) + 1 == final_month_after_index,
    days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
    adjusted_visit_count = case_when(
      month_after_index != final_month_after_index ~ 1,
      died_in_final_month ~ 1,
      days_into_final_month >= 30.44 ~ 1,
      days_into_final_month >= 15.22 ~ (1 / days_into_final_month) * 30.44,
      TRUE ~ 0
    )
  ) %>%
  filter(!(month_after_index == final_month_after_index & adjusted_visit_count == 0)) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(adjusted_visits = sum(adjusted_visit_count), .groups = "drop")

# Wide Format for Adjusted Visit Counts
adjusted_visits_long_o <- adjusted_visits_data_o %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  group_by(subject_id, month_col) %>%
  summarise(adjusted_visits = sum(adjusted_visits), .groups = "drop")

adjusted_visits_matrix_o <- adjusted_visits_long_o %>%
  pivot_wider(names_from = month_col, values_from = adjusted_visits)

# Merge with Exposure Grid and Coalesce
month_cols <- grep("^month_", names(cohort_grid_omop), value = TRUE)

merged_matrix_o <- cohort_grid_omop %>%
  left_join(adjusted_visits_matrix_o, by = "subject_id", suffix = c("_exp", "_visit"))

for (col in month_cols) {
  visit_col <- paste0(col, "_visit")
  exp_col <- paste0(col, "_exp")
  if (!(visit_col %in% names(merged_matrix_o))) {
    merged_matrix_o[[visit_col]] <- NA_real_
  }
  merged_matrix_o[[col]] <- dplyr::coalesce(
    as.numeric(merged_matrix_o[[visit_col]]),
    as.numeric(merged_matrix_o[[exp_col]])
  )
}

final_matrix_adjusted_visits_o <- merged_matrix_o %>%
  select(subject_id, all_of(month_cols))

# Convert to Long Format
final_long_df_adjusted_visits_o <- final_matrix_adjusted_visits_o %>%
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "adjusted_visit_count"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# Identify eligible users
users_adj_o <- visit_data_omop %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  distinct(subject_id) %>%
  pull(subject_id)

# Full 24-month grid for all users
full_patient_month_grid_o <- cohort_data_omop %>%
  filter(subject_id %in% users_adj_o) %>%
  select(subject_id) %>%
  distinct() %>%
  crossing(month_after_index = 1:24)

# Summarise adjusted visits
total_visits_per_patient_month_o <- adjusted_visits_data_o %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visits = sum(adjusted_visits), .groups = "drop")

# Merge to full grid (fill missing with 0)
adjusted_visits_full_o <- full_patient_month_grid_o %>%
  left_join(total_visits_per_patient_month_o, by = c("subject_id", "month_after_index")) %>%
  mutate(total_visits = replace_na(total_visits, 0))

# Mean visits per month across cohort
mean_visits_per_month_o <- adjusted_visits_full_o %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = sum(total_visits) / n_distinct(subject_id), .groups = "drop")

fc_grouped_visits <- function(data, indices) {
  sampled <- data[indices, ]
  mean(sampled$total_visits)
}

set.seed(626)

boot_user_visits_summary_per_month_o <- adjusted_visits_full_o |>
  group_by(month_after_index) |>
  group_split() |>
  map_df(function(df_month) {
    boot_result <- boot(df_month, statistic = fc_grouped_visits, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "norm")
    
    tibble(
      month_after_index = unique(df_month$month_after_index),
      mean_visits_boot = round(boot_result$t0, 3),
      lower_ci = round(as.numeric(boot_ci$normal[2]), 3),
      upper_ci = round(as.numeric(boot_ci$normal[3]), 3)
    )
  })

mean_visits_combined_o <- mean_visits_per_month_o |>
  left_join(boot_user_visits_summary_per_month_o, by = "month_after_index")

### Plot ----
# Add a source label to each
adj_mean_visits_raw  <- mean_visits_combined_r  %>% mutate(source = "Source")
adj_mean_visits_omop <- mean_visits_combined_o %>% mutate(source = "OMOP")

# Combine into one long-format dataframe
adj_combined_mean_visits <- bind_rows(adj_mean_visits_raw, adj_mean_visits_omop)

# Plot
(adj_plot_visits <- ggplot(adj_combined_mean_visits, 
                 aes(x = month_after_index, 
                     y = mean_visits_boot, color = source)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.0) +
    geom_errorbar(aes(ymin=lower_ci,
                      ymax=upper_ci), width = 0.4)+
    labs(
      title = "Mean Primary Care Visits by Data Source",
      x = "Month After Index Fracture",
      y = "Mean Primary Care Visits",
      color = "Data Source",
      caption = "Adjusted for partial follow-up: included patients with ≥15.22 days/month.
     \nPartial months scaled by exposure;
     \n full months or death-months used raw counts."
    ) +
    scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c")) +
    scale_y_continuous(breaks = seq(0, 3, by = 1), limits = c(0, 3)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    ))

ggsave(here::here(
  "Comparison Analysis", "Results", "Plots",
  "Mean_visits_per_data_source_adjusted.PNG"),
  plot = adj_plot_visits,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)

### Save the output datasets ----
adjusted_visits_list <- list(
  mean_visits_combined_o = mean_visits_combined_o,
  mean_visits_combined_r = mean_visits_combined_r
)
write_xlsx(adjusted_visits_list, here::here("Comparison Analysis", "Results", "Output_monthly_mean_visits-Adjusted.xlsx"))

# Adjusted Costs ----
## Source ----
# Build Exposure Grid with 24 Months
cohort_grid_raw <- cohort_data_raw %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)
  ) %>%
  select(patid, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# Adjust Visit Counts
adjusted_costs_data_r_c <- visits_data_raw %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(days_exposed_total / 30.44),
    month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1,
    died_in_final_month = !is.na(death_date) &
      floor(as.numeric(difftime(death_date, index_date, units = "days")) / 30.44) + 1 == final_month_after_index,
    days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
    adjusted_visit_count = case_when(
      month_after_index != final_month_after_index ~ 1,
      died_in_final_month ~ 1,
      days_into_final_month >= 30.44 ~ 1,
      days_into_final_month >= 15.22 ~ (1 / days_into_final_month) * 30.44,
      TRUE ~ 0
    )
  ) %>%
  filter(!(month_after_index == final_month_after_index & adjusted_visit_count == 0)) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    unit_cost = unit_cost,
    adjusted_visits = sum(adjusted_visit_count),
    .groups = "drop") %>% 
  distinct() %>% 
  mutate(adjust_visit_costs = unit_cost * adjusted_visits)

# Wide Format for Adjusted Visits (cleaned, 1 value per patient/month)
adjusted_costs_long_r <- adjusted_costs_data_r_c %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  group_by(patid, month_col) %>%
  summarise(adjust_visit_costs = sum(adjust_visit_costs), 
            .groups = "drop")

adjusted_costs_matrix_r <- adjusted_costs_long_r %>%
  pivot_wider(names_from = month_col, values_from = adjust_visit_costs)

# Merge Adjusted Visits with Exposure Grid
month_cols <- grep("^month_", names(cohort_grid_raw), value = TRUE)

merged_matrix_c <- cohort_grid_raw %>%
  left_join(adjusted_costs_matrix_r, by = "patid", suffix = c("_exp", "_visit"))

# Coalesce exposure and adjusted visits
for (col in month_cols) {
  visit_col <- paste0(col, "_visit")
  exp_col <- paste0(col, "_exp")
  if (!(visit_col %in% names(merged_matrix_c))) {
    merged_matrix_c[[visit_col]] <- NA_real_
  }
  merged_matrix_c[[col]] <- dplyr::coalesce(
    as.numeric(merged_matrix_c[[visit_col]]),
    as.numeric(merged_matrix_c[[exp_col]])
  )
}

final_matrix_adjusted_costs_r <- merged_matrix_c %>%
  select(patid, all_of(month_cols))

# Convert to long format
final_long_df_adjusted_costs_r <- final_matrix_adjusted_costs_r %>%
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "adjusted_visit_costs"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# But first, let's filter for users only
users_adj_r <- visits_data_raw %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  distinct(patid) %>% 
  pull(patid)

# Full patient-month grid (for all 24 months and all patients)
full_patient_month_grid_cost_r <- cohort_data_raw %>%
  filter(patid %in% users_adj_r) %>% 
  select(patid) %>%
  distinct() %>%
  crossing(month_after_index = 1:24)  # Ensures all 24 months per patient

# Summarise costs
total_costs_per_patient_month_r <- adjusted_costs_data_r_c %>%
  group_by(patid, month_after_index) %>%
  summarise(total_costs = sum(adjust_visit_costs), .groups = "drop")

# Merge to the full grid so missing patient-months get zero
adjusted_costs_full_r <- full_patient_month_grid_cost_r %>%
  left_join(total_costs_per_patient_month_r, by = c("patid", "month_after_index")) %>%
  mutate(total_costs = replace_na(total_costs, 0))

mean_costs_per_month_r <- adjusted_costs_full_r %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_cost = sum(total_costs)/n_distinct(patid), .groups = "drop")

fc_grouped_costs <- function(data, indices) {
  sampled <- data[indices, ]
  mean(sampled$total_costs)
}

set.seed(626)

boot_user_costs_summary_per_month_r <- adjusted_costs_full_r |>
  group_by(month_after_index) |>
  group_split() |>
  map_df(function(df_month) {
    boot_result <- boot(df_month, statistic = fc_grouped_costs, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "norm")
    
    tibble(
      month_after_index = unique(df_month$month_after_index),
      mean_costs_boot = round(boot_result$t0, 3),
      lower_ci = round(as.numeric(boot_ci$normal[2]), 3),
      upper_ci = round(as.numeric(boot_ci$normal[3]), 3)
    )
  })

mean_costs_combined_r <- mean_costs_per_month_r |>
  left_join(boot_user_costs_summary_per_month_r, by = "month_after_index")


## OMOP ----
# Build Exposure Grid with 24 Months
cohort_grid_omop <- cohort_data_omop %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)
  ) %>%
  select(subject_id, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# Adjust Visit Counts
adjusted_costs_data_o <- visit_data_omop %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(days_exposed_total / 30.44),
    month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1,
    died_in_final_month = !is.na(death_date) &
      floor(as.numeric(difftime(death_date, index_date, units = "days")) / 30.44) + 1 == final_month_after_index,
    days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
    adjusted_visit_count = case_when(
      month_after_index != final_month_after_index ~ 1,
      died_in_final_month ~ 1,
      days_into_final_month >= 30.44 ~ 1,
      days_into_final_month >= 15.22 ~ (1 / days_into_final_month) * 30.44,
      TRUE ~ 0
    )
  ) %>%
  filter(!(month_after_index == final_month_after_index & adjusted_visit_count == 0)) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(
    unit_cost = unit_cost,
    adjusted_visits = sum(adjusted_visit_count),
    .groups = "drop") %>% 
  distinct() %>% 
  mutate(adjust_visit_costs = unit_cost * adjusted_visits)

# Wide Format for Adjusted Visits (cleaned, 1 value per patient/month)
adjusted_costs_long_o <- adjusted_costs_data_o %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  group_by(subject_id, month_col) %>%
  summarise(adjust_visit_costs = sum(adjust_visit_costs), .groups = "drop")

adjusted_costs_matrix_o <- adjusted_costs_long_o %>%
  pivot_wider(names_from = month_col, values_from = adjust_visit_costs)

# Merge Adjusted Visits with Exposure Grid
month_cols <- grep("^month_", names(cohort_grid_omop), value = TRUE)

merged_matrix_c_o <- cohort_grid_omop %>%
  left_join(adjusted_costs_matrix_o, by = "subject_id", suffix = c("_exp", "_visit"))

# Coalesce exposure and adjusted visits
for (col in month_cols) {
  visit_col <- paste0(col, "_visit")
  exp_col <- paste0(col, "_exp")
  if (!(visit_col %in% names(merged_matrix_c_o))) {
    merged_matrix_c_o[[visit_col]] <- NA_real_
  }
  merged_matrix_c_o[[col]] <- dplyr::coalesce(
    as.numeric(merged_matrix_c_o[[visit_col]]),
    as.numeric(merged_matrix_c_o[[exp_col]])
  )
}

final_matrix_adjusted_costs_o <- merged_matrix_c_o %>%
  select(subject_id, all_of(month_cols))

# Convert to long format
final_long_df_adjusted_costs_o <- final_matrix_adjusted_costs_o %>%
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "adjusted_visit_costs"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# But first, let's filter for users only
users_adj <- visit_data_omop %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  distinct(subject_id) %>% 
  pull(subject_id)

# Full patient-month grid (for all 24 months and all patients)
full_patient_month_grid_cost_o <- cohort_data_omop %>%
  filter(subject_id %in% users_adj) %>% 
  select(subject_id) %>%
  distinct() %>%
  crossing(month_after_index = 1:24)  # Ensures all 24 months per patient

# Summarise costs
total_costs_per_patient_month_o <- adjusted_costs_data_o %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_costs = sum(adjust_visit_costs), .groups = "drop")

# Merge to the full grid so missing patient-months get zero
adjusted_costs_full_o <- full_patient_month_grid_cost_o %>%
  left_join(total_costs_per_patient_month_o, by = c("subject_id", "month_after_index")) %>%
  mutate(total_costs = replace_na(total_costs, 0))

mean_costs_per_month_o <- adjusted_costs_full_o %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_cost = sum(total_costs)/n_distinct(subject_id), .groups = "drop")

fc_grouped_costs <- function(data, indices) {
  sampled <- data[indices, ]
  mean(sampled$total_costs)
}

set.seed(123)

boot_user_costs_summary_per_month_o <- adjusted_costs_full_o |>
  group_by(month_after_index) |>
  group_split() |>
  map_df(function(df_month) {
    boot_result <- boot(df_month, statistic = fc_grouped_costs, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "norm")
    
    tibble(
      month_after_index = unique(df_month$month_after_index),
      mean_costs_boot = round(boot_result$t0, 3),
      lower_ci = round(as.numeric(boot_ci$normal[2]), 3),
      upper_ci = round(as.numeric(boot_ci$normal[3]), 3)
    )
  })

mean_costs_combined_o <- mean_costs_per_month_o |>
  left_join(boot_user_costs_summary_per_month_o, by = "month_after_index")

adj_raw_costs_by_month <- mean_costs_combined_r |> 
  mutate(source = "Source")

adj_omop_costs_by_month <- mean_costs_combined_o |> 
  mutate(source = "OMOP")

# Combine into one long-format dataframe
adj_combined_costs <- bind_rows(adj_raw_costs_by_month, adj_omop_costs_by_month)



### Plot ----
(plot_costs_adj <- ggplot(adj_combined_costs, 
                       aes(x = month_after_index, 
                           y = mean_visit_cost, color = source)) +
   geom_line(size = 0.8) +
   geom_point(size = 1.0) +
   geom_errorbar(aes(ymin=lower_ci,
                     ymax=upper_ci), width = 0.4)+
   labs(
     title = "Mean Costs of Primary Care Visit by Data Source",
     x = "Month After Index Fracture",
     y = "Mean Costs of Primary Care Visits (£)",
     color = "Data Source",
     caption = "Adjusted for partial follow-up: included patients with ≥15.22 days/month.
     \nPartial months scaled by exposure;
     \n full months or death-months used raw counts."
   ) +
   scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c")) +
   scale_y_continuous(breaks = seq(0,120 , by = 20), limits = c(0, 120)) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", hjust = 0.5),
     axis.title = element_text(size = 12),
     axis.text = element_text(size = 10),
     legend.title = element_text(size = 11),
     legend.text = element_text(size = 10)
   ))

ggsave(here::here(
  "Comparison Analysis", "Results", "Plots",
  "Mean_costs_per_data_source_adjusted.PNG"),
  plot = plot_costs_adj,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)

### Save the output datasets ----
adjusted_costs_list <- list(
  mean_costs_combined_o = mean_costs_combined_o,
  mean_costs_combined_r = mean_costs_combined_r
)
write_xlsx(adjusted_costs_list, here::here("Comparison Analysis", "Results", "Output_monthly_mean_costs-Adjusted.xlsx"))


df_test_r <- full_join(adjusted_visits_data_r, adjusted_costs_data_r_c, by = c("patid", "month_after_index", "specialty_reduced")) |>
  mutate(adjusted_visits.x =  round(adjusted_visits.x, 4),
         adjusted_visits.y =  round(adjusted_visits.y, 4)) |> 
  mutate(check_visits = if_else(adjusted_visits.x == adjusted_visits.y, TRUE, FALSE))

df_test_o <- full_join(adjusted_visits_data_o, adjusted_costs_data_o, by = c("subject_id", "month_after_index", "specialty_reduced")) |>
  mutate(adjusted_visits.x =  round(adjusted_visits.x, 4),
         adjusted_visits.y =  round(adjusted_visits.y, 4)) |> 
  mutate(check_visits = if_else(adjusted_visits.x == adjusted_visits.y, TRUE, FALSE))

df_combo_t <- inner_join(adjusted_costs_data_r_c, adjusted_costs_data_o |> rename("patid" = "subject_id"),
                         by = c("patid", "month_after_index", "specialty_reduced")) |> 
  mutate(diff_adjusted_visits = adjusted_visits.y - adjusted_visits.x,
         diff_adjusted_costs = adjust_visit_costs.y - adjust_visit_costs.x)
