#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Differences between OMOP and Raw using visits data - Unadjusted analysis
# Date: 10.05.2025
# Last date edited: 10.06.2025
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

# Import data ----
visit_data_omop <- rio::import("H:/Projects/Validation/Data/visit_data_omop.csv")
cohort_data_omop <- rio::import("H:/Projects/Validation/Data/cohort_table_omop.csv")

visits_data_raw <- rio::import("H:/Projects/Validation/Data/visit_data_raw.csv")
cohort_data_raw <- rio::import("H:/Projects/Validation/Data/cohort_table_raw.csv")

# Unadjusted Visits ----
## Source ----
# Step 1: Create base 24-month matrix per patient
cohort_grid_raw <- cohort_data_raw |>
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) |>
  rowwise() |>
  mutate(month_vector = list(0:23)) |>
  unnest(month_vector) |>
  mutate(month_col = paste0("month_", month_vector + 1),
         value = ifelse(month_vector < months_exposed, 0, NA_integer_)) |>
  select(patid, month_col, value) |>
  pivot_wider(names_from = month_col, values_from = value)

visit_data_with_month_raw <- visits_data_raw |>
  mutate(index_date = ymd(index_date),
         follow_up_end = ymd(follow_up_end)) |>
  left_join(cohort_data_raw |> mutate(index_date = ymd(index_date),
                                      follow_up_end = ymd(follow_up_end)), 
            by = c("patid", "follow_up_end", "index_date")) |>
  mutate(month_since_index = trunc((as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1)) |>
  group_by(patid, month_since_index) |>
  summarise(visits = sum(if_else(specialty == "Not Clinically Relevant", 0L, 1L)), .groups = 'drop') |>
  mutate(month_col = paste0("month_", month_since_index)) |>
  select(patid, month_col, visits)


# Step 3: Pivot visit counts to wide format
visit_matrix_raw <- visit_data_with_month_raw |>
  pivot_wider(names_from = month_col, values_from = visits)

# Step: Identify original month columns
month_cols <- grep("^month_", names(cohort_grid_raw), value = TRUE)

# Step: Join with suffixes (_exp and _visit)
merged_matrix <- cohort_grid_raw |>
  left_join(visit_matrix_raw, by = "patid", suffix = c("_exp", "_visit"))

# Step: Create new final columns by coalescing visit over exposure
for (col in month_cols) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  
  # Create or overwrite the base column with coalesced value
  merged_matrix[[col]] <- dplyr::coalesce(merged_matrix[[visit_col]], merged_matrix[[exp_col]])
}

# Step: Keep only final result
final_matrix_raw <- merged_matrix |>
  select(patid, all_of(month_cols))

final_long_df_raw <- final_matrix_raw |>
  # Replace all NAs with 0
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) |>
  
  # Pivot to long format
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visit_count"
  ) |>
  
  # Optionally, convert month column to numeric (e.g., "month_1" -> 1)
  mutate(month_after_index = as.integer(gsub("month_", "", month)))


# Compute the mean number of visits per month across fully contributing patients
# Source: https://online.stat.psu.edu/stat500/lesson/5/5.4
mean_visits_per_month_r <- final_long_df_raw |>
  group_by(month_after_index) |>
  summarise(
    mean_visits = sum(visit_count) / n_distinct(patid),  # Actual mean
    .groups = "drop"
  )

fc_grouped_visits <- function(data, indices) {
  sampled <- data[indices, ]
  mean(sampled$visit_count)
}

set.seed(626)

boot_user_visits_summary_per_month_r <- final_long_df_raw |>
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
  left_join(boot_user_visits_summary_per_month_r, by = "month_after_index")

m1_12 <- mean_visits_combined_r |> 
  filter(month_after_index %in% c(1:12))
sum(m1_12$mean_visits)
rm(m1_12)

m12_24 <- mean_visits_combined_r |> 
  filter(month_after_index %in% c(13:24))
sum(m12_24$mean_visits)
rm(m12_24)

## OMOP ----
# Step 1: Create base 24-month matrix per patient
cohort_grid_omop <- cohort_data_omop |>
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) |>
  rowwise() |>
  mutate(month_vector = list(0:23)) |>
  unnest(month_vector) |>
  mutate(month_col = paste0("month_", month_vector + 1),
         value = ifelse(month_vector < months_exposed, 0, NA_integer_)) |>
  select(subject_id, month_col, value) |>
  pivot_wider(names_from = month_col, values_from = value)

# Step 2: Calculate month of visit relative to index date
visit_data_with_month_omop <- visit_data_omop |>
  # filter(specialty != "Not Clinically Relevant", user_status == "User") |> 
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end)
  )

# Ensure cohort_data_omop is clean before join
common_cols <- intersect(
  setdiff(names(visit_data_with_month_omop), c("subject_id", "index_date", "follow_up_end")),
  setdiff(names(cohort_data_omop), c("subject_id", "index_date", "follow_up_end"))
)

cohort_data_omop <- cohort_data_omop |>
  select(-all_of(common_cols))

# Now do the join
visit_data_with_month_omop <- visit_data_with_month_omop |>
  left_join(cohort_data_omop, by = c("subject_id", "follow_up_end", "index_date"))

visit_data_with_month_omop <- visit_data_with_month_omop |>
  mutate(month_since_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) |>
  # filter(month_since_index >= 0, month_since_index < 24) |>
  group_by(subject_id, month_since_index) |>
  summarise(visits = sum(if_else(specialty == "Not Clinically Relevant", 0L, 1L)), .groups = 'drop') |>
  mutate(month_col = paste0("month_", month_since_index)) |>
  select(subject_id, month_col, visits)

# Step 3: Pivot visit counts to wide format
visit_matrix_omop <- visit_data_with_month_omop |>
  pivot_wider(names_from = month_col, values_from = visits)

# Step 4: Replace 0s in exposure grid with actual visit counts
# Step: Identify original month columns
month_cols <- grep("^month_", names(cohort_grid_raw), value = TRUE)

# Step: Join with suffixes (_exp and _visit)
merged_matrix_omop <- cohort_grid_omop |>
  left_join(visit_matrix_omop, by = "subject_id", suffix = c("_exp", "_visit"))

# Step: Create new final columns by coalescing visit over exposure
for (col in month_cols) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  
  # Create or overwrite the base column with coalesced value
  merged_matrix_omop[[col]] <- dplyr::coalesce(merged_matrix_omop[[visit_col]], merged_matrix_omop[[exp_col]])
}

# Step: Keep only final result
final_matrix_omop <- merged_matrix_omop |>
  select(subject_id, all_of(month_cols))

final_long_df_omop <- final_matrix_omop |>
  # Replace all NAs with 0
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) |>
  
  # Pivot to long format
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visit_count"
  ) |>
  
  # Optionally, convert month column to numeric (e.g., "month_1" -> 1)
  mutate(month_after_index = as.integer(gsub("month_", "", month)))


# Compute the mean number of visits per month across fully contributing patients
mean_visits_per_month_o <- final_long_df_omop |>
  group_by(month_after_index) |>
  summarise(
    mean_visit_count = sum(visit_count) / n_distinct(subject_id),  # Actual mean
    .groups = "drop"
  ) |> 
  rename("mean_visits" = "mean_visit_count")

fc_grouped_visits <- function(data, indices) {
  sampled <- data[indices, ]
  mean(sampled$visit_count)
}

set.seed(626)

boot_user_visits_summary_per_month_o <- final_long_df_omop |>
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

m1_12o <- mean_visits_combined_o |> 
  filter(month_after_index %in% c(1:12))
sum(m1_12o$mean_visits)
rm(m1_12o)
m12_24o <- mean_visits_combined_o |> 
  filter(month_after_index %in% c(13:24))
sum(m12_24o$mean_visits)
rm(m12_24o)

### Plot ----
# Add a source label to each
mean_visits_raw  <- mean_visits_combined_r  %>% mutate(source = "Source")
mean_visits_omop <- mean_visits_combined_o %>% mutate(source = "OMOP")

# Combine into one long-format dataframe
combined_mean_visits <- bind_rows(mean_visits_raw, mean_visits_omop)

library(ggplot2)

# Plot
(plot1 <- ggplot(combined_mean_visits, 
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
      color = "Data Source"
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
  "Mean_visits_per_data_source_unadjusted.PNG"),
  plot = plot1,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)
### Save the output datasets ----
unadjusted_visits_list <- list(
  mean_visits_combined_o = mean_visits_combined_o,
  mean_visits_combined_r = mean_visits_combined_r
)
write_xlsx(unadjusted_visits_list, here::here("Comparison Analysis", "Results", "Output_monthly_mean_visits-Unadjusted.xlsx"))

# Unadjusted costs ----
## Source ----
# Step 1: Create base 24-month matrix per patient
cohort_grid_raw <- cohort_data_raw |>
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) |>
  rowwise() |>
  mutate(month_vector = list(0:23)) |>
  unnest(month_vector) |>
  mutate(month_col = paste0("month_", month_vector + 1),
         value = ifelse(month_vector < months_exposed, 0, NA_integer_)) |>
  select(patid, month_col, value) |>
  pivot_wider(names_from = month_col, values_from = value)

# Step 2: Calculate month of visit relative to index date
costs_visit_data_with_month_raw <- visits_data_raw |>
  # filter(specialty != "Not Clinically Relevant", 
  #        user_status == "User") |> 
  mutate(index_date = ymd(index_date),
         follow_up_end = ymd(follow_up_end)) |> 
  left_join(cohort_data_raw |> mutate(index_date = ymd(index_date),
                                      follow_up_end = ymd(follow_up_end)), by = c("patid", "follow_up_end", "index_date")) |>
  mutate(month_since_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) |>
  # filter(month_since_index >= 0) |>
  # filter(month_since_index < 24) |> 
  group_by(patid, month_since_index, specialty) |>
  summarise(visits = n(), 
            unit_cost = unit_cost,  
            .groups = 'drop') |>
  mutate(visits_costs = visits * unit_cost) |>
  mutate(visits_costs = if_else(specialty == "Not Clinically Relevant", 0, visits_costs)) |> 
  distinct() |> 
  group_by(patid, month_since_index) |> 
  summarise(total_visits_cost = sum(visits_costs),
            .groups = "drop") |> 
  mutate(month_col = paste0("month_", month_since_index)) |>
  select(patid, month_col, total_visits_cost)

# Step 3: Pivot visit counts to wide format
costs_matrix_raw <- costs_visit_data_with_month_raw |>
  pivot_wider(names_from = month_col, values_from = total_visits_cost)

# Step: Identify original month columns
month_cols <- grep("^month_", names(cohort_grid_raw), value = TRUE)

# Step: Join with suffixes (_exp and _visit)
merged_matrix_cost <- cohort_grid_raw |>
  left_join(costs_matrix_raw, by = "patid", suffix = c("_exp", "_visit"))

# Step: Create new final columns by coalescing visit over exposure
for (col in month_cols) {
  exp_col <- paste0(col, "_exp")
  cost_col <- paste0(col, "_visit")
  
  # Create or overwrite the base column with coalesced value
  merged_matrix_cost[[col]] <- dplyr::coalesce(merged_matrix_cost[[cost_col]], merged_matrix_cost[[exp_col]])
}

# Step: Keep only final result
final_matrix_cost_raw <- merged_matrix_cost |>
  select(patid, all_of(month_cols))

final_long_df_cost_raw <- final_matrix_cost_raw |>
  # Replace all NAs with 0
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) |>
  
  # Pivot to long format
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visits_cost_count"
  ) |>
  
  # Optionally, convert month column to numeric (e.g., "month_1" -> 1)
  mutate(month_after_index = as.integer(gsub("month_", "", month)))


# Compute the mean number of visits per month across fully contributing patients
# Source: https://online.stat.psu.edu/stat500/lesson/5/5.4
mean_costs_per_month_r <- final_long_df_cost_raw |>
  group_by(month_after_index) |>
  summarise(
    mean_costs = sum(visits_cost_count) / n_distinct(patid),  # Actual mean
    .groups = "drop"
  )

fc_grouped_costs <- function(data, indices) {
  sampled <- data[indices, ]
  mean(sampled$visits_cost_count)
}

set.seed(626)

boot_user_costs_summary_per_month_r <- final_long_df_cost_raw |>
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

m1_12_c <- mean_costs_combined_r |> 
  filter(month_after_index %in% c(1:12))
sum(m1_12_c$mean_costs)
rm(m1_12_c)
m12_24_c <- mean_costs_combined_r |> 
  filter(month_after_index %in% c(13:24))
sum(m12_24_c$mean_costs)
rm(m12_24_c)

## OMOP ----

# Step 1: Create base 24-month matrix per patient
cohort_grid_omop <- cohort_data_omop |>
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) |>
  rowwise() |>
  mutate(month_vector = list(0:23)) |>
  unnest(month_vector) |>
  mutate(month_col = paste0("month_", month_vector + 1),
         value = ifelse(month_vector < months_exposed, 0, NA_integer_)) |>
  select(subject_id, month_col, value) |>
  pivot_wider(names_from = month_col, values_from = value)

# Step 2: Calculate month of visit relative to index date
costs_visit_data_with_month_omop <- visit_data_omop |>
  filter(specialty != "Not Clinically Relevant", 
         user_status == "User") |> 
  mutate(index_date = ymd(index_date),
         follow_up_end = ymd(follow_up_end)) |> 
  left_join(cohort_data_omop |> mutate(index_date = ymd(index_date),
                                       follow_up_end = ymd(follow_up_end)), by = c("subject_id", "follow_up_end", "index_date")) |>
  mutate(month_since_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) |>
  # filter(month_since_index >= 0) |>
  # filter(month_since_index < 24) |> 
  group_by(subject_id, month_since_index, specialty) |>
  summarise(visits = n(), 
            unit_cost = unit_cost,  
            .groups = 'drop') |>
  mutate(visits_costs = visits * unit_cost) |> 
  distinct() |> 
  group_by(subject_id, month_since_index) |> 
  summarise(total_visits_cost = sum(visits_costs),
            .groups = "drop") |> 
  mutate(month_col = paste0("month_", month_since_index)) |>
  select(subject_id, month_col, total_visits_cost)

# Step 3: Pivot visit counts to wide format
costs_matrix_omop <- costs_visit_data_with_month_omop |>
  pivot_wider(names_from = month_col, values_from = total_visits_cost)

# Step: Identify original month columns
month_cols <- grep("^month_", names(cohort_grid_omop), value = TRUE)

# Step: Join with suffixes (_exp and _visit)
merged_matrix_cost_o <- cohort_grid_omop |>
  left_join(costs_matrix_omop, by = "subject_id", suffix = c("_exp", "_visit"))

# Step: Create new final columns by coalescing visit over exposure
for (col in month_cols) {
  exp_col <- paste0(col, "_exp")
  cost_col <- paste0(col, "_visit")
  
  # Create or overwrite the base column with coalesced value
  merged_matrix_cost_o[[col]] <- dplyr::coalesce(merged_matrix_cost_o[[cost_col]], merged_matrix_cost_o[[exp_col]])
}

# Step: Keep only final result
final_matrix_cost_omop <- merged_matrix_cost_o |>
  select(subject_id, all_of(month_cols))

final_long_df_cost_omop <- final_matrix_cost_omop |>
  # Replace all NAs with 0
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) |>
  
  # Pivot to long format
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visits_cost_count"
  ) |>
  
  # Optionally, convert month column to numeric (e.g., "month_1" -> 1)
  mutate(month_after_index = as.integer(gsub("month_", "", month)))


# Compute the mean number of visits per month across fully contributing patients
# Source: https://online.stat.psu.edu/stat500/lesson/5/5.4
mean_costs_per_month_o <- final_long_df_cost_omop |>
  group_by(month_after_index) |>
  summarise(
    mean_costs = sum(visits_cost_count) / n_distinct(subject_id),  # Actual mean
    .groups = "drop"
  )

fc_grouped_costs <- function(data, indices) {
  sampled <- data[indices, ]
  mean(sampled$visits_cost_count)
}

set.seed(626)

boot_user_costs_summary_per_month_o <- final_long_df_cost_omop |>
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

m1_12_co <- mean_costs_combined_o |> 
  filter(month_after_index %in% c(1:12))
sum(m1_12_co$mean_costs)
rm(m1_12_co)
m12_24_co <- mean_costs_combined_o |> 
  filter(month_after_index %in% c(13:24))
sum(m12_24_co$mean_costs)
rm(m12_24_co)

raw_costs_by_month <- mean_costs_combined_r |> 
  mutate(source = "Source")

omop_costs_by_month <- mean_costs_combined_o |> 
  mutate(source = "OMOP")

# Combine into one long-format dataframe
combined_costs <- bind_rows(raw_costs_by_month, omop_costs_by_month)

### Plot ----
(plot_costs1 <- ggplot(combined_costs, 
                       aes(x = month_after_index, 
                           y = mean_costs, color = source)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.0) +
    geom_errorbar(aes(ymin=lower_ci,
                      ymax=upper_ci), width = 0.4)+
    labs(
      title = "Mean Costs of Primary Care Visit by Data Source",
      x = "Month After Index Fracture",
      y = "Mean Costs of Primary Care Visits (£)",
      color = "Data Source"
    ) +
    scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c")) +
    scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
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
  "Mean_costs_per_data_source_unadjustedt.PNG"),
  plot = plot_costs1,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)

### Save the output datasets ----
unadjusted_costs_list <- list(
  mean_costs_combined_o = mean_costs_combined_o,
  mean_costs_combined_r = mean_costs_combined_r
)
write_xlsx(unadjusted_costs_list, here::here("Comparison Analysis", "Results", "Output_monthly_mean_costs-Unadjusted.xlsx"))
