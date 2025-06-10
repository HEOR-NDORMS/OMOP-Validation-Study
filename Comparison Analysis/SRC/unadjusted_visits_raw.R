library(dplyr)
library(tidyr)
library(lubridate)
library(tidylog)
library(ggplot2)
library(ggExtra)
library(ggtext)

# Import data ----
# visit_data_omop <- rio::import("H:/Projects/Validation/Data/visit_data_omop.csv")
# # glimpse(visit_data_omop)
# cohort_data_omop <- rio::import("H:/Projects/Validation/Data/cohort_table_omop.csv")
# 
# visits_data_raw <- rio::import("H:/Projects/Validation/Data/visit_data_raw.csv")
# cohort_data_raw <- rio::import("H:/Projects/Validation/Data/cohort_table_raw.csv")



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

library(boot)
library(tidytable)
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

m12_24 <- mean_visits_combined_r |> 
  filter(month_after_index %in% c(13:24))
sum(m12_24$mean_visits)


