visits_data_omop <- rio::import("H:/Projects/Validation/Data/visits_data_omop.csv")

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
