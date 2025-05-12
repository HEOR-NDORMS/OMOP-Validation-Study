visits_data_raw <- rio::import("H:/Projects/Validation/Data/visits_data_raw.csv")

# Step 1: Create base 24-month matrix per patient
cohort_grid_raw <- cohort_data_raw %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(month_col = paste0("month_", month_vector + 1),
         value = ifelse(month_vector < months_exposed, 0, NA_integer_)) %>%
  select(patid, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# Step 2: Calculate month of visit relative to index date
costs_visit_data_with_month_raw <- visits_data_raw %>%
  filter(specialty != "Not Clinically Relevant", 
         user_status == "User") |> 
  mutate(index_date = ymd(index_date),
         follow_up_end = ymd(follow_up_end)) |> 
  left_join(cohort_data_raw |> mutate(index_date = ymd(index_date),
                                      follow_up_end = ymd(follow_up_end)), by = c("patid", "follow_up_end", "index_date")) %>%
  mutate(month_since_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
  # filter(month_since_index >= 0) %>%
  # filter(month_since_index < 24) |> 
  group_by(patid, month_since_index, specialty) %>%
  summarise(visits = n(), 
            unit_cost = unit_cost,
            visits_costs = visits * unit_cost,
            .groups = 'drop') %>%
  distinct() |> 
  group_by(patid, month_since_index) |> 
  summarise(total_visits_cost = sum(visits_costs),
            .groups = "drop") |> 
  mutate(month_col = paste0("month_", month_since_index)) %>%
  select(patid, month_col, total_visits_cost)

# Step 3: Pivot visit counts to wide format
costs_matrix_raw <- costs_visit_data_with_month_raw %>%
  pivot_wider(names_from = month_col, values_from = total_visits_cost)

# Step: Identify original month columns
month_cols <- grep("^month_", names(cohort_grid_raw), value = TRUE)

# Step: Join with suffixes (_exp and _visit)
merged_matrix_cost <- cohort_grid_raw %>%
  left_join(costs_matrix_raw, by = "patid", suffix = c("_exp", "_visit"))

# Step: Create new final columns by coalescing visit over exposure
for (col in month_cols) {
  exp_col <- paste0(col, "_exp")
  cost_col <- paste0(col, "_visit")
  
  # Create or overwrite the base column with coalesced value
  merged_matrix_cost[[col]] <- dplyr::coalesce(merged_matrix_cost[[cost_col]], merged_matrix_cost[[exp_col]])
}

# Step: Keep only final result
final_matrix_cost_raw <- merged_matrix_cost %>%
  select(patid, all_of(month_cols))

final_long_df_cost_raw <- final_matrix_cost_raw %>%
  # Replace all NAs with 0
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) %>%
  
  # Pivot to long format
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visits_cost_count"
  ) %>%
  
  # Optionally, convert month column to numeric (e.g., "month_1" -> 1)
  mutate(month_after_index = as.integer(gsub("month_", "", month)))


# Compute the mean number of visits per month across fully contributing patients
# Source: https://online.stat.psu.edu/stat500/lesson/5/5.4
mean_costs_per_month_r <- final_long_df_cost_raw %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs = sum(visits_cost_count) / n_distinct(patid),  # Actual mean
    # mean_visits = mean(visits),
    n = n(),
    se = sd(visits_cost_count) / sqrt(n),
    lower = mean_costs - qt(0.975, df = n - 1) * se,
    upper = mean_costs + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

