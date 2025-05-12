library(dplyr)
library(tidyr)
library(lubridate)
library(tidylog)

# Import data ----

visit_data_omop <- rio::import("H:/Projects/Validation/Data/visits_data_omop.csv")
glimpse(visit_data_omop)
cohort_data_omop <- rio::import("H:/Projects/Validation/Data/cohort_table_cprd.csv")


# OMOP
# Step 1: Create base 24-month matrix per patient
cohort_grid_omop <- cohort_data_omop %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(month_col = paste0("month_", month_vector + 1),
         value = ifelse(month_vector < months_exposed, 0, NA_integer_)) %>%
  select(subject_id, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# Step 2: Calculate month of visit relative to index date
visit_data_with_month_omop <- visit_data_omop %>%
  filter(specialty != "Not Clinically Relevant", 
         user_status == "User") |> 
  mutate(index_date = ymd(index_date),
         follow_up_end = ymd(follow_up_end)) |> 
  left_join(cohort_data_omop |> mutate(index_date = ymd(index_date),
                                      follow_up_end = ymd(follow_up_end)), by = c("subject_id", "follow_up_end", "index_date")) %>%
  mutate(month_since_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
  # filter(month_since_index >= 0, month_since_index < 24) %>%
  group_by(subject_id, month_since_index) %>%
  summarise(visits = n(), .groups = 'drop') %>%
  mutate(month_col = paste0("month_", month_since_index)) %>%
  select(subject_id, month_col, visits)

# Step 3: Pivot visit counts to wide format
visit_matrix_omop <- visit_data_with_month_omop %>%
  pivot_wider(names_from = month_col, values_from = visits)

# Step 4: Replace 0s in exposure grid with actual visit counts
# Step: Identify original month columns
month_cols <- grep("^month_", names(cohort_grid_raw), value = TRUE)

# Step: Join with suffixes (_exp and _visit)
merged_matrix_omop <- cohort_grid_omop %>%
  left_join(visit_matrix_omop, by = "subject_id", suffix = c("_exp", "_visit"))

# Step: Create new final columns by coalescing visit over exposure
for (col in month_cols) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  
  # Create or overwrite the base column with coalesced value
  merged_matrix_omop[[col]] <- dplyr::coalesce(merged_matrix_omop[[visit_col]], merged_matrix_omop[[exp_col]])
}

# Step: Keep only final result
final_matrix_omop <- merged_matrix_omop %>%
  select(subject_id, all_of(month_cols))

final_long_df_omop <- final_matrix_omop %>%
  # Replace all NAs with 0
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) %>%
  
  # Pivot to long format
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visit_count"
  ) %>%
  
  # Optionally, convert month column to numeric (e.g., "month_1" -> 1)
  mutate(month_after_index = as.integer(gsub("month_", "", month)))


# Compute the mean number of visits per month across fully contributing patients
mean_visits_per_month_o <- final_long_df_omop %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_count = sum(visit_count) / n_distinct(subject_id),  # Actual mean
    n = n(),
    se = sd(visit_count) / sqrt(n),
    lower = mean_visit_count - qt(0.975, df = n - 1) * se,
    upper = mean_visit_count + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  ) |> 
  rename("mean_visits" = "mean_visit_count")
