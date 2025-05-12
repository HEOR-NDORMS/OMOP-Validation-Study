library(dplyr)
library(tidyr)
library(lubridate)
library(tidylog)

# Import data ----
visit_data_raw <- rio::import("H:/Projects/Validation/Data/visit_data_min_raw.csv")

glimpse(visit_data_raw)

visit_data_omop <- rio::import("H:/Projects/Validation/Data/visit_data_min_omop.csv")
glimpse(visit_data_omop)

cohort_data_raw <- rio::import("H:/Projects/Validation/Data/cohort_table_raw.csv")
cohort_data_omop <- rio::import("H:/Projects/Validation/Data/cohort_table_cprd.csv")

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
visit_data_with_month_raw <- visit_data_raw %>%
  filter(specialty != "Not Clinically Relevant", 
         user_status == "User") |> 
  mutate(index_date = ymd(index_date),
         follow_up_end = ymd(follow_up_end)) |> 
  left_join(cohort_data_raw |> mutate(index_date = ymd(index_date),
                                      follow_up_end = ymd(follow_up_end)), by = c("patid", "follow_up_end", "index_date")) %>%
  mutate(month_since_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
  # filter(month_since_index >= 0) %>%
  # filter(month_since_index < 24) |> 
  group_by(patid, month_since_index) %>%
  summarise(visits = n(), .groups = 'drop') %>%
  mutate(month_col = paste0("month_", month_since_index)) %>%
  select(patid, month_col, visits)

# Step 3: Pivot visit counts to wide format
visit_matrix_raw <- visit_data_with_month_raw %>%
  pivot_wider(names_from = month_col, values_from = visits)

# Step: Identify original month columns
month_cols <- grep("^month_", names(cohort_grid_raw), value = TRUE)

# Step: Join with suffixes (_exp and _visit)
merged_matrix <- cohort_grid_raw %>%
  left_join(visit_matrix_raw, by = "patid", suffix = c("_exp", "_visit"))

# Step: Create new final columns by coalescing visit over exposure
for (col in month_cols) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  
  # Create or overwrite the base column with coalesced value
  merged_matrix[[col]] <- dplyr::coalesce(merged_matrix[[visit_col]], merged_matrix[[exp_col]])
}

# Step: Keep only final result
final_matrix_raw <- merged_matrix %>%
  select(patid, all_of(month_cols))

final_long_df_raw <- final_matrix_raw %>%
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
# Source: https://online.stat.psu.edu/stat500/lesson/5/5.4
mean_visits_per_month_r <- final_long_df_raw %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visits = sum(visit_count) / n_distinct(patid),  # Actual mean
    # mean_visits = mean(visits),
    n = n(),
    se = sd(visit_count) / sqrt(n),
    lower = mean_visits - qt(0.975, df = n - 1) * se,
    upper = mean_visits + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

