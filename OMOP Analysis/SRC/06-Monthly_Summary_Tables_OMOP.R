# Visits ----
visit_data_reduced_MIN <- visit_data_reduced %>% select(
  subject_id, index_date, visit_detail_start_date, user_status,
  specialty_reduced, follow_up_end, death_date
)
## Unweighted ----
# Step 1: Get patient-month grid (from cohort_data_omop)
patient_months <- cohort_data_omop %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_after_index = list(1:(months_exposed))) %>%
  unnest(month_after_index) %>%
  select(subject_id, month_after_index)

# Step 2: Summarise actual visits
visits_summary <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1) %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visits = n(), .groups = "drop")

# Step 3: Merge to include zeros
full_patient_months <- patient_months %>%
  left_join(visits_summary, by = c("subject_id", "month_after_index")) %>%
  mutate(total_visits = replace_na(total_visits, 0))

# Step 4: Calculate summary stats
monthly_visit_summary_unweighted <- full_patient_months %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visits = round(mean(total_visits), 2),
    min_visits = round(min(total_visits), 2),
    max_visits = round(max(total_visits), 2),
    sd_visits = round(sd(total_visits), 2),
    q1_visits = round(quantile(total_visits, 0.25), 2),
    median_visits = round(median(total_visits), 2),
    q3_visits = round(quantile(total_visits, 0.75), 2),
    n_pts = n_distinct(subject_id),
    .groups = "drop"
  )

## Weighted ----
# Monthly summary stats for visits (filtered)
# Prepare data with filtering on follow_up_end
# STEP 1: Compute days exposed + month info
patient_months <- visit_data_reduced_MIN_c %>%
  filter(user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, death_date), lubridate::ymd)) %>%
  mutate(
    total_days = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(total_days / 30.44),
    days_into_final_month = total_days - 30.44 * (final_month_after_index - 1),
    died_in_final_month = !is.na(death_date) &
      (floor(as.numeric(difftime(death_date, index_date, units = "days")) / 30.44) + 1 == final_month_after_index)
  ) %>%
  rowwise() %>%
  mutate(month_after_index = list(1:final_month_after_index)) %>%
  unnest(month_after_index) %>%
  mutate(
    valid_exposure = case_when(
      month_after_index < final_month_after_index ~ TRUE,
      month_after_index == final_month_after_index &
        (died_in_final_month | days_into_final_month >= 15.22) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(valid_exposure) %>%
  select(subject_id, month_after_index)

# STEP 2: Adjusted visit counts
adjusted_visits <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), lubridate::ymd)) %>%
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(days_exposed_total / 30.44),
    visit_month_after_index = floor(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1,
    died_in_final_month = !is.na(death_date) &
      floor(as.numeric(difftime(death_date, index_date, units = "days")) / 30.44) + 1 == final_month_after_index,
    days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
    adjusted_visit_count = case_when(
      visit_month_after_index != final_month_after_index ~ 1,
      died_in_final_month ~ 1,
      days_into_final_month >= 30.44 ~ 1,
      days_into_final_month >= 15.22 ~ (1 / days_into_final_month) * 30.44,
      TRUE ~ 0
    )
  ) %>%
  filter(!(visit_month_after_index == final_month_after_index & adjusted_visit_count == 0)) %>%
  group_by(subject_id, visit_month_after_index) %>%
  summarise(total_adjusted_visits = sum(adjusted_visit_count), .groups = "drop")

# STEP 3: Merge with patient-months at risk
final_df <- patient_months %>%
  left_join(adjusted_visits, by = c("subject_id", "month_after_index" = "visit_month_after_index")) %>%
  mutate(total_adjusted_visits = replace_na(total_adjusted_visits, 0)) %>% 
  distinct()

# STEP 4: Now calculate accurate stats
monthly_visit_summary_weighted <- final_df %>%
  group_by(month_after_index) %>%
  summarise(
    mean = round(mean(total_adjusted_visits), 2),
    min = round(min(total_adjusted_visits), 2),
    max = round(max(total_adjusted_visits), 2),
    sd = round(sd(total_adjusted_visits), 2),
    q1 = round(quantile(total_adjusted_visits, 0.25), 2),
    median = round(median(total_adjusted_visits), 4),
    q3 = round(quantile(total_adjusted_visits, 0.75), 2),
    n_pts = n_distinct(subject_id),
    .groups = "drop"
  )

# Export
monthly_visits_list <- list(
  monthly_visit_unweighted = monthly_visit_summary_unweighted,
  monthly_visit_weighted = monthly_visit_summary_weighted
)

# Write to an Excel file
write_xlsx(monthly_visits_list, here::here("CPRD Raw Analysis", "Results", "Output_monthly_visits_Raw.xlsx"))


# Costs ----
visit_data_reduced_MIN_c <- visit_data_reduced %>% 
  select(
    subject_id, index_date, visit_detail_start_date, user_status,
    specialty_reduced, follow_up_end, unit_cost, death_date
  )

## Unweighted ----
cohort_grid_costs <- cohort_data_omop |>
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) |>
  rowwise() |>
  mutate(month_vector = list(0:23)) |>
  unnest(month_vector) |>
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)
  ) |>
  select(subject_id, month_col, value) |>
  pivot_wider(names_from = month_col, values_from = value)

# STEP 1: Create exposed patient-month dataset (denominator)
exposed_patient_months_cost <- cohort_grid_costs %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "exposure"
  ) %>%
  filter(!is.na(exposure)) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month))) %>%
  select(subject_id, month_after_index)

# STEP 2: Compute total costs per patient-month
total_costs_per_patient_month <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1) %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_costs = sum(unit_cost), .groups = "drop")

# STEP 3: Join with exposure and fill missing (i.e., 0-cost months)
monthly_costs_full <- exposed_patient_months_cost %>%
  left_join(total_costs_per_patient_month, by = c("subject_id", "month_after_index")) %>%
  mutate(total_costs = replace_na(total_costs, 0))

# STEP 4: Generate monthly summary statistics
monthly_summary_costs_unweighted <- monthly_costs_full %>%
  group_by(month_after_index) %>%
  summarise(
    mean_cost   = round(mean(total_costs), 2),
    min_cost    = round(min(total_costs), 2),
    max_cost    = round(max(total_costs), 2),
    sd_cost     = round(sd(total_costs), 2),
    q1_cost     = round(quantile(total_costs, 0.25), 2),
    median_cost = round(median(total_costs), 2),
    q3_cost     = round(quantile(total_costs, 0.75), 2),
    n_pts       = n_distinct(subject_id),
    .groups = "drop"
  )

## Weighted ----
# Step 1: Preprocess visits and calculate adjusted costs as before
adjusted_costs <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>% 
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), lubridate::ymd)) %>%
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(days_exposed_total / 30.44),
    died_during_followup = !is.na(death_date) & death_date <= follow_up_end,
    died_in_final_month = died_during_followup &
      (floor(as.numeric(difftime(death_date, index_date, units = "days")) / 30.44) + 1 == final_month_after_index),
    days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
    visit_month_after_index = floor(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1,
    visit_count = 1L,
    adjusted_visit_count = case_when(
      visit_month_after_index != final_month_after_index ~ visit_count,
      died_in_final_month ~ visit_count,
      days_into_final_month >= 30.44 ~ visit_count,
      days_into_final_month >= 15.22 ~ (visit_count / days_into_final_month) * 30.44,
      TRUE ~ 0
    ),
    adjusted_cost = adjusted_visit_count * unit_cost
  ) %>%
  distinct() %>% 
  filter(!(visit_month_after_index == final_month_after_index & adjusted_visit_count == 0)) %>%
  group_by(subject_id, visit_month_after_index) %>%
  summarise(
    total_adjusted_cost = sum(adjusted_cost, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Create full patient-month grid for exposed months
patient_month_grid <- visit_data_reduced_MIN_c %>%
  mutate(across(c(index_date, follow_up_end), lubridate::ymd)) %>%
  mutate(
    months_exposed = ceiling(as.numeric(follow_up_end - index_date) / 30.44)
  ) %>%
  select(subject_id, months_exposed) %>%
  distinct() %>%
  rowwise() %>%
  mutate(month_list = list(1:months_exposed)) %>%
  unnest(month_list) %>%
  rename(visit_month_after_index = month_list)

# Step 3: Join with adjusted costs and fill zeros for missing months
full_patient_costs <- patient_month_grid %>%
  left_join(adjusted_costs, by = c("subject_id", "visit_month_after_index")) %>%
  mutate(total_adjusted_cost = replace_na(total_adjusted_cost, 0))

# Step 4: Final monthly cost summary
monthly_cost_summary_weighted <- full_patient_costs %>%
  group_by(visit_month_after_index) %>%
  summarise(
    mean_cost = round(mean(total_adjusted_cost), 2),
    min_cost = round(min(total_adjusted_cost), 2),
    max_cost = round(max(total_adjusted_cost), 2),
    sd_cost = round(sd(total_adjusted_cost), 2),
    q1_cost = round(quantile(total_adjusted_cost, 0.25), 2),
    median_cost = round(median(total_adjusted_cost), 4),
    q3_cost = round(quantile(total_adjusted_cost, 0.75), 2),
    n_pts = n(),
    .groups = "drop"
  )


# Export
monthly_costs_list <- list(
  monthly_costs_unweighted = monthly_summary_costs_unweighted,
  monthly_costs_weighted = monthly_cost_summary_weighted
)

# Write to an Excel file
write_xlsx(monthly_costs_list, here::here("CPRD Raw Analysis", "Results", "Output_monthly_costs_Raw.xlsx"))
