# Visits ----
visit_data_reduced_MIN <- visit_data_reduced %>% select(
  subject_id, index_date, visit_detail_start_date, user_status,is_clinically_relevant,
  specialty_reduced, follow_up_end, death_date
)
## Unweighted ----
# Create base 24-month matrix per patient
cohort_grid_omop <- cohort_data_omop |>
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) |>
  rowwise() |>
  mutate(month_vector = list(0:23)) |>
  unnest(month_vector) |>
  mutate(month_col = paste0("month_", month_vector + 1),
         value = ifelse(month_vector < months_exposed, 0, NA_real_)) |>  # <- Use NA_real_ for numeric cost
  select(subject_id, month_col, value) |>
  pivot_wider(names_from = month_col, values_from = value)

# Calculate month of visit and total visit cost
# Only for users, so we filter out non-users
unadj_user_visits_summary <- visit_data |>
  filter(is_clinically_relevant == 1, user_status == "User") %>%
  mutate(index_date = ymd(index_date),
         follow_up_end = ymd(follow_up_end)) |>
  left_join(cohort_data_omop |> mutate(index_date = ymd(index_date),
                                       follow_up_end = ymd(follow_up_end)),
            by = c("subject_id", "follow_up_end", "index_date")) |>
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1)) |>
  group_by(subject_id, specialty, month_after_index) |>
  summarise(
    visit_count = n(),
    .groups = 'drop'
  ) |>
  # mutate(visit_count = if_else(specialty == "Not Clinically Relevant", 0, visit_count)) %>%
  group_by(subject_id, month_after_index) %>% 
  summarise(total_visits = sum(visit_count)) %>% 
  mutate(month_col = paste0("month_", month_after_index)) |>
  select(subject_id, month_col, total_visits)

# Convert to wide format
unadj_visit_matrix <- unadj_user_visits_summary |>
  pivot_wider(names_from = month_col, values_from = total_visits)

# Replace base zeros with actual costs
month_cols <- grep("^month_", names(cohort_grid_omop), value = TRUE)

merged_matrix_visits <- cohort_grid_omop |>
  left_join(unadj_visit_matrix, by = "subject_id", suffix = c("_exp", "_visit"))

# Coalesce cost columns over exposure base
for (col in month_cols) {
  exp_col <- paste0(col, "_exp")
  visit_col <- paste0(col, "_visit")
  
  merged_matrix_visits[[col]] <- dplyr::coalesce(merged_matrix_visits[[visit_col]], merged_matrix_visits[[exp_col]])
}

# Final reshape
final_matrix_visits <- merged_matrix_visits |>
  select(subject_id, all_of(month_cols))

final_long_df_visits <- final_matrix_visits |>
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) |>
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "tot_visits"
  ) |>
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# But first, let's filter for users only
users_ids <- visit_data %>%
  filter(is_clinically_relevant == 1, user_status == "User") %>%
  distinct(subject_id) %>% 
  pull(subject_id)

final_long_df_visits <- final_long_df_visits %>% 
  filter(subject_id %in% users_ids)

# Generate monthly summary statistics
monthly_visit_summary_unweighted <- final_long_df_visits %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visits   = mean(tot_visits), 
    min_visits    = min(tot_visits),
    max_visits    = max(tot_visits), 
    sd_visits     = sd(tot_visits), 
    q1_visits     = quantile(tot_visits, 0.25), 
    median_visitst = median(tot_visits), 
    q3_visits     = quantile(tot_visits, 0.75), 
    n       = n_distinct(subject_id),
    .groups = "drop"
  ) %>% 
  mutate(
    se = sd_visits / sqrt(n),
    lower = mean_visits - qt(0.975, df = n - 1) * se,
    upper = mean_visits + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))

m1_12o <- monthly_visit_summary_unweighted |> 
  filter(month_after_index %in% c(1:12))

sum(m1_12o$mean_visits)
rm(m1_12o)

m12_24o <- monthly_visit_summary_unweighted |> 
  filter(month_after_index %in% c(13:24))
sum(m12_24o$mean_visits)
rm(m12_24o)

## Weighted ----
# Monthly summary stats for visits (filtered)
# Prepare data with filtering on follow_up_end
# Compute days exposed + month info
patient_months <- visit_data_reduced_MIN_c %>%
  filter(user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, death_date), lubridate::ymd)) %>%
  mutate(
    total_days = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(total_days / 30.44),
    days_into_final_month = total_days - 30.44 * (final_month_after_index - 1),
    died_in_final_month = !is.na(death_date) &
      (floor(as.numeric(
        difftime(
          death_date, index_date, units = "days")) / 30.44) + 1 == final_month_after_index)
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

# Adjusted visit counts
adjusted_visits <- visit_data_reduced_MIN %>%
  filter(is_clinically_relevant == 1, user_status == "User") %>%
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

# Merge with patient-months at risk
final_df <- patient_months %>%
  left_join(adjusted_visits, by = c("subject_id", "month_after_index" = "visit_month_after_index")) %>%
  mutate(total_adjusted_visits = replace_na(total_adjusted_visits, 0)) %>% 
  distinct()

# Now calculate accurate stats
monthly_visit_summary_weighted <- final_df %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visits = mean(total_adjusted_visits),
    min_visits = min(total_adjusted_visits),
    max_visits = max(total_adjusted_visits),
    sd_visits = sd(total_adjusted_visits),
    q1_visits = quantile(total_adjusted_visits, 0.25),
    median_visits = median(total_adjusted_visits),
    q3_visits = quantile(total_adjusted_visits, 0.75),
    n = n_distinct(subject_id),
    .groups = "drop"
  ) %>% 
  mutate(
    se = sd_visits / sqrt(n),
    lower = mean_visits - qt(0.975, df = n - 1) * se,
    upper = mean_visits + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))

m1_12o <- monthly_visit_summary_weighted |> 
  filter(month_after_index %in% c(1:12))

sum(m1_12o$mean_visits)
rm(m12_24o)

m12_24o <- monthly_visit_summary_weighted |> 
  filter(month_after_index %in% c(13:24))
sum(m12_24o$mean_visits)
rm(m12_24o)


# Export
monthly_visits_list <- list(
  monthly_visit_unweighted = monthly_visit_summary_unweighted,
  monthly_visit_weighted = monthly_visit_summary_weighted
)

# Write to an Excel file
write_xlsx(monthly_visits_list, here::here("OMOP Analysis", "Results", "Output_monthly_visits_OMOP.xlsx"))


# Costs ----
visit_data_reduced_MIN_c <- visit_data_reduced %>% 
  select(
    subject_id, index_date, visit_detail_start_date, user_status, is_clinically_relevant,
    specialty_reduced, follow_up_end, unit_cost, death_date
  )

## Unweighted ----
# Step 1: Create base 24-month matrix per patient
cohort_grid_omop <- cohort_data_omop |>
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) |>
  rowwise() |>
  mutate(month_vector = list(0:23)) |>
  unnest(month_vector) |>
  mutate(month_col = paste0("month_", month_vector + 1),
         value = ifelse(month_vector < months_exposed, 0, NA_real_)) |>  # <- Use NA_real_ for numeric cost
  select(subject_id, month_col, value) |>
  pivot_wider(names_from = month_col, values_from = value)

# Step 2: Calculate month of visit and total visit cost
visit_data_with_month_omop <- visit_data |>
  mutate(index_date = ymd(index_date),
         follow_up_end = ymd(follow_up_end)) |>
  left_join(cohort_data_omop |> mutate(index_date = ymd(index_date),
                                       follow_up_end = ymd(follow_up_end)),
            by = c("subject_id", "follow_up_end", "index_date")) |>
  mutate(month_since_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1)) |>
  group_by(subject_id, month_since_index, specialty_reduced) |>
  summarise(
    visit_count = n(),
    unit_cost = unit_cost,
    .groups = 'drop') %>% 
  distinct() %>% 
  mutate(
    visit_cost = visit_count * unit_cost
  )|>
  group_by(subject_id, month_since_index) %>% 
  summarise(
    total_visit_cost = sum(visit_cost),
    .groups = 'drop'
  ) |>
  mutate(month_col = paste0("month_", month_since_index)) |>
  select(subject_id, month_col, total_visit_cost)

# Step 3: Pivot to wide format
visit_cost_matrix <- visit_data_with_month_omop |>
  pivot_wider(names_from = month_col, values_from = total_visit_cost)

# Step 4: Replace base zeros with actual costs
month_cols <- grep("^month_", names(cohort_grid_omop), value = TRUE)

merged_matrix_costs <- cohort_grid_omop |>
  left_join(visit_cost_matrix, by = "subject_id", suffix = c("_exp", "_visit"))

# Coalesce cost columns over exposure base
for (col in month_cols) {
  exp_col <- paste0(col, "_exp")
  cost_col <- paste0(col, "_visit")
  
  merged_matrix_costs[[col]] <- dplyr::coalesce(merged_matrix_costs[[cost_col]], merged_matrix_costs[[exp_col]])
}

# Final reshape
final_matrix_costs <- merged_matrix_costs |>
  select(subject_id, all_of(month_cols))

final_long_df_costs <- final_matrix_costs |>
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) |>
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visit_cost"
  ) |>
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# But first, let's filter for users only
users_ids <- visit_data %>%
  filter(is_clinically_relevant == 1, user_status == "User") %>%
  distinct(subject_id) %>% 
  pull(subject_id)

final_long_df_costs <- final_long_df_costs %>% 
  filter(subject_id %in% users_ids)

# STEP 4: Generate monthly summary statistics
monthly_summary_costs_unweighted <- final_long_df_costs %>%
  group_by(month_after_index) %>%
  summarise(
    mean_cost   = mean(visit_cost), 
    min_cost    = min(visit_cost), 
    max_cost    = max(visit_cost), 
    sd_cost     = sd(visit_cost), 
    q1_cost     = quantile(visit_cost, 0.25), 
    median_cost = median(visit_cost),
    q3_cost     = quantile(visit_cost, 0.75),
    n           = n_distinct(subject_id),
    .groups = "drop"
  )%>% 
  mutate(
    se = sd_cost / sqrt(n),
    lower = mean_cost - qt(0.975, df = n - 1) * se,
    upper = mean_cost + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))

m1_12o <- monthly_summary_costs_unweighted |> 
  filter(month_after_index %in% c(1:12))

sum(m1_12o$mean_cost)
rm(m1_12o)

m12_24o <- monthly_summary_costs_unweighted |> 
  filter(month_after_index %in% c(13:24))
sum(m12_24o$mean_cost)
rm(m12_24o)

## Weighted ----
patient_months <- visit_data_reduced_MIN_c %>%
  filter(user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, death_date), lubridate::ymd)) %>%
  mutate(
    total_days = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(total_days / 30.44),
    days_into_final_month = total_days - 30.44 * (final_month_after_index - 1),
    died_in_final_month = !is.na(death_date) &
      (floor(as.numeric(
        difftime(
          death_date, index_date, units = "days")) / 30.44) + 1 == final_month_after_index)
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

# Adjusted visit counts
adjusted_costs_monthly <- visit_data_reduced_MIN_c %>%
  filter(is_clinically_relevant == 1, user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), lubridate::ymd)) %>%
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(days_exposed_total / 30.44),
    month_after_index = floor(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1,
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


# Merge with patient-months at risk
final_df_costs <- patient_months %>%
  left_join(adjusted_costs_monthly, by = c("subject_id", "month_after_index")) %>%
  mutate(adjust_visit_costs = replace_na(adjust_visit_costs, 0)) %>% 
  distinct() %>% 
  select(subject_id, month_after_index, adjust_visit_costs)

# Find summary stats
monthly_cost_summary_weighted <- final_df_costs %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs = sum(adjust_visit_costs)/n_distinct(subject_id), 
    min_costs = min(adjust_visit_costs),
    max_costs = max(adjust_visit_costs),
    sd_costs = sd(adjust_visit_costs), 
    q1_costs = quantile(adjust_visit_costs, 0.25), 
    median_costs = median(adjust_visit_costs), 
    q3_costs = quantile(adjust_visit_costs, 0.75), 
    n = n_distinct(subject_id),
    .groups = "drop"
  ) %>% 
  mutate(
    se = sd_costs / sqrt(n),
    lower = mean_costs - qt(0.975, df = n - 1) * se,
    upper = mean_costs + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))


# Export
monthly_costs_list <- list(
  monthly_costs_unweighted = monthly_summary_costs_unweighted,
  monthly_costs_weighted = monthly_cost_summary_weighted
)

# Write to an Excel file
write_xlsx(monthly_costs_list, here::here("OMOP Analysis", "Results", "Output_monthly_costs_OMOP.xlsx"))

# Specialty count ----
monthly_category_count <- visit_data %>% 
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1)) |>
  group_by(month_after_index, specialty_reduced) %>% 
  summarise(count_category = n(),
            .groups = "drop") %>% 
  mutate(percent_category = round(count_category / sum(count_category) * 100, 2))

monthly_specific_role_count <- visit_data %>% 
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1)) |>
  group_by(month_after_index, specialty_reduced, specialty) %>% 
  summarise(count_specific_role = n(),
            .groups = "drop") %>% 
  mutate(percent_specific_role = count_specific_role / sum(count_specific_role) * 100) 


monthly_healthcare_role_list <- list(
  monthly_category_count = monthly_category_count,
  monthly_specific_role_count = monthly_specific_role_count
)

write_xlsx(monthly_healthcare_role_list, here::here("OMOP Analysis", "Results", "Output_monthly_healthcare_roles-OMOP.xlsx"))

library(gtsummary)

(gt_monthly_healthcare_roles_count <- visit_data %>%
    mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1)) |>
    tbl_summary(
      by = month_after_index,
      include = c(specialty_reduced, specialty),
      label = list(
        specialty_reduced = "Healthcare Category", 
        specialty = "Healthcare Specific Role"
      ),
      statistic = all_categorical() ~ "{n} ({p}%)",
      sort = all_categorical() ~ "frequency"
    ) %>%
    modify_header(label ~ "**Table of Healthcare Roles**") %>%
    bold_labels())

gt_monthly_healthcare_roles_count_df <- as_tibble(gt_monthly_healthcare_roles_count)

rio::export(gt_monthly_healthcare_roles_count_df, here::here("OMOP Analysis", "Results", "Output_GT_monthly_healthcare_roles-OMOP.xlsx"))
