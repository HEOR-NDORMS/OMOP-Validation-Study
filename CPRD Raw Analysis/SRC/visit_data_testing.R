# Compute dynamic exposure time per visit

# Why use this -> pmin(consdate, follow_up_end)? 
## Ensures exposure time is specific to each visit

# If we use follow_up_end - index_date, the exposure time would be the 
# same for all visits of a patient, which doesn't reflect reality.
# Instead, we use pmin(consdate, follow_up_end) to ensure that exposure is 
# calculated up to the visit date, but not beyond the follow-up period.
# Handles visits occurring before follow-up end

# Patients have multiple visits (consdate) before follow_up_end.
# Using pmin(consdate, follow_up_end) - index_date ensures 
# we capture only the time up to that visit.

# Prevents overestimation of exposure time
# Some patients have visits long before follow_up_end.
# If we use follow_up_end - index_date, it assumes all patients 
# contribute exposure time equally, even if they stopped contributing visits early.
# Using pmin(consdate, follow_up_end) ensures exposure is limited at their actual visit times.
visit_data_t <- visit_data %>%
  mutate(
    # To ensures exposure time is specific to each visit.
    raw_exposure_interval = interval(index_date, pmin(consdate, follow_up_end)),
    exposed_yrs = as.numeric(pmin(consdate, follow_up_end) - index_date) / 360.25,
    exposed_months = exposed_yrs * 12,  # Convert years to months
    excluded_flag = if_else(exposed_months < 1, "Excluded", "Included")
  )

# Identify patients who are only excluded
excluded_only_patients <- visit_data_t %>%
  group_by(patid) %>%
  summarise(all_excluded = all(excluded_flag == "Excluded"), .groups = "drop") %>%
  filter(all_excluded) %>%
  # Extract only patient IDs
  pull(patid)  

# Filter the dataset to include only these patients
excluded_only_patients_df <- visit_data_t %>%
  filter(patid %in% excluded_only_patients)

# Exclude patients with exposure < 1 month
visit_data_filtered_t <- visit_data_t %>%
  filter(exposed_months >= 1)

# Compute year and month after index for visit grouping
visit_data_filtered_t <- visit_data_filtered_t %>%
  mutate(
    month_after_index = interval(index_date, consdate) %/% months(1),
    year_after_index = case_when(
      month_after_index %in% c(1:12) ~ 1,
      TRUE ~ 2
    )
    # year_after_index_2 = floor(interval(index_date, consdate) / years(1)) + 1
  )

# Count visits for the first, second year, and overall exposure time
visit_counts_by_year_t <- visit_data_filtered_t %>%
  group_by(patid, year_after_index) %>%
  summarise(
    visits_per_year = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = year_after_index, values_from = visits_per_year,
              names_prefix = "year_",
              values_fill = list(visits_per_year = 0))

# Compute total visits over the entire exposure period
total_visits_overall_t <- visit_data_filtered_t %>%
  group_by(patid) %>%
  summarise(total_visits = n(), .groups = "drop")

# Merge everything back to visit_data
visit_data_final_t <- visit_data_filtered_t %>%
  left_join(visit_counts_by_year_t, by = "patid") %>%
  left_join(total_visits_overall_t, by = "patid")

visit_data_final_t.1 <- visit_data_final_t %>% 
  group_by(patid, index_date, specialty_source_value_summarised) %>% 
  summarise(visit_c = n()) %>%
  ungroup()

visit_data_reduced_MIN <- visit_data_reduced %>% select(
  patid, index_date, consdate, user_status,
  specialty_reduced, follow_up_end
)

### PLOT visits per month/specialty   -------

# Filter for relevant specialties and compute months since index
visit_data_reduced_user_t <- visit_data_final_t %>%
  filter(specialty_source_value_summarised != "Not Clinically Relevant", 
         user_status == "User") %>%
  group_by(patid, month_after_index, specialty_source_value_summarised) %>%
  summarise(visit_count = n(), .groups = "drop")


# Step 1: Compute the total number of visits per patient per month 
# (sum across specialties)
total_visits_per_patient_month_t <- visit_data_reduced_user_t %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute the mean number of visits per month across all patients
mean_visits_per_month_t <- total_visits_per_patient_month_t %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_count = sum(total_visits) / n_distinct(patid),
    .groups = "drop"
  )

# Step 3: Compute the total number of visits per specialty per month
visits_per_specialty_month_t <- visit_data_reduced_user_t %>%
  group_by(month_after_index, specialty_source_value_summarised) %>%
  summarise(total_visits_specialty = sum(visit_count), .groups = "drop")

# Step 4: Merge the total mean visits per month
plots_users_mean_visits_specialty_t <- visits_per_specialty_month_t %>%
  left_join(mean_visits_per_month_t, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE)) %>% 
  mutate(prop_mean = mean_visit_count * proportion)

# Step 5: Plot the mean visits per month with specialty proportions
(plot1_t <- ggplot(
  plots_users_mean_visits_specialty_t,
  aes(
    x = month_after_index,
    y = mean_visit_count * proportion,
    fill = specialty_source_value_summarised
  )
) +
    geom_bar(
      stat = "identity",
      position = "stack"
    ) +
    scale_fill_manual(values = c(
      "General Practitioner" = "#B4CDCD",
      "Nurse" = "#377eb8",
      "Other" = "#253494"
    )) +
    labs(
      x = "Month After Index",
      y = "Mean Visits per User",
      fill = "Specialty"
    ) +
    theme_minimal())

# Step 1: Merge `exposed_yrs` into the user-specific data
visit_data_reduced_user_t <- visit_data_final_t %>%
  filter(specialty_source_value_summarised != "Not Clinically Relevant") %>%
  group_by(patid, month_after_index, specialty_source_value_summarised) %>%
  summarise(
    visit_count = n(),
    exposed_months = exposed_months, # Keep the exposed_yrs for each patient
    visit_count_f = first(visit_count),
    .groups = "drop"
  )

# Step 2: Compute the total number of visits per patient per month
total_visits_per_patient_month_t <- visit_data_reduced_user_t %>%
  group_by(patid, month_after_index) %>%
  summarise(
    total_visits = first(visit_count),
    exposed_months = exposed_months, # Get exposure years for each patient
    .groups = "drop"
  )

# Step 3: Compute the exposure-weighted mean number of visits per month across all patients
mean_visits_per_month_weighted_t <- total_visits_per_patient_month_t %>%
  group_by(month_after_index) %>%
  summarise(
    weighted_visits = total_visits / exposed_months,
    .groups = "drop"
  )

# Step 4: Compute the total number of visits per specialty per month
visits_per_specialty_month_t <- visit_data_reduced_user_t %>%
  group_by(month_after_index, specialty_source_value_summarised) %>%
  summarise(
    total_visits_specialty = sum(visit_count),
    exposed_months = exposed_months, # Get exposure years for each patient
    .groups = "drop"
  )

# Step 5: Merge the total exposure-weighted mean visits per month
plots_users_mean_visits_specialty_weighted <- visits_per_specialty_month %>%
  left_join(mean_visits_per_month_weighted, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE))

# Step 6: Plot the exposure-weighted mean visits per month with specialty proportions
(plot3 <- ggplot(
  plots_users_mean_visits_specialty_weighted,
  aes(x = month_after_index, y = weighted_visits * proportion, fill = specialty_reduced)
) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c(
      "General Practitioner" = "#B4CDCD",
      "Nurse" = "#377eb8",
      "Other" = "#253494"
    )) +
    labs(x = "Month After Index", y = "Exposure-Weighted Visits per User", fill = "Specialty") +
    theme_minimal())
