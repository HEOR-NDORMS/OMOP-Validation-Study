# Identify eligible patients with at least 30.44 days in the second year
eligible_patients <- cohort_table %>% 
  mutate(exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days"))) %>% 
  mutate(
    included_second_year = exposed_days > 365.25 & (exposed_days - 365.25) >= 30.44 & exposed_days <= 730
  ) %>%
  filter(included_second_year) %>%  # Keep only patients who qualify
  select(patid)

# Filter visit data to only include eligible patients
visit_data_filtered_t <- visit_data_reduced_MIN %>%
  inner_join(eligible_patients, by = "patid") %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(month_after_index = interval(index_date, consdate) %/% months(1)) %>% 
  # to ensure that at least 1 full month has passed (NOT SURE!)
  filter(month_after_index >= 1)

# Compute total visits per patient per month
total_visits_per_patient_month <- visit_data_filtered_t %>%
  group_by(patid, month_after_index) %>%
  summarise(
    visit_count = n(), .groups = "drop")

# Compute mean visits per month across patients
mean_visits_per_month <- total_visits_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_count = sum(visit_count) / n_distinct(patid),
    .groups = "drop"
  )

# Compute total visits per specialty per month
visits_per_specialty_month <- visit_data_filtered_t %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(
    total_visits_specialty = n(), 
    .groups = "drop")

# Merge total mean visits per month
plots_users_mean_visits_specialty <- visits_per_specialty_month %>%
  left_join(mean_visits_per_month, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE)) %>%
  mutate(prop_mean = mean_visit_count * proportion)

# Step 5: Plot the mean visits per month with specialty proportions
(plot1 <- ggplot(
  plots_users_mean_visits_specialty,
  aes(
    x = month_after_index,
    y = mean_visit_count * proportion,
    fill = specialty_reduced
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

# Generate the underlying data table
underlying_table <- plots_users_mean_visits_specialty %>%
  select(month_after_index, specialty_reduced, total_visits_specialty, mean_visit_count, proportion, prop_mean)

# Compute summary statistics for `prop_mean`
summary_stats_for_underlying_table <- underlying_table %>%
  # group_by(specialty_reduced) %>%
  summarise(
    mean = mean(mean_visit_count, na.rm = TRUE),
    min = min(mean_visit_count, na.rm = TRUE),
    q1 = quantile(mean_visit_count, 0.25, na.rm = TRUE),
    median = median(mean_visit_count, na.rm = TRUE),
    q3 = quantile(mean_visit_count, 0.75, na.rm = TRUE),
    max = max(mean_visit_count, na.rm = TRUE),
    sd = sd(mean_visit_count, na.rm = TRUE),
    .groups = "drop"
  )
summary_stats_for_underlying_table_specialty <- underlying_table %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(
    mean = mean(prop_mean, na.rm = TRUE),
    min = min(prop_mean, na.rm = TRUE),
    q1 = quantile(prop_mean, 0.25, na.rm = TRUE),
    median = median(prop_mean, na.rm = TRUE),
    q3 = quantile(prop_mean, 0.75, na.rm = TRUE),
    max = max(prop_mean, na.rm = TRUE),
    sd = sd(prop_mean, na.rm = TRUE),
    .groups = "drop"
  )

### PLOT Tot Visits
visit_data_reduced_user2 <- visit_data_filtered_t %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = interval(
    index_date, consdate) %/% months(1)) %>%
  group_by(month_after_index) %>%
  summarise(visit_count = n(), .groups = "drop")

(plot2 <- ggplot(
  visit_data_reduced_user2,
  aes(
    x = month_after_index,
    y = visit_count
  )
) +
    geom_bar(
      stat = "identity",
      fill = "#1f77b4"
    ) + # Blue color for the bars
    labs(
      x = "Month After Index",
      y = "Total Visits"
    ) +
    theme_minimal())
