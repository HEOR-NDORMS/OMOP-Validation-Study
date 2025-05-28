library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# ----------------------------
# STEP 1: Create 24-Month Exposure Matrix Per Patient
# ----------------------------
cohort_grid_raw <- cohort_table_raw |>
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) |>
  rowwise() |>
  mutate(month_vector = list(0:23)) |>
  unnest(month_vector) |>
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_integer_)
  ) |>
  select(patid, month_col, value) |>
  pivot_wider(names_from = month_col, values_from = value)

# ----------------------------
# STEP 2: Create Visit Matrix
# ----------------------------
visit_data_with_month_raw <- visit_data |>
  filter(specialty != "Not Clinically Relevant", user_status == "User") |> 
  mutate(across(c(index_date, follow_up_end), ymd)) |> 
  left_join(cohort_table_raw |> mutate(across(c(index_date, follow_up_end), ymd)), 
            by = c("patid", "index_date", "follow_up_end")) |>
  mutate(month_since_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1) |>
  group_by(patid, month_since_index) |>
  summarise(visits = n(), .groups = 'drop') |>
  mutate(month_col = paste0("month_", month_since_index)) |>
  select(patid, month_col, visits)

visit_matrix_raw <- visit_data_with_month_raw |>
  pivot_wider(names_from = month_col, values_from = visits)

# ----------------------------
# STEP 3: Merge and Coalesce Visit + Exposure Data
# ----------------------------
month_cols <- grep("^month_", names(cohort_grid_raw), value = TRUE)

merged_matrix <- cohort_grid_raw |>
  left_join(visit_matrix_raw, by = "patid", suffix = c("_exp", "_visit"))

for (col in month_cols) {
  merged_matrix[[col]] <- dplyr::coalesce(
    merged_matrix[[paste0(col, "_visit")]],
    merged_matrix[[paste0(col, "_exp")]]
  )
}

final_matrix_raw <- merged_matrix |>
  select(patid, all_of(month_cols))

# ----------------------------
# STEP 4: Convert to Long Format
# ----------------------------
final_long_df_raw <- final_matrix_raw |>
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) |>
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visit_count"
  ) |>
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# ----------------------------
# STEP 5: Mean Visits Per Month (Unadjusted)
# ----------------------------
mean_visits_per_month_r <- final_long_df_raw |>
  group_by(month_after_index) |>
  summarise(
    mean_visits = sum(visit_count) / n_distinct(patid),
    .groups = "drop"
  )

# ----------------------------
# STEP 6: Specialty-Specific Visit Proportions
# ----------------------------
visit_data_reduced_user.1 <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Total visits per patient/month
total_visits_per_patient_month.1 <- visit_data_reduced_user.1 %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Proportion per specialty per patient per month
visits_within_patient_month <- visit_data_reduced_user.1 %>%
  left_join(total_visits_per_patient_month.1, by = c("patid", "month_after_index")) %>%
  mutate(prop_within_patient = visit_count / total_visits)

# Average proportions across patients
avg_prop_specialty_month <- visits_within_patient_month %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(avg_prop = mean(prop_within_patient, na.rm = TRUE), .groups = "drop")

# ----------------------------
# STEP 7: Combine with Mean Visit Counts
# ----------------------------
final_combined_prop_df <- avg_prop_specialty_month %>%
  left_join(mean_visits_per_month_r, by = "month_after_index") %>%
  mutate(prop_mean = mean_visits * avg_prop)

# ----------------------------
# STEP 8: Plotting
# ----------------------------
(plot1 <- ggplot(
  final_combined_prop_df,
  aes(
    x = factor(month_after_index),
    y = prop_mean,
    fill = specialty_reduced
  )
) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_fill_manual(values = c(
    "General Practitioner" = "#B4CDCD",
    "Nurse" = "#377eb8",
    "Other" = "#253494"
  )) +
  labs(
    x = "Month After Index",
    y = "Mean Visits per Healthcare Service User",
    fill = "Specialty"
  ) +
  scale_y_continuous(limits = c(0, 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(hjust = 0.5, size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.position = "top",
    panel.spacing = unit(2, "lines")
  ))


library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# ----------------------------
# STEP 1: Create 24-Month Exposure Matrix Per Patient
# ----------------------------
cohort_grid_raw <- cohort_table_raw %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)
  ) %>%
  select(patid, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# ----------------------------
# STEP 2: Adjusted Visit Count Calculation
# ----------------------------

adjusted_visits_data_r <- visit_data %>%
  filter(specialty != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
  
  # Step 1: Compute month of each visit and total exposure
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(days_exposed_total / 30.44),
    month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  
  # Step 2: Compute exposure in final month
  mutate(
    days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
    died_in_final_month = !is.na(death_date) &
      floor(as.numeric(death_date - index_date) / 30.44) + 1 == final_month_after_index
  ) %>%
  
  # Step 3: Count visits per patient/month/specialty
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop") %>%
  
  # Step 4: Join back final_month info per patient
  left_join(
    visit_data %>%
      mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
      mutate(
        days_exposed_total = as.numeric(follow_up_end - index_date),
        final_month_after_index = ceiling(days_exposed_total / 30.44),
        days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
        died_in_final_month = !is.na(death_date) &
          floor(as.numeric(death_date - index_date) / 30.44) + 1 == final_month_after_index
      ) %>%
      distinct(patid, final_month_after_index, days_into_final_month, died_in_final_month),
    by = "patid"
  ) %>%
  
  # Step 5: Adjust visits in final month
  mutate(
    adjusted_visits = case_when(
      month_after_index != final_month_after_index ~ visit_count,
      died_in_final_month ~ visit_count,
      days_into_final_month >= 30.44 ~ visit_count,
      days_into_final_month >= 15.22 ~ (visit_count / days_into_final_month) * 30.44,
      TRUE ~ 0
    )
  ) %>%
  
  # Drop months with 0 adjusted visits, as these are the ones with less than 15.22 days
  filter(adjusted_visits > 0) %>%
  # If true, then no adjusted was done as this was the last month visit, but 
  # if false then adjusted visits done based on the days_into_final_month
  mutate(visit_count == adjusted_visits)

# ----------------------------
# STEP 3: Create Adjusted Visit Matrix (Wide Format)
# ----------------------------
adjusted_visits_long <- adjusted_visits_data_r %>%
  group_by(patid, month_after_index) %>%
  summarise(adjusted_visits = sum(adjusted_visits), .groups = "drop") %>%
  mutate(month_col = paste0("month_", month_after_index))

adjusted_visit_matrix <- adjusted_visits_long %>%
  select(patid, month_col, adjusted_visits) %>%
  pivot_wider(names_from = month_col, values_from = adjusted_visits)

# ----------------------------
# STEP 4: Merge with Exposure Matrix and Coalesce
# ----------------------------
month_cols <- grep("^month_", names(cohort_grid_raw), value = TRUE)

merged_matrix <- cohort_grid_raw %>%
  left_join(adjusted_visit_matrix, by = "patid", suffix = c("_exp", "_visit"))

for (col in month_cols) {
  merged_matrix[[col]] <- dplyr::coalesce(
    merged_matrix[[paste0(col, "_visit")]],
    merged_matrix[[paste0(col, "_exp")]]
  )
}

final_matrix_adjusted <- merged_matrix %>%
  select(patid, all_of(month_cols))

# ----------------------------
# STEP 5: Convert to Long Format (Optional)
# ----------------------------
final_long_df_adjusted <- final_matrix_adjusted %>%
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "adjusted_visit_count"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# ----------------------------
# STEP 6: Mean Adjusted Visits per Month
# ----------------------------
total_visits_per_patient_month <- adjusted_visits_data_r %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(adjusted_visits), .groups = "drop")

mean_visits_per_month <- total_visits_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_count = sum(total_visits) / n_distinct(patid),
    .groups = "drop"
  )

# ----------------------------
# STEP 7: Specialty-Specific Proportions (Adjusted)
# ----------------------------
visits_per_specialty_month <- adjusted_visits_data_r %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_visits_specialty = sum(adjusted_visits), .groups = "drop")

plots_users_mean_visits_specialty <- visits_per_specialty_month %>%
  left_join(mean_visits_per_month, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(
    proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE),
    prop_mean = mean_visit_count * proportion
  ) %>%
  ungroup()

# ----------------------------
# STEP 8: Plot Adjusted Mean Visits by Specialty
# ----------------------------
(plot3 <- ggplot(
  plots_users_mean_visits_specialty,
  aes(x = factor(month_after_index), y = prop_mean, fill = specialty_reduced)
) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_fill_manual(values = c(
    "General Practitioner" = "#B4CDCD",
    "Nurse" = "#377eb8",
    "Other" = "#253494"
  )) +
  labs(
    x = "Month After Index",
    y = "Mean Visits per Healthcare Service User (Adjusted)",
    fill = "Specialty",
    caption = "Adjusted for partial follow-up: included patients with ≥15.22 days/month.\nPartial months scaled by exposure; full months or death-months used raw counts."
  ) +
  scale_y_continuous(limits = c(0, 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(hjust = 0.5, size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.position = "top",
    plot.caption = element_text(size = 12),
    panel.spacing = unit(2, "lines")
  ))

# Show the plot
print(plot3)


library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# ----------------------------
# STEP 1: Create 24-Month Exposure Matrix Per Patient
# ----------------------------
cohort_grid_costs <- cohort_table_raw |>
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) |>
  rowwise() |>
  mutate(month_vector = list(0:23)) |>
  unnest(month_vector) |>
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)
  ) |>
  select(patid, month_col, value) |>
  pivot_wider(names_from = month_col, values_from = value)

# ----------------------------
# STEP 2: Calculate Visit Costs Per Month (Raw)
# ----------------------------
visit_cost_data <- visit_data |>
  filter(specialty != "Not Clinically Relevant", user_status == "User") |> 
  mutate(across(c(index_date, follow_up_end), ymd)) |> 
  left_join(cohort_table_raw |> mutate(across(c(index_date, follow_up_end), ymd)), 
            by = c("patid", "index_date", "follow_up_end")) |>
  mutate(month_since_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1) |>
  group_by(patid, month_since_index, specialty) |>
  summarise(visits = n(), unit_cost = first(unit_cost), .groups = 'drop') |>
  mutate(visit_costs = visits * unit_cost) |>
  group_by(patid, month_since_index) |> 
  summarise(total_costs = sum(visit_costs), .groups = "drop") |>
  mutate(month_col = paste0("month_", month_since_index)) |>
  select(patid, month_col, total_costs)

# ----------------------------
# STEP 3: Merge Costs with Exposure Matrix (Fill 0 Where Exposed But No Visit)
# ----------------------------
costs_matrix <- cohort_grid_costs |>
  left_join(visit_cost_data, by = "patid", suffix = c("_exp", "_cost"))

month_cols <- paste0("month_", 1:24)

for (col in month_cols) {
  cost_col <- paste0(col, "_cost")
  exp_col <- paste0(col, "_exp")
  
  if (cost_col %in% names(costs_matrix) && exp_col %in% names(costs_matrix)) {
    costs_matrix[[col]] <- dplyr::coalesce(costs_matrix[[cost_col]], costs_matrix[[exp_col]])
  } else if (cost_col %in% names(costs_matrix)) {
    costs_matrix[[col]] <- costs_matrix[[cost_col]]
  } else if (exp_col %in% names(costs_matrix)) {
    costs_matrix[[col]] <- costs_matrix[[exp_col]]
  } else {
    costs_matrix[[col]] <- NA_real_
  }
}


for (col in month_cols) {
  costs_matrix[[col]] <- dplyr::coalesce(
    costs_matrix[[paste0(col, "_cost")]],
    costs_matrix[[paste0(col, "_exp")]]
  )
}


final_cost_matrix <- costs_matrix |>
  select(patid, all_of(month_cols))

# ----------------------------
# STEP 4: Convert to Long Format
# ----------------------------
final_long_cost_df <- final_cost_matrix |>
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) |>
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "total_costs"
  ) |>
  mutate(month_after_index = as.integer(gsub("month_", "", month))) %>% 
  distinct()

# ----------------------------
# STEP 5: Mean Costs per Month (Unadjusted)
# ----------------------------
mean_costs_per_month_r <- final_long_cost_df |>
  group_by(month_after_index) |>
  summarise(
    mean_costs = sum(total_costs) / n_distinct(patid),
    .groups = "drop"
  )

# ----------------------------
# STEP 6: Specialty-Specific Proportions
# ----------------------------
visit_data_reduced_user_cost <- visit_data_reduced_MIN |>
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") |>
  mutate(month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1) |>
  group_by(patid, month_after_index, specialty_reduced) |>
  summarise(
    visit_count = n(),
    unit_cost = first(unit_cost),
    .groups = "drop"
  ) |>
  mutate(visit_costs = visit_count * unit_cost)

# Total cost per patient/month
total_costs_per_patient_month <- visit_data_reduced_user_cost |>
  group_by(patid, month_after_index) |>
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# Proportion of cost per specialty within patient/month
costs_within_patient_month <- visit_data_reduced_user_cost |>
  left_join(total_costs_per_patient_month, by = c("patid", "month_after_index")) |>
  mutate(prop_within_patient = visit_costs / total_costs)

# Average proportion per specialty per month
avg_prop_cost_specialty <- costs_within_patient_month |>
  group_by(month_after_index, specialty_reduced) |>
  summarise(avg_prop = mean(prop_within_patient, na.rm = TRUE), .groups = "drop")

# ----------------------------
# STEP 7: Combine with Mean Cost
# ----------------------------
final_cost_plot_data <- avg_prop_cost_specialty |>
  left_join(mean_costs_per_month_r, by = "month_after_index") |>
  mutate(prop_mean = mean_costs * avg_prop)

# ----------------------------
# STEP 8: Plotting
# ----------------------------
(plot_cost_unadjusted <- ggplot(
  final_cost_plot_data,
  aes(x = factor(month_after_index), y = prop_mean, fill = specialty_reduced)
) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_fill_manual(values = c(
    "General Practitioner" = "#B4CDCD",
    "Nurse" = "#377eb8",
    "Other" = "#253494"
  )) +
  labs(
    x = "Month After Index",
    y = "Mean Costs per Healthcare Service User",
    fill = "Specialty"
  ) +
  scale_y_continuous(limits = c(0, 140)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(hjust = 0.5, size = 18),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 15),
    legend.position = "top",
    panel.spacing = unit(2, "lines")
  ))



library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# ---------------------------------------------
# 1. Prepare unit cost lookup table (one cost per specialty)
# ---------------------------------------------
unit_costs_lookup <- visit_data |>
  select(specialty, unit_cost) |>
  distinct()

# ---------------------------------------------
# 2. Clean and calculate raw monthly costs per patient
# ---------------------------------------------
costs_visit_data_with_month_raw <- visit_data |>
  filter(specialty != "Not Clinically Relevant", user_status == "User") |>
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end)
  ) |>
  left_join(
    cohort_table_raw |> 
      mutate(index_date = ymd(index_date), follow_up_end = ymd(follow_up_end)),
    by = c("patid", "follow_up_end", "index_date")
  ) |>
  mutate(month_since_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1) |>
  group_by(patid, month_since_index, specialty) |>
  summarise(visits = n(), .groups = "drop") |>
  left_join(unit_costs_lookup, by = "specialty") |>
  mutate(visits_costs = visits * unit_cost)

# ---------------------------------------------
# 3. Aggregate total costs per patient per month
# ---------------------------------------------
costs_matrix_long <- costs_visit_data_with_month_raw |>
  group_by(patid, month_since_index) |>
  summarise(total_visits_cost = sum(visits_costs, na.rm = TRUE), .groups = "drop") |>
  mutate(month_col = paste0("month_", month_since_index)) |>
  select(patid, month_col, total_visits_cost)

# ---------------------------------------------
# 4. Pivot wide so each month is a column
# ---------------------------------------------
costs_matrix_raw <- costs_matrix_long |>
  pivot_wider(names_from = month_col, values_from = total_visits_cost)

# ---------------------------------------------
# 5. Merge with cohort exposure grid
# ---------------------------------------------
costs_matrix <- cohort_grid_costs |>
  left_join(costs_matrix_raw, by = "patid", suffix = c("_exp", "_cost"))

# ---------------------------------------------
# 6. For each month, take either cost or exposure cost (if one is missing)
# ---------------------------------------------
month_cols <- paste0("month_", 1:24)

for (col in month_cols) {
  cost_col <- paste0(col, "_cost")
  exp_col <- paste0(col, "_exp")
  
  if (cost_col %in% names(costs_matrix) && exp_col %in% names(costs_matrix)) {
    costs_matrix[[col]] <- dplyr::coalesce(costs_matrix[[cost_col]], costs_matrix[[exp_col]])
  } else if (cost_col %in% names(costs_matrix)) {
    costs_matrix[[col]] <- costs_matrix[[cost_col]]
  } else if (exp_col %in% names(costs_matrix)) {
    costs_matrix[[col]] <- costs_matrix[[exp_col]]
  } else {
    costs_matrix[[col]] <- NA_real_
  }
}

# ---------------------------------------------
# 7. Pivot to long format for analysis
# ---------------------------------------------
costs_long <- costs_matrix |>
  pivot_longer(cols = all_of(month_cols), names_to = "month", values_to = "total_costs") |>
  mutate(
    month_after_index = as.integer(gsub("month_", "", month)),
    total_costs = replace_na(total_costs, 0)
  )

# ---------------------------------------------
# 8. Calculate mean monthly cost (unadjusted)
# ---------------------------------------------
mean_costs_per_month <- costs_long |>
  group_by(month_after_index) |>
  summarise(mean_costs = mean(total_costs, na.rm = TRUE), .groups = "drop")

# ---------------------------------------------
# 9. Plot unadjusted mean costs per patient by month
# ---------------------------------------------
ggplot(mean_costs_per_month, aes(x = month_after_index, y = mean_costs)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") +
  labs(
    title = "Unadjusted Mean Monthly Costs per Patient",
    x = "Month After Index",
    y = "Mean Cost"
  ) +
  theme_minimal()

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# STEP 0: Process visits per user, by month/specialty
unadj_visit_data_reduced_user_costs <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(
    index_date = ymd(index_date),
    consdate = ymd(consdate),
    month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct()

# STEP 1: Get full grid of all patient-month-specialty combinations
unadj_all_months <- 1:24  # Adjust if needed
unadj_all_pats <- unique(unadj_visit_data_reduced_user_costs$patid)
unadj_all_specialties <- unique(unadj_visit_data_reduced_user_costs$specialty_reduced)

unadj_full_grid <- expand.grid(
  patid = unadj_all_pats,
  month_after_index = unadj_all_months,
  specialty_reduced = unadj_all_specialties,
  stringsAsFactors = FALSE
)

# STEP 2: Left join to fill in missing combinations with 0 cost
unadj_visit_data_complete <- unadj_full_grid %>%
  left_join(unadj_visit_data_reduced_user_costs, by = c("patid", "month_after_index", "specialty_reduced")) %>%
  mutate(visit_costs = replace_na(visit_costs, 0))

# STEP 3: Total cost per patient per month
unadj_total_costs_per_patient_month <- unadj_visit_data_complete %>%
  group_by(patid, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# STEP 4: Mean cost per month (including zeros)
unadj_mean_costs_per_month <- unadj_total_costs_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(mean_costs = sum(total_costs)/n_distinct(patid), .groups = "drop")

# STEP 5: Total cost per specialty per month
unadj_costs_per_specialty_month <- unadj_visit_data_complete %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_costs_specialty = sum(visit_costs), .groups = "drop")

# STEP 6: Merge and weight mean by proportion
unadj_plots_users_mean_costs_specialty_weighted <- unadj_costs_per_specialty_month %>%
  left_join(unadj_mean_costs_per_month, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(
    proportion = total_costs_specialty / sum(total_costs_specialty, na.rm = TRUE),
    prop_mean = mean_costs * proportion
  )

# STEP 7: Plot
(plot_costs <- ggplot(
  unadj_plots_users_mean_costs_specialty_weighted,
  aes(x = factor(month_after_index), y = prop_mean, fill = specialty_reduced)
) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
    scale_fill_manual(values = c(
      "General Practitioner" = "#B4CDCD",
      "Nurse" = "#377eb8",
      "Other" = "#253494"
    )) +
    labs(
      title = "Mean Monthly Healthcare Costs per User by Specialty",
      x = "Month After Index",
      y = "Mean Costs per User (£)",
      fill = "Specialty"
    ) +
    scale_y_continuous(limits = c(0, 130)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 15),
      legend.position = "top",
      panel.spacing = unit(2, "lines")
    ))


# ----------------------------
# STEP 1: Create 24-Month Exposure Grid (per patient)
# ----------------------------
adj_cohort_grid_costs <- cohort_table_raw |>
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) |>
  rowwise() |>
  mutate(month_vector = list(0:23)) |>
  unnest(month_vector) |>
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)
  ) |>
  select(patid, month_col, value) |>
  pivot_wider(names_from = month_col, values_from = value)

# ----------------------------
# STEP 2: Cost Matrix - Visits x Unit Cost
# ----------------------------
adj_cost_data_with_month <- visit_data |>
  filter(specialty != "Not Clinically Relevant", user_status == "User") |>
  mutate(across(c(index_date, follow_up_end), ymd)) |>
  left_join(
    cohort_table_raw |> mutate(across(c(index_date, follow_up_end), ymd)),
    by = c("patid", "index_date", "follow_up_end")
  ) |>
  mutate(
    month_since_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1
  ) |>
  group_by(patid, month_since_index) |>
  summarise(
    visit_count = n(),
    unit_cost = mean(unit_cost),  # optional: if varying by visit
    total_cost = visit_count * unit_cost,
    .groups = "drop"
  ) |>
  mutate(month_col = paste0("month_", month_since_index)) |>
  select(patid, month_col, total_cost)

# ----------------------------
# STEP 3: Pivot to Wide Format
# ----------------------------
adj_cost_matrix_raw <- adj_cost_data_with_month |>
  pivot_wider(names_from = month_col, values_from = total_cost)

# ----------------------------
# STEP 4: Merge with Exposure Matrix
# ----------------------------
adj_costs_matrix <- adj_cohort_grid_costs |>
  left_join(adj_cost_matrix_raw, by = "patid", suffix = c("_exp", "_cost"))

month_cols <- grep("^month_", names(cohort_grid_costs), value = TRUE)

for (col in month_cols) {
  cost_col <- paste0(col, "_cost")
  exp_col <- paste0(col, "_exp")
  
  if (cost_col %in% names(adj_costs_matrix) && exp_col %in% names(adj_costs_matrix)) {
    adj_costs_matrix[[col]] <- dplyr::coalesce(adj_costs_matrix[[cost_col]], adj_costs_matrix[[exp_col]])
  } else if (cost_col %in% names(adj_costs_matrix)) {
    adj_costs_matrix[[col]] <- adj_costs_matrix[[cost_col]]
  } else if (exp_col %in% names(adj_costs_matrix)) {
    adj_costs_matrix[[col]] <- adj_costs_matrix[[exp_col]]
  } else {
    adj_costs_matrix[[col]] <- NA_real_
  }
}

adj_final_matrix_costs <- adj_costs_matrix |>
  select(patid, all_of(month_cols))

# ----------------------------
# STEP 5: Convert to Long Format
# ----------------------------
adj_final_long_df_costs <- adj_final_matrix_costs |>
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) |>
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visit_cost"
  ) |>
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# ----------------------------
# STEP 6: Adjust for Specialty via Weighted Proportions
# ----------------------------
adj_visit_data_reduced_user_costs <- visit_data_reduced_MIN_c |>
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") |>
  mutate(month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1) |>
  group_by(patid, month_after_index, specialty_reduced) |>
  summarise(
    visit_count = n(),
    unit_cost = unit_cost,
    .groups = "drop"
  ) %>% 
  mutate(visit_cost = visit_count * unit_cost) %>% distinct()

# Total cost per patient/month
adj_total_costs_per_patient_month <- adj_visit_data_reduced_user_costs |>
  group_by(patid, month_after_index) |>
  summarise(total_costs = sum(visit_cost), .groups = "drop")

# Proportion per specialty per patient per month
adj_costs_within_patient_month <- adj_visit_data_reduced_user_costs |>
  left_join(total_costs_per_patient_month, by = c("patid", "month_after_index")) |>
  mutate(prop_within_patient = visit_cost / total_costs) %>% 
  distinct()

# Average proportion across patients
adj_avg_prop_specialty_month_costs <- adj_costs_within_patient_month |>
  group_by(month_after_index, specialty_reduced) |>
  summarise(avg_prop = mean(prop_within_patient, na.rm = TRUE), .groups = "drop")

# ----------------------------
# STEP 7: Mean Monthly Cost Across Patients
# ----------------------------
adj_mean_costs_per_month <- adj_final_long_df_costs |>
  group_by(month_after_index) |>
  summarise(mean_cost = mean(visit_cost), .groups = "drop")

# ----------------------------
# STEP 8: Merge and Calculate Adjusted Costs by Specialty
# ----------------------------
adj_final_combined_costs_df <- adj_avg_prop_specialty_month_costs |>
  left_join(adj_mean_costs_per_month, by = "month_after_index") |>
  mutate(prop_mean_cost = mean_cost * avg_prop)

# ----------------------------
# STEP 9: Plot
# ----------------------------
(plot_costs_adjusted <- ggplot(
  adj_final_combined_costs_df,
  aes(
    x = factor(month_after_index),
    y = prop_mean_cost,
    fill = specialty_reduced
  )
) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_fill_manual(values = c(
    "General Practitioner" = "#B4CDCD",
    "Nurse" = "#377eb8",
    "Other" = "#253494"
  )) +
  labs(
    x = "Month After Index",
    y = "Adjusted Mean Costs per User",
    fill = "Specialty"
  ) +
  scale_y_continuous(limits = c(0, 200)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(hjust = 0.5, size = 18),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 14),
    legend.position = "top",
    panel.spacing = unit(2, "lines")
  ))

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Step 1: Generate Adjusted Visit Costs ------------------
adjusted_costs_data <- visit_data %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
  mutate(
    month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1,
    month_start = index_date + as.difftime((month_after_index - 1) * 30.44, units = "days"),
    month_end = index_date + as.difftime(month_after_index * 30.44, units = "days"),
    exposure_end = follow_up_end,
    days_exposed_in_month = as.numeric(pmin(exposure_end, month_end) - pmax(index_date, month_start) + 1),
    half_contributed = days_exposed_in_month >= 15.22,
    fully_contributed = days_exposed_in_month >= 30.44,
    died_in_month = !is.na(death_date) & death_date >= month_start & death_date <= month_end
  ) %>%
  filter(half_contributed) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    unit_cost = mean(unit_cost),
    days_exposed_in_month = first(days_exposed_in_month),
    fully_contributed = first(fully_contributed),
    died_in_month = first(died_in_month),
    .groups = "drop"
  ) %>%
  mutate(
    adjusted_visits = case_when(
      fully_contributed ~ visit_count,
      died_in_month ~ visit_count,
      TRUE ~ (visit_count / days_exposed_in_month) * 30.44
    ),
    visit_cost = adjusted_visits * unit_cost
  )

# Step 2: Fill Missing Patient-Month-Specialty with 0s ------------------
adjusted_all_months <- 1:24
adjusted_all_pats <- unique(cohort_table_raw$patid)
adjusted_all_specialties <- unique(adjusted_costs_data$specialty_reduced)

adjusted_full_grid <- expand.grid(
  patid = adjusted_all_pats,
  month_after_index = adjusted_all_months,
  specialty_reduced = adjusted_all_specialties,
  stringsAsFactors = FALSE
)

adjusted_costs_complete <- adjusted_full_grid %>%
  left_join(adjusted_costs_data, by = c("patid", "month_after_index", "specialty_reduced")) %>%
  mutate(visit_cost = replace_na(visit_cost, 0))

# Step 3: Total Cost per Patient-Month ------------------
adjusted_total_costs_per_patient_month <- adjusted_costs_complete %>%
  group_by(patid, month_after_index) %>%
  summarise(total_costs = sum(visit_cost), .groups = "drop")

# Step 4: Specialty Cost Proportions per Patient-Month ------------------
adjusted_prop_costs <- adjusted_costs_complete %>%
  left_join(adjusted_total_costs_per_patient_month, by = c("patid", "month_after_index")) %>%
  mutate(prop_within_patient = ifelse(total_costs > 0, visit_cost / total_costs, 0))

# Step 5: Average Proportion per Month/Specialty ------------------
adjusted_avg_prop_specialty_month <- adjusted_prop_costs %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(avg_prop = mean(prop_within_patient, na.rm = TRUE), .groups = "drop")

# Step 6: Mean Cost per Month ------------------
adjusted_mean_cost_per_month <- adjusted_total_costs_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(mean_cost = mean(total_costs), .groups = "drop")

# Step 7: Adjusted Cost per Specialty ------------------
final_adjusted_costs <- adjusted_avg_prop_specialty_month %>%
  left_join(adjusted_mean_cost_per_month, by = "month_after_index") %>%
  mutate(prop_mean_cost = mean_cost * avg_prop)

# Step 8: Plot ------------------
(plot_adjusted_costs <- ggplot(
  final_adjusted_costs,
  aes(
    x = factor(month_after_index),
    y = prop_mean_cost,
    fill = specialty_reduced
  )
) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_fill_manual(values = c(
    "General Practitioner" = "#B4CDCD",
    "Nurse" = "#377eb8",
    "Other" = "#253494"
  )) +
  labs(
    x = "Month After Index",
    y = "Adjusted Mean Visit Costs per User",
    fill = "Specialty"
  ) +
  scale_y_continuous(limits = c(0, 200)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(hjust = 0.5, size = 18),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 14),
    legend.position = "top",
    panel.spacing = unit(2, "lines")
  ))

# Show the plot
print(plot_adjusted_costs)

# ----------------------------
# STEP 2: Adjusted Visit Count Calculation (Final Logic)
# ----------------------------
adjusted_visits_data_r <- visit_data %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
  mutate(
    month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1,
    days_exposed_total = as.numeric(follow_up_end - index_date + 1),
    exposure_day_cutoff = 30.44 * (month_after_index - 1),
    days_exposed_in_month = pmin(days_exposed_total - exposure_day_cutoff, 30.44),
    died_in_month = !is.na(death_date) &
      death_date >= index_date + as.difftime(exposure_day_cutoff, units = "days") &
      death_date < index_date + as.difftime(month_after_index * 30.44, units = "days")
  ) %>%
  # filter(days_exposed_in_month > 0) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    days_exposed_in_month = first(days_exposed_in_month),
    died_in_month = first(died_in_month),
    .groups = "drop"
  ) %>%
  mutate(
    adjusted_visits = case_when(
      died_in_month ~ visit_count,
      days_exposed_in_month >= 30.44 ~ visit_count,
      days_exposed_in_month >= 15.22 ~ (visit_count / days_exposed_in_month) * 30.44,
      TRUE ~ 0  # Drop visits if exposure is too short and not due to death
    )
  ) 
# %>%
#   filter(adjusted_visits > 0)  # Drop completely excluded rows


library(dplyr)
library(lubridate)

# Prepare data
adjusted_visits_data_r <- visit_data %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
  
  # Determine final month after index
  mutate(final_month_after_index = trunc(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44) + 1) %>%
  
  # Keep only visits in the *final month*
  mutate(visit_month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1) %>%
  
  # Calculate days exposed in final month
  mutate(
    exposure_day_cutoff = 30.44 * (final_month_after_index - 1),
    days_exposed_in_month = pmin(as.numeric(follow_up_end - index_date + 1) - exposure_day_cutoff, 30.44),
    
    died_in_month = !is.na(death_date) &
      death_date >= index_date + exposure_day_cutoff &
      death_date < index_date + final_month_after_index * 30.44
  ) %>%
  
  group_by(patid, final_month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    days_exposed_in_month = first(days_exposed_in_month),
    died_in_month = first(died_in_month),
    .groups = "drop"
  ) %>%
  
  mutate(
    adjusted_visits = case_when(
      died_in_month ~ visit_count,
      days_exposed_in_month >= 30.44 ~ visit_count,
      days_exposed_in_month >= 15.22 ~ (visit_count / days_exposed_in_month) * 30.44,
      TRUE ~ 0
    )
  ) %>%
  filter(adjusted_visits > 0)

library(dplyr)
library(lubridate)

adjusted_visits_simple <- visit_data %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, consdate, death_date), ymd)) %>%
  
  # Calculate month_after_index for each visit
  # Step 1: Compute month of each visit and total exposure
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date + 1),
    final_month_after_index = ceiling(days_exposed_total / 30.44),
    month_after_index = floor(as.numeric(consdate - index_date) / 30.44) + 1
  ) %>%
  
  # Step 2: Compute exposure in final month
  mutate(
    days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
    died_in_final_month = !is.na(death_date) &
      floor(as.numeric(death_date - index_date) / 30.44) + 1 == final_month_after_index
  )

# Summarise visits
adjusted_visits_summary <- adjusted_visits_simple %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    final_month_after_index = first(final_month_after_index),
    days_into_final_month = first(days_into_final_month),
    died_in_final_month = first(died_in_final_month),
    .groups = "drop"
  ) %>%
  
  # Adjust only final month visits
  mutate(
    adjusted_visits = case_when(
      month_after_index != final_month_after_index ~ visit_count,  # Not final month → use as-is
      died_in_final_month ~ visit_count,                           # Died in final month → use as-is
      days_into_final_month < 15.22 ~ 0,                           # < half month exposure → drop visits
      TRUE ~ (visit_count / days_into_final_month) * 30.44         # Partial month adjustment
    )
  )




library(dplyr)
library(lubridate)

