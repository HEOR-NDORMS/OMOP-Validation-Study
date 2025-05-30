# **************************************************************************** #
# 	Project name: Validation Study
# 	Author: Njoki Njuki/ Gianluca Fabiano
#   Do file name: Primary care analysis - Plots
#   Date: 10.03.2025
#   Last date edited: 10.03.2025
# **************************************************************************** #

# starting with visit_data_reduced from 04-Analysis-PrimaryCare_Raw.R
# source(here("OMOP Analysis", "SRC", "04-Analysis-PrimaryCare_OMOP.R"))

# Plots ----
## HRCU ----
# turn off the scientific numbers
options(scipen = 999)
library(tidylog)

visit_data_reduced_MIN <- visit_data_reduced %>% select(
  subject_id, index_date, visit_detail_start_date, user_status,
  specialty_reduced, follow_up_end, death_date
)

### PLOT visits per month/specialty   -------
# STEP 1: Create 24-Month Exposure Matrix Per Patient
cohort_grid_omop <- cohort_data_omop |>
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) |>
  rowwise() |>
  mutate(month_vector = list(0:23)) |>
  unnest(month_vector) |>
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_integer_)
  ) |>
  select(subject_id, month_col, value) |>
  pivot_wider(names_from = month_col, values_from = value)

# STEP 2: Create Visit Matrix
visit_data_with_month_raw <- visit_data |>
  filter(specialty != "Not Clinically Relevant", user_status == "User") |> 
  mutate(across(c(index_date, follow_up_end), ymd)) |> 
  left_join(cohort_data_omop |> mutate(across(c(index_date, follow_up_end), ymd)), 
            by = c("subject_id", "index_date", "follow_up_end")) |>
  mutate(month_since_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1) |>
  group_by(subject_id, month_since_index) |>
  summarise(visits = n(), .groups = 'drop') |>
  mutate(month_col = paste0("month_", month_since_index)) |>
  select(subject_id, month_col, visits)

visit_matrix_raw <- visit_data_with_month_raw |>
  pivot_wider(names_from = month_col, values_from = visits)

# STEP 3: Merge and Coalesce Visit + Exposure Data
month_cols <- grep("^month_", names(cohort_grid_omop), value = TRUE)

merged_matrix <- cohort_grid_omop |>
  left_join(visit_matrix_raw, by = "subject_id", suffix = c("_exp", "_visit"))

for (col in month_cols) {
  merged_matrix[[col]] <- dplyr::coalesce(
    merged_matrix[[paste0(col, "_visit")]],
    merged_matrix[[paste0(col, "_exp")]]
  )
}

final_matrix_raw <- merged_matrix |>
  select(subject_id, all_of(month_cols))

# STEP 4: Convert to Long Format
final_long_df_raw <- final_matrix_raw |>
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) |>
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visit_count"
  ) |>
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# STEP 5: Mean Visits Per Month (Unadjusted)
mean_visits_per_month_r <- final_long_df_raw |>
  group_by(month_after_index) |>
  summarise(
    mean_visits = sum(visit_count) / n_distinct(subject_id),
    .groups = "drop"
  )


# STEP 6: Specialty-Specific Visit Proportions
visit_data_reduced_user.1 <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Total visits per patient/month
total_visits_per_patient_month.1 <- visit_data_reduced_user.1 %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Proportion per specialty per patient per month
visits_within_patient_month <- visit_data_reduced_user.1 %>%
  left_join(total_visits_per_patient_month.1, by = c("subject_id", "month_after_index")) %>%
  mutate(prop_within_patient = visit_count / total_visits)

# Average proportions across patients
avg_prop_specialty_month <- visits_within_patient_month %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(avg_prop = mean(prop_within_patient, na.rm = TRUE), .groups = "drop")

# STEP 7: Combine with Mean Visit Counts

final_combined_prop_df <- avg_prop_specialty_month %>%
  left_join(mean_visits_per_month_r, by = "month_after_index") %>%
  mutate(prop_mean = mean_visits * avg_prop)


# STEP 8: Plotting
(plot_visits_unadjusted <- ggplot(
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
      "Other" = "#253494",
      "Uncategorised" = "#53868B"
    )) +
    labs(
      x = "Month After Index",
      y = "Mean Visits per Healthcare Service User",
      fill = "Specialty"
    ) +
    scale_y_continuous(limits = c(0, 8)) +
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

ggsave(here::here(
  "OMOP Analysis", "Results", "Plots",
  "Mean_visits_per_specialty_unweighted.PNG"),
  plot = plot_visits_unadjusted,
  width = 7.5,
  height = 7,
  bg = "white"
)

### PLOT Tot Visits ------

visit_data_reduced_user2 <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
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
      y = "Total Visits (Healthcare Service Users)"
    ) +
    scale_y_continuous(labels = scales::comma,
                       limits = c(0, 65000)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 16),
      panel.spacing = unit(0, "lines")
    ))

ggsave(here::here(
  "OMOP Analysis", "Results", "Plots", "Tot_visits.PNG"),
  plot = plot2,
  width = 7,
  height = 6,
  bg = "white"
)


### PLOT exposure weighted ------
# STEP 1: Create 24-Month Exposure Matrix Per Patient
cohort_grid_omop <- cohort_data_omop %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_real_)
  ) %>%
  select(subject_id, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# STEP 2: Adjusted Visit Count Calculation
adjusted_visits_data_r <- visit_data %>%
  filter(specialty != "Not Clinically Relevant", user_status == "User") %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(days_exposed_total / 30.44),
    month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1,
    days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
    died_in_final_month = !is.na(death_date) &
      floor(as.numeric(death_date - index_date) / 30.44) + 1 == final_month_after_index
  ) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop") %>%
  left_join(
    visit_data %>%
      mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
      mutate(
        days_exposed_total = as.numeric(follow_up_end - index_date),
        final_month_after_index = ceiling(days_exposed_total / 30.44),
        days_into_final_month = days_exposed_total - 30.44 * (final_month_after_index - 1),
        died_in_final_month = !is.na(death_date) &
          floor(as.numeric(death_date - index_date) / 30.44) + 1 == final_month_after_index
      ) %>%
      distinct(subject_id, final_month_after_index, days_into_final_month, died_in_final_month),
    by = "subject_id"
  ) %>%
  mutate(
    adjusted_visits = case_when(
      month_after_index != final_month_after_index ~ visit_count,
      died_in_final_month ~ visit_count,
      days_into_final_month >= 30.44 ~ visit_count,
      days_into_final_month >= 15.22 ~ (visit_count / days_into_final_month) * 30.44,
      TRUE ~ 0
    )
  ) %>%
  filter(adjusted_visits > 0)

# STEP 3: Create Adjusted Visit Matrix (Wide Format)
adjusted_visits_long <- adjusted_visits_data_r %>%
  group_by(subject_id, month_after_index) %>%
  summarise(adjusted_visits = sum(adjusted_visits), .groups = "drop") %>%
  mutate(month_col = paste0("month_", month_after_index))

adjusted_visit_matrix <- adjusted_visits_long %>%
  select(subject_id, month_col, adjusted_visits) %>%
  pivot_wider(names_from = month_col, values_from = adjusted_visits)

# STEP 4: Merge with Exposure Matrix and Coalesce
month_cols <- grep("^month_", names(cohort_grid_omop), value = TRUE)

merged_matrix <- cohort_grid_omop %>%
  left_join(adjusted_visit_matrix, by = "subject_id", suffix = c("_exp", "_visit"))

for (col in month_cols) {
  merged_matrix[[col]] <- dplyr::coalesce(
    merged_matrix[[paste0(col, "_visit")]],
    merged_matrix[[paste0(col, "_exp")]]
  )
}

final_matrix_adjusted <- merged_matrix %>%
  select(subject_id, all_of(month_cols))

# STEP 5: Convert to Long Format
final_long_df_adjusted <- final_matrix_adjusted %>%
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "adjusted_visit_count"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# STEP 6: Accurate Mean Adjusted Visits per Month
# Get patient-months with valid exposure
exposed_patient_months <- cohort_grid_omop %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "exposure"
  ) %>%
  filter(!is.na(exposure)) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month))) %>%
  select(subject_id, month_after_index)

# Sum adjusted visits per patient-month
total_visits_per_patient_month <- adjusted_visits_data_r %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visits = sum(adjusted_visits), .groups = "drop")

# Join exposure data and fill 0s
adjusted_visits_full <- exposed_patient_months %>%
  left_join(total_visits_per_patient_month, by = c("subject_id", "month_after_index")) %>%
  mutate(total_visits = replace_na(total_visits, 0))

# Calculate mean visits using full exposed cohort
mean_visits_per_month <- adjusted_visits_full %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = mean(total_visits), .groups = "drop")

# STEP 7: Specialty-Specific Proportions (Adjusted)
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

# STEP 8: Plot Adjusted Mean Visits by Specialty
(plot_visits_adjusted <- ggplot(
  plots_users_mean_visits_specialty,
  aes(x = factor(month_after_index), y = prop_mean, fill = specialty_reduced)
) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
    scale_fill_manual(values = c(
      "General Practitioner" = "#B4CDCD",
      "Nurse" = "#377eb8",
      "Other" = "#253494",
      "Uncategorised" = "#53868B"
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

ggsave(here::here("OMOP Analysis", "Results",
                  "Plots", "Mean_visits_per_specialty_weighted.PNG"),
       plot = plot_visits_adjusted, 
       width = 7.5, 
       height = 7, 
       bg = "white"
)

### PLOT - Total Number of Visits ----
visit_data_MIN <- visit_data %>% select(
  subject_id, index_date, visit_detail_start_date, user_status,visit_count,visits_per_year,
  specialty, follow_up_end, exposed_yrs, visit_cost
)

pt_count_visits <- visit_data_MIN %>%
  group_by(subject_id, specialty) %>%
  summarise(total_patient_visits = first(visit_count), .groups = 'drop') %>%
  group_by(subject_id) %>% 
  summarise(total_patient_visits = sum(total_patient_visits), .groups = 'drop') %>% 
  group_by(total_patient_visits) %>%
  summarise(Subject_Count = n(), .groups = 'drop') %>%
  mutate(Percent_Subjects = Subject_Count / sum(Subject_Count) * 100)

(plot4 <- pt_count_visits %>% 
    ggplot(aes(x = total_patient_visits, y = Percent_Subjects)) +
    geom_bar(stat = "identity", fill="#A52A2A", alpha=0.9) +
    labs(x = "Total Number of Visits", y = "% of Subjects") +
    scale_y_continuous(limits = c(0, 6) ,
                       breaks = seq(0, 6, by = 1))+
    coord_cartesian(xlim = c(0, 250)) +
    theme_minimal(base_family = 20) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(hjust = 0.5, size = 16),
      panel.spacing = unit(0, "lines")
    ))

ggsave(here::here("OMOP Analysis", "Results", "Plots", "Bar_Graph_number_of_visits_per_pt.PNG"),
       plot = plot4,
       width = 5,
       height = 4,
       bg = "white"
)

### PLOT - Exposure Time ----
visit_data_MIN_t <- visit_data %>% 
  select(subject_id, follow_up_time, follow_up_end) %>% 
  distinct() %>% 
  mutate(follow_up_time = as.numeric(follow_up_time))

(plot5 <- visit_data_MIN_t %>% 
    ggplot(aes(x = follow_up_time)) +
    geom_histogram(bins = 30, colour = "white", fill = "#00CDCD") +
    labs(x = "Follow up time",
         y = "Count") +
    scale_y_continuous(labels = scales::comma,
                       limits = c(0, 20000)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 16),
      axis.text = element_text(size = 14),
      panel.spacing = unit(0, "lines")
    ))

ggsave(here::here("OMOP Analysis", "Results", "Plots", "Exposure_Time.PNG"),
       plot = plot5,
       width = 5,
       height = 4,
       bg = "white"
)

## Cost ----

visit_data_reduced_MIN_c <- visit_data_reduced %>% 
  select(
    subject_id, index_date, visit_detail_start_date, user_status,
    specialty_reduced, follow_up_end, unit_cost, death_date
  )

### PLOT costs per month/specialty   -------
# STEP 0: Process visits per user, by month/specialty
unadj_visit_data_reduced_user_costs <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(
    index_date = ymd(index_date),
    visit_detail_start_date = ymd(visit_detail_start_date),
    month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1
  ) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct()

# STEP 1: Get full grid of all patient-month-specialty combinations
unadj_all_months <- 1:24  # Adjust if needed
unadj_all_pats <- unique(unadj_visit_data_reduced_user_costs$subject_id)
unadj_all_specialties <- unique(unadj_visit_data_reduced_user_costs$specialty_reduced)

unadj_full_grid <- expand.grid(
  subject_id = unadj_all_pats,
  month_after_index = unadj_all_months,
  specialty_reduced = unadj_all_specialties,
  stringsAsFactors = FALSE
)

# STEP 2: Left join to fill in missing combinations with 0 cost
unadj_visit_data_complete <- unadj_full_grid %>%
  left_join(unadj_visit_data_reduced_user_costs, by = c("subject_id", "month_after_index", "specialty_reduced")) %>%
  mutate(visit_costs = replace_na(visit_costs, 0))

# STEP 3: Total cost per patient per month
unadj_total_costs_per_patient_month <- unadj_visit_data_complete %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# STEP 4: Mean cost per month (including zeros)
unadj_mean_costs_per_month <- unadj_total_costs_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(mean_costs = sum(total_costs)/n_distinct(subject_id), .groups = "drop")

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
(plot_costs_unadjusted <- ggplot(
  unadj_plots_users_mean_costs_specialty_weighted,
  aes(x = factor(month_after_index), y = prop_mean, fill = specialty_reduced)
) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
    scale_fill_manual(values = c(
      "General Practitioner" = "#B4CDCD",
      "Nurse" = "#377eb8",
      "Other" = "#253494",
      "Uncategorised" = "#53868B"
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

ggsave(here::here(
  "OMOP Analysis", "Results", "Plots", 
  "Mean_costs_per_specialty_unweighted.PNG"),
  plot = plot_costs_unadjusted, 
  width = 7.5,
  height = 7, 
  bg = "white"
)

### PLOT Tot Costs ------

visit_data_reduced_user_c2 <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(),
            visit_costs = visit_count * unit_cost,
            .groups = "drop") %>% 
  distinct(subject_id, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE) %>% 
  group_by(month_after_index) %>% 
  summarise(tot_costs = sum(visit_costs),
            .groups = "drop")

(plot_costs2 <- ggplot(
  visit_data_reduced_user_c2,
  aes(
    x = month_after_index,
    y = tot_costs
  )
) +
    geom_bar(
      stat = "identity",
      fill = "#1f77b4"
    ) + # Blue color for the bars
    labs(
      x = "Month After Index",
      y = "Total Costs (Healthcare Service Users)"
    ) +
    scale_y_continuous(labels = scales::comma,
                       limits = c(0, 2500000)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 16)
    ))


ggsave(here::here(
  "OMOP Analysis", "Results", "Plots", "Tot_costs.PNG"),
  plot = plot_costs2,
  width = 7,
  height = 6,
  bg = "white"
)


### PLOT exposure weighted ------
# STEP 1: Create 24-Month Exposure Grid (per patient)
adj_cohort_grid_costs <- cohort_data_omop |>
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

# STEP 2: Cost Matrix - Visits x Unit Cost
adj_cost_data_with_month <- visit_data |>
  filter(specialty != "Not Clinically Relevant", user_status == "User") |>
  mutate(across(c(index_date, follow_up_end), ymd)) |>
  left_join(
    cohort_data_omop |> mutate(across(c(index_date, follow_up_end), ymd)),
    by = c("subject_id", "index_date", "follow_up_end")
  ) |>
  mutate(
    month_since_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1
  ) |>
  group_by(subject_id, month_since_index) |>
  summarise(
    visit_count = n(),
    unit_cost = mean(unit_cost),
    total_cost = visit_count * unit_cost,
    .groups = "drop"
  ) |>
  mutate(month_col = paste0("month_", month_since_index)) |>
  select(subject_id, month_col, total_cost)

# STEP 3: Pivot to Wide Format
adj_cost_matrix_raw <- adj_cost_data_with_month |>
  pivot_wider(names_from = month_col, values_from = total_cost)

# STEP 4: Merge with Exposure Matrix
adj_costs_matrix <- adj_cohort_grid_costs |>
  left_join(adj_cost_matrix_raw, by = "subject_id", suffix = c("_exp", "_cost"))

month_cols <- grep("^month_", names(adj_cohort_grid_costs), value = TRUE)

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
  select(subject_id, all_of(month_cols))

# STEP 5: Convert to Long Format
adj_final_long_df_costs <- adj_final_matrix_costs |>
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) |>
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visit_cost"
  ) |>
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# STEP 6: Generate all exposed patient-month-specialty combinations
exposed_patient_months <- adj_cohort_grid_costs %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "exposed"
  ) %>%
  filter(!is.na(exposed)) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month))) %>%
  select(subject_id, month_after_index)

# All combinations of patient x month x specialty
all_specialties <- unique(visit_data_reduced_MIN_c$specialty_reduced)
patient_month_specialty_grid <- exposed_patient_months %>%
  crossing(specialty_reduced = all_specialties)

# Visit costs by patient x month x specialty
visit_costs_by_specialty <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant", user_status == "User") %>%
  mutate(month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(visit_cost = sum(unit_cost), .groups = "drop")

# Fill zeros for missing combinations
filled_costs <- patient_month_specialty_grid %>%
  left_join(visit_costs_by_specialty, by = c("subject_id", "month_after_index", "specialty_reduced")) %>%
  mutate(visit_cost = replace_na(visit_cost, 0))

# STEP 7: Filter out NCR and Recalculate Total Costs (only relevant specialties)
total_costs_per_patient_month <- filled_costs %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_cost = sum(visit_cost), .groups = "drop")

# STEP 8: Compute proportion within patient
costs_with_proportion <- filled_costs %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  left_join(total_costs_per_patient_month, by = c("subject_id", "month_after_index")) %>%
  mutate(prop_within_patient = if_else(total_cost > 0, visit_cost / total_cost, 0))

# STEP 9: Average over all exposed patients
adj_avg_prop_specialty_month_costs <- costs_with_proportion %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(avg_prop = mean(prop_within_patient), .groups = "drop")

# STEP 10: Mean Monthly Cost Using Only Relevant Specialties
adj_mean_costs_per_month <- total_costs_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(mean_cost = mean(total_cost), .groups = "drop")

# STEP 11: Merge and Calculate Adjusted Costs by Specialty
adj_final_combined_costs_df <- adj_avg_prop_specialty_month_costs |>
  left_join(adj_mean_costs_per_month, by = "month_after_index") |>
  mutate(prop_mean_cost = mean_cost * avg_prop)

# STEP 12: Plot
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


ggsave(here::here(
  "OMOP Analysis", "Results", "Plots",
  "Mean_costs_per_specialty_weighted.PNG"),
  plot = plot_costs_adjusted, 
  width = 7.5, 
  height = 7, 
  bg = "white"
)
