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
  subject_id, index_date, visit_detail_start_date, user_status, is_clinically_relevant,
  specialty_reduced, follow_up_end, death_date
)

### PLOT visits per month/specialty   -------
# STEP 0: Identify users
users_adj <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  distinct(subject_id) %>%
  pull(subject_id)

# STEP 1: Build 24-Month Exposure Grid
cohort_grid_omop <- cohort_data_omop %>%
  filter(subject_id %in% users_adj) %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_integer_)
  ) %>%
  select(subject_id, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# STEP 2: Create Visit Matrix
unadj_visit_data_with_month <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date), ymd)) %>%
  left_join(cohort_data_omop %>%
              mutate(across(c(index_date, follow_up_end), ymd)),
            by = c("subject_id", "index_date", "follow_up_end")) %>%
  mutate(month_since_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1) %>%
  filter(month_since_index >= 1 & month_since_index <= 24) %>%
  group_by(subject_id, month_since_index) %>%
  summarise(visits = n(), .groups = 'drop') %>%
  mutate(month_col = paste0("month_", month_since_index)) %>%
  select(subject_id, month_col, visits)

visit_matrix_raw <- unadj_visit_data_with_month %>%
  pivot_wider(names_from = month_col, values_from = visits)

# STEP 3: Merge and Prioritize Visit Data over Exposure Default (0)
month_cols <- grep("^month_", names(cohort_grid_omop), value = TRUE)

merged_matrix <- cohort_grid_omop %>%
  left_join(visit_matrix_raw, by = "subject_id", suffix = c("_exp", "_visit"))

for (col in month_cols) {
  visit_col <- paste0(col, "_visit")
  exp_col <- paste0(col, "_exp")
  merged_matrix[[col]] <- dplyr::coalesce(
    merged_matrix[[visit_col]],
    merged_matrix[[exp_col]],
    0
  )
}

final_matrix_raw <- merged_matrix %>%
  select(subject_id, all_of(month_cols))

# STEP 4: Convert to Long Format
final_long_df_raw <- final_matrix_raw %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visit_count"
  ) %>%
  mutate(
    visit_count = replace_na(visit_count, 0),
    month_after_index = as.integer(gsub("month_", "", month))
  )

# STEP 5: Mean Visits Per Month (Unadjusted)
mean_visits_per_month_r <- final_long_df_raw %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visits = sum(visit_count) / n_distinct(subject_id),
    .groups = "drop"
  )

# STEP 6: Specialty-Specific Visit Proportions
visit_data_reduced_user <- visit_data_reduced_MIN %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  mutate(
    month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1
  ) %>%
  filter(month_after_index >= 1 & month_after_index <= 24) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Total visits per patient/month
total_visits_per_patient_month <- visit_data_reduced_user %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Proportion per specialty per patient per month
visits_within_patient_month <- visit_data_reduced_user %>%
  left_join(total_visits_per_patient_month, by = c("subject_id", "month_after_index")) %>%
  mutate(prop_within_patient = visit_count / total_visits)

# Average specialty proportions per month
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
    scale_y_continuous(limits = c(0, 30)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 14),
      legend.position = "top",
      plot.caption = element_text(size = 14),
      panel.spacing = unit(2, "lines")
    ))

ggsave(here::here(
  "OMOP Analysis", "Results", "Plots",
  "Mean_visits_per_specialty_unweighted.PNG"),
  plot = plot_visits_unadjusted,
  width = 7.5,
  height = 6,
  bg = "white"
)

### PLOT Tot Visits ------

visit_data_reduced_user2 <- visit_data_reduced_MIN %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
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
                       limits = c(0, 250000)) +
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
# Prepare data (remove "Not Clinically Relevant" and compute monthly bins)
# Sense check: 55196220200
# Create 24-Month Exposure Matrix Per Patient
# Build Exposure Grid with 24 Months
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

# Adjusted Visit Counts
adjusted_visits_data_r <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(days_exposed_total / 30.44),
    month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1,
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
  summarise(adjusted_visits = sum(adjusted_visit_count), .groups = "drop")

# Wide Format for Adjusted Visit Counts
adjusted_visits_long <- adjusted_visits_data_r %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  group_by(subject_id, month_col) %>%
  summarise(adjusted_visits = sum(adjusted_visits), .groups = "drop")

adjusted_visits_matrix <- adjusted_visits_long %>%
  pivot_wider(names_from = month_col, values_from = adjusted_visits)

# Merge with Exposure Grid and Coalesce
month_cols <- grep("^month_", names(cohort_grid_omop), value = TRUE)

merged_matrix_v <- cohort_grid_omop %>%
  left_join(adjusted_visits_matrix, by = "subject_id", suffix = c("_exp", "_visit"))

for (col in month_cols) {
  visit_col <- paste0(col, "_visit")
  exp_col <- paste0(col, "_exp")
  if (!(visit_col %in% names(merged_matrix_v))) {
    merged_matrix_v[[visit_col]] <- NA_real_
  }
  merged_matrix_v[[col]] <- dplyr::coalesce(
    as.numeric(merged_matrix_v[[visit_col]]),
    as.numeric(merged_matrix_v[[exp_col]])
  )
}

final_matrix_adjusted_visits <- merged_matrix_v %>%
  select(subject_id, all_of(month_cols))

# Convert to Long Format
final_long_df_adjusted_visits <- final_matrix_adjusted_visits %>%
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "adjusted_visit_count"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# Identify eligible users
users_adj <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  distinct(subject_id) %>%
  pull(subject_id)

# Full 24-month grid for all users
full_patient_month_grid <- cohort_data_omop %>%
  filter(subject_id %in% users_adj) %>%
  select(subject_id) %>%
  distinct() %>%
  crossing(month_after_index = 1:24)

# Summarise adjusted visits
total_visits_per_patient_month <- adjusted_visits_data_r %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visits = sum(adjusted_visits), .groups = "drop")

# Merge to full grid (fill missing with 0)
adjusted_visits_full <- full_patient_month_grid %>%
  left_join(total_visits_per_patient_month, by = c("subject_id", "month_after_index")) %>%
  mutate(total_visits = replace_na(total_visits, 0))

# Mean visits per month across cohort
mean_visits_per_month <- adjusted_visits_full %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = sum(total_visits) / n_distinct(subject_id), .groups = "drop")

# Specialty proportions (optional: for plots)
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

# Plot Adjusted Mean Visits by Specialty
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
      y = "Adjusted Mean Visits per Healthcare Service User",
      fill = "Specialty",
      caption = "Adjusted for partial follow-up: included patients with ≥15.22 days/month.\nPartial months scaled by exposure;\n full months or death-months used raw counts."
    ) +
    scale_y_continuous(limits = c(0, 10)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 14),
      legend.position = "top",
      plot.caption = element_text(size = 14),
      panel.spacing = unit(2, "lines")
    ))


ggsave(here::here("OMOP Analysis", "Results",
                  "Plots", "Mean_visits_per_specialty_weighted.PNG"),
       plot = plot_visits_adjusted, 
       width = 7.5, 
       height = 6.5, 
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
    scale_y_continuous(limits = c(0, 3) ,
                       breaks = seq(0, 3, by = 1))+
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
    subject_id, index_date, visit_detail_start_date, user_status, is_clinically_relevant,
    specialty_reduced, follow_up_end, unit_cost, death_date
  )

### PLOT costs per month/specialty   -------
# STEP 0: Identify users
users_adj <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  distinct(subject_id) %>%
  pull(subject_id)

# STEP 1: Build 24-Month Exposure Grid
cohort_grid_omop <- cohort_data_omop %>%
  filter(subject_id %in% users_adj) %>%
  mutate(months_exposed = interval(index_date, follow_up_end) %/% months(1)) %>%
  rowwise() %>%
  mutate(month_vector = list(0:23)) %>%
  unnest(month_vector) %>%
  mutate(
    month_col = paste0("month_", month_vector + 1),
    value = ifelse(month_vector < months_exposed, 0, NA_integer_)
  ) %>%
  select(subject_id, month_col, value) %>%
  pivot_wider(names_from = month_col, values_from = value)

# STEP 2: Create Visit Matrix
cost_data_with_month <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date), ymd)) %>%
  left_join(cohort_data_omop %>%
              mutate(across(c(index_date, follow_up_end), ymd)),
            by = c("subject_id", "index_date", "follow_up_end")) %>%
  mutate(month_since_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1) %>%
  # filter(month_since_index >= 1 & month_since_index <= 24) %>%
  group_by(subject_id, month_since_index, specialty) |>
  summarise(visits = n(), 
            unit_cost = unit_cost,  
            .groups = 'drop') |>
  mutate(visits_costs = visits * unit_cost) |> 
  distinct() |> 
  group_by(subject_id, month_since_index) |> 
  summarise(visit_costs = sum(visits_costs),
            .groups = "drop") %>% 
  mutate(month_col = paste0("month_", month_since_index)) %>%
  select(subject_id, month_col, visit_costs) 


cost_matrix_raw <- cost_data_with_month %>%
  pivot_wider(names_from = month_col, values_from = visit_costs)

# STEP 3: Merge and Prioritize Visit Data over Exposure Default (0)
month_cols <- grep("^month_", names(cohort_grid_omop), value = TRUE)

merged_matrix_c <- cohort_grid_omop %>%
  left_join(cost_matrix_raw, by = "subject_id", suffix = c("_exp", "_visit"))

# Create new final columns by coalescing visit over exposure
for (col in month_cols) {
  exp_col <- paste0(col, "_exp")
  cost_col <- paste0(col, "_visit")
  
  # Create or overwrite the base column with coalesced value
  merged_matrix_c[[col]] <- dplyr::coalesce(merged_matrix_c[[cost_col]], merged_matrix_c[[exp_col]])
}

final_matrix_costs <- merged_matrix_c %>%
  select(subject_id, all_of(month_cols))

# STEP 4: Convert to Long Format
final_long_df_costs <- final_matrix_costs %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "visit_costs"
  ) %>%
  mutate(
    visit_costs = replace_na(visit_costs, 0),
    month_after_index = as.integer(gsub("month_", "", month))
  )

# STEP 5: Mean Visits Per Month (Unadjusted)
mean_costs_per_month <- final_long_df_costs %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_cost = sum(visit_costs) / n_distinct(subject_id),
    .groups = "drop"
  )

# STEP 6: Specialty-Specific Visit Proportions
cost_data_reduced_user <- visit_data_reduced_MIN_c %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  mutate(
    month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1
  ) %>%
  filter(month_after_index >= 1 & month_after_index <= 24) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), 
            visit_costs = visit_count * unit_cost,
            .groups = "drop") %>% 
  distinct()

# Total costs per patient/month
total_costs_per_patient_month <- cost_data_reduced_user %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visit_costs = sum(visit_costs), .groups = "drop")

# Proportion per specialty per patient per month
cost_within_patient_month <- cost_data_reduced_user %>%
  left_join(total_costs_per_patient_month, by = c("subject_id", "month_after_index")) %>%
  mutate(prop_within_patient = visit_costs / total_visit_costs)

# Average specialty proportions per month
avg_cost_prop_specialty_month <- cost_within_patient_month %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(avg_prop = mean(prop_within_patient, na.rm = TRUE), .groups = "drop")

# STEP 7: Combine with Mean Visit Counts
cost_final_combined_prop_df <- avg_cost_prop_specialty_month %>%
  left_join(mean_costs_per_month, by = "month_after_index") %>%
  mutate(prop_mean = mean_visit_cost * avg_prop)

# STEP 8: Plot
(plot_costs_unadjusted <- ggplot(
  cost_final_combined_prop_df,
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
      y = "Mean Costs per User",
      fill = "Specialty"
    ) +
    scale_y_continuous(limits = c(0, 800)) +
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
  height = 6, 
  bg = "white"
)

### PLOT Tot Costs ------

visit_data_reduced_user_c2 <- visit_data_reduced_MIN_c %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
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
                       limits = c(0, 8000000)) +
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
# Build Exposure Grid with 24 Months
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

# Adjust Visit Counts
adjusted_costs_data_r <- visit_data_reduced_MIN_c %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  mutate(across(c(index_date, follow_up_end, visit_detail_start_date, death_date), ymd)) %>%
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_month_after_index = ceiling(days_exposed_total / 30.44),
    month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1,
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

# Wide Format for Adjusted Visits (cleaned, 1 value per patient/month)
adjusted_costs_long <- adjusted_costs_data_r %>%
  mutate(month_col = paste0("month_", month_after_index)) %>%
  group_by(subject_id, month_col) %>%
  summarise(adjust_visit_costs = sum(adjust_visit_costs), .groups = "drop")

adjusted_costs_matrix <- adjusted_costs_long %>%
  pivot_wider(names_from = month_col, values_from = adjust_visit_costs)

# Merge Adjusted Visits with Exposure Grid
month_cols <- grep("^month_", names(cohort_grid_omop), value = TRUE)

merged_matrix_c <- cohort_grid_omop %>%
  left_join(adjusted_costs_matrix, by = "subject_id", suffix = c("_exp", "_visit"))

# Coalesce exposure and adjusted visits
for (col in month_cols) {
  visit_col <- paste0(col, "_visit")
  exp_col <- paste0(col, "_exp")
  if (!(visit_col %in% names(merged_matrix_c))) {
    merged_matrix_c[[visit_col]] <- NA_real_
  }
  merged_matrix_c[[col]] <- dplyr::coalesce(
    as.numeric(merged_matrix_c[[visit_col]]),
    as.numeric(merged_matrix_c[[exp_col]])
  )
}

final_matrix_adjusted_costs <- merged_matrix_c %>%
  select(subject_id, all_of(month_cols))

# Convert to long format
final_long_df_adjusted_costs <- final_matrix_adjusted_costs %>%
  mutate(across(starts_with("month_"), ~ replace_na(.x, 0))) %>%
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "adjusted_visit_costs"
  ) %>%
  mutate(month_after_index = as.integer(gsub("month_", "", month)))

# But first, let's filter for users only
users_adj <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  distinct(subject_id) %>% 
  pull(subject_id)

# Full patient-month grid (for all 24 months and all patients)
full_patient_month_grid_cost <- cohort_data_omop %>%
  filter(subject_id %in% users_adj) %>% 
  select(subject_id) %>%
  distinct() %>%
  crossing(month_after_index = 1:24)  # Ensures all 24 months per patient

# Summarise costs
total_costs_per_patient_month <- adjusted_costs_data_r %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_costs = sum(adjust_visit_costs), .groups = "drop")

# Merge to the full grid so missing patient-months get zero
adjusted_costs_full <- full_patient_month_grid_cost %>%
  left_join(total_costs_per_patient_month, by = c("subject_id", "month_after_index")) %>%
  mutate(total_costs = replace_na(total_costs, 0))

mean_costs_per_month <- adjusted_costs_full %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_cost = sum(total_costs)/n_distinct(subject_id), .groups = "drop")

# Proportions per Specialty (Optional: for plotting)
costs_per_specialty_month <- adjusted_costs_data_r %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_costs_specialty = sum(adjust_visit_costs), .groups = "drop")

plots_users_mean_costs_specialty <- costs_per_specialty_month %>%
  left_join(mean_costs_per_month, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(
    proportion = total_costs_specialty / sum(total_costs_specialty, na.rm = TRUE),
    prop_mean = mean_visit_cost * proportion
  ) %>%
  ungroup()

# STEP 12: Plot
(plot_costs_adjusted <- ggplot(
  plots_users_mean_costs_specialty,
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
      y = "Adjusted Mean Costs per Healthcare Service User",
      fill = "Specialty",
      caption = "Adjusted for partial follow-up: included patients with ≥15.22 days/month.\nPartial months scaled by exposure;\n full months or death-months used raw counts."
    ) +
    scale_y_continuous(limits = c(0, 350)) +
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
  height = 6, 
  bg = "white"
)
