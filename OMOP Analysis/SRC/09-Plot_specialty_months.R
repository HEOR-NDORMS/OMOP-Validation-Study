## --- Specialty monthly plot - ALL PATIENTS ----
# Compute calendar months including partial ones
cohort_months_all_months <- cohort_table %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  select(subject_id, max_month_exposed)
# a) Encounters per specialty per patient-month
monthly_specialty_encounters_all_months <- visit_data_all %>%
  mutate(
    index_date = ymd(index_date),
    visit_detail_start_date   = ymd(visit_detail_start_date),
    month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_all_months, by = "subject_id") %>%
  filter(month_after_index <= max_month_exposed) %>%   # keep only exposed months (incl. partial)
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(encounters = sum(!is.na(visit_detail_id)), .groups = "drop")

# b) Total encounters per patient-month
total_patient_month_all_months <- monthly_specialty_encounters_all_months %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_encounters = sum(encounters), .groups = "drop")

# c) Proportion of encounters per specialty within each patient-month
specialty_prop_patient_month_all_months <- monthly_specialty_encounters_all_months %>%
  left_join(total_patient_month_all_months, by = c("subject_id", "month_after_index")) %>%
  mutate(prop_within_patient = encounters / total_encounters)

# d) Average proportions per specialty per month
avg_prop_specialty_month_all_months <- specialty_prop_patient_month_all_months %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(avg_prop = mean(prop_within_patient, na.rm = TRUE), .groups = "drop") %>%
  group_by(month_after_index) %>%
  mutate(avg_prop = avg_prop / sum(avg_prop, na.rm = TRUE)) %>%
  ungroup()

# e) Combine with monthly mean encounter counts (from your all-months summary)
final_combined_prop_df_all_months <- avg_prop_specialty_month_all_months %>%
  left_join(
    observed_visit_summary_all_patients_all_encounters_all_months %>%
      select(month_after_index, mean_encounters_exposed),
    by = "month_after_index"
  ) %>%
  mutate(prop_mean = mean_encounters_exposed * avg_prop)

# f) Plot stacked bar chart

# fix the order of specialties for consistent stacking
final_combined_prop_df_all_months <- final_combined_prop_df_all_months %>%
  mutate(specialty_reduced = factor(
    specialty_reduced,
    levels = c(
      "GP",
      "Medic non-GP",
      "Nurse",
      "Non-registered other",
      "AHP",
      "Dental",
      "Social",
      "Service",
      "Admin",
      "Uncategorised"
    )
  ))

plot_visits_specialty_all_months <- ggplot(
  final_combined_prop_df_all_months,
  aes(
    x = factor(month_after_index),
    y = prop_mean,
    fill = specialty_reduced
  )
) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_fill_manual(values = c(
    "GP" = "#B4CDCD", 
    "Medic non-GP" = "#377eb8", 
    "Nurse" = "#253494", 
    "Non-registered other" = "#53868B", 
    "AHP" = "#8da0cb", 
    "Dental" = "#e78ac3", 
    "Social" = "#a6d854", 
    "Admin" = "#ffd92f", 
    "Service" = "#e5c494",
    "Uncategorised" = "#333333")) +
  labs(
    x = "Month After Index",
    y = "Mean Encounters per Patient",
    fill = "Specialty"
  ) +
  labs(
    title = "OMOP",
    x = "Month After Index",
    y = "Mean Encounters per Patient",
    fill = "Specialty"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.title   = element_text(size = 9),
    axis.text    = element_text(size = 9),
    legend.text  = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.position = "top"
  )

#plot_visits_specialty_all_months

ggsave(
  here::here("OMOP Analysis", "Results", "Plots", "Mean_encounters_per_specialty_all_months.PNG"),
  plot = plot_visits_specialty_all_months,
  width = 8, height = 6, bg = "white",
  device = function(...) png(..., type = "cairo", units = "in", res = 300)
)


## SPECIALTY MONTHLY PLOT - USERS -----

# a) Encounters per specialty per patient-month (users only)
monthly_specialty_encounters_all_months <- visit_data_all %>%
  filter(user_status == "User") %>%
  mutate(
    index_date = ymd(index_date),
    visit_detail_start_date   = ymd(visit_detail_start_date),
    month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_all_months, by = "subject_id") %>%
  filter(month_after_index <= max_month_exposed) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(encounters = sum(!is.na(visit_detail_id)), .groups = "drop")

# b) Total encounters per patient-month
total_patient_month_all_months <- monthly_specialty_encounters_all_months %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_encounters = sum(encounters), .groups = "drop")

# c) Proportion of encounters per specialty within each patient-month
specialty_prop_patient_month_all_months <- monthly_specialty_encounters_all_months %>%
  left_join(total_patient_month_all_months, by = c("subject_id", "month_after_index")) %>%
  mutate(prop_within_patient = encounters / total_encounters)

# d) Average proportions per specialty per month (renormalised)
avg_prop_specialty_month_all_months <- specialty_prop_patient_month_all_months %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(avg_prop = mean(prop_within_patient, na.rm = TRUE), .groups = "drop") %>%
  group_by(month_after_index) %>%
  mutate(avg_prop = avg_prop / sum(avg_prop, na.rm = TRUE)) %>%
  ungroup()

# e) Combine with monthly mean encounter counts for *users only*
final_combined_prop_df_all_months <- avg_prop_specialty_month_all_months %>%
  left_join(
    observed_visit_summary_users_all_months %>%   # <-- needs to be computed same way as before, but filter(user_status=="User")
      select(month_after_index, mean_encounters_exposed),
    by = "month_after_index"
  ) %>%
  mutate(prop_mean = mean_encounters_exposed * avg_prop)

# f) Fix stacking order
final_combined_prop_df_all_months <- final_combined_prop_df_all_months %>%
  mutate(specialty_reduced = factor(
    specialty_reduced,
    levels = c(
      "GP",
      "Medic non-GP",
      "Nurse",
      "Non-registered other",
      "AHP",
      "Dental",
      "Social",
      "Service",
      "Admin"
    )
  ))

plot_visits_specialty_all_months_users <- ggplot(
  final_combined_prop_df_all_months,
  aes(
    x = factor(month_after_index),
    y = prop_mean,
    fill = specialty_reduced
  )
) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_fill_manual(values = c(
    "GP" = "#B4CDCD", 
    "Medic non-GP" = "#377eb8", 
    "Nurse" = "#253494", 
    "Non-registered other" = "#53868B", 
    "AHP" = "#8da0cb", 
    "Dental" = "#e78ac3", 
    "Social" = "#a6d854", 
    "Admin" = "#ffd92f", 
    "Service" = "#e5c494"  )) +
  labs(
    x = "Month After Index",
    y = "Mean Encounters per Patient",
    fill = "Specialty"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.title   = element_text(size = 16),
    axis.text    = element_text(size = 14),
    legend.text  = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = "top"
  )



ggsave(
  here::here("OMOP Analysis", "Results", "Plots", "Mean_encounters_per_specialty_all_months_users.PNG"),
  plot = plot_visits_specialty_all_months_users,
  width = 8, height = 6, bg = "white"
)

