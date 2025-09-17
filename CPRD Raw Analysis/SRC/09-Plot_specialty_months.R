## --- Specialty monthly plot - ALL PATIENTS ----
# Compute calendar months including partial ones
cohort_months_all_months <- cohort_table %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  select(patid, max_month_exposed)
# a) Encounters per specialty per patient-month
monthly_specialty_encounters_all_months <- visit_data_all %>%
  mutate(
    index_date = ymd(index_date),
    consdate   = ymd(consdate),
    month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_all_months, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>%   # keep only exposed months (incl. partial)
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(encounters = sum(!is.na(consid)), .groups = "drop")

# b) Total encounters per patient-month
total_patient_month_all_months <- monthly_specialty_encounters_all_months %>%
  group_by(patid, month_after_index) %>%
  summarise(total_encounters = sum(encounters), .groups = "drop")

# c) Proportion of encounters per specialty within each patient-month
specialty_prop_patient_month_all_months <- monthly_specialty_encounters_all_months %>%
  left_join(total_patient_month_all_months, by = c("patid", "month_after_index")) %>%
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
      "Admin"
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
    "Service" = "#e5c494"  
  )) +
  labs(
    title = "Source CPRD",
    x = "Month After Index",
    y = "Mean Encounters per Patient",
    fill = "Specialty"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title   = element_text(size = 9),
    axis.text    = element_text(size = 9),
    legend.text  = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.position = "top"
  )

#plot_visits_specialty_all_months

ggsave(
  here::here("CPRD Raw Analysis", "Results", "Plots", "Mean_encounters_per_specialty_all_months.PNG"),
  plot = plot_visits_specialty_all_months,
  width = 8, height = 6, bg = "white"
)


## SPECIALTY MONTHLY PLOT - USERS -----

# a) Encounters per specialty per patient-month (users only)
monthly_specialty_encounters_all_months <- visit_data_all %>%
  filter(user_status == "User") %>%
  mutate(
    index_date = ymd(index_date),
    consdate   = ymd(consdate),
    month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_all_months, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(encounters = sum(!is.na(consid)), .groups = "drop")

# b) Total encounters per patient-month
total_patient_month_all_months <- monthly_specialty_encounters_all_months %>%
  group_by(patid, month_after_index) %>%
  summarise(total_encounters = sum(encounters), .groups = "drop")

# c) Proportion of encounters per specialty within each patient-month
specialty_prop_patient_month_all_months <- monthly_specialty_encounters_all_months %>%
  left_join(total_patient_month_all_months, by = c("patid", "month_after_index")) %>%
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
  here::here("CPRD Raw Analysis", "Results", "Plots", "Mean_encounters_per_specialty_all_months_users.PNG"),
  plot = plot_visits_specialty_all_months_users,
  width = 8, height = 6, bg = "white"
)


## --- Specialty monthly plot - ALL PATIENTS (Costs) ----

# Compute calendar months including partial ones
cohort_months_all_months <- cohort_table %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  select(patid, max_month_exposed)

# a) Costs per specialty per patient-month
monthly_specialty_costs_all_months <- visit_data_all %>%
  mutate(
    index_date = ymd(index_date),
    consdate   = ymd(consdate),
    month_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_all_months, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>%   # keep only exposed months
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(costs = sum(unit_cost, na.rm = TRUE), .groups = "drop")

# b) Total costs per patient-month
total_patient_month_all_months <- monthly_specialty_costs_all_months %>%
  group_by(patid, month_after_index) %>%
  summarise(total_costs = sum(costs, na.rm = TRUE), .groups = "drop")

# c) Proportion of costs per specialty within each patient-month
specialty_prop_patient_month_all_months <- monthly_specialty_costs_all_months %>%
  left_join(total_patient_month_all_months, by = c("patid", "month_after_index")) %>%
  mutate(prop_within_patient = costs / total_costs)

# d) Average proportions per specialty per month
avg_prop_specialty_month_all_months <- specialty_prop_patient_month_all_months %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(avg_prop = mean(prop_within_patient, na.rm = TRUE), .groups = "drop") %>%
  group_by(month_after_index) %>%
  mutate(avg_prop = avg_prop / sum(avg_prop, na.rm = TRUE)) %>%
  ungroup()

# e) Combine with monthly mean cost (instead of mean encounter count)
final_combined_prop_df_all_months <- avg_prop_specialty_month_all_months %>%
  left_join(
    observed_cost_summary_all_patients_all_encounters_all_months %>%
      select(month_after_index, mean_costs_exposed),   
    by = "month_after_index"
  ) %>%
  mutate(prop_mean = mean_costs_exposed * avg_prop)

# f) Plot stacked bar chart
plot_costs_specialty_all_months <- ggplot(
  final_combined_prop_df_all_months,
  aes(
    x = factor(month_after_index),
    y = prop_mean,
    fill = specialty_reduced
  )
) +
  
  scale_y_continuous(
    limits = c(0, max_y),
    breaks = seq(0, max_y, by = 30)
  )+
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
    "Service" = "#e5c494"  
  )) +
  labs(
    title = "Source CPRD",
    x = "Month After Index",
    y = "Mean Costs per Patient",
    fill = "Specialty"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title   = element_text(size = 9),
    axis.text    = element_text(size = 9),
    legend.text  = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.position = "top"
  )

# Save plot
ggsave(
  here::here("CPRD Raw Analysis", "Results", "Plots", "Mean_costs_per_specialty_all_months.PNG"),
  plot = plot_costs_specialty_all_months,
  width = 8, height = 6, bg = "white"
)

## Table by specialty ------

library(gtsummary)

gt_overall_healthcare_roles_count <- visit_data_all %>%
  filter(!is.na(consid)) %>%
  tbl_summary(
    include = specialty_reduced,
    label = list(specialty_reduced = "Healthcare Category"),
    statistic = all_categorical() ~ "{n} ({p}%)",
    sort = all_categorical() ~ "frequency"
  ) %>%
  modify_header(label ~ "Source-CPRD") %>%
  bold_labels()


gt_overall_healthcare_roles_count_df <- as_tibble(gt_overall_healthcare_roles_count) 

rio::export(gt_overall_healthcare_roles_count_df, here::here("CPRD Raw Analysis", "Results", "Cohort", "Output_GT_overall_healthcare_roles-Raw.xlsx"))
