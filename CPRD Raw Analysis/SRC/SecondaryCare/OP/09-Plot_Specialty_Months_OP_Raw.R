appt_data_plots <- table_one_op_raw_appt_recoded %>% select(
  patid, index_date, apptdate,attendkey, mainspef_recoded, tretspef_recoded,
  follow_up_end, death_date, user_status
) %>% 
  mutate(mainspef_recoded_reduced = case_when(
    mainspef_recoded == "Trauma and Orthopaedics" ~ "Trauma and Orthopaedics",
    mainspef_recoded == "Allied Health Professional" ~ "Allied Health Professional",
    mainspef_recoded == "Nursing" ~ "Nursing",
    mainspef_recoded == "Ophthalmology" ~ "Ophthalmology",
    mainspef_recoded == "Radiology" ~ "Radiology",
    mainspef_recoded == "General Surgery" ~ "General Surgery",
    mainspef_recoded == "General Internal Medicine" ~ "General Internal Medicine",
    mainspef_recoded == "Rheumatology" ~ "Rheumatology",
    mainspef_recoded == "Clinical Haematology" ~ "Clinical Haematology",
    mainspef_recoded == "Cardiology" ~ "Cardiology",
    is.na(mainspef_recoded) ~ NA_character_,
    TRUE ~ "Other"),
    tretspef_recoded_reduced = case_when(
      tretspef_recoded == "Trauma and Orthopaedic Service" ~ "Trauma and Orthopaedic Service",
      tretspef_recoded == "Physiotherapy Service" ~ "Physiotherapy Service",
      tretspef_recoded == "Ophthalmology Service" ~ "Ophthalmology Service",
      tretspef_recoded == "General Internal Medicine Service" ~ "General Internal Medicine Service",
      tretspef_recoded == "Diagnostic Imaging Service" ~ "Diagnostic Imaging Service",
      tretspef_recoded == "Rheumatology Service" ~ "Rheumatology Service",
      tretspef_recoded == "Cardiology Service" ~ "Cardiology Service",
      tretspef_recoded == "Elderly Medicine Service" ~ "Elderly Medicine Service",
      tretspef_recoded == "Anticoagulant Service" ~ "Anticoagulant Service",
      tretspef_recoded == "Dermatology Service" ~ "Dermatology Service",
      is.na(tretspef_recoded) ~ NA_character_,
      TRUE ~ "Other"
    )
  )

## --- Specialty monthly plot - ALL PATIENTS ----
### Main Specialty ----
# Compute calendar months including partial ones
cohort_months_all_months <- cohort_table %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  select(patid, max_month_exposed)

# a) Encounters per specialty per patient-month
monthly_main_specialty_encounters_all_months <- appt_data_plots %>%
  mutate(
    index_date = ymd(index_date),
    apptdate   = ymd(apptdate),
    month_after_index = trunc(as.numeric(difftime(apptdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_all_months, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>%   # keep only exposed months (incl. partial)
  group_by(patid, month_after_index, mainspef_recoded_reduced) %>%
  summarise(encounters = sum(!is.na(attendkey)), .groups = "drop")

# b) Total encounters per patient-month
main_total_patient_month_all_months <- monthly_main_specialty_encounters_all_months %>%
  group_by(patid, month_after_index) %>%
  summarise(total_encounters = sum(encounters), .groups = "drop")

# c) Proportion of encounters per specialty within each patient-month
main_specialty_prop_patient_month_all_months <- monthly_main_specialty_encounters_all_months %>%
  left_join(total_patient_month_all_months, by = c("patid", "month_after_index")) %>%
  mutate(prop_within_patient = encounters / total_encounters)

# d) Average proportions per specialty per month
avg_prop_main_specialty_month_all_months <- main_specialty_prop_patient_month_all_months %>%
  group_by(month_after_index, mainspef_recoded_reduced) %>%
  summarise(avg_prop = mean(prop_within_patient, na.rm = TRUE), .groups = "drop") %>%
  group_by(month_after_index) %>%
  mutate(avg_prop = avg_prop / sum(avg_prop, na.rm = TRUE)) %>%
  ungroup()

# e) Combine with monthly mean encounter counts (from your all-months summary)
main_final_combined_prop_df_all_months <- avg_prop_main_specialty_month_all_months %>%
  left_join(
    observed_visit_summary_all_patients_all_encounters_all_months %>%
      select(month_after_index, mean_encounters_exposed),
    by = "month_after_index"
  ) %>%
  mutate(prop_mean = mean_encounters_exposed * avg_prop)

# f) Plot stacked bar chart
(plot_visits_main_specialty_all_months <- ggplot(
  main_final_combined_prop_df_all_months,
  aes(
    x = factor(month_after_index),
    y = prop_mean,
    fill = mainspef_recoded_reduced
  )
) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_fill_manual(values = c(
    "Trauma and Orthopaedics" = "#B4CDCD", 
    "Allied Health Professional" = "#377eb8", 
    "Nursing" = "#253494", 
    "Ophthalmology" = "#53868B", 
    "Radiology" = "#8da0cb", 
    "General Surgery" = "#e78ac3", 
    "General Internal Medicine" = "#a6d854", 
    "Rheumatology" = "#ffd92f", 
    "Clinical Haematology" = "#e5c494",
    "Cardiology" = "#CD950C",
    "Other" = "#4F4F4F"
    
  )) +
  labs(
    title = "Source CPRD",
    x = "Month After Index",
    y = "Mean Encounters per Patient (Main Specialty)",
    fill = "Specialty"
  ) +
    theme_minimal(base_size = 14, base_family = "Times") +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title   = element_text(size = 9),
    axis.text    = element_text(size = 9),
    legend.text  = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.position = "top"
  ))


ggsave(
  here::here("CPRD Raw Analysis", "Results",  "SecondaryCareAnalysis", "OP",
             "Plots", "Mean_encounters_per_main_specialty_all_months.PNG"),
  plot = plot_visits_main_specialty_all_months,
  width = 8, height = 6, bg = "white"
)

### Treatment Specialty ----
# Compute calendar months including partial ones
cohort_months_all_months <- cohort_table %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  select(patid, max_month_exposed)

# a) Encounters per specialty per patient-month
monthly_tret_specialty_encounters_all_months <- appt_data_plots %>%
  mutate(
    index_date = ymd(index_date),
    apptdate   = ymd(apptdate),
    month_after_index = trunc(as.numeric(difftime(apptdate, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_all_months, by = "patid") %>%
  filter(month_after_index <= max_month_exposed) %>%   # keep only exposed months (incl. partial)
  group_by(patid, month_after_index, tretspef_recoded_reduced) %>%
  summarise(encounters = sum(!is.na(attendkey)), .groups = "drop")

# b) Total encounters per patient-month
tret_total_patient_month_all_months <- monthly_tret_specialty_encounters_all_months %>%
  group_by(patid, month_after_index) %>%
  summarise(total_encounters = sum(encounters), .groups = "drop")

# c) Proportion of encounters per specialty within each patient-month
tret_specialty_prop_patient_month_all_months <- monthly_tret_specialty_encounters_all_months %>%
  left_join(total_patient_month_all_months, by = c("patid", "month_after_index")) %>%
  mutate(prop_within_patient = encounters / total_encounters)

# d) Average proportions per specialty per month
avg_prop_tret_specialty_month_all_months <- tret_specialty_prop_patient_month_all_months %>%
  group_by(month_after_index, tretspef_recoded_reduced) %>%
  summarise(avg_prop = mean(prop_within_patient, na.rm = TRUE), .groups = "drop") %>%
  group_by(month_after_index) %>%
  mutate(avg_prop = avg_prop / sum(avg_prop, na.rm = TRUE)) %>%
  ungroup()

# e) Combine with monthly mean encounter counts (from your all-months summary)
tret_final_combined_prop_df_all_months <- avg_prop_tret_specialty_month_all_months %>%
  left_join(
    observed_visit_summary_all_patients_all_encounters_all_months %>%
      select(month_after_index, mean_encounters_exposed),
    by = "month_after_index"
  ) %>%
  mutate(prop_mean = mean_encounters_exposed * avg_prop)

# f) Plot stacked bar chart
(plot_visits_tret_specialty_all_months <- ggplot(
  tret_final_combined_prop_df_all_months,
  aes(
    x = factor(month_after_index),
    y = prop_mean,
    fill = tretspef_recoded_reduced
  )
) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
    scale_fill_manual(values = c(
      "Trauma and Orthopaedic Service" = "#B4CDCD", 
      "Physiotherapy Service" = "#377eb8", 
      "Ophthalmology Service" = "#253494", 
      "General Internal Medicine Service" = "#53868B", 
      "Diagnostic Imaging Service" = "#8da0cb", 
      "Rheumatology Service" = "#e78ac3", 
      "Cardiology Service" = "#a6d854", 
      "Elderly Medicine Service" = "#ffd92f", 
      "Anticoagulant Service" = "#e5c494",
      "Dermatology Service" = "#CD950C",
      "Other" = "#4F4F4F"
      )) +
    labs(
      title = "Source CPRD",
      x = "Month After Index",
      y = "Mean Encounters per Patient (Treatment Specialty)",
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
    ))

ggsave(
  here::here("CPRD Raw Analysis", "Results", "SecondaryCareAnalysis", "OP",
             "Plots", "Mean_encounters_per_tret_specialty_all_months.PNG"),
  plot = plot_visits_tret_specialty_all_months,
  width = 8, height = 6, bg = "white"
)
