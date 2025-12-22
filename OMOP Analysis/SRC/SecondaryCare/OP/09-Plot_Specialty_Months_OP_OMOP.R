appt_data_plots_omop <- appt_data_omop %>% select(
  subject_id, index_date, visit_detail_start_date, visit_detail_id,
  follow_up_end, user_status, category)

## --- Specialty monthly plot - ALL PATIENTS ----
### Main Specialty ----
# Compute calendar months including partial ones
cohort_months_all_months_omop <- cohort_table_omop %>%
  mutate(
    index_date = ymd(index_date),
    follow_up_end = ymd(follow_up_end),
    max_month_exposed = ceiling(as.numeric(difftime(follow_up_end, index_date, units = "days")) / 30.44)
  ) %>%
  select(subject_id, max_month_exposed)

# a) Encounters per specialty per patient-month
monthly_main_specialty_encounters_all_months_omop <- appt_data_plots_omop %>%
  mutate(
    index_date = ymd(index_date),
    visit_detail_start_date   = ymd(visit_detail_start_date),
    month_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1
  ) %>%
  left_join(cohort_months_all_months_omop, by = "subject_id") %>%
  filter(month_after_index <= max_month_exposed) %>%   # keep only exposed months (incl. partial)
  group_by(subject_id, month_after_index, category) %>%
  summarise(encounters = sum(!is.na(visit_detail_id)), .groups = "drop")

# b) Total encounters per patient-month
main_total_patient_month_all_months_omop <- monthly_main_specialty_encounters_all_months_omop %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_encounters = sum(encounters), .groups = "drop")

# c) Proportion of encounters per specialty within each patient-month
main_specialty_prop_patient_month_all_months_omop <- monthly_main_specialty_encounters_all_months_omop %>%
  left_join(main_total_patient_month_all_months_omop, by = c("subject_id", "month_after_index")) %>%
  mutate(prop_within_patient = encounters / total_encounters)

# d) Average proportions per specialty per month
avg_prop_main_specialty_month_all_months_omop <- main_specialty_prop_patient_month_all_months_omop %>%
  group_by(month_after_index, category) %>%
  summarise(avg_prop = mean(prop_within_patient, na.rm = TRUE), .groups = "drop") %>%
  group_by(month_after_index) %>%
  mutate(avg_prop = avg_prop / sum(avg_prop, na.rm = TRUE)) %>%
  ungroup()

# e) Combine with monthly mean encounter counts (from your all-months summary)
main_final_combined_prop_df_all_months_omop <- avg_prop_main_specialty_month_all_months_omop %>%
  left_join(
    observed_visit_summary_all_patients_all_encounters_all_months_omop %>%
      select(month_after_index, mean_encounters_exposed),
    by = "month_after_index"
  ) %>%
  mutate(prop_mean = mean_encounters_exposed * avg_prop) |> 
  mutate(category = if_else(is.na(category), "Unknown Specialty", category))

# f) Plot stacked bar chart
(plot_visits_main_specialty_all_months_omop <- ggplot(
  main_final_combined_prop_df_all_months_omop,
  aes(
    x = factor(month_after_index),
    y = prop_mean,
    fill = category
  )
) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
# scale_fill_manual(values = c(
    #   "Trauma and Orthopaedics" = "#B4CDCD", 
    #   "Allied Health Professional" = "#377eb8", 
    #   "Nursing" = "#253494", 
    #   "Ophthalmology" = "#53868B", 
    #   "Radiology" = "#8da0cb", 
    #   "General Surgery" = "#e78ac3", 
    #   "General Internal Medicine" = "#a6d854", 
    #   "Rheumatology" = "#ffd92f", 
    #   "Clinical Haematology" = "#e5c494",
    #   "Cardiology" = "#CD950C",
    #   "Other" = "#4F4F4F"
    #   
    # )) +
    labs(
      title = "OMOP-Mapped",
      x = "Month After Index",
      y = "Mean Encounters per Patient (Main Specialty)",
      fill = "Specialty"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title   = element_text(face = "bold", hjust = 0.5, size = 14),
      axis.title   = element_text(size = 12),
      axis.text    = element_text(size = 10),
      legend.text  = element_text(size = 10),
      legend.title = element_text(size = 12),
      legend.position = "top"
    ))


ggsave(
  here::here("OMOP Analysis", "Results",  "SecondaryCareAnalysis", "OP",
             "Plots", "Mean_encounters_per_main_specialty_all_months_omop.PNG"),
  plot = plot_visits_main_specialty_all_months_omop,
  width = 14, height = 10, bg = "white"
)
