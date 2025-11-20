combined_data_visits_users <- rbind(final_long_df_encounters_users_all_encounters_omop |> 
  rename(patid = subject_id) |> 
    mutate(data_source = "OMOP"),
  
  final_long_df_encounters_users_all_encounters_raw |> 
    mutate(data_source = "Source")
) |> 
  mutate(tot_encounters = if_else(is.na(tot_encounters), 0, tot_encounters)) |> 
  group_by(data_source, month_after_index) |> 
  summarise(mean_visits = mean(tot_encounters),
            n_exposed = sum(tot_encounters),
            se_visits = sd(tot_encounters)/n_exposed)

export(combined_data_visits_users, here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", 
  "combined_data_visits_users.xlsx"))

(mean_visits_plot_combined_users <- ggplot(combined_data_visits_users, 
                                           aes(x = month_after_index,
                                               y = mean_visits, 
                                               color = data_source,
                                               group = data_source)) +
    geom_line(size = 1) +
    geom_point() +
    geom_errorbar(aes(
      ymin = mean_visits - se_visits,
      ymax = mean_visits + se_visits
    ), width = 1, alpha = 0.6) +
    scale_color_manual(values = c(
      "OMOP" = "#1f77b4",   # blue
      "Source" = "#ff7f0e"    # orange
    )) +
  
    labs(
      title = "Mean Monthly Outpatient Encounters Over Time\nfor Healthcare Users",
      x = "Month After Index",
      y = "Mean Number of Encounters",
      color = "Data Source"
    ) +
    scale_x_continuous(breaks = 1:24) +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      plot.caption = element_text(size = 10, hjust = 0),
      legend.position = "bottom"
    )
)

# Save the plot
ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "Plots", 
  "mean_visits_plot_users.PNG"),
  plot = mean_visits_plot_combined_users,
  width = 10,
  height = 7,
  bg = "white"
)

combined_data_visits_all <- rbind(final_long_df_encounters_all_patients_all_encounters_omop |> 
                                      rename(patid = subject_id) |> 
                                      mutate(data_source = "OMOP"),
                                    
                                  final_long_df_encounters_all_patients_all_encounters_raw |> 
                                      mutate(data_source = "Source")
) |> 
  mutate(tot_encounters = if_else(is.na(tot_encounters), 0, tot_encounters)) |> 
  group_by(data_source, month_after_index) |> 
  summarise(mean_visits = mean(tot_encounters),
            n_exposed = sum(tot_encounters),
            se_visits = sd(tot_encounters)/n_exposed)

(mean_visits_plot_combined_all<- ggplot(combined_data_visits_all, 
                                           aes(x = month_after_index,
                                               y = mean_visits, 
                                               color = data_source,
                                               group = data_source)) +
    geom_line(size = 1) +
    geom_point() +
    geom_errorbar(aes(
      ymin = mean_visits - se_visits,
      ymax = mean_visits + se_visits
    ), width = 1, alpha = 0.6) +
    scale_color_manual(values = c(
      "OMOP" = "#1f77b4",   # blue
      "Source" = "#ff7f0e"    # orange
    )) +
    
    labs(
      title = "Mean Monthly Outpatient Encounters Over Time\nfor All Patients",
      x = "Month After Index",
      y = "Mean Number of Encounters",
      color = "Data Source"
    ) +
    scale_x_continuous(breaks = 1:24) +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      plot.caption = element_text(size = 10, hjust = 0),
      legend.position = "bottom"
    )
)

# Save the plot
ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "Plots", 
  "mean_visits_plot_all.PNG"),
  plot = mean_visits_plot_combined_all,
  width = 10,
  height = 7,
  bg = "white"
)

export(combined_data_visits_all, here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", 
  "combined_data_visits_all.xlsx"))

# Costs ----
combined_data_costs_all <- rbind(final_long_df_costs_all_patients_all_encounters_omop |> 
                                      rename(patid = subject_id) |> 
                                      mutate(data_source = "OMOP"),
                                    
                                    final_long_df_costs_all_patients_all_encounters_raw |> 
                                      mutate(data_source = "Source")
) |> 
  mutate(total_patient_cost = if_else(is.na(total_patient_cost), 0, total_patient_cost)) |> 
  group_by(data_source, month_after_index) |> 
  summarise(mean_visit_costs = mean(total_patient_cost),
            n_exposed = sum(total_patient_cost),
            se_costs = sd(total_patient_cost)/n_exposed)

export(combined_data_costs_all, here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", 
  "combined_data_costs_all.xlsx"))



(mean_visits_costs_plot_combined_all <- ggplot(combined_data_costs_users, 
                                           aes(x = month_after_index,
                                               y = mean_visit_costs, 
                                               color = data_source,
                                               group = data_source)) +
    geom_line(size = 1) +
    geom_point() +
    geom_errorbar(aes(
      ymin = mean_visit_costs - se_costs,
      ymax = mean_visit_costs + se_costs
    ), width = 1, alpha = 0.6) +
    scale_color_manual(values = c(
      "OMOP" = "#2ca02c",   # blue
      "Source" = "#9467bd"    # orange
    )) +
    
    labs(
      title = "Mean Monthly Outpatient Costs Over Time\nfor All Patients",
      x = "Month After Index",
      y = "Mean Costs",
      color = "Data Source"
    ) +
    scale_x_continuous(breaks = 1:24) +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      plot.caption = element_text(size = 10, hjust = 0),
      legend.position = "bottom"
    )
)

ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "Plots", 
  "mean_costs_plot_all.PNG"),
  plot = mean_visits_costs_plot_combined_all,
  width = 10,
  height = 7,
  bg = "white"
)


combined_data_costs_users <- rbind(final_long_df_costs_users_all_encounters_omop |> 
                                     rename(patid = subject_id) |> 
                                     mutate(data_source = "OMOP"),
                                   
                                 final_long_df_costs_users_all_encounters_raw |> 
                                     mutate(data_source = "Source")
) |> 
  mutate(total_patient_cost = if_else(is.na(total_patient_cost), 0, total_patient_cost)) |> 
  group_by(data_source, month_after_index) |> 
  summarise(mean_visit_costs = mean(total_patient_cost),
            n_exposed = sum(total_patient_cost),
            se_costs = sd(total_patient_cost)/n_exposed)

export(combined_data_costs_users, here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", 
  "combined_data_costs_users.xlsx"))



(mean_visits_costs_plot_combined_users <- ggplot(combined_data_costs_users, 
                                                 aes(x = month_after_index,
                                                     y = mean_visit_costs, 
                                                     color = data_source,
                                                     group = data_source)) +
    geom_line(size = 1) +
    geom_point() +
    geom_errorbar(aes(
      ymin = mean_visit_costs - se_costs,
      ymax = mean_visit_costs + se_costs
    ), width = 1, alpha = 0.6) +
    scale_color_manual(values = c(
      "OMOP" = "#2ca02c",   # blue
      "Source" = "#9467bd"    # orange
    )) +
    
    labs(
      title = "Mean Monthly Outpatient Costs Over Time\nfor Healthcare Users",
      x = "Month After Index",
      y = "Mean Costs",
      color = "Data Source"
    ) +
    scale_x_continuous(breaks = 1:24) +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      plot.caption = element_text(size = 10, hjust = 0),
      legend.position = "bottom"
    )
)

ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "Plots", 
  "mean_costs_plot_users.PNG"),
  plot = mean_visits_costs_plot_combined_users,
  width = 10,
  height = 7,
  bg = "white"
)
