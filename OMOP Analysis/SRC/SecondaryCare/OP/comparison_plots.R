monthly_bootstrap_combined_data_visits_users <- rbind(
  monthly_bootstrap_users_all_encounters_all_months_omop |> 
    mutate(data_source = "OMOP"), 
  monthly_bootstrap_cost_all_patients_all_encounters_all_months_raw |> 
    mutate(data_source = "Source")
) 
export(monthly_bootstrap_combined_data_visits_users, here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", 
  "monthly_bootstrap_combined_data_visits_users.xlsx"))

(mean_visits_plot_combined_users <- ggplot(monthly_bootstrap_combined_data_visits_users, 
                                           aes(x = month,
                                               y = mean, 
                                               color = data_source,
                                               group = data_source)) +
    geom_line(size = 1) +
    geom_point() +
    geom_errorbar(aes(
      ymin = ci_lower_basic,
      ymax = ci_upper_basic
    ), width = 1, alpha = 0.6) +
    scale_color_manual(values = c(
      "OMOP" = "#1f77b4",   # blue
      "Source" = "#ff7f0e"    # orange
    )) +
  
    labs(
      title = "Bootstrap Mean Monthly Outpatient Encounters Over Time\nfor Healthcare Users",
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
  "bootstrap_mean_visits_plot_users.PNG"),
  plot = mean_visits_plot_combined_users,
  width = 10,
  height = 7,
  bg = "white"
)

monthly_bootstrap_combined_data_visits_all <- rbind(
  monthly_bootstrap_all_patients_all_encounters_all_months_omop |> 
    mutate(data_source = "OMOP"),
  monthly_bootstrap_all_patients_all_encounters_all_months_raw |> 
    mutate(data_source = "Source")
) 
(mean_visits_plot_combined_all<- ggplot(monthly_bootstrap_combined_data_visits_all, 
                                           aes(x = month,
                                               y = mean, 
                                               color = data_source,
                                               group = data_source)) +
    geom_line(size = 1) +
    geom_point() +
    geom_errorbar(aes(
      ymin = ci_lower_basic,
      ymax = ci_upper_basic
    ), width = 1, alpha = 0.6) +
    scale_color_manual(values = c(
      "OMOP" = "#1f77b4",   # blue
      "Source" = "#ff7f0e"    # orange
    )) +
    
    labs(
      title = "Bootstrap Mean Monthly Outpatient Encounters Over Time\nfor All Patients",
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
  "boostratp_mean_visits_plot_all.PNG"),
  plot = mean_visits_plot_combined_all,
  width = 10,
  height = 7,
  bg = "white"
)

export(monthly_bootstrap_combined_data_visits_all, here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", 
  "bootstrap_combined_data_visits_all.xlsx"))

# Costs ----
monthly_bootstrap_combined_data_costs_all <- rbind(
  monthly_bootstrap_cost_all_patients_all_encounters_all_months_omop |> 
    mutate(data_source = "OMOP"),
  monthly_bootstrap_cost_all_patients_all_encounters_all_months_raw |>
    mutate(data_source = "Source")
  ) 

export(monthly_bootstrap_combined_data_costs_all, here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", 
  "monthly_bootstrap_combined_data_costs_all.xlsx"))



(mean_visits_costs_plot_combined_all <- ggplot(monthly_bootstrap_combined_data_costs_all, 
                                           aes(x = month,
                                               y = mean, 
                                               color = data_source,
                                               group = data_source)) +
    geom_line(size = 1) +
    geom_point() +
    geom_errorbar(aes(
      ymin = ci_lower_basic,
      ymax = ci_upper_basic
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
  "bootstrap_mean_costs_plot_all.PNG"),
  plot = mean_visits_costs_plot_combined_all,
  width = 10,
  height = 7,
  bg = "white"
)


monthly_bootstrap_combined_data_costs_users <- rbind(
  monthly_bootstrap_cost_users_all_encounters_all_months_omop |>
    mutate(data_source = "OMOP"),
  monthly_bootstrap_cost_users_all_encounters_all_months_raw |>
    mutate(data_source = "Source")
  ) 

export(monthly_bootstrap_combined_data_costs_users, here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", 
  "monthly_bootstrap_combined_data_costs_users.xlsx"))



(mean_visits_costs_plot_combined_users <- ggplot(monthly_bootstrap_combined_data_costs_users, 
                                                 aes(x = month,
                                                     y = mean, 
                                                     color = data_source,
                                                     group = data_source)) +
    geom_line(size = 1) +
    geom_point() +
    geom_errorbar(aes(
      ymin = ci_lower_basic,
      ymax = ci_upper_basic
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
  "bootstrap_mean_costs_plot_users.PNG"),
  plot = mean_visits_costs_plot_combined_users,
  width = 10,
  height = 7,
  bg = "white"
)
