# **************************************************************************** #
# 	Project name: Validation Study
# 	Author: Njoki Njuki/ Gianluca Fabiano
#   Do file name: Primary care analysis - Plots
#   Date: 03.03.2025
#   Last date edited: 03.03.2025
# **************************************************************************** #

# starting with visit_data_reduced from 04-Analysis-PrimaryCare_Raw.R


# Plots ----
## HRCU ----

visit_data_reduced_MIN <- visit_data_reduced %>% select(
  patid, index_date, consdate, user_status,
  specialty_reduced, follow_up_end
)


### PLOT visits per month/specialty   -------

# Filter for relevant specialties
visit_data_reduced_user <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>%
  mutate(month_after_index = interval(index_date, consdate) %/% months(1),
         month_start = index_date %m+% months(month_after_index),
         month_end = ceiling_date(month_start, unit = "month") - days(1)) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute the total number of visits per patient per month
total_visits_per_patient_month <- visit_data_reduced_user %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute the mean number of visits per month across fully contributing patients
mean_visits_per_month <- total_visits_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_count = sum(total_visits) / n_distinct(patid),  # Actual mean
    .groups = "drop"
  )

# Step 3: Compute the total number of visits per specialty per month
visits_per_specialty_month <- visit_data_reduced_user %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_visits_specialty = sum(visit_count), .groups = "drop")

# Step 4: Merge mean visits and compute proportional contribution per specialty
plots_users_mean_visits_specialty <- visits_per_specialty_month %>%
  left_join(mean_visits_per_month, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE)) %>%
  mutate(prop_mean = mean_visit_count * proportion)

# Step 5: Plot
(plot1 <- ggplot(
  plots_users_mean_visits_specialty,
  aes(
    x = month_after_index,
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
      y = "Mean Visits per User",
      fill = "Specialty"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 14),
      legend.position = "top",
      panel.spacing = unit(0, "lines")
    ))

ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots",
  "Mean_visits_per_specialty_unweighted.PNG"),
  plot = plot1,
  width = 6,
  height = 5,
  bg = "white"
)

### PLOT Tot Visits ------

visit_data_reduced_user2 <- visit_data_reduced_MIN %>%
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
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 16),
      panel.spacing = unit(0, "lines")
    ))

ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots", "Tot_visits.PNG"),
  plot = plot2,
  width = 5,
  height = 4,
  bg = "white"
)


### PLOT exposure weighted ------
# Prepare data (remove "Not Clinically Relevant" and compute monthly bins)
visit_data_reduced_user <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(month_after_index = interval(index_date, consdate) %/% months(1),
         month_start = index_date %m+% months(month_after_index),
         month_end = ceiling_date(month_start, unit = "month") - days(1)) %>%
  filter(follow_up_end >= month_end) %>%  # Fully contributing patients only
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute total visits per patient per month
total_visits_per_patient_month <- visit_data_reduced_user %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute mean number of visits per month (no weighting)
mean_visits_per_month <- total_visits_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = sum(total_visits) / n_distinct(patid), .groups = "drop")

# Step 3: Compute total visits per specialty per month
visits_per_specialty_month <- visit_data_reduced_user %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_visits_specialty = sum(visit_count), .groups = "drop")

# Step 4: Merge and compute specialty proportions
plots_users_mean_visits_specialty <- visits_per_specialty_month %>%
  left_join(mean_visits_per_month, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE)) %>%
  mutate(prop_mean = mean_visit_count * proportion)

# Step 5: Plot
(plot3 <- ggplot(
  plots_users_mean_visits_specialty,
  aes(x = month_after_index, y = prop_mean, fill = specialty_reduced)
) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
    scale_fill_manual(values = c(
      "General Practitioner" = "#B4CDCD",
      "Nurse" = "#377eb8",
      "Other" = "#253494"
    )) +
    labs(
      x = "Month After Index",
      y = "Mean Visits per User",
      fill = "Specialty",
      caption = "Mean calculated among users contributing fully to each month."
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 14),
      legend.position = "top",
      plot.caption = element_text(size = 13),
      panel.spacing = unit(0, "lines")
    ))

ggsave(here::here("CPRD Raw Analysis", "Results", "Plots", "Mean_visits_per_specialty_weighted.PNG"),
       plot = plot3,
       width = 6,
       height = 5,
       bg = "white"
       )

### PLOT of number of visits and visits per year

visit_data_MIN <- visit_data %>% select(
  patid, index_date, consdate, user_status,visit_count,visits_per_year,
  specialty, follow_up_end, exposed_yrs, visit_cost
)
# 
# sum_of_visits <- visit_data_MIN %>% 
#   group_by(patid, specialty) %>%
#   summarise(total_patient_visits = first(visit_count),
#             total_exposed_yrs = first(exposed_yrs),
#             visit_cost = first(visit_cost),
#             .groups = "drop") %>% 
#   group_by(patid) %>% 
#   summarise(sum_visits = sum(total_patient_visits),
#             total_exposed_yrs = first(total_exposed_yrs),
#             visits_per_year = sum_visits / total_exposed_yrs,
#             sum_visit_cost = sum(visit_cost),
#             visit_cost_per_year = sum_visit_cost / total_exposed_yrs,
#             .groups = "drop")
# 
# (plot4 <- sum_of_visits %>% 
#   ggplot() +
#   geom_histogram(aes(x = sum_visit_cost, y = ..density.., 
#                      fill = "Sum Visit Cost"), alpha=0.5, bins=30) +
#   geom_histogram(aes(x = visit_cost_per_year, y = ..density.., 
#                      fill = "Visit Cost Per Year"), alpha=0.5, bins=30) +
#   geom_density(aes(x = sum_visit_cost, y = ..density.., 
#                    color = "Sum Visit Cost"), alpha=0.5) +
#   geom_density(aes(x = visit_cost_per_year, y = ..density.., 
#                    color="Visit Cost Per Year"), alpha=0.5) +
#   labs(x = "Number of Visits", y = "Density",
#        title = "") +
#   scale_fill_manual(name = "", values = c("Sum Visit Cost" = "#CD00CD",
#                                           "Visit Cost Per Year" = "#2A52A5")) +
#   scale_color_manual(name = "", values = c("Sum Visit Cost" = "#CD00CD", 
#                                            "Visit Cost Per Year" = "#2A52A5")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
#     axis.title = element_text(hjust = 0.5, size = 11),
#     panel.spacing = unit(0.1, "lines"),
#     legend.position = "right"
#   ))
# 
# # ggsave(here::here("CPRD Raw Analysis", "Results", "Plots", "Density_Plot.PNG"),
# #        plot = plot4, 
# #        width = 8, 
# #        height = 6, 
# #        bg = "white"
# # )
# 
# (plot6 <- sum_of_visits %>% 
#   ggplot() +
#   geom_histogram(aes(x = sum_visits, y = ..count..), 
#                      fill = "#CD2626",
#                  alpha=0.8, bins=100) +
#   labs(x = "Number of Visits", y = "Count",
#        title = "") +
#   theme_minimal() +
#     theme(
#     plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
#     axis.title = element_text(hjust = 0.5, size = 11),
#     panel.spacing = unit(0.1, "lines")
#   ))
# 
# ggsave(here::here("CPRD Raw Analysis", "Results", "Plots", "Histogram_number_of_visits.PNG"),
#        plot = plot6,
#        width = 8,
#        height = 6,
#        bg = "white"
# )



### PLOT - Total Number of Visits ----
pt_count_visits <- visit_data_MIN %>%
  group_by(patid, specialty) %>%
  summarise(total_patient_visits = first(visit_count), .groups = 'drop') %>%
  group_by(patid) %>% 
  summarise(total_patient_visits = sum(total_patient_visits), .groups = 'drop') %>% 
  group_by(total_patient_visits) %>%
  summarise(Subject_Count = n(), .groups = 'drop') %>%
  mutate(Percent_Subjects = Subject_Count / sum(Subject_Count) * 100)

(plot7 <- pt_count_visits %>% 
   ggplot(aes(x = total_patient_visits, y = Percent_Subjects)) +
   geom_bar(stat = "identity", fill="#A52A2A", alpha=0.9, width = 1.0) +
   labs(x = "Total Number of Visits", y = "% of Subjects") +
   # scale_y_continuous(limits = c(0, 9) , 
   #                    breaks = seq(0, 9, by = 1))+
   # coord_cartesian(xlim = c(0, 400)) +
   theme_minimal(base_family = 20) +
   theme(
     plot.title = element_text(face = "bold", hjust = 0.5),
     axis.title = element_text(hjust = 0.5, size = 18),
     axis.text = element_text(size = 16),
     panel.spacing = unit(0, "lines")
   ))
ggsave(here::here("CPRD Raw Analysis", "Results", "Plots", "Bar_Graph_number_of_visits_per_pt.PNG"),
       plot = plot7,
       width = 5,
       height = 4,
       bg = "white"
)

### PLOT - Exposure Time ----
visit_data_MIN_t <- visit_data %>% 
  select(patid, follow_up_time, follow_up_end) %>% 
  distinct() %>% 
  mutate(follow_up_time = as.numeric(follow_up_time))

(plot8 <- visit_data_MIN_t %>% 
  ggplot(aes(x = follow_up_time)) +
    geom_histogram(bins = 30, colour = "white", fill = "#00CDCD") +
    labs(x = "Follow up time",
         y = "Count") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 16),
      axis.text = element_text(size = 14),
      panel.spacing = unit(0, "lines")
    ))

ggsave(here::here("CPRD Raw Analysis", "Results", "Plots", "Exposure_Time.PNG"),
       plot = plot8,
       width = 5,
       height = 4,
       bg = "white"
)
  
## Cost ----

visit_data_reduced_MIN_c <- visit_data_reduced %>% 
  select(
    patid, index_date, consdate, user_status,
    specialty_reduced, follow_up_end, unit_cost
  )

### PLOT costs per month/specialty   -------

# Step 0: Filter and calculate month_end
visit_data_reduced_user_c <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(
    month_after_index = interval(index_date, consdate) %/% months(1),
    month_start = index_date %m+% months(month_after_index),
    month_end = ceiling_date(month_start, unit = "month") - days(1)
  ) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct(patid, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE)

# Step 1: Total cost per patient per month
total_costs_per_patient_month <- visit_data_reduced_user_c %>%
  group_by(patid, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# Step 2: Mean cost per month across all users
mean_costs_per_month <- total_costs_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs = mean(total_costs),
    .groups = "drop"
  )

# Step 3: Total cost per specialty per month
costs_per_specialty_month <- visit_data_reduced_user_c %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_costs_specialty = sum(visit_costs), .groups = "drop")

# Step 4: Merge and calculate proportions
plots_users_mean_costs_specialty <- costs_per_specialty_month %>%
  left_join(mean_costs_per_month, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(
    proportion = total_costs_specialty / sum(total_costs_specialty, na.rm = TRUE),
    prop_mean = mean_costs * proportion
  )

# Step 5: Plot
(plot_costs1 <- ggplot(
  plots_users_mean_costs_specialty,
  aes(
    x = month_after_index,
    y = prop_mean,
    fill = specialty_reduced
  )
) +
    geom_bar(stat = "identity", position = "stack", width = 0.9) +
    scale_fill_manual(values = c(
      "General Practitioner" = "#B4CDCD",
      "Nurse" = "#377eb8",
      "Other" = "#253494"
    )) +
    labs(
      x = "Month After Index",
      y = "Mean Costs per User",
      fill = "Specialty"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 14),
      legend.position = "top",
      plot.caption = element_text(size = 14),
      panel.spacing = unit(0, "lines")
    ))


ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots",
  "Mean_costs_per_specialty_unweighted.PNG"),
  plot = plot_costs1,
  width = 5,
  height = 4,
  bg = "white"
)

### PLOT Tot Costs ------

visit_data_reduced_user_c2 <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = interval(
    index_date, consdate) %/% months(1)) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(),
            visit_costs = visit_count * unit_cost,
            .groups = "drop") %>% 
  distinct(patid, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE)


(plot_costs2 <- ggplot(
  visit_data_reduced_user_c2,
  aes(
    x = month_after_index,
    y = visit_costs
  )
) +
    geom_bar(
      stat = "identity",
      fill = "#1f77b4"
    ) + # Blue color for the bars
    labs(
      x = "Month After Index",
      y = "Total Costs"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 16),
      panel.spacing = unit(0, "lines")
    ))

ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots", "Tot_costs.PNG"),
  plot = plot_costs2,
  width = 5,
  height = 4,
  bg = "white"
)


### PLOT exposure weighted ------
# Step 0: Filter and calculate month_end
visit_data_reduced_user_c <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(
    month_after_index = interval(index_date, consdate) %/% months(1),
    month_start = index_date %m+% months(month_after_index),
    month_end = ceiling_date(month_start, unit = "month") - days(1)
  ) %>%
  filter(follow_up_end >= month_end) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct(patid, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE)

# Step 1: Total cost per patient per month
total_costs_per_patient_month <- visit_data_reduced_user_c %>%
  group_by(patid, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# Step 2: Mean cost per month
mean_costs_per_month <- total_costs_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs = mean(total_costs),
    .groups = "drop"
  )

# Step 3: Total cost per specialty per month
costs_per_specialty_month <- visit_data_reduced_user_c %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_costs_specialty = sum(visit_costs), .groups = "drop")

# Step 4: Merge and calculate proportions
plots_users_mean_costs_specialty_weighted <- costs_per_specialty_month %>%
  left_join(mean_costs_per_month, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(
    proportion = total_costs_specialty / sum(total_costs_specialty, na.rm = TRUE),
    prop_mean = mean_costs * proportion
  )

# Step 5: Plot
(plot_costs3 <- ggplot(
  plots_users_mean_costs_specialty_weighted,
  aes(x = month_after_index, y = prop_mean, fill = specialty_reduced)
) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c(
      "General Practitioner" = "#B4CDCD",
      "Nurse" = "#377eb8",
      "Other" = "#253494"
    )) +
    labs(
      x = "Month After Index",
      y = "Mean Costs per User",
      fill = "Specialty",
      caption = "Includes only users contributing full month."
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 14),
      legend.position = "top",
      plot.caption = element_text(size = 14),
      panel.spacing = unit(0, "lines")
    ))


ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots", "Mean_costs_per_specialty_weighted.PNG"),
  plot = plot_costs3,
  width = 6,
  height = 5,
  bg = "white"
)
