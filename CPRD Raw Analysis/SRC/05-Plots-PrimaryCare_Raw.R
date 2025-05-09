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
  specialty_reduced, follow_up_end, death_date
)


### Users: PLOT visits per month/specialty   -------

# Filter for relevant specialties
visit_data_reduced_user.1 <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute the total number of visits per patient per month
total_visits_per_patient_month.1 <- visit_data_reduced_user.1 %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute the mean number of visits per month across fully contributing patients
mean_visits_per_month.1 <- total_visits_per_patient_month.1 %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_count = sum(total_visits) / n_distinct(patid),  # Actual mean
    .groups = "drop"
  )

# Step 3: Compute the total number of visits per specialty per month
visits_per_specialty_month.1 <- visit_data_reduced_user.1 %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_visits_specialty = sum(visit_count), .groups = "drop")

# Step 4: Merge mean visits and compute proportional contribution per specialty
plots_users_mean_visits_specialty.1 <- visits_per_specialty_month.1 %>%
  left_join(mean_visits_per_month.1, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE)) %>%
  mutate(prop_mean = mean_visit_count * proportion)

# Step 5: Plot
(plot1 <- ggplot(
  plots_users_mean_visits_specialty.1,
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
    scale_y_continuous(limits = c(0, 4)) +
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
  "CPRD Raw Analysis", "Results", "Plots",
  "Mean_visits_per_specialty_unweighted.PNG"),
  plot = plot1,
  width = 7.5,
  height = 7,
  bg = "white"
)

### All: PLOT visits per month/specialty   -------
# 
# # Filter for relevant specialties
# # Sense check :2471999720603
# visit_data_reduced_all <- visit_data_reduced_MIN %>%
#   mutate(month_after_index = trunc((as.numeric(difftime(
#     consdate, index_date,units = "days"))/30.44) + 1)) %>%
#   mutate(month_after_index = replace_na(month_after_index, 0)) %>% 
#   group_by(patid, month_after_index, specialty_reduced) %>%
#   summarise(visit_count = n(), .groups = "drop") %>% 
#   mutate(visit_count = if_else(
#     specialty_reduced == "Not Clinically Relevant", 0, visit_count
#   ))
# 
# # Step 1: Compute the total number of visits per patient per month
# total_visits_per_patient_month <- visit_data_reduced_all %>%
#   group_by(patid, month_after_index) %>%
#   summarise(total_visits = sum(visit_count), .groups = "drop")
# 
# # Step 2: Compute the mean number of visits per month across fully contributing patients
# mean_visits_per_month <- total_visits_per_patient_month %>%
#   group_by(month_after_index) %>%
#   summarise(
#     mean_visit_count = sum(total_visits) / n_distinct(patid),  # Actual mean
#     .groups = "drop"
#   ) 
# 
# # Step 3: Compute the total number of visits per specialty per month
# visits_per_specialty_month <- visit_data_reduced_all %>%
#   group_by(month_after_index, specialty_reduced) %>%
#   summarise(total_visits_specialty = sum(visit_count), .groups = "drop") 
# 
# # Step 4: Merge mean visits and compute proportional contribution per specialty
# plots_users_mean_visits_specialty <- visits_per_specialty_month %>%
#   left_join(mean_visits_per_month, by = "month_after_index") %>%
#   group_by(month_after_index) %>%
#   mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE)) %>%
#   mutate(prop_mean = mean_visit_count * proportion)
# 
# # Step 5: Plot
# (plot1.1 <- ggplot(
#   plots_users_mean_visits_specialty,
#   aes(
#     x = month_after_index,
#     y = prop_mean,
#     fill = specialty_reduced
#   )
# ) +
#     geom_bar(stat = "identity", position = "stack", width = 0.8) +
#     scale_fill_manual(values = c(
#       "General Practitioner" = "#B4CDCD",
#       "Nurse" = "#377eb8",
#       "Other" = "#253494"
#     )) +
#     labs(
#       x = "Month After Index",
#       y = "Mean Visits per Healthcare & Non- Service User",
#       fill = "Specialty"
#     ) +
#     scale_y_continuous(limits = c(0, 2.5)) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(face = "bold", hjust = 0.5),
#       axis.title = element_text(hjust = 0.5, size = 18),
#       axis.text = element_text(size = 16),
#       legend.text = element_text(size = 15),
#       legend.title = element_text(size = 14),
#       legend.position = "top",
#       panel.spacing = unit(0, "lines")
#     ))
# 
# ggsave(here::here(
#   "CPRD Raw Analysis", "Results", "Plots",
#   "Mean_visits_per_specialty_unweighted_all.PNG"),
#   plot = plot1.1,
#   width = 6,
#   height = 5,
#   bg = "white"
# )

### User: PLOT Tot Visits ------

visit_data_reduced_user2 <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(
    consdate, index_date,units = "days"))/30.44) + 1)) %>%
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
                       limits = c(0, 70000)) +
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
  width = 7,
  height = 6,
  bg = "white"
)

### All: PLOT Tot Visits ------

visit_data_reduced_all2 <- visit_data_reduced_MIN %>%
  mutate(month_after_index = interval(
    index_date, consdate) %/% months(1)) %>%
  mutate(month_after_index = replace_na(month_after_index, 0)) %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop") %>% 
  mutate(visit_count = if_else(
    specialty_reduced == "Not Clinically Relevant", 0, visit_count
  )) %>% 
  group_by(month_after_index) %>% 
  summarise(tot_visits = sum(visit_count),
            .groups = "drop")

(plot2.1 <- ggplot(
  visit_data_reduced_all2,
  aes(
    x = month_after_index,
    y = tot_visits
  )
) +
    geom_bar(
      stat = "identity",
      fill = "#1f77b4"
    ) + # Blue color for the bars
    labs(
      x = "Month After Index",
      y = "Total Visits (Healthcare & Non- Service Users)"
    ) +
    scale_y_continuous(labels = scales::comma,
                       limits = c(0, 60000)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 16),
      panel.spacing = unit(0, "lines")
    ))

ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots", "Tot_visits_all.PNG"),
  plot = plot2.1,
  width = 6,
  height = 5,
  bg = "white"
)
### PLOT exposure weighted ------
# Prepare data (remove "Not Clinically Relevant" and compute monthly bins)
# Sense check: 55196220200
adjusted_visits_data_r <- visit_data_reduced %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  group_by(patid) %>% 
  mutate(
    # Calculate month after index using 30.44 days/month approximation
    month_after_index = trunc((as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1),
    
    # Define month start and end using as.difftime to handle decimal days
    month_start = index_date + as.difftime((month_after_index - 1) * 30.44, units = "days"),
    month_end = index_date + as.difftime(month_after_index * 30.44, units = "days"),
    
    # Calculate exposure details
    exposure_end = as.Date(follow_up_end),
    days_exposed_in_month = as.numeric(pmin(exposure_end, month_end) - pmax(index_date, month_start) + 1),
    
    # Flags for contribution and death
    fully_contributed = days_exposed_in_month >= 30.44,
    half_contributed = days_exposed_in_month >= 15.22,
    died_in_month = !is.na(death_date) & death_date >= month_start & death_date <= month_end
  ) %>%
  ungroup() %>% 
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    days_exposed_in_month = first(days_exposed_in_month),
    fully_contributed = first(fully_contributed),
    half_contributed = first(half_contributed),
    died_in_month = first(died_in_month),
    .groups = "drop"
  ) %>%
  filter(half_contributed == TRUE) %>%
  mutate(
    adjusted_visits = case_when(
      fully_contributed ~ visit_count,
      died_in_month ~ visit_count,  # No adjustment if patient died that month
      TRUE ~ (visit_count / days_exposed_in_month) / 30.44
    )
  )
# Step 1: Compute total visits per patient per month
total_visits_per_patient_month.2 <- adjusted_visits_data_r %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute mean number of visits per month (no weighting)
mean_visits_per_month.2 <- total_visits_per_patient_month.2 %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = sum(total_visits) / n_distinct(patid), .groups = "drop")

# Step 3: Compute total visits per specialty per month
visits_per_specialty_month.2 <- adjusted_visits_data_r %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_visits_specialty = sum(visit_count), .groups = "drop")

# Step 4: Merge and compute specialty proportions
plots_users_mean_visits_specialty.2 <- visits_per_specialty_month.2 %>%
  left_join(mean_visits_per_month.2, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE)) %>%
  mutate(prop_mean = mean_visit_count * proportion)

# Step 5: Plot
(plot3 <- ggplot(
  plots_users_mean_visits_specialty.2,
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
      y = "Mean Visits per Healthcare Service User",
      fill = "Specialty",
      caption = "Visit counts were adjusted for partial month exposure when patients contributed ≥15.22 days. \nCounts were scaled by exposure days, except for full-month contributions or if the patient died during the month. \nRecords with <15.22 days were excluded."
    ) +
    scale_y_continuous(limits = c(0, 4)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 15),
      legend.position = "top",
      plot.caption = element_text(size = 15),
      panel.spacing = unit(2, "lines")
    ))

ggsave(here::here("CPRD Raw Analysis", "Results", "Plots", "Mean_visits_per_specialty_weighted.PNG"),
       plot = plot3,
       width = 7.5,
       height = 7,
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
   geom_bar(stat = "identity", fill="#A52A2A", alpha=0.9) +
   labs(x = "Total Number of Visits", y = "% of Subjects") +
   scale_y_continuous(limits = c(0, 4) ,
                      breaks = seq(0, 4, by = 1))+
   coord_cartesian(xlim = c(0, 250)) +
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
    scale_y_continuous(labels = scales::comma,
                       limits = c(0, 20000)) +
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
    specialty_reduced, follow_up_end, unit_cost, death_date
  )

### PLOT costs per month/specialty   -------
# Step 0: Filter and calculate month_end
visit_data_reduced_user_c.1 <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
  ungroup() %>% 
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct(patid, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE)

# Step 1: Total cost per patient per month
total_costs_per_patient_month.1 <- visit_data_reduced_user_c.1 %>%
  group_by(patid, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# Step 2: Mean cost per month
mean_costs_per_month.1 <- total_costs_per_patient_month.1 %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs = mean(total_costs),
    .groups = "drop"
  )

# Step 3: Total cost per specialty per month
costs_per_specialty_month.1 <- visit_data_reduced_user_c.1 %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_costs_specialty = sum(visit_costs), .groups = "drop")

# Step 4: Merge and calculate proportions
plots_users_mean_costs_specialty_weighted.1 <- costs_per_specialty_month.1 %>%
  left_join(mean_costs_per_month.1, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(
    proportion = total_costs_specialty / sum(total_costs_specialty, na.rm = TRUE),
    prop_mean = mean_costs * proportion
  )

# Step 5: Plot
(plot_costs1 <- ggplot(
  plots_users_mean_costs_specialty_weighted.1,
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
      plot.caption = element_text(size = 15),
      panel.spacing = unit(2, "lines")
    ))

ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots",
  "Mean_costs_per_specialty_unweighted.PNG"),
  plot = plot_costs1,
  width = 7.5,
  height = 7,
  bg = "white"
)

### PLOT Tot Costs ------

visit_data_reduced_user_c2 <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(
    consdate, index_date,units = "days"))/30.44) + 1)) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(),
            visit_costs = visit_count * unit_cost,
            .groups = "drop") %>% 
  distinct(patid, month_after_index, specialty_reduced, visit_costs, 
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
      axis.text = element_text(size = 16),
      panel.spacing = unit(0, "lines")
    ))

ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots", "Tot_costs.PNG"),
  plot = plot_costs2,
  width = 7,
  height = 6,
  bg = "white"
)


### PLOT exposure weighted ------
# Step 0: Filter and calculate month_end
# Sense check: 23520018,243220018
adjusted_costs_data_r <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(
    # Calculate month index
    month_after_index = trunc((as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1),
    
    # Fix class conflict with as.Date()
    month_start = as.Date(index_date) + as.difftime((month_after_index - 1) * 30.44, units = "days"),
    month_end   = as.Date(index_date) + as.difftime(month_after_index * 30.44, units = "days"),
    
    # Exposure calculations
    exposure_end = as.Date(follow_up_end),
    days_exposed_in_month = as.numeric(pmin(exposure_end, month_end) - pmax(as.Date(index_date), month_start) + 1),
    
    # Contribution flags
    fully_contributed = days_exposed_in_month >= 30.44,
    half_contributed  = days_exposed_in_month >= 15.22,
    died_in_month     = !is.na(death_date) & death_date >= month_start & death_date <= month_end
  ) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    days_exposed_in_month = first(days_exposed_in_month),
    fully_contributed = first(fully_contributed),
    half_contributed  = first(half_contributed),
    died_in_month     = first(died_in_month),
    unit_cost         = first(unit_cost),
    .groups = "drop"
  ) %>%
  filter(half_contributed == TRUE) %>%
  mutate(
    adjusted_visits = case_when(
      fully_contributed ~ visit_count,
      died_in_month     ~ visit_count,
      TRUE              ~ (visit_count / days_exposed_in_month) / 30.44
    ),
    visit_costs = adjusted_visits * unit_cost
  )

# Step 1: Total cost per patient per month
total_costs_per_patient_month.2 <- adjusted_costs_data_r %>%
  group_by(patid, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# Step 2: Mean cost per month
mean_costs_per_month.2 <- total_costs_per_patient_month.2 %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs = mean(total_costs),
    .groups = "drop"
  )

# Step 3: Total cost per specialty per month
costs_per_specialty_month.2 <- adjusted_costs_data_r %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_costs_specialty = sum(visit_costs), .groups = "drop")

# Step 4: Merge and calculate proportions
plots_users_mean_costs_specialty_weighted.2 <- costs_per_specialty_month.2 %>%
  left_join(mean_costs_per_month.2, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(
    proportion = total_costs_specialty / sum(total_costs_specialty, na.rm = TRUE),
    prop_mean = mean_costs * proportion
  )

# Step 5: Plot
(plot_costs3 <- ggplot(
  plots_users_mean_costs_specialty_weighted.2,
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
      fill = "Specialty",
      caption = "Visit counts were adjusted for partial month exposure when patients contributed ≥15.22 days. \nCounts were scaled by exposure days, except for full-month contributions or if the patient died during the month. \nRecords with <15.22 days were excluded."
    ) +
    scale_y_continuous(limits = c(0, 135)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 15),
      legend.position = "top",
      plot.caption = element_text(size = 15),
      panel.spacing = unit(2, "lines")
    ))


ggsave(here::here(
  "CPRD Raw Analysis", "Results", "Plots", "Mean_costs_per_specialty_weighted.PNG"),
  plot = plot_costs3,
  width = 7.5,
  height = 7,
  bg = "white"
)


