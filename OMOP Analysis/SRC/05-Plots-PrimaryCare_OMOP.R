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
  specialty_reduced, follow_up_end
)

### PLOT visits per month/specialty   -------

# Filter for relevant specialties and compute months since index
visit_data_reduced_user.1 <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>% 
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute the total number of visits per patient per month 
# (sum across specialties)
total_visits_per_patient_month.1 <- visit_data_reduced_user.1 %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute the mean number of visits per month across all patients
mean_visits_per_month.1 <- total_visits_per_patient_month.1 %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_count = sum(total_visits) / n_distinct(subject_id),
    .groups = "drop"
  )

# Step 3: Compute the total number of visits per specialty per month
visits_per_specialty_month.1 <- visit_data_reduced_user.1 %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_visits_specialty = sum(visit_count), .groups = "drop")

# Step 4: Merge the total mean visits per month
plots_users_mean_visits_specialty.1 <- visits_per_specialty_month.1 %>%
  left_join(mean_visits_per_month.1, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE)) %>% 
  mutate(prop_mean = mean_visit_count * proportion)

# Step 5: Plot the mean visits per month with specialty proportions
(plot1 <- ggplot(
  plots_users_mean_visits_specialty.1,
  aes(
    x = factor(month_after_index),
    y = mean_visit_count * proportion,
    fill = specialty_reduced
  )
) +
    geom_bar(
      stat = "identity",
      position = "stack",
      width = 0.8
    ) +
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
    scale_y_continuous(limits = c(0, 4)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 14),
      legend.position = "top",
      panel.spacing = unit(0, "lines")
    )
  )

ggsave(here::here(
  "OMOP Analysis", "Results", "Plots",
  "Mean_visits_per_specialty_unweighted.PNG"),
  plot = plot1,
  width = 8.5,
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
visit_data_reduced_MIN <- visit_data_reduced %>% select(
  subject_id, index_date, visit_detail_start_date, user_status,
  specialty_reduced, follow_up_end, exposed_yrs
)

# Prepare data (remove "Not Clinically Relevant" and compute monthly bins)
visit_data_reduced_user.2 <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
  mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44) + 1)) %>% 
  group_by(subject_id) %>% 
  filter(full_month_after_index >= month_after_index) %>%  # Fully contributing patients only
  # removed 29,293 rows
  ungroup() %>% # Fully contributing patients only
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute total visits per patient per month
total_visits_per_patient_month.2 <- visit_data_reduced_user.2 %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute mean number of visits per month (no weighting)
mean_visits_per_month.2 <- total_visits_per_patient_month.2 %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = sum(total_visits) / n_distinct(subject_id), .groups = "drop")

# Step 3: Compute total visits per specialty per month
visits_per_specialty_month.2 <- visit_data_reduced_user.2 %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_visits_specialty = sum(visit_count), .groups = "drop")

# Step 4: Merge and compute specialty proportions
plots_users_mean_visits_specialty.2 <- visits_per_specialty_month.2 %>%
  left_join(mean_visits_per_month.2, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE)) %>%
  mutate(prop_mean = mean_visit_count * proportion)

# Step 6: Plot the exposure-weighted mean visits per month with specialty proportions
(plot3 <- ggplot(
  plots_users_mean_visits_specialty.2,
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
      y = "Mean Visits per Health Care Service Users",
      fill = "Specialty",
      caption = "Mean calculated among healthcare service users \ncontributing fully to each month."
      ) +
    scale_y_continuous(limits = c(0, 3.5)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 15),
      legend.position = "top",
      plot.caption = element_text(size = 15),
      panel.spacing = unit(0, "lines")
    ))

ggsave(here::here("OMOP Analysis", "Results",
                  "Plots", "Mean_visits_per_specialty_weighted.PNG"),
       plot = plot3, 
       width = 8.5, 
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
    scale_y_continuous(limits = c(0, 4) ,
                       breaks = seq(0, 4, by = 1))+
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
    specialty_reduced, follow_up_end, unit_cost
  )

### PLOT costs per month/specialty   -------

# Step 0: Filter and calculate month_end
visit_data_reduced_user_c.1 <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct(subject_id, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE)

# Step 1: Total cost per patient per month
total_costs_per_patient_month.1 <- visit_data_reduced_user_c.1 %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# Step 2: Mean cost per month across all users
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
plots_users_mean_costs_specialty.1 <- costs_per_specialty_month.1 %>%
  left_join(mean_costs_per_month.1, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(
    proportion = total_costs_specialty / sum(total_costs_specialty, na.rm = TRUE),
    prop_mean = mean_costs * proportion
  )

# Step 5: Plot
(plot_costs1 <- ggplot(
  plots_users_mean_costs_specialty.1,
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
      y = "Mean Costs per Healthcare Service User",
      fill = "Specialty"
    ) +
    # scale_y_continuous(limits = c(0, 100)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15),
      legend.position = "top",
      panel.spacing = unit(0, "lines")
    ))

ggsave(here::here(
  "OMOP Analysis", "Results", "Plots", 
  "Mean_costs_per_specialty_unweighted.PNG"),
  plot = plot_costs1, 
  width = 8.5,
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
# Step 0: Filter and calculate month_end
visit_data_reduced_user_c.1 <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
  mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44))) %>% 
  group_by(subject_id) %>% 
  filter(full_month_after_index >= month_after_index) %>%  # Fully contributing patients only
  # removed 29,293 rows
  ungroup() %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct(subject_id, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE)

# Step 1: Total cost per patient per month
total_costs_per_patient_month.1 <- visit_data_reduced_user_c.1 %>%
  group_by(subject_id, month_after_index) %>%
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
(plot_costs3 <- ggplot(
  plots_users_mean_costs_specialty_weighted.1,
  aes(x = factor(month_after_index), y = prop_mean, fill = specialty_reduced)
) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c(
      "General Practitioner" = "#B4CDCD",
      "Nurse" = "#377eb8",
      "Other" = "#253494",
      "Uncategorised" = "#53868B"
    )) +
    labs(
      x = "Month After Index",
      y = "Mean Costs per Healthcare Service User",
      fill = "Specialty",
      caption = "Includes only healthcare service users \ncontributing full month."
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 15),
      legend.position = "top",
      plot.caption = element_text(size = 15),
      panel.spacing = unit(0, "lines")
    ))


ggsave(here::here(
  "OMOP Analysis", "Results", "Plots",
  "Mean_costs_per_specialty_weighted.PNG"),
  plot = plot_costs3, 
  width = 8.5, 
  height = 7, 
  bg = "white"
)
