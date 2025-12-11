# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: AE Analysis of the patients' AE attendances
# Date: 26.08.2025
# Last date edited: 28.08.2025
# ****************************************************************************** #

# Starting with table_two_ae_raw from 03-Table2_AE_Raw.R ----
ae_attendance_data <- table_two_ae_raw_final 

# HRCU ----
## Users ----
users_ae_attendance_data <- ae_attendance_data %>%
  filter(user_status == "User") 

users_ae_attendance_per_patient <- users_ae_attendance_data %>%
  group_by(patid) %>%                     # group data per patient
  summarise(n_att = n()) %>%   # derive total number of appointments by picking max value of apptorder
  ungroup()  %>%                          # now, ungroup the data and save only one row summary per patient
  select(patid, n_att)  %>%             # save only patid and nr of appts
  distinct()


### Summary table ----
users_overall_summary_ae_attendance <-  users_ae_attendance_per_patient %>%
  summarise(
    n        = n(),
    mean     = mean(n_att),
    min      = min(n_att),
    max      = max(n_att),
    sd       = sd(n_att),
    median   = quantile(n_att, 0.5),
    lower_q  = quantile(n_att, 0.25),
    upper_q  = quantile(n_att, 0.75),
    se               = sd / sqrt(n),
    ci_lower         = mean - qt(0.975, df = n - 1) * se,
    ci_upper         = mean + qt(0.975, df = n - 1) * se
  ) %>% 
  mutate(across(everything(), round, 2))

## All ----
all_ae_attendance_per_patient <- ae_attendance_data %>%
  group_by(patid) %>%                     
  summarise(
    n_att = if (all(is.na(arrivaldate))) 0L else sum(!is.na(arrivaldate)),
    .groups = "drop"
  )


### Summary table ----
all_overall_summary_ae_attendance <-  all_ae_attendance_per_patient %>%
  summarise(
    n        = n(),
    mean     = mean(n_att),
    min      = min(n_att),
    max      = max(n_att),
    sd       = sd(n_att),
    median   = quantile(n_att, 0.5),
    lower_q  = quantile(n_att, 0.25),
    upper_q  = quantile(n_att, 0.75),
    se               = sd / sqrt(n),
    ci_lower         = mean - qt(0.975, df = n - 1) * se,
    ci_upper         = mean + qt(0.975, df = n - 1) * se
  ) %>% 
  mutate(across(everything(), round, 2))

# Export the file

library(writexl)

ae_attandances_list <- list(
  users_overall_summary_atts = users_overall_summary_ae_attendance,
  all_overall_summary_atts = all_overall_summary_ae_attendance
)

# Write to an Excel file
write_xlsx(ae_attandances_list, here::here("CPRD Raw Analysis", "Results",
                                  "SecondaryCareAnalysis", "AE", "Overall", "Output_AE_attdances_Raw.xlsx"))
# Plot ----
library(ggplot2)

## Users ----
# Calculate the mean number of appointments
users_mean_atts <- mean(users_ae_attendance_per_patient$n_att, na.rm = TRUE)

(users_atts_plot_hist <- ggplot(users_ae_attendance_per_patient, 
                                    aes(x = n_att)) +
    geom_histogram(binwidth = 2, 
                   fill = "#1976d2", 
                   color = "#1e88e5") +
    geom_vline(xintercept = users_mean_atts, 
               linetype = "dashed", 
               color = "red", 
               size = 0.5) +
    annotate("text", 
             x = users_mean_atts, 
             y = Inf, 
             label = paste0("Mean = ", round(users_mean_atts, 1)), 
             vjust = -0.5, 
             hjust = 0.5, 
             color = "red", 
             fontface = "bold") +
    labs(title = "", 
         subtitle = "",
         x = "Number of Attendances (bin width = 2)", 
         y = "Count of Patients",
         caption = "Each bar represents a 2-attendances range e.g., 1-2, 3-4, ...\n up to the range of the data.\nRed line shows the average attendances per OP Healthcare Service User.") +
  theme_minimal(base_size = 14, base_family = "Times") +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 16),
    plot.caption = element_text(size = 14, hjust = 0)
  )
)

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" , "AE", "Plots",
  "Mean_atts_hist_users-Raw.PNG"),
  plot = users_atts_plot_hist,
  width = 10,
  height = 7,
  bg = "white"
)

(users_atts_plot_density <- ggplot(users_ae_attendance_per_patient,
                                    aes(x = n_att)) +
    geom_density(fill = "#1a7431",
                 color = "#155d27") +
    labs(
      title = "",
      subtitle = "",
      x = "Number of Attendances",
      y = "Density") +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 16),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 16),
      plot.caption = element_text(size = 14, hjust = 0)
    )
)

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" ,"AE", "Plots",
  "Mean_atts_density_users-Raw.PNG"),
  plot = users_atts_plot_density,
  width = 7.5,
  height = 6,
  bg = "white"
)

## All ----
# Calculate the mean number of appointments
all_mean_atts <- mean(all_ae_attendance_per_patient$n_att, na.rm = TRUE)

(all_atts_plot_hist <- ggplot(all_ae_attendance_per_patient, 
                                  aes(x = n_att)) +
    geom_histogram(binwidth = 2, 
                   fill = "#FF7F50", 
                   color = "#FF7256") +
    geom_vline(xintercept = all_mean_atts, 
               linetype = "dashed", 
               color = "#2F4F4F", 
               size = 0.5) +
    labs(title = "", 
         subtitle = "",
         x = "Number of Attendances (bin width = 2)", 
         y = "Count of Patients",
         caption = "Each bar represents a 2-attendances range e.g., 1-2, 3-4, ...\n up to the range of the data.\nGreen line shows the average attendances both AE Healthcare Service Users & Non-Users.") +
  theme_minimal(base_size = 14, base_family = "Times") +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 16),
    plot.caption = element_text(size = 14, hjust = 0)
  )
)

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" , "AE", "Plots",
  "Mean_atts_hist_all-Raw.PNG"),
  plot = all_atts_plot_hist,
  width = 10,
  height = 7,
  bg = "white"
)

(all_atts_plot_density <- ggplot(all_ae_attendance_per_patient,
                                  aes(x = n_att)) +
    geom_density(fill = "#7f5539",
                 color = "#b08968") +
    labs(
      title = "",
      subtitle = "",
      x = "Number of Attendances",
      y = "Density") +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 16),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 16),
      plot.caption = element_text(size = 14, hjust = 0)
    )
)

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" , "AE", "Plots",
  "Mean_appts_density_all-Raw.PNG"),
  plot = all_atts_plot_density,
  width = 7.5,
  height = 6,
  bg = "white"
)

# Prepare costs data ----
# get the age
validation_old <- my_dm$`_validation_old` %>% collect()
costs_data_hrg_ae_users <- ae_attendance_data %>% 
  filter(user_status == "User") %>% 
  arrange(patid)

costs_data_hrg_ae_users <- left_join(costs_data_hrg_ae_users, validation_old %>% select(patid, yob), by = "patid") %>% 
  mutate(age = as.numeric(format(arrivaldate, "%Y")) - yob) %>% 
  mutate(aepatientgroup = NA_character_) %>% 
  rename(
    inv_01 = invest_01, inv_02 = invest_02, inv_03  = invest_03, 
    inv_04 = invest_04, inv_05 = invest_05, inv_06 = invest_06,
    inv_07 = invest_07, inv_08 = invest_08, inv_09 = invest_09,
    inv_10 = invest_10, inv_11 = invest_11, inv_12 = invest_12) %>% 
  arrange(patid)

# Select the variables required
costs_data_hrg_ae_users_min <- costs_data_hrg_ae_users %>% 
  select(age, aepatientgroup, inv_01, inv_02, inv_03, inv_04, inv_05, inv_06,
         inv_07, inv_08, inv_09, inv_10, inv_11, inv_12,
         treat_01, treat_02, treat_03, treat_04, treat_05,
         treat_06, treat_07, treat_07, treat_08, treat_09, treat_10,
         treat_11, treat_12) %>% 
  rename_with(toupper)

export(costs_data_hrg_ae_users_min, 
       here("CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" , "AE",
            "AE_costs_data_HRG.csv"))
costs_data_hrg_ae_users_ids <- costs_data_hrg_ae_users %>% 
  select(patid)

export(costs_data_hrg_ae_users_ids, 
       here("CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" , "AE",
            "costs_data_hrg_ae_users_ids.csv"))

costs_data_hrg_ae_non_users <-  ae_attendance_data %>% 
  filter(user_status == "Non-User") %>% 
  arrange(patid)

export(costs_data_hrg_ae_non_users, 
       here("CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" , "AE",
            "costs_data_hrg_ae_non-users.csv"))
