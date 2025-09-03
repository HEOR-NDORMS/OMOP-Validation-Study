# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: AE Analysis of the patients' AE attendances
# Date: 26.08.2025
# Last date edited: 28.08.2025
# ****************************************************************************** #

# Starting with table_two_ae_raw from 03-Table2_AE_Raw.R ----
ae_attendance_data <- table_two_ae_raw 

# HRCU ----
## Users ----
users_ae_attendance_data <- ae_attendance_data %>%
  filter(user_status == "User") 

users_ae_attendances <- users_ae_attendance_data %>% 
  group_by(patid) %>%                     # group data by patid
  select(patid, arrivaldate, aekey) %>%  # select only variables of interest: patid, apptdate, attendkey
  arrange(patid, arrivaldate) %>%            # sort data according to appointment date
  distinct() %>%                          # deduplicate       
  mutate(att_no  = 1:n()) %>%            # generate an appointment counter
  ungroup()

users_ae_attendance_data_final <- merge(users_ae_attendance_data, users_ae_attendances , 
                                all.x=T, all.y=T,
                                by=c("patid", "arrivaldate", "aekey")) %>% 
  arrange(patid)
rm(users_ae_attendances)

users_ae_attendance_per_patient <- users_ae_attendance_data_final %>%
  group_by(patid) %>%                     # group data per patient
  summarise(n_att = max(att_no)) %>%   # derive total number of appointments by picking max value of apptorder
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
# users_appts_data <- appt_data_min 
all_ae_attendance <- ae_attendance_data %>% 
  group_by(patid) %>%                     # group data by patid
  select(patid, arrivaldate, aekey) %>%  # select only variables of interest: patid, apptdate, attendkey
  arrange(patid, arrivaldate) %>%            # sort data according to appointment date
  distinct() %>%                          # deduplicate       
  mutate(att_no  = 1:n()) %>%            # generate an appointment counter
  ungroup() %>% 
  mutate(att_no = if_else(is.na(arrivaldate), 0, att_no))

all_ae_attendance_data_final <- merge(all_ae_attendance, ae_attendance_data , 
                              all.x=T, all.y=T,
                              by=c("patid", "arrivaldate", "aekey")) %>% 
  arrange(patid)
rm(all_ae_attendance)

all_ae_attendance_per_patient <- all_ae_attendance_data_final %>%
  group_by(patid) %>%                     # group data per patient
  summarise(n_att = max(att_no)) %>%   # derive total number of appointments by picking max value of apptorder
  ungroup()  %>%                          # now, ungroup the data and save only one row summary per patient
  select(patid, n_att)  %>%             # save only patid and nr of appts
  distinct()


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
                                  "SecondaryCareAnalysis", "AE","Output_AE_attdances_Raw.xlsx"))
# Plot ----
library(ggplot2)

## Users ----
# Calculate the mean number of appointments
users_mean_atts <- mean(users_ae_attendance_per_patient$n_att, na.rm = TRUE)

(users_atts_plot_hist <- ggplot(users_ae_attendance_per_patient, 
                                    aes(x = n_att)) +
    geom_histogram(binwidth = 2, 
                   fill = "cadetblue1", 
                   color = "cadetblue") +
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
    labs(title = "Distribution of AE Attendances", 
         subtitle = "AE Healthcare Service Users",
         x = "Number of Attendances (bin width = 2)", 
         y = "Count of Patients",
         caption = "Each bar represents a 2-attendances range e.g., 1-2, 3-4, ...
         \n up to the range of the data.\n 
         Red line shows the average attendances per OP Healthcare Service User.")) +
  theme_minimal()

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" , "AE","Plots",
  "Mean_atts_hist-Users_va.PNG"),
  plot = users_atts_plot_hist,
  width = 10,
  height = 7,
  bg = "white"
)

(users_atts_plot_density <- ggplot(users_ae_attendance_per_patient,
                                    aes(x = n_att)) +
    geom_density(fill = "cadetblue1",
                 color = "cadetblue") +
    labs(
      title = "Density Plot of AE Attendances",
      subtitle = "AE Healthcare Service Users",
      x = "Number of Attendances",
      y = "Density") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(hjust = 0.5, size = 14),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 12),
      panel.spacing = unit(2, "lines")
    )
)

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" ,"AE", "Plots",
  "Mean_atts_density-Users.PNG"),
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
    labs(title = "Distribution of AE Attendances", 
         subtitle = "All patients (Both Healthcare Service User and Non-Users)",
         x = "Number of Attendances (bin width = 2)", 
         y = "Count of Patients",
         caption = "Each bar represents a 2-attendances range e.g., 1-2, 3-4, ...
         \n up to the range of the data. 
         \nGreen line shows the average attendances both AE Healthcare Service 
         Users & Non-Users.")) +
  theme_minimal()

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" , "AE", "Plots",
  "Mean_atts_hist-All.PNG"),
  plot = all_atts_plot_hist,
  width = 10,
  height = 7,
  bg = "white"
)

(all_atts_plot_density <- ggplot(all_ae_attendance_per_patient,
                                  aes(x = n_att)) +
    geom_density(fill = "#FF7F50",
                 color = "#FF7256") +
    labs(
      title = "Density Plot of AE Attendances",
      subtitle = "All patients (Both Healthcare Service User and Non-Users)",
      x = "Number of Attendances",
      y = "Density") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(hjust = 0.5, size = 14),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 12),
      panel.spacing = unit(2, "lines")
    )
)

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" , "AE", "Plots",
  "Mean_appts_density-All.PNG"),
  plot = all_atts_plot_density,
  width = 7.5,
  height = 6,
  bg = "white"
)
