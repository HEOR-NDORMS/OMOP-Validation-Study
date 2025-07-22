# **************************************************************************** #
# 	Project name: Validation Study
# 	Author: Njoki Njuki
#   Do file name: Secondary Care Analysis - OP
#   Date: 14.07.2025
#   Last date edited: 14.07.2025
# **************************************************************************** #

# From Antonella:

# 2.	The following tables have been added
# validation._hes_ccare
# validation._hes_diagnosis_epi
# validation._hes_episodes
# validation._hes_hospital
# validation._hes_hrg
# validation._hes_procedures_epi
# validation._hesae_attendance
# validation._hesae_hrg
# validation._hesae_investigation
# validation._hesae_treatment
# validation._hesop_appointment
# validation._hesop_cliinical
# 
# 3.	The AURUM validation tables have not been modified
# 
# 4.	Several patients have fractures on index date belonging to different sites (e.g. hip and vert): in those cases, more than one site flag is set to 1

# Data Dictionary:
# Hospital Episode Statistics (HES) Outpatient Care and  
# CPRD primary care data: 
# https://www.cprd.com/sites/default/files/2024-11/HES_OP_Documentation_v2.1.pdf

# SOME NOTES FROM THE DATA DICTIONARY

# HES OP data are a collection of individual records of outpatient appointments 
# occurring in England only. 

# It includes information on the type of outpatient consultation appointment 
# dates, the main specialty and treatment specialty under which the patient was 
# treated, referral source, waiting times, clinical diagnosis and procedures 
# performed

# Each record represents a single outpatient attendance at a consultant or
# allied health clinic at a single hospital provider.

# Loading appointments data ----
appt_data <- table_one_op_raw_appt_recoded

# Minimise data ----
names(appt_data)
appt_data_min <- appt_data %>% 
  select(patid, site, index_date, follow_up_end, follow_up_reason,
         follow_up_time, attendkey, apptdate, atentype,
         firstatt, attended_recoded, atentype_recoded, firstatt_recoded,
         diag_01_description, collapsed_diag_01, opertn_01_description,
         collapsed_opertn_01, user_status) %>% 
  arrange(patid)

# HRCU ----
## Users ----
users_appts_data <- appt_data_min %>%
  filter(user_status == "User") 

users_appts <- users_appts_data %>% 
  group_by(patid) %>%                     # group data by patid
  select(patid, apptdate, attendkey) %>%  # select only variables of interest: patid, apptdate, attendkey
  arrange(patid, apptdate) %>%            # sort data according to appointment date
  distinct() %>%                          # deduplicate       
  mutate(appt_no  = 1:n()) %>%            # generate an appointment counter
  ungroup()

users_appts_data_final <- merge(users_appts_data, users_appts , 
                                all.x=T, all.y=T,
                                by=c("patid", "apptdate", "attendkey")) %>% 
  arrange(patid)
rm(users_appts)

users_appts_per_patient <- users_appts_data_final %>%
  group_by(patid) %>%                     # group data per patient
  summarise(n_appts = n()) %>%   # derive total number of appointments by picking max value of apptorder
  ungroup()  %>%                          # now, ungroup the data and save only one row summary per patient
  select(patid, n_appts)  %>%             # save only patid and nr of appts
  distinct()


### Summary table ----
users_overall_summary_appts <-  users_appts_per_patient %>%
  summarise(
    mean_appts = mean(n_appts),
    min_appts = min(n_appts),
    median_appts = median(n_appts),
    max_appts = max(n_appts),
    sd_appts = sd(n_appts),
    q1_appts = quantile(n_appts, probs = 0.25),
    q3_appts = quantile(n_appts, probs = 0.75)
  ) %>% 
  mutate(across(everything(), round, 2))

## All ----
# users_appts_data <- appt_data_min 
all_appts <- appt_data_min %>% 
  group_by(patid) %>%                     # group data by patid
  select(patid, apptdate, attendkey) %>%  # select only variables of interest: patid, apptdate, attendkey
  arrange(patid, apptdate) %>%            # sort data according to appointment date
  distinct() %>%                          # deduplicate       
  mutate(appt_no  = 1:n()) %>%            # generate an appointment counter
  ungroup() %>% 
  mutate(appt_no = if_else(is.na(apptdate), 0, appt_no))

all_appts_data_final <- merge(all_appts, appt_data_min , 
                                all.x=T, all.y=T,
                                by=c("patid", "apptdate", "attendkey")) %>% 
  arrange(patid)
rm(all_appts)

all_appts_per_patient <- all_appts_data_final %>%
  group_by(patid) %>%                     # group data per patient
  summarise(n_appts = n()) %>%   # derive total number of appointments by picking max value of apptorder
  ungroup()  %>%                          # now, ungroup the data and save only one row summary per patient
  select(patid, n_appts)  %>%             # save only patid and nr of appts
  distinct()


### Summary table ----
all_overall_summary_appts <-  all_appts_per_patient %>%
  summarise(
    mean_appts = mean(n_appts),
    min_appts = min(n_appts),
    median_appts = median(n_appts),
    max_appts = max(n_appts),
    sd_appts = sd(n_appts),
    q1_appts = quantile(n_appts, probs = 0.25),
    q3_appts = quantile(n_appts, probs = 0.75)
  ) %>% 
  mutate(across(everything(), round, 2))

# Export the file

library(writexl)

appts_list <- list(
  users_overall_summary_appts = users_overall_summary_appts,
  all_overall_summary_appts = all_overall_summary_appts
)

# Write to an Excel file
write_xlsx(appts_list, here::here("CPRD Raw Analysis", "Results",
                                              "SecondaryCareAnalysis", "Output_appointments_Raw.xlsx"))


# Plot ----
library(ggplot2)

## Users ----
# Calculate the mean number of appointments
users_mean_appts <- mean(users_appts_per_patient$n_appts, na.rm = TRUE)

(users_appts_plot_hist_va <- ggplot(users_appts_per_patient, 
                           aes(x = n_appts)) +
    geom_histogram(binwidth = 2, 
                   fill = "cadetblue1", 
                   color = "cadetblue") +
    geom_vline(xintercept = mean_appts, 
               linetype = "dashed", 
               color = "red", 
               size = 0.5) +
    annotate("text", 
             x = mean_appts, 
             y = Inf, 
             label = paste0("Mean = ", round(mean_appts, 1)), 
             vjust = -0.5, 
             hjust = 0.5, 
             color = "red", 
             fontface = "bold") +
    labs(title = "Distribution of Outpatient Appointments", 
         subtitle = "OP Healthcare Service Users",
         x = "Number of Admissions (bin width = 2)", 
         y = "Count of Patients",
         caption = "Each bar represents a 2-appointment range e.g., 1-2, 3-4, ...
         \n up to the range of the data")) +
  theme_minimal()

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" ,"Plots",
  "Mean_appts_hist-Users_va.PNG"),
  plot = users_appts_plot_hist_va,
  width = 9.5,
  height = 7,
  bg = "white"
)

# Build the initial plot first
users_appts_plot_hist_vb <- ggplot(users_appts_per_patient, 
                                 aes(x = n_appts)) +
  geom_histogram(binwidth = 2, 
                 fill = "cadetblue1", 
                 color = "cadetblue")

# Calculate the maximum y-value for annotation placement
users_max_count <- max(ggplot_build(users_appts_plot_hist_vb)$data[[1]]$count)

# Update the plot with a specific y-value for the annotation
(users_appts_plot_hist_vb <- users_appts_plot_hist_vb +
  geom_vline(xintercept = mean_appts, 
             linetype = "dashed", 
             color = "red", 
             size = 0.5) +
  annotate("text",
           x = users_mean_appts,
           y = users_max_count + 5,
           label = paste0("Mean = ", round(users_mean_appts, 1)),
           vjust = 0,
           hjust = 0.5,
           color = "red",
           fontface = "bold") +
  labs(title = "Distribution of Outpatient Appointments", 
       subtitle = "OP Healthcare Service Users",
       x = "Number of Admissions (bin width = 2)", 
       y = "Count of Patients",
       caption = "Each bar represents a 2-appointment range e.g., 1-2, 3-4, ...\n
       up to the range of the data") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 14),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 12),
      panel.spacing = unit(2, "lines")
    ))


# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" ,"Plots",
  "Mean_appts_hist-Users_vb.PNG"),
  plot = users_appts_plot_hist_vb,
  width = 9.5,
  height = 7,
  bg = "white"
)


(users_appts_plot_density <- ggplot(users_appts_per_patient, 
       aes(x = n_appts)) +
    geom_density(fill = "cadetblue1", 
                 color = "cadetblue") +
    labs(
      title = "Density Plot of Outpatient Appointments", 
      subtitle = "OP Healthcare Service Users",
       x = "Number of Admissions", 
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
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" ,"Plots",
  "Mean_appts_density-Users.PNG"),
  plot = users_appts_plot_density,
  width = 7.5,
  height = 6,
  bg = "white"
)

## All ----
# Calculate the mean number of appointments
all_mean_appts <- mean(all_appts_per_patient$n_appts, na.rm = TRUE)

(all_appts_plot_hist_va <- ggplot(all_appts_per_patient, 
                                    aes(x = n_appts)) +
    geom_histogram(binwidth = 2, 
                   fill = "#FF7F50", 
                   color = "#FF7256") +
    geom_vline(xintercept = all_mean_appts, 
               linetype = "dashed", 
               color = "#2F4F4F", 
               size = 0.5) +
    # annotate("text", 
    #          x = all_mean_appts, 
    #          y = Inf, 
    #          label = paste0("Mean = ", round(all_mean_appts, 1)), 
    #          vjust = -0.5, 
    #          hjust = 0.5, 
    #          color = "#2F4F4F", 
    #          fontface = "bold") +
    labs(title = "Distribution of Outpatient Appointments", 
         subtitle = "OP Healthcare Service All patients",
         x = "Number of Admissions (bin width = 2)", 
         y = "Count of Patients",
         caption = "Each bar represents a 2-appointment range e.g., 1-2, 3-4, ...
         \n up to the range of the data")) +
  theme_minimal()

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" ,"Plots",
  "Mean_appts_hist-All_va.PNG"),
  plot = all_appts_plot_hist_va,
  width = 9.5,
  height = 7,
  bg = "white"
)


# Build the initial plot first
all_appts_plot_hist_vb <- ggplot(all_appts_per_patient, 
                                 aes(x = n_appts)) +
  geom_histogram(binwidth = 2, 
                 fill = "#FF7F50", 
                 color = "#FF7256")

# Now extract the max y-value from the built plot
all_max_count <- max(ggplot_build(all_appts_plot_hist_vb)$data[[1]]$count)

# Now re-build and display the full plot with annotation
(all_appts_plot_hist_vb <- all_appts_plot_hist_vb +
  geom_vline(xintercept = all_mean_appts, 
             linetype = "dashed", 
             color = "#2F4F4F", 
             size = 0.5) +
  annotate("text", 
           x = all_mean_appts, 
           y = all_max_count + 5, 
           label = paste0("Mean = ", round(all_mean_appts, 1)), 
           vjust = 0, 
           hjust = 0.5, 
           color = "#2F4F4F", 
           fontface = "bold") +
  labs(title = "Distribution of Outpatient Appointments", 
       subtitle = "OP Healthcare Service Users",
       x = "Number of Admissions (bin width = 2)", 
       y = "Count of Patients",
       caption = "Each bar represents a 2-appointment range e.g., 1-2, 3-4, ...\nup to the range of the data") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 12),
    panel.spacing = unit(2, "lines")
  ))



# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" ,"Plots",
  "Mean_appts_hist-All_vb.PNG"),
  plot = all_appts_plot_hist_vb,
  width = 9.5,
  height = 7,
  bg = "white"
)


(all_appts_plot_density <- ggplot(all_appts_per_patient, 
                                    aes(x = n_appts)) +
    geom_density(fill = "#FF7F50", 
                 color = "#FF7256") +
    labs(
      title = "Density Plot of Outpatient Appointments", 
      subtitle = "OP Healthcare Service All Patients",
      x = "Number of Admissions", 
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
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" ,"Plots",
  "Mean_appts_density-All.PNG"),
  plot = all_appts_plot_density,
  width = 7.5,
  height = 6,
  bg = "white"
)

# Costs ----

## Prepare the data ----
validation_old <- my_dm$`_validation_old` %>% collect()

# Merge with the above to get the sex and age which are required in the grouper
costs_data_hrg <- left_join(appt_data, validation_old %>% select(patid, gender, yob), 
                            by = "patid") %>% 
  select(patid, yob, apptdate, gender, mainspef, tretspef, firstatt) 

costs_data_hrg_min <- costs_data_hrg %>% 
  mutate(startage = as.numeric(format(apptdate, "%Y")) - yob) %>% 
  mutate(gender = if_else(gender == "F", 2, 1)) %>% 
  filter(mainspef != "&") %>% 
  filter(tretspef != "&") %>% 
  rename(sex = gender) %>% 
  mutate(firstatt = ifelse(firstatt == "X", NA, firstatt)) %>% 
  select(startage, sex, mainspef, tretspef, firstatt)

rio::export(costs_data_hrg_min, here::here("CPRD Raw Analysis", "Results",
                                               "SecondaryCareAnalysis", "costs_data_hrg_min.csv"))

