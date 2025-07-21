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
         firstatt, attended_recoded, atentype_recoded, firstatt_recoded) %>% 
  arrange(patid)

# Number of appointments per patient ----
appts_no <- appt_data_min %>%
  group_by(patid) %>%                     # group data by patid
  select(patid, apptdate, attendkey) %>%  # select only variables of interest: patid, apptdate, attendkey
  arrange(patid, apptdate) %>%            # sort data according to appointment date
  distinct() %>%                          # deduplicate       
  mutate(appt_no  = 1:n()) %>%            # generate an appointment counter
  ungroup() %>% 
  mutate(appt_no = if_else(is.na(apptdate), 0, appt_no))

appts_data_final <- merge(appt_data_min, appts_no , all.x=T, all.y=T, by=c("patid", "apptdate", "attendkey"))
rm(appts_no)

appts_per_patient <- appts_data_final %>%
  group_by(patid) %>%                     # group data per patient
  summarise(n_appts = max(appt_no)) %>%   # derive total number of appointments by picking max value of apptorder
  ungroup()  %>%                          # now, ungroup the data and save only one row summary per patient
  select(patid, n_appts)  %>%             # save only patid and nr of appts
  distinct()


# Summary table ----
overall_summary_appts <-  appts_per_patient %>%
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

# Plot ----
library(ggplot2)

# Calculate the mean number of appointments
mean_appts <- mean(appts_per_patient$n_appts, na.rm = TRUE)

(appts_plot_hist <- ggplot(appts_per_patient, 
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
         x = "Number of Admissions (bin width = 2)", 
         y = "Count of Patients",
         caption = "Each bar represents a 2-appointment range e.g., 1-2, 3-4, ...
         \n up to the range of the data"))

# Calculate the maximum y-value for annotation placement
max_count <- max(ggplot_build(appts_plot_hist)$data[[1]]$count)

# Update the plot with a specific y-value for the annotation
(appts_plot_hist <- ggplot(appts_per_patient, 
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
           y = max_count + 5, 
           label = paste0("Mean = ", round(mean_appts, 1)), 
           vjust = 0, 
           hjust = 0.5, 
           color = "red", 
           fontface = "bold") +
  labs(title = "Distribution of Outpatient Appointments", 
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
  "Mean_appts_hist.PNG"),
  plot = appts_plot_hist,
  width = 9.5,
  height = 7,
  bg = "white"
)


(appts_plot_density <- ggplot(appts_per_patient, 
       aes(x = n_appts)) +
    geom_density(fill = "cadetblue1", 
                 color = "cadetblue") +
    labs(title = "Density Plot of Outpatient Appointments", 
       x = "Number of Admissions", 
       y = "Density") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 14),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 12),
      panel.spacing = unit(2, "lines")
    )
  )

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" ,"Plots",
  "Mean_appts_density.PNG"),
  plot = appts_plot_density,
  width = 7.5,
  height = 6,
  bg = "white"
)

pt_count_appts <- appts_per_patient %>%
  group_by(n_appts) %>%
  summarise(Subject_Count = n(), .groups = 'drop') %>%
  mutate(Percent_Subjects = Subject_Count / sum(Subject_Count) * 100)

(appts_plot_barplot <- pt_count_appts %>% 
  ggplot(aes(x = n_appts, 
             y = Percent_Subjects)) +
  geom_bar(stat = "identity", 
           fill="#CD1076", 
           alpha=0.9,
           width = 0.6) +
  labs(x = "Total Number of appts", 
       y = "% of Subjects") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.000001))) +
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
  "Mean_appts_barplot.PNG"),
  plot = appts_plot_barplot,
  width = 7.5,
  height = 6,
  bg = "white"
)
