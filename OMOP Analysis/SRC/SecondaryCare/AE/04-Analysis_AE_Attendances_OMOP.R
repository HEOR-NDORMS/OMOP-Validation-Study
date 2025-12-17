# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: AE Analysis of the patients' AE attendances
# Date: 31.10.2025
# Last date edited: 01.12.2025
# ****************************************************************************** #

# Starting with the table_two_ae_omop dataset from 03-Table2_AE_OMOP.R

ae_attendance_data <- table_two_ae_omop

# HRCU ----
## Users ----
users_ae_attendance_data <- ae_attendance_data %>%
  filter(user_status == "User") 

users_ae_attendance_per_patient <- users_ae_attendance_data %>%
  group_by(subject_id) %>%                     # group data per patient
  summarise(n_att = n()) %>%   # derive total number of appointments by picking max value of apptorder
  ungroup()  %>%                          # now, ungroup the data and save only one row summary per patient
  select(subject_id, n_att)  %>%             # save only subject_id and nr of appts
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
  group_by(subject_id) %>%                     
  summarise(
    n_att = if (all(is.na(visit_detail_start_date))) 0L else sum(!is.na(visit_detail_start_date)),
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
write_xlsx(ae_attandances_list, here::here("OMOP Analysis", "Results",
                                           "SecondaryCareAnalysis", "AE", "Overall","Output_AE_attendances_Raw.xlsx"))
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
         caption = "Each bar represents a 2-attendances range e.g., 1-2, 3-4, ...\nup to the range of the data.\nRed line shows the average attendances per OP Healthcare Service User.") +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10, hjust = 0)
    )
)

# Save the plot
ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis" , "AE", "Plots",
  "Mean_atts_hist_users-OMOP.PNG"),
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
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10, hjust = 0)
    )
)

# Save the plot
ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis" ,"AE", "Plots",
  "Mean_atts_density_users-OMOP.PNG"),
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
         caption = "Each bar represents a 2-attendances range e.g., 1-2, 3-4, ...\nup to the range of the data.\nGreen line shows the average attendances both AE Healthcare Service Users & Non-Users.") +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10, hjust = 0)
    )
)

# Save the plot
ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis" , "AE", "Plots",
  "Mean_atts_hist_all-OMOP.PNG"),
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
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10, hjust = 0)
    )
)

# Save the plot
ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis" , "AE", "Plots",
  "Mean_appts_density_all-OMOP.PNG"),
  plot = all_atts_plot_density,
  width = 7.5,
  height = 6,
  bg = "white"
)

# Costs
# Since in OMOP investigation and procedures are not mapped, hence they are not
# available in the data.
# Therefore to solve this, the cost for all patients will be weighted 
# (weighted_unit_cost = total costs/number of attendances), this will be same for
# all patients

ncc_costs_ae <- import(here("NHS_Collection_Costs_2024_2025.xlsx"), 
                       sheet = "EM") |> 
  clean_names() %>% 
  mutate(
    n_attendances = as.character(number_of_attendances) %>%
      str_replace_all("\\*", "3") %>%
      str_remove_all("[,]") %>%
      as.numeric(),
    cost = as.character(cost) %>%
      str_remove_all("[£,]") %>%
      as.numeric()
  )

(em_weighted_cost = round(sum(ncc_costs_ae$cost)/sum(ncc_costs_ae$n_attendances), 2))

em_weighted_cost_df <- data.frame(
  em_weighted_cost = em_weighted_cost
)
export(em_weighted_cost_df, 
       here("OMOP Analysis", "Results", "SecondaryCareAnalysis" , "AE",
            "OMOP_cost_value.csv"))


ae_attendance_data <- ae_attendance_data %>% 
  mutate(unit_cost = if_else(user_status == "User", em_weighted_cost, 0))

export(ae_attendance_data, 
       here("OMOP Analysis", "Results", "SecondaryCareAnalysis" , "AE",
            "ae_attendance_data_omop.csv"))
