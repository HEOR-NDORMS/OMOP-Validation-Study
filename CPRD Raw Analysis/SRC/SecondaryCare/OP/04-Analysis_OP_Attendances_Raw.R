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

# Load R Packages ----
library(tidyverse)

# Loading appointments data ----
appt_data <- table_one_op_raw_appt_recoded

# Minimise data ----
names(appt_data)
appt_data_min <- appt_data %>% 
  select(patid, site, index_date, follow_up_end, follow_up_reason,
         follow_up_time, attendkey, apptdate, attended, atentype,
         firstatt, attended_recoded, atentype_recoded, firstatt_recoded,
         diag_01_description, collapsed_diag_01, opertn_01_description,
         collapsed_opertn_01, user_status) %>% 
  arrange(patid)

# HRCU ----
## Users ----
users_appts_data <- appt_data_min %>%
  filter(user_status == "User") 

users_attendance_no  <- users_appts_data %>%
  group_by(patid) %>%                       
  summarise(n_appts  = n()) %>%         
  ungroup()                                 


### Summary table ----
users_overall_summary_appts <-  users_attendance_no %>%
  summarise(
    n        = n(),
    mean     = mean(n_appts),
    min      = min(n_appts),
    max      = max(n_appts),
    sd       = sd(n_appts),
    median   = quantile(n_appts, 0.5),
    lower_q  = quantile(n_appts, 0.25),
    upper_q  = quantile(n_appts, 0.75),
    se               = sd / sqrt(n),
    ci_lower         = mean - qt(0.975, df = n - 1) * se,
    ci_upper         = mean + qt(0.975, df = n - 1) * se
  ) %>% 
  mutate(across(everything(), round, 2))

## All ----
all_attendance_no <- appt_data_min %>%
  group_by(patid) %>%
  summarise(
    n_appts = if (all(is.na(apptdate))) 0L else sum(!is.na(apptdate)),
    .groups = "drop"
  )


### Summary table ----
all_overall_summary_appts <-  all_attendance_no %>%
  summarise(
    n        = n(),
    mean     = mean(n_appts),
    min      = min(n_appts),
    max      = max(n_appts),
    sd       = sd(n_appts),
    median   = quantile(n_appts, 0.5),
    lower_q  = quantile(n_appts, 0.25),
    upper_q  = quantile(n_appts, 0.75),
    se               = sd / sqrt(n),
    ci_lower         = mean - qt(0.975, df = n - 1) * se,
    ci_upper         = mean + qt(0.975, df = n - 1) * se
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
                                              "SecondaryCareAnalysis", "OP","Output_appointments_Raw.xlsx"))


# Plot ----
library(ggplot2)

## Users ----
# Calculate the mean number of appointments
users_mean_appts <- mean(users_attendance_no$n_appts, na.rm = TRUE)

(users_appts_plot_hist <- ggplot(users_attendance_no, 
                           aes(x = n_appts)) +
    geom_histogram(binwidth = 2, 
                   fill = "cadetblue1", 
                   color = "cadetblue") +
    geom_vline(xintercept = users_mean_appts, 
               linetype = "dashed", 
               color = "red", 
               size = 0.5) +
    annotate("text", 
             x = users_mean_appts, 
             y = Inf, 
             label = paste0("Mean = ", round(users_mean_appts, 1)), 
             vjust = -0.5, 
             hjust = 0.5, 
             color = "red", 
             fontface = "bold") +
    labs(title = "", 
         subtitle = "",
         x = "Number of Appointments (bin width = 2)", 
         y = "Count of Patients",
         caption = "Each bar represents a 2-appointment range e.g., 1-2, 3-4, ...up to the range of the data.\nRed line shows the average appointments per OP Healthcare Service User.") +
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
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis", "OP", "Plots",
  "Mean_appts_hist_users-Raw.PNG"),
  plot = users_appts_plot_hist,
  width = 10,
  height = 7,
  bg = "white"
)

(users_appts_plot_density <- ggplot(users_attendance_no,
       aes(x = n_appts)) +
    geom_density(fill = "#008000",
                 color = "#006400") +
    labs(
      title = "",
      subtitle = "",
       x = "Number of Appointments",
       y = "Density") +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10, hjust = 0)
    ))

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis", "OP", "Plots",
  "Mean_appts_density_users-Raw.PNG"),
  plot = users_appts_plot_density,
  width = 7.5,
  height = 6,
  bg = "white"
)

## All ----
# Calculate the mean number of appointments
all_mean_appts <- mean(all_attendance_no$n_appts, na.rm = TRUE)

(all_appts_plot_hist <- ggplot(all_attendance_no, 
                                    aes(x = n_appts)) +
    geom_histogram(binwidth = 2, 
                   fill = "#FF7F50", 
                   color = "#FF7256") +
    geom_vline(xintercept = all_mean_appts, 
               linetype = "dashed", 
               color = "#2F4F4F", 
               size = 0.5) +
    labs(title = "", 
         subtitle = "",
         x = "Number of Appointments (bin width = 2)", 
         y = "Count of Patients",
         caption = "Each bar represents a 2-appointment range e.g., 1-2, 3-4, ...up to the range of the data.\nGreen line shows the average appointments both OP Healthcare Service Users & Non-Users.") +
  theme_minimal(base_size = 14, base_family = "Times") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0)
  ))

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis", "OP", "Plots",
  "Mean_appts_hist_all-Raw.PNG"),
  plot = all_appts_plot_hist,
  width = 10,
  height = 7,
  bg = "white"
)


(all_appts_plot_density <- ggplot(all_attendance_no,
                                    aes(x = n_appts)) +
    geom_density(fill = "#8b5e34",
                 color = "#a47148") +
    labs(
      title = "",
      subtitle = "",
      x = "Number of Appointments",
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
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" , "OP", "Plots",
  "Mean_appts_density_all-Raw.PNG"),
  plot = all_appts_plot_density,
  width = 7.5,
  height = 6,
  bg = "white"
)

# Bootstrap visits -----

# --- Function to compute bootstrap summary for a given dataset

bootstrap_summary <- function(data, varname = "n_appts", R = 1000, seed = 626) {
  
  stat_fn <- function(d, i) {
    d_sample <- d[i, ]
    mean(d_sample[[varname]], na.rm = TRUE)
  }
  
  set.seed(seed)
  boot_result <- boot(data = data, statistic = stat_fn, R = R)
  boot_ci <- boot.ci(boot_result, type = c("norm", "basic", "perc"))
  
  boot_mean     <- mean(boot_result$t)
  boot_values   <- boot_result$t
  boot_variance <- var(as.numeric(boot_values))
  boot_sd       <- sd(boot_values)
  boot_se       <- boot_sd / sqrt(length(boot_values))
  
  ci_norm  <- boot_ci$normal[2:3]
  ci_basic <- boot_ci$basic[4:5]
  ci_perc  <- boot_ci$perc[4:5]
  
  # Return in wide format
  tibble(
    mean     = boot_mean,
    variance = boot_variance,
    sd       = boot_sd,
    se       = boot_se,
    ci_norm_lower = ci_norm[1],
    ci_norm_upper = ci_norm[2],
    ci_basic_lower = ci_basic[1],
    ci_basic_upper = ci_basic[2],
    ci_perc_lower = ci_perc[1],
    ci_perc_upper = ci_perc[2]
  ) %>%
    mutate(across(everything(), ~round(.x, 3)))
}

# --- Bootstrap: All Patients & All Encounters
boot_summary_all_patients_all_encounters <- bootstrap_summary(all_attendance_no, varname = "n_appts")

# --- Bootstrap: All Patients & Relevant Encounters
# boot_summary_all_patients_relevant_encounters <- bootstrap_summary(all_patients_relevant_encounters_counts, varname = "total_patient_encounters")

# --- Bootstrap: Users Only & All Encounters
boot_summary_users_all_encounters <- bootstrap_summary(users_attendance_no, varname = "n_appts")

# --- Bootstrap: Users Only & Relevant Encounters
# boot_summary_users_relevant_encounters <- bootstrap_summary(users_relevant_encounters_counts, varname = "total_patient_encounters")


combined_bootstrap_summary_encounters <- bind_rows(
  boot_summary_all_patients_all_encounters %>%
    mutate(group = "All Patients & all encounters"),
  
  # boot_summary_all_patients_relevant_encounters %>%
  #   mutate(group = "All Patients & relevant encounters"),
  
  boot_summary_users_all_encounters %>%
    mutate(group = "Users & all encounters"),
  
  # boot_summary_users_relevant_encounters %>%
  #   mutate(group = "Users & relevant encounters")
) %>%
  select(group, everything())

combined_bootstrap_summary_encounters


# Export
export(combined_bootstrap_summary_encounters, 
       here("CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" , "OP",
            "Overall", "overall_bootstrap_summary_encounters_SOURCE.xlsx"))

# Costs ----

## Prepare the data ----
validation_old <- my_dm$`_validation_old` %>% collect()

# Merge with the above to get the sex and age which are required in the grouper
costs_data_hrg_op <- left_join(appt_data, validation_old %>% select(patid, gender, yob), 
                            by = "patid") %>% 
  select(patid, yob, gender, apptdate,attended ,gender, mainspef, tretspef, firstatt) 

costs_data_hrg_op <- costs_data_hrg_op %>% 
  mutate(startage = as.numeric(format(apptdate, "%Y")) - yob) %>% 
  rename(sex = gender) %>% 
  mutate(sex = if_else(sex == "F", 2, 1))  

# Let's filter out the op non_users, as in primary analysis they'll 
# have cost = 0, so we will bring them back 
op_costs_non_users_data <- costs_data_hrg_op %>% 
  filter(is.na(apptdate)) %>% 
  arrange(patid)  %>% 
  select(!c(yob, attended, apptdate))

rio::export(op_costs_non_users_data, 
            here::here("CPRD Raw Analysis", "Results",
                       "SecondaryCareAnalysis", "OP", "op_costs_non_users_data.csv"))

costs_data_hrg_op <- costs_data_hrg_op %>%
  
  mutate(
    mainspef = as.character(mainspef),
    tretspef = as.character(tretspef)
  ) %>%
  
  mutate(
    mainspef = case_when(
      mainspef == "&" ~ NA_character_,
      TRUE ~ mainspef
    ),
    tretspef = case_when(
      tretspef == "&" ~ NA_character_,
      TRUE ~ tretspef
    )
  ) %>%
  
  mutate(
    mainspef = as.numeric(mainspef),
    tretspef = as.numeric(tretspef)
  )

# Filter for users only
op_costs_users_data <- costs_data_hrg_op %>% 
  filter(!is.na(apptdate))
  

# Assessing missing mainspef and tretspef ----
(missing_summary <- op_costs_users_data |>
   summarise(
     total_records = n(),
     missing_mainspef = sum(is.na(mainspef)),
     missing_tretspef = sum(is.na(tretspef)),
     missing_both = sum(is.na(mainspef) & is.na(tretspef))
   ))

# missing mainspef
missing_mainspef <- op_costs_users_data  %>% 
  filter(is.na(mainspef))  %>% 
  dplyr::count(mainspef, sort = TRUE)

# missing tretspef
missing_tretspef <- op_costs_users_data |>
  filter(is.na(tretspef)) |>
  dplyr::count(tretspef, sort = TRUE)

### Define valid specialty codes ----
valid_mainspef_codes <- c(
  100, 101, 107, 110, 120, 130, 140, 141, 142, 143, 145, 146, 147, 148, 149, 150, 160, 170, 171,
  180, 190, 192, 300, 301, 302, 303, 304, 305, 310, 311, 313, 314, 315, 317, 320, 321, 
  325, 326, 330, 340,350, 352, 360, 361, 370, 371, 400, 401, 410, 420, 421, 430, 450, 460,
  501, 502, 560, 600, 601, 700, 710, 711, 712, 713, 715, 800, 810, 820, 822, 823, 824, 
  830, 831, 833, 900, 901, 902, 903, 950
)

valid_tretspef_codes <- c(
  100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 113, 115, 120, 130, 140, 141, 142,
  143, 144, 145, 150, 160, 161, 170, 171, 172, 173, 174, 180, 190, 191, 192, 199, 200, 211, 212,
  213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 230, 240, 241, 242, 250,
  251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 270, 280, 290, 291, 300,
  301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 313, 314, 315, 316, 317, 318, 319, 320,
  321, 322, 323, 324, 325, 326, 327, 328, 329, 330, 331, 333, 335, 340, 341, 342, 343, 344, 345,
  346, 347, 348, 350, 352, 360, 361, 370, 371,
  400, 401, 410, 420, 421, 422, 424, 430, 431, 450, 451, 460, 461, 499, 501, 502, 503, 504, 505,
  560, 650, 651, 652, 653, 654, 655, 656, 657, 658, 659, 660, 661, 662, 663, 670, 673, 675, 677,
  700, 710, 711, 712, 713, 715, 720, 721, 722, 723, 724, 725, 726, 727, 730, 800, 811, 812, 822,
  834, 840, 920
  
)

# Helper: mode function
get_mode <- function(x) {
  x2 <- na.omit(x)
  if (length(x2) == 0) return(NA_real_)
  ux <- unique(x2)
  ux[which.max(tabulate(match(x2, ux)))]
}

### 1: If MAINSPEF missing, copy from TRETSPEF (and vice versa) if valid ----
# apply the rule and keep new columns (do not overwrite original yet)
rule1_applied <- costs_data_hrg %>%
  filter(!is.na(apptdate)) %>%
  group_by(patid, apptdate, firstatt) %>%
  mutate(
    new_mainspef = if_else(
      (is.na(mainspef) | !(mainspef %in% valid_mainspef_codes)) &
        tretspef %in% valid_mainspef_codes,
      tretspef,
      mainspef
    ),
    new_tretspef = if_else(
      (is.na(tretspef) | !(tretspef %in% valid_tretspef_codes)) &
        mainspef %in% valid_tretspef_codes,
      mainspef,
      tretspef
    )
  ) %>%
  ungroup()

# Accurate replacement counts
rule1_mainspef_replaced <- sum(
  !is.na(rule1_applied$new_mainspef) &
    (is.na(costs_data_hrg$mainspef) |
       rule1_applied$new_mainspef != costs_data_hrg$mainspef),
  na.rm = TRUE
)

rule1_tretspef_replaced <- sum(
  !is.na(rule1_applied$new_tretspef) &
    (is.na(costs_data_hrg$tretspef) |
       rule1_applied$new_tretspef != costs_data_hrg$tretspef),
  na.rm = TRUE
)

cat("MAINSPEF replaced from TRETSPEF in", rule1_mainspef_replaced, "cases.\n")
cat("TRETSPEF replaced from MAINSPEF in", rule1_tretspef_replaced, "cases.\n")


## 2 & 3: If both specialties missing & Invalid mainspef and tretspef: ----

# If one specialty is missing but the other is present and valid, fill the 
# missing value with the mode of that missing specialty among all appointments 
# that have the present specialty code.
# (E.g. if tretspef == 320 and mainspef is missing, replace mainspef with the 
# mode of mainspef across rows where tretspef==320.)

# If both missing and the patient attended, fall back to the global mode

# Where no conditional-mode exists (e.g. sparse codes), 
# fall back to global mode only for attended rows.

# Global modes
mode_mainspef_global <- get_mode(rule1_applied$new_mainspef)
mode_tretspef_global <- get_mode(rule1_applied$new_tretspef)

# Build conditional-mode lookups:
#  - mode of mainspef for each tretspef value (only use valid pairs)
mainspef_mode_by_tret <- rule1_applied %>%
  filter(!is.na(new_mainspef), !is.na(new_tretspef),
         new_mainspef %in% valid_mainspef_codes,
         new_tretspef %in% valid_tretspef_codes) %>%
  group_by(new_tretspef) %>%
  summarise(mode_mainspef = get_mode(new_mainspef), .groups = "drop")

#  - mode of tretspef for each mainspef value
tretspef_mode_by_main <- rule1_applied %>%
  filter(!is.na(new_mainspef), !is.na(new_tretspef),
         new_mainspef %in% valid_mainspef_codes,
         new_tretspef %in% valid_tretspef_codes) %>%
  group_by(new_mainspef) %>%
  summarise(mode_tretspef = get_mode(new_tretspef), .groups = "drop")


rule2_3_applied <- rule1_applied %>%
  left_join(mainspef_mode_by_tret, by = c("new_tretspef")) %>%
  left_join(tretspef_mode_by_main, by = c("new_mainspef")) %>%
  mutate(
    attended_flag = attended == 5,
    
    new_mainspef2 = if_else(
      is.na(new_mainspef) & !is.na(mode_mainspef),
      mode_mainspef,
      new_mainspef
    ),
    
    new_tretspef2 = if_else(
      is.na(new_tretspef) & !is.na(mode_tretspef),
      mode_tretspef,
      new_tretspef
    ),
    
    # global fallback (source uses attended-only)
    new_mainspef3 = if_else(
      is.na(new_mainspef2) & attended_flag,
      mode_mainspef_global,
      new_mainspef2
    ),
    
    new_tretspef3 = if_else(
      is.na(new_tretspef2) & attended_flag,
      mode_tretspef_global,
      new_tretspef2
    )
  )

### 4. Handle FIRSTATT == "X" ----
# In the earlier cleaning steps we select attended == 5 
# since Antonella did the same for omop
# After checking the single option with Grouper, we replace X with 4,
# which is least resource use intensive.
rule4_applied <- rule2_3_applied %>% 
  mutate(firstatt = case_when(
    firstatt == "X" ~ "4",
    TRUE ~ firstatt
  ))

rule4_firstatt_imputed <- sum(rule2_3_applied$firstatt == "X", na.rm = TRUE)
cat(" Replace firstatt == X with 4:", rule4_firstatt_imputed, "cases\n")

## Final OP costs dataset ----
op_costs_data <- rule4_applied %>%
  arrange(patid) %>%
  select(startage, gender, new_mainspef3, new_tretspef3, firstatt) %>%
  rename(
    sex = gender,
    mainspef = new_mainspef3,
    tretspef = new_tretspef3
  ) %>%
  rename_with(toupper)


# lets check that we dont have any nas
sapply(op_costs_data, function(x) sum(is.na(x))) # YAS!!!

rio::export(op_costs_data, here::here("CPRD Raw Analysis", "Results",
                                      "SecondaryCareAnalysis", "op_costs_data.csv"))
rule4_applied <- rule4_applied %>% 
  arrange(patid) %>% 
  select(patid)

rio::export(rule4_applied_ids, here::here("CPRD Raw Analysis", "Results",
                                          "SecondaryCareAnalysis", "op_costs_data_ids.csv"))

op_costs_non_users <- costs_data_hrg %>% 
  filter(is.na(apptdate))
rio::export(op_costs_non_users, here::here("CPRD Raw Analysis", "Results",
                                           "SecondaryCareAnalysis", "op_costs_non_users.csv"))
rio::export(appt_data, here::here("CPRD Raw Analysis", "Results",
                                  "SecondaryCareAnalysis", "OP", 
                                  "appt_data_raw.csv"))

# Costs have been done from my local R, but the script is uploaded in the Github.

# Bootstrap cost summaries ----
boot_summary_all_patients_all_encounters_costs <- bootstrap_summary(all_patients_all_encounters_costs, varname = "total_patient_cost")
# boot_summary_all_patients_relevant_encounters_costs <- bootstrap_summary(all_patients_relevant_encounters_costs, varname = "total_patient_cost")
boot_summary_users_all_encounters_costs <- bootstrap_summary(users_all_encounters_costs, varname = "total_patient_cost")
# boot_summary_users_relevant_encounters_costs <- bootstrap_summary(users_relevant_encounters_costs, varname = "total_patient_cost")

# Combine bootstrap summaries
combined_bootstrap_costs_summary <- bind_rows(
  boot_summary_all_patients_all_encounters_costs %>%
    mutate(group = "All Patients & all encounters"),
  
  # boot_summary_all_patients_relevant_encounters_costs %>%
  #   mutate(group = "All Patients & relevant encounters"),
  
  boot_summary_users_all_encounters_costs %>%
    mutate(group = "Users & all encounters"),
  
  # boot_summary_users_relevant_encounters_costs %>%
  #   mutate(group = "Users & relevant encounters")
) %>%
  select(group, everything())
combined_bootstrap_costs_summary



# Remove objects from this analysis
rm(all_ae_attendance, all_ae_attendance_data_final, all_ae_attendance_per_patient, 
   all_overall_summary_ae_attendance, all_overall_by_tretspef, 
   all_ae_attendance_plot_hist_va, all_mean_ae_attendance, users_ae_attendance_data,
   users_ae_attendance_data_final, users_ae_attendance_per_patient, users_ae_attendance_plot_density,
   users_ae_attendance_plot_hist_va, users_overall_by_tretspef, users_overall_summary_ae_attendance,
   op_costs_data, op_costs_non_users, op_gt_summary_table_df,
   op_gt_summary_table, table_one_op_raw, table_one_op_raw_ae_attendance,
   table_one_op_raw_ae_attendance_recoded, validation_appointment,
   validation_clinical, rule1_applied, rule2_applied, rule3_applied,
   rule4_applied, rule4_applied_ids, rule1_mainspef_replaced,
   rule1_tretspef_replaced, rule2_count, rule2_imputed,
   rule3_mainspef_imputed, rule3_tretspef_imputed, rule4_firstatt_imputed,
   mode_mainspef, mode_tretspef, ae_attendance_data,
   ae_attendance_data_final)
