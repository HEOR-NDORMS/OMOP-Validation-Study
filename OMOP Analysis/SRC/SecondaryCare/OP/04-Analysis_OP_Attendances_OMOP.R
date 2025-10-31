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
appt_data <- table_one_op_omop_appt

# Minimise data ----
names(appt_data)


# HRCU ----
## Users ----
users_appts_data <- appt_data %>%
  filter(user_status == "User") 

users_attendance_no  <- users_appts_data %>%
  group_by(subject_id) %>%                       
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
all_attendance_no <- appt_data %>%
  group_by(subject_id) %>%
  summarise(
    n_appts = if (all(is.na(visit_detail_start_date))) 0L else sum(!is.na(visit_detail_start_date)),
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
write_xlsx(appts_list,
           here::here("OMOP Analysis", "Results", "SecondaryCareAnalysis", 
                      "OP", "Overall", "Output_appointments_OMOP.xlsx"))


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
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "OP", "Plots",
  "Mean_appts_hist_users-OMOP.PNG"),
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
      x = "Number of OP Appointments",
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
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "OP", "Plots",
  "Mean_appts_density_users-OMOP.PNG"),
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

library(boot)
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
       here("OMOP Analysis", "Results", "SecondaryCareAnalysis" , "OP",
            "Overall", "overall_bootstrap_summary_encounters_SOURCE.xlsx"))

# Costs ----
## Preparing data ----
# Merge with the above to get the sex and age which are required in the grouper
costs_data_hrg_op <- appt_data %>% 
  select(subject_id, year_of_birth, gender_source_value, visit_detail_start_date,
         mainspef_code, tretspef_code) 

### Firstatt ----

# FIRSTATT - First Attendance: an indication of whether a patient is making a 
# first attendance or follow-up attendance, and whether the consultation was 
# face-to-face or via telephone/telemedicine consultation.
# 1 = First attendance face to face 
# 2 = Follow-up attendance face to face 
# 3 = First telephone or telemedicine consultation 
# 4 = Follow-up telephone or telemedicine consultation 
# X = Not known (from 2008/09) 
# 9 = Not known (prior to 2008/09)

# Assumption made: random allocation or least resource use choice
costs_data_hrg_op <- costs_data_hrg_op %>%
  mutate(
    firstatt = sample(c(1, 2, 3, 4), size = n(), replace = TRUE),
    firstatt_desc = case_when(
      firstatt == 1 ~ "First attendance face to face",
      firstatt == 2 ~ "Follow-up attendance face to face",
      firstatt == 3 ~ "First telephone/telemedicine consultation",
      firstatt == 4 ~ "Follow-up telephone/telemedicine consultation"
    )
  )

# Let's filter out the op non_users, as in primary analysis they'll 
# have cost = 0, so we will bring them back 
op_costs_non_users_data <- costs_data_hrg_op %>% 
  filter(is.na(visit_detail_start_date)) %>% 
  arrange(subject_id) %>% 
  mutate(startage = NA_integer_) %>% 
  select(subject_id, startage, sex = gender_source_value, mainspef =  mainspef_code, 
         tretspef = tretspef_code, firstatt) 

rio::export(op_costs_non_users_data, 
            here::here("OMOP Analysis", "Results",
                       "SecondaryCareAnalysis", "OP", "op_costs_non_users_data.csv"))
# Filter for users only
op_costs_users_data <- costs_data_hrg_op %>% 
  filter(!is.na(visit_detail_start_date)) %>% 
  mutate(startage = as.numeric(format(visit_detail_start_date, "%Y")) - year_of_birth) %>% 
  mutate(gender_source_value = if_else(gender_source_value == "F", 2, 1)) 

# Assessing missing mainspef and tretspef ----
(missing_summary <- op_costs_users_data |>
   summarise(
     total_records = n(),
     missing_mainspef = sum(is.na(mainspef_code)),
     missing_tretspef = sum(is.na(tretspef_code)),
     missing_both = sum(is.na(mainspef_code) & is.na(tretspef_code))
   ))

# missing mainspef
missing_mainspef <- op_costs_users_data  %>% 
  filter(is.na(mainspef_code))  %>% 
  dplyr::count(mainspef_code, sort = TRUE)

# missing tretspef
missing_tretspef <- op_costs_users_data |>
  filter(is.na(tretspef_code)) |>
  dplyr::count(tretspef_code, sort = TRUE)

## Define valid specialty codes ----
valid_mainspef_codes <- c(
  100, 101, 107, 110, 120, 130, 140, 141, 142, 143, 145, 146, 147, 148, 149, 150, 160, 170, 171,
  180, 190, 192, 300, 301, 302, 303, 304, 305, 310, 311, 313, 314, 315, 317, 320, 321, 330, 340,
  350, 352, 360, 361, 370, 371, 400, 401, 410, 420, 421, 430, 450, 460, 501, 502, 560, 600, 601,
  700, 710, 711, 712, 713, 715, 800, 810, 822, 823, 824, 830, 831, 900, 901, 902, 903, 950
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

# Rename specialty variables
op_costs_users_data <- op_costs_users_data %>% 
  rename(mainspef = mainspef_code,
         tretspef = tretspef_code)

## 1: If MAINSPEF missing, copy from TRETSPEF (and vice versa) if valid ----
# apply the rule and keep new columns (do not overwrite original yet)
rule1_applied <- op_costs_users_data %>%
  group_by(subject_id, visit_detail_start_date) |>
  mutate(
    new_mainspef = if_else(
      (is.na(mainspef) | !(mainspef %in% valid_mainspef_codes)) &
        (tretspef %in% valid_mainspef_codes),
      tretspef,
      mainspef
    ),
    new_tretspef = if_else(
      (is.na(tretspef) | !(tretspef %in% valid_tretspef_codes)) &
        (mainspef %in% valid_tretspef_codes),
      mainspef,
      tretspef
    )
  ) %>% 
  ungroup()

# count rows where the value actually changed (handle NAs safely)
rule1_mainspef_replaced <- sum(
  !is.na(rule1_applied$new_mainspef) &
    (is.na(op_costs_users_data$mainspef) | rule1_applied$new_mainspef != op_costs_users_data$mainspef),
  na.rm = TRUE
)

rule1_tretspef_replaced <- sum(
  !is.na(rule1_applied$new_tretspef) &
    (is.na(op_costs_users_data$tretspef) | rule1_applied$new_tretspef != op_costs_users_data$tretspef),
  na.rm = TRUE
)

cat("MAINSPEF replaced from TRETSPEF in", rule1_mainspef_replaced, "cases.\n")
cat("TRETSPEF replaced from MAINSPEF in", rule1_tretspef_replaced, "cases.\n")

## 2 & 3: If both specialties missing & Invalid mainspef and tretspef: ----

# Function to get the code
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Compute global modes (as you had)
mode_mainspef <- get_mode(rule1_applied$mainspef)
mode_tretspef <- get_mode(rule1_applied$tretspef)

cat("Global mode mainspef:", mode_mainspef, "\n")
cat("Global mode tretspef:", mode_tretspef, "\n")

# If one specialty is missing but the other is present and valid, fill the 
# missing value with the mode of that missing specialty among all appointments 
# that have the present specialty code.
# (E.g. if tretspef == 320 and mainspef is missing, replace mainspef with the 
# mode of mainspef across rows where tretspef==320.)

# If both missing and the patient attended, fall back to the global mode

# Where no conditional-mode exists (e.g. sparse codes), 
# fall back to global mode only for attended rows.

# Build conditional-mode lookups:
#  - mode of mainspef for each tretspef value (only use valid pairs)
mainspef_mode_by_tret_tbl <- rule1_applied |>
  filter(!is.na(new_tretspef), !is.na(new_mainspef), new_tretspef %in% valid_tretspef_codes, new_mainspef %in% valid_mainspef_codes) |>
  group_by(new_tretspef) |>
  summarise(mode_mainspef_for_tret = get_mode(new_mainspef), .groups = "drop")

#  - mode of tretspef for each mainspef value
tretspef_mode_by_main_tbl <- rule1_applied |>
  filter(!is.na(new_mainspef), !is.na(new_tretspef), new_mainspef %in% valid_mainspef_codes, new_tretspef %in% valid_tretspef_codes) |>
  group_by(new_mainspef) |>
  summarise(mode_tretspef_for_main = get_mode(new_tretspef), .groups = "drop")

# Convert to named vectors for fast lookup
mainspef_mode_by_tret_vec <- set_names(mainspef_mode_by_tret_tbl$mode_mainspef_for_tret,
                                       as.character(mainspef_mode_by_tret_tbl$new_tretspef))
tretspef_mode_by_main_vec <- set_names(tretspef_mode_by_main_tbl$mode_tretspef_for_main,
                                       as.character(tretspef_mode_by_main_tbl$new_mainspef))

# Logic:
# 1. Flag where each specialty is missing.
# 2. Try to fill missing MAINSPEF using the modal MAINSPEF for that TRETSPEF.
# 3. Try to fill missing TRETSPEF using the modal TRETSPEF for that MAINSPEF.
# 4. If still missing after conditional fill, replace with global modes.
# (All patients attended, so fallback is unconditional.)

rule2_3_applied <- rule1_applied |>
  mutate(
    # Record whether each specialty was originally missing (for diagnostics later)
    before_mainspef_na = is.na(new_mainspef),
    before_tretspef_na = is.na(new_tretspef),
    
    # Conditional imputation step 1
    # If MAINSPEF is missing but TRETSPEF is present and
    # that TRETSPEF exists in the conditional lookup table,
    # fill it with the modal MAINSPEF observed for that TRETSPEF.
    mainspef_from_tret = if_else(
      is.na(new_mainspef) & !is.na(new_tretspef) &
        as.character(new_tretspef) %in% names(mainspef_mode_by_tret_vec),
      as.numeric(mainspef_mode_by_tret_vec[as.character(new_tretspef)]),
      NA_real_
    ),
    
    # Conditional imputation step 2 
    # If TRETSPEF is missing but MAINSPEF is present and
    # that MAINSPEF exists in the conditional lookup table,
    # fill it with the modal TRETSPEF observed for that MAINSPEF.
    tretspef_from_main = if_else(
      is.na(new_tretspef) & !is.na(new_mainspef) &
        as.character(new_mainspef) %in% names(tretspef_mode_by_main_vec),
      as.numeric(tretspef_mode_by_main_vec[as.character(new_mainspef)]),
      NA_real_
    )
  ) |>
  mutate(
    # Apply conditional fills
    # Use the newly derived conditional values where available.
    # Example sanity check: if tretspef == 663, new_mainspef2 should equal
    # the most common MAINSPEF among records with tretspef == 663.
    new_mainspef2 = if_else(
      is.na(new_mainspef) & !is.na(mainspef_from_tret),
      mainspef_from_tret,
      new_mainspef
    ),
    new_tretspef2 = if_else(
      is.na(new_tretspef) & !is.na(tretspef_from_main),
      tretspef_from_main,
      new_tretspef
    )
  ) |>
  mutate(
    # Global-mode fallback
    # Any remaining missing values (after conditional fill)
    # are replaced with the overall (global) mode values.
    # This applies to all rows since all patients attended.
    new_mainspef3 = if_else(is.na(new_mainspef2),
                            as.numeric(mode_mainspef),
                            new_mainspef2),
    new_tretspef3 = if_else(is.na(new_tretspef2),
                            as.numeric(mode_tretspef),
                            new_tretspef2)
  ) |>
  # Drop intermediate temporary columns used for conditional lookups
  select(-mainspef_from_tret, -tretspef_from_main)



# Diagnostics / Summary counts

# Count how many MAINSPEF values were filled using TRETSPEF-based modes
mainspef_conditional_filled <- sum(
  rule2_3_applied$before_mainspef_na &
    !is.na(rule2_3_applied$new_mainspef2),
  na.rm = TRUE
)

# Count how many TRETSPEF values were filled using MAINSPEF-based modes
tretspef_conditional_filled <- sum(
  rule2_3_applied$before_tretspef_na &
    !is.na(rule2_3_applied$new_tretspef2),
  na.rm = TRUE
)

# Count how many remaining missing MAINSPEF were filled using the global mode
mainspef_final_imputed_with_global <- sum(
  rule2_3_applied$before_mainspef_na &
    is.na(rule1_applied$new_mainspef) &
    !is.na(rule2_3_applied$new_mainspef3) &
    (rule2_3_applied$new_mainspef3 == mode_mainspef),
  na.rm = TRUE
)

# Count how many remaining missing TRETSPEF were filled using the global mode
tretspef_final_imputed_with_global <- sum(
  rule2_3_applied$before_tretspef_na &
    is.na(rule1_applied$new_tretspef) &
    !is.na(rule2_3_applied$new_tretspef3) &
    (rule2_3_applied$new_tretspef3 == mode_tretspef),
  na.rm = TRUE
)

# Print diagnostic summary
cat("Conditional fills (mode by the other specialty):\n")
cat(" - MAINSPEF filled from TRETSPEF-mode in", mainspef_conditional_filled, "rows\n")
cat(" - TRETSPEF filled from MAINSPEF-mode in", tretspef_conditional_filled, "rows\n\n")

cat("Fallback global-mode imputations applied to:\n")
cat(" - MAINSPEF:", mainspef_final_imputed_with_global, "rows\n")
cat(" - TRETSPEF:", tretspef_final_imputed_with_global, "rows\n")

### 4. Final OP costs data ----
op_costs_data <- rule2_3_applied %>%
  arrange(subject_id) %>% 
  select(startage, gender_source_value, new_mainspef3, new_tretspef3, firstatt) %>%
  rename(sex = gender_source_value,
         mainspef = new_mainspef3,
         tretspef = new_tretspef3) %>%
  rename_with(toupper)

# lets check that we dont have any nas
sapply(op_costs_data, function(x) sum(is.na(x))) # YAS!!!

rio::export(op_costs_data, here::here("OMOP Analysis", "Results",
                                      "SecondaryCareAnalysis", "OP", 
                                      "op_costs_data.csv"))
op_users_ids_data <- rule2_3_applied %>%
  arrange(subject_id) %>% 
  select(subject_id)

rio::export(op_users_ids_data, here::here("OMOP Analysis", "Results",
                                          "SecondaryCareAnalysis", "OP", 
                                          "op_costs_data_ids.csv"))
