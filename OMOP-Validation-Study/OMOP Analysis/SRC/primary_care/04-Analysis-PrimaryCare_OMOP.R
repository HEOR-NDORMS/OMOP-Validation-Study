# **************************************************************************** #
# 	Project name: Validation Study
#   Script: Primary care analysis
# **************************************************************************** #
# DESCRIPTION:
# This script generates overall (patient-level) summary statistics for healthcare resource use (HCRU) and costs
# from primary care encounter data. The summaries are structured by population and encounter type, and include 
# both descriptive statistics and bootstrap-derived confidence intervals.

# Specifically, the script provides:
# 
# 1. HCRU (encounter count) and cost summaries for:
#     - All patients in the cohort
#     - Users only (i.e., patients with at least one clinically relevant encounter)
# 
# 2. Within each population, two encounter definitions are used:
#     (a) All encounters (any provider type)
#     (b) Clinically relevant encounters only
#
# 3. For each of the 4 combinations (2 populations × 2 encounter types), the following are produced:
#     - Descriptive statistics: mean, median, quartiles, min/max, standard deviation, standard error,
#       and 95% confidence intervals.
#     - Bootstrap summaries of the mean and confidence intervals using normal, basic, and percentile methods.
#
# 4. The script also counts:
#     - The distribution of healthcare roles (by specialty and provider category)
#     - The proportion of encounters contributed by each category and role


# Libraries

library(boot)
library(tibble)
library(dplyr)

# --- Visits ------

visit_data_all <- cohort_visit_data_all

## All patients ----

# --- a) All patients & all encounters
all_patients_all_encounters_counts <- visit_data_all %>%
  group_by(subject_id) %>%
  summarise(
    total_patient_encounters = sum(!is.na(visit_detail_id)), # this way non users have 0 count
    .groups = "drop"
  )

# Summary statistics
summary_all_patients_all_encounters <- all_patients_all_encounters_counts %>%
  summarise(
    n                = n(),
    mean      = mean(total_patient_encounters),
    min       = min(total_patient_encounters),
    max       = max(total_patient_encounters),
    sd        = sd(total_patient_encounters),
    median    = quantile(total_patient_encounters, 0.5),
    lower_q   = quantile(total_patient_encounters, 0.25),
    upper_q   = quantile(total_patient_encounters, 0.75),
    se               = sd / sqrt(n),
    ci_lower         = mean - qt(0.975, df = n - 1) * se,
    ci_upper         = mean + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))

# # --- b) All patients & relevant encounters
#   group_by(subject_id) %>%
#   summarise(
#     total_patient_encounters = n(),
#     .groups = "drop"
#   )
#   select(subject_id, total_patient_encounters) 
# 
# # Summary statistics

## User-only ----

cohort_table_users <- visit_data_all %>% filter(user_status== "User") %>% 
  distinct(subject_id, index_date, fracture_site, follow_up_end, follow_up_reason, follow_up_time)

# --- a) Users & all encounters

# Filter encounters for users
users_all_encounters_counts <- visit_data_all %>%
  filter(user_status == "User") %>% # only relevant encounters
  group_by(subject_id) %>%
  summarise(
    total_patient_encounters = n(),
    .groups = "drop"
  )

users_all_encounters_counts <- left_join(cohort_table_users, users_all_encounters_counts,
                                         by = "subject_id") %>%
  replace_na(list(total_patient_encounters = 0)) %>%  # patients without relevant encounters are assigned a zero
  select(subject_id, total_patient_encounters) 

# Summary statistics
summary_users_all_encounters <- users_all_encounters_counts %>%
  summarise(
    n                = n(),
    mean      = mean(total_patient_encounters),
    min       = min(total_patient_encounters),
    max       = max(total_patient_encounters),
    sd        = sd(total_patient_encounters),
    median    = quantile(total_patient_encounters, 0.5),
    lower_q   = quantile(total_patient_encounters, 0.25),
    upper_q   = quantile(total_patient_encounters, 0.75),
    se               = sd / sqrt(n),
    ci_lower         = mean - qt(0.975, df = n - 1) * se,
    ci_upper         = mean + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))


# --- b) Users & relevant encounters

# # Filter encounters for users and clinically relevant specialties
# # Compute total encounters per user
#   select(subject_id, total_patient_encounters) 
# 
# # Summary statistics for user-only group


# --- Output: Combine All Summaries 

combined_summary_encounters <- bind_rows(
  
  summary_all_patients_all_encounters %>%
    mutate(group = "All Patients"),
  
  
  summary_users_all_encounters %>%
    mutate(group = "Users"),
  
) %>%
  select(group, everything())

combined_summary_encounters

# Bootstrap visits -----

# --- Function to compute bootstrap summary for a given dataset
bootstrap_summary <- function(data, varname = "total_patient_encounters", R = 1000, seed = 626) {
  
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
boot_summary_all_patients_all_encounters <- bootstrap_summary(all_patients_all_encounters_counts, varname = "total_patient_encounters")

# --- Bootstrap: All Patients & Relevant Encounters

# --- Bootstrap: Users Only & All Encounters
boot_summary_users_all_encounters <- bootstrap_summary(users_all_encounters_counts, varname = "total_patient_encounters")

# --- Bootstrap: Users Only & Relevant Encounters


combined_bootstrap_summary_encounters <- bind_rows(
  boot_summary_all_patients_all_encounters %>%
    mutate(group = "All Patients"),
  
  
  boot_summary_users_all_encounters %>%
    mutate(group = "Users"),
  
) %>%
  select(group, everything())

combined_bootstrap_summary_encounters



# --- Costs ------

## All patients ----

# --- a) All patients & all encounters
all_patients_all_encounters_costs <- visit_data_all %>%
  group_by(subject_id) %>%
  summarise(
    total_patient_cost = sum(unit_cost, na.rm = TRUE),
    .groups = "drop"
  )

# Summary statistics
summary_all_patients_all_encounters_costs <- all_patients_all_encounters_costs %>%
  summarise(
    n                  = n(),
    mean          = mean(total_patient_cost),
    min           = min(total_patient_cost),
    max           = max(total_patient_cost),
    sd            = sd(total_patient_cost),
    median        = quantile(total_patient_cost, 0.5),
    lower_q       = quantile(total_patient_cost, 0.25),
    upper_q       = quantile(total_patient_cost, 0.75),
    se                 = sd / sqrt(n),
    ci_lower           = mean - qt(0.975, df = n - 1) * se,
    ci_upper           = mean + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))


# --- b) All patients & relevant encounters
#   select(subject_id, total_patient_cost)
# 
# # Summary statistics


## Users only ----

cohort_table_users <- visit_data_all %>%
  filter(user_status == "User") %>%
  distinct(subject_id, index_date, fracture_site, follow_up_end, follow_up_reason, follow_up_time)

# --- a) Users & all encounters
users_all_encounters_costs <- visit_data_all %>%
  filter(user_status == "User") %>%
  group_by(subject_id) %>%
  summarise(
    total_patient_cost = sum(unit_cost, na.rm = TRUE),
    .groups = "drop"
  )

users_all_encounters_costs <- left_join(cohort_table_users, users_all_encounters_costs,
                                        by = "subject_id") %>%
  replace_na(list(total_patient_cost = 0)) %>% # assign 0 to users with no relevant encounters
  select(subject_id, total_patient_cost) 

# Summary statistics
summary_users_all_encounters_costs <- users_all_encounters_costs %>%
  summarise(
    n                  = n(),
    mean          = mean(total_patient_cost),
    min           = min(total_patient_cost),
    max           = max(total_patient_cost),
    sd            = sd(total_patient_cost),
    median        = quantile(total_patient_cost, 0.5),
    lower_q       = quantile(total_patient_cost, 0.25),
    upper_q       = quantile(total_patient_cost, 0.75),
    se                 = sd / sqrt(n),
    ci_lower           = mean - qt(0.975, df = n - 1) * se,
    ci_upper           = mean + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))


# --- b) Users & relevant encounters
#   select(subject_id, total_patient_cost) 
# 
# # Summary statistics


# --- Combine summaries 
combined_summary_costs <- bind_rows(
  summary_all_patients_all_encounters_costs %>%
    mutate(group = "All Patients"),
  
  
  summary_users_all_encounters_costs %>%
    mutate(group = "Users"),
  
) %>%
  select(group, everything())

# Bootstrap cost  ----
boot_summary_all_patients_all_encounters_costs <- bootstrap_summary(all_patients_all_encounters_costs, varname = "total_patient_cost")
boot_summary_users_all_encounters_costs <- bootstrap_summary(users_all_encounters_costs, varname = "total_patient_cost")

# Combine bootstrap summaries
combined_bootstrap_costs_summary <- bind_rows(
  boot_summary_all_patients_all_encounters_costs %>%
    mutate(group = "All Patients"),
  
  
  boot_summary_users_all_encounters_costs %>%
    mutate(group = "Users"),
  
) %>%
  select(group, everything())
combined_bootstrap_costs_summary

# ---Specialty count ----
library(dplyr)
library(writexl)
library(gtsummary)
library(here)

# --- Count of specific roles within categories ---
overall_specific_role_count <- visit_data_all %>%
  filter(!is.na(visit_detail_start_date)) %>% # keep only visits 
  group_by(specialty_reduced, specialty) %>% 
  summarise(
    count_specific_role = n(),
    .groups = "drop"
  ) %>% 
  mutate(
    percent_specific_role = round(count_specific_role / sum(count_specific_role) * 100, 2)
  )

# --- Count of categories ---
overall_category_count <- visit_data_all %>% 
  filter(!is.na(visit_detail_start_date)) %>% # keep only visits 
  group_by(specialty_reduced) %>% 
  summarise(
    count_category = n(),
    .groups = "drop"
  ) %>% 
  mutate(
    percent_category = round(count_category / sum(count_category) * 100, 2)
  )

# --- Export to Excel ---
overall_healthcare_role_list <- list(
  overall_category_count = overall_category_count,
  overall_specific_role_count = overall_specific_role_count
)

write_xlsx(
  overall_healthcare_role_list,
  here("OMOP-Validation-Study", "OMOP Analysis", "Results",  "Final", "Primary care", "Output_overall_healthcare_roles-OMOP.xlsx")
)

# --- Create gtsummary Table ---
gt_overall_healthcare_roles_count <- visit_data_all %>%
  filter(!is.na(visit_detail_start_date)) %>% # keep only visits 
  tbl_summary(
    include = c(specialty_reduced, specialty),
    label = list(
      specialty_reduced = "Healthcare Provider Category", 
      specialty = "Healthcare Provider Roles"
    ),
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ c(0, 1),  # 0 for count (n), 1 for percent (p)
    sort = all_categorical() ~ "frequency"
  ) %>%
  modify_header(label ~ " **Total Encounters**") %>%
  bold_labels()

gt_overall_healthcare_roles_count

gt_table_export <- as_tibble(gt_overall_healthcare_roles_count)

# Export 
library(writexl)
# Write to an Excel file
write_xlsx(combined_summary_encounters, here::here("OMOP-Validation-Study", "OMOP Analysis", "Results", "Final", "Primary care",
                                                   "overall_combined_summary_encounters_OMOP.xlsx"))

write_xlsx(combined_bootstrap_summary_encounters, here::here("OMOP-Validation-Study","OMOP Analysis", "Results", "Final", "Primary care",
                                                             "overall_combined_bootstrap_summary_encounters_OMOP.xlsx"))

write_xlsx(combined_summary_costs, here::here("OMOP-Validation-Study","OMOP Analysis", "Results",  "Final", "Primary care",
                                              "overall_combined_summary_costs_OMOP.xlsx"))

write_xlsx(combined_bootstrap_costs_summary, here::here("OMOP-Validation-Study","OMOP Analysis", "Results",  "Final", "Primary care",
                                                        "overall_combined_bootstrap_summary_costs_OMOP.xlsx"))

write_xlsx(gt_table_export, here::here("OMOP-Validation-Study","OMOP Analysis", "Results",  "Final", "Primary care",
                                       "Output_gt_overall_healthcare_roles_count_OMOP.xlsx"))

# move them to standard or source folder based on what you're running

# Clean environment

#rm(list = setdiff(ls(), c(
  ## From previous codes
  
  # "cohort_table", "cdm", "cohort_visit_data_all", "all_patients", "AttritionReportCoh", 
  # "AttritionReportFrac_Raw" ,"db", "my_dm", "visit_data_all", 
  # "provider_cost_inputs", "table_one_raw_visit_data", "study_start_date", 
  # "study_end_date", "gt_summary_table", "gt_summary_table_df", 
  # 
  # # From this code
  # "all_patients_all_encounters_counts", "summary_all_patients_all_encounters", 
  # "all_patients_relevant_encounters_counts", "summary_all_patients_relevant_encounters",
  # "summary_users_relevant_encounters", "combined_summary_encounters", 
  # "boot_summary_all_patients_all_encounters","boot_summary_all_patients_relevant_encounters", 
  # "boot_summary_users_all_encounters","boot_summary_users_relevant_encounters",
  # "combined_bootstrap_summary_encounters", "all_patients_all_encounters_costs", 
  # "summary_all_patients_all_encounters_costs", "all_patients_relevant_encounters_costs", 
  # "summary_all_patients_relevant_encounters_costs", "users_all_encounters_costs", "summary_users_all_encounters_costs",
  # "users_relevant_encounters_costs", "summary_users_relevant_encounters_costs", 
  # "combined_summary_costs", "boot_summary_all_patients_all_encounters_costs",
  # "combined_bootstrap_costs_summary", "gt_overall_healthcare_roles_count")))

