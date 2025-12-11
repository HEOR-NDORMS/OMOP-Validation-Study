# Load packages ----
library(dplyr)
library(rio)
library(janitor)
library(tidyverse)
library(tidylog)
library(bit64)
library(here)

# AE ----

## Read the files ----
ae_hrg_df <- read_csv(
  "H:/Projects/Validation/HES HRG/Source/AE/AE_costs_HRG_output_attend.csv"
) |> 
  clean_names()

# Select variables of interest
# ae_hrg_df_min <- ae_hrg_df |> 
#   select(!row_no)

ae_study_ids_df <- read_csv(
  "H:/Projects/Validation/HES HRG/Source/AE/costs_data_hrg_ae_users_ids.csv"
)

# Merge the two together 
ae_hrg_costs_df <- cbind(ae_study_ids_df, ae_hrg_df) |> 
  mutate(patid = as.integer64(patid))

# Import the NHS cost collection 23/24 data
costs_hrg_ae_df <- import(
  "H:/Projects/Validation/HES HRG/NHS_Collection_Costs_2024_2025.xlsx",
  sheet = "EM"
) |> 
  clean_names() |> 
  rename("em_hrg" = "currency_code")

## Select variables of interest ----
## Step 1: Clean and flag rows with '*' ----
costs_hrg_clean_ae <- costs_hrg_ae_df |> 
  select(service_description, service_code, currency_description, em_hrg, 
         number_of_attendances, 
         national_average_unit_cost, cost) |>
  
  mutate(
    # Flag rows where the cost field contains '*'
    has_star_cost = grepl("\\*", as.character(number_of_attendances))
  )

costs_hrg_clean_ae <- costs_hrg_clean_ae |> 
  mutate(
    # Replace * (indicating <5) with 3
    number_of_attendances = ifelse(has_star_cost, 3,
                                   number_of_attendances),
    number_of_attendances = as.numeric(number_of_attendances)
  )

## Step 2: Impute cost ----
ae_costs_hrg_imputed <- costs_hrg_clean_ae |>
  group_by(em_hrg) |>
  summarise(
    total_costs = sum(cost),
    total_attendances = sum(number_of_attendances),
    unit_cost = total_costs/total_attendances,
    .groups = "drop"
  )

# join onto AE patient table
ae_hrg_df_merged <- ae_hrg_costs_df |>
  left_join(ae_costs_hrg_imputed, by = "em_hrg") |>
  mutate(user_status = "User") |>
  select(!c(row_no, total_attendances, total_costs))


# diagnostic
ae_hrg_df_merged %>% 
  summarise(
    total = n(),
    missing_costs = sum(is.na(unit_cost)),
    pct_missing = mean(is.na(unit_cost))*100
  )


## Bring back the non-users ----
costs_data_hrg_ae_non_users_df <- read_csv(
  "H:/Projects/Validation/HES HRG/Source/AE/costs_data_hrg_ae_non-users.csv"
) |> 
  mutate(patid = as.integer64(patid)) |> 
  mutate(em_hrg = NA,
         unit_cost = 0) |> 
  mutate(user_status = "Non-User") |> 
  mutate(
    age = NA_integer_,
    aepatientgroup = NA_integer_,
    inv_01 = NA_character_, inv_02 = NA_character_, inv_03 = NA_character_,
    inv_04 = NA_character_, inv_05 = NA_character_, inv_06 = NA_character_,
    inv_07 = NA_character_, inv_08 = NA_character_, inv_09 = NA_character_,
    inv_10 = NA_character_, inv_11 = NA_character_, inv_12 = NA_character_,
    treat_01 = NA_character_, treat_02 = NA_character_, treat_03 = NA_character_,
    treat_04 = NA_character_, treat_05 = NA_character_, treat_06 = NA_character_,
    treat_07 = NA_character_, treat_08 = NA_character_, treat_09 = NA_character_,
    treat_10 = NA_character_, treat_11 = NA_character_, treat_12 = NA_character_,
    em_hrg = NA_character_,
    unit_cost = 0
    ) |> 
  select(patid, age, aepatientgroup, inv_01, inv_02, inv_03, inv_04, inv_05,
         inv_06, inv_07, inv_08, inv_09, inv_10, inv_11, inv_12, treat_01,
         treat_02, treat_03, treat_04, treat_05, treat_06, treat_07, treat_08,
         treat_09, treat_10, treat_11, treat_12, em_hrg, unit_cost, user_status)

ae_costs_final_data_raw <- rbind(ae_hrg_df_merged, costs_data_hrg_ae_non_users_df) 


## Summary Stats ----
users_summary_costs_ae <- ae_costs_final_data_raw %>% 
  filter(user_status == "User")

overall_users_costs_df_ae_raw <- users_summary_costs_ae |> 
  group_by(patid) %>%                     # group data per patient
  summarise(total_patient_cost = sum(unit_cost)) %>%   # derive total number of appointments by picking max value of apptorder
  ungroup()
  
  
users_summary_costs_stats_ae_raw <- overall_users_costs_df_ae_raw |>   
  summarise(
    n_attendances = n(),
    mean_attendance_costs = mean(total_patient_cost, na.rm = TRUE),
    min_attendance_costs = min(total_patient_cost, na.rm = TRUE),
    median_attendance_costs = median(total_patient_cost, na.rm = TRUE),
    max_attendance_costs = max(total_patient_cost, na.rm = TRUE),
    sd_attendance_costs = sd(total_patient_cost, na.rm = TRUE),
    q1_attendance_costs = quantile(total_patient_cost, probs = 0.25, na.rm = TRUE),
    q3_attendance_costs = quantile(total_patient_cost, probs = 0.75, na.rm = TRUE),
    se               = sd_attendance_costs / sqrt(n_attendances),
    ci_lower         = mean_attendance_costs - qt(0.975, df = n_attendances - 1) * se,
    ci_upper         = mean_attendance_costs + qt(0.975, df = n_attendances - 1) * se,
    .groups = "drop"
  ) |> 
  mutate(across(everything(), round, 2))

overall_all_costs_df_ae_raw <- ae_costs_final_data_raw |> 
  group_by(patid) %>%                     # group data per patient
  summarise(total_patient_cost = sum(unit_cost)) %>%   # derive total number of appointments by picking max value of apptorder
  ungroup()

all_pts_summary_costs_stats_ae_raw <- overall_all_costs_df_ae_raw %>% 
  summarise(
    n_attendances = n(),
    mean_attendance_costs = mean(total_patient_cost, na.rm = TRUE),
    min_attendance_costs = min(total_patient_cost, na.rm = TRUE),
    median_attendance_costs = median(total_patient_cost, na.rm = TRUE),
    max_attendance_costs = max(total_patient_cost, na.rm = TRUE),
    sd_attendance_costs = sd(total_patient_cost, na.rm = TRUE),
    q1_attendance_costs = quantile(total_patient_cost, probs = 0.25, na.rm = TRUE),
    q3_attendance_costs = quantile(total_patient_cost, probs = 0.75, na.rm = TRUE),
    se               = sd_attendance_costs / sqrt(n_attendances),
    ci_lower         = mean_attendance_costs - qt(0.975, df = n_attendances - 1) * se,
    ci_upper         = mean_attendance_costs + qt(0.975, df = n_attendances - 1) * se,
    .groups = "drop"
  ) |> 
  mutate(across(everything(), round, 2))

# Export the file
library(writexl)

ae_costs_list <- list(
  summary_users_atts_costs = users_summary_costs_stats_ae_raw,
  summary_all_atts_costs = all_pts_summary_costs_stats_ae_raw
)

# Write to an Excel file
write_xlsx(ae_costs_list, here::here("CPRD Raw Analysis", "Results",
                                  "SecondaryCareAnalysis", "AE", "Output_Costs_Raw.xlsx"))

