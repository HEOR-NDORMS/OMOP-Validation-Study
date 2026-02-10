# Load dplyr package
library(dplyr)
library(rio)
library(janitor)
library(tidyverse)
library(tidylog)
library(bit64)
library(here)
library(boot)

# OP ----

## Read the files ----
op_hrg_df <- read_csv(
  "H:/Projects/Validation/HES HRG/Source/OP/OP_costs_data_HRG_attend.csv"
  ) |> 
  clean_names()

# Select variables of interest
op_hrg_df_min <- op_hrg_df |> 
  select(startage, sex, mainspef, tretspef, firstatt, nac_hrg)

op_study_ids_df <- read_csv(
  "H:/Projects/Validation/HES HRG/Source/OP/op_costs_data_ids.csv"
)

# Merge the two together 
op_hrg_costs_df <- cbind(op_study_ids_df, op_hrg_df_min) |> 
  mutate(patid = as.integer64(patid))

costs_hrg_df <- import(here("NHS_Collection_Costs_2024_2025.xlsx"),
                       sheet = "OP"
) |> 
  clean_names() |> 
  rename("nac_hrg" = "currency_code",
         "tretspef" = "service_code")

## Select variables of interest ----
# Step 1: Clean and flag rows with '*'
costs_hrg_clean_op <- costs_hrg_df %>%
    # Clean and transform columns
    mutate(
      # Flag rows where the cost field contains '*'
      has_star_cost = grepl("\\*", as.character(number_of_attendances)),
      ) |>
    mutate(
      # Replace * (indicating <5) with 3
      number_of_attendances = ifelse(has_star_cost, 3,
               number_of_attendances),
      number_of_attendances = as.numeric(number_of_attendances)
    )

# Step 2: Impute cost based on your logic
op_costs_hrg_imputed <- costs_hrg_clean_op |>
  group_by(tretspef, nac_hrg) |>
  summarise(
    total_costs = sum(cost),
    total_attendances = sum(number_of_attendances),
    unit_cost = total_costs/total_attendances
    ) |>
  ungroup() |> 
  distinct(tretspef, nac_hrg, unit_cost)

## Merge on both hrg_code and tretspef ----
op_hrg_df_merged <- left_join(
  op_hrg_costs_df |> mutate(tretspef = as.character(tretspef)),
  op_costs_hrg_imputed |> select(tretspef, nac_hrg, unit_cost) |> distinct(),
  by = c("nac_hrg", "tretspef")
) |> 
  mutate(user_status = "User")

## Bring back the non-users ----
op_costs_non_users_df <- read_csv(
  "H:/Projects/Validation/HES HRG/Source/OP/op_costs_non_users_data.csv"
) |> 
  mutate(patid = as.integer64(patid)) |> 
  rename(sex = gender) |> 
  select(patid, startage, sex, mainspef, tretspef,
         firstatt) |> 
  mutate(nac_hrg = NA,
         unit_cost = 0) |> 
  mutate(user_status = "Non-User")

op_costs_final_data <- rbind(op_hrg_df_merged, 
                             op_costs_non_users_df) 

### Recode specialty variables
op_costs_final_data_recoded_raw <- op_costs_final_data |> 
  mutate_at(c("mainspef", "tretspef"), as.factor) |> 
  mutate(
    mainspef_recoded = fct_recode(
      mainspef, 
      "General Surgery" = "100", 
      "Urology" = "101",
      "Trauma and Orthopaedics" = "110",
      "Ear Nose and Throat" = "120",
      "Ophthalmology" = "130", 
      "Oral Surgery" = "140", 
      "Oral Surgery" = "145", 
      "Neurosurgery" = "150", 
      "Plastic Surgery" = "160",
      "Emergency Medicine" = "180",
      "Anaesthetics" = "190", 
      "General Internal Medicine" = "300",
      "Gastroenterology" = "301", 
      "Endocrinology and Diabetes" = "302",
      "Clinical Haematology" = "303", 
      "Cardiology" = "320",
      "Acute Internal Medicine" = "326",
      "Dermatology" = "330", 
      "Respiratory Medicine" = "340",
      "Infectious Diseases" = "350",
      "Renal Medicine" = "361",
      "Medical Oncology" = "370",
      "Neurology" = "400",
      "Rheumatology" = "410",
      "Geriatric Medicine" = "430",
      "Obstetrics" = "501",
      "Gynaecology" = "502",
      "Adult Mental Illness" = "710",
      "Clinical Oncology" = "800", 
      "Radiology" = "810",
      "Radiology" = "823",
      "Restorative Dentistry" = "141",
      "Orthodontics" = "143", 
      "Clinical Physiology" = "304",
      "Audio Vestibular Medicine" = "310", 
      "Clinical Genetics" = "311", 
      "Clinical Immunology" = "313" ,
      "Rehabilitation Medicine" =  "314",
      "Clinical Neurophysiology" = "401", 
      "Midwifery" = "560", 
      "Learning Disability" = "700",
      "Medical Microbiology" = "833",
      "Medical Psychotherapy" = "713",
      "Paediatrics" = "420", 
      "Community Medicine" = "900",
      "Nursing " = "950",
      "Allied Health Professional" = "960",
      "Old Age Psychiatry" = "715",
      "Cardiothoracic Surgery" = "170",
      "General Medical Practice" = "600",
      "Palliative Medicine" = "315",
      "Dental Medicine" = "450",
      "Nuclear Medicine" = "371",
      "Clinical Pharmacology" = "305",
      "Chemical Pathology" = "822",
      "General Pathology" = "820",
      "Medical Ophthalmology" = "460",
      "Intensive Care Medicine" = "192",
      "Occupational Medicine" = "901",
      "Prosthodontics" = "148",
      "Periodontics" = "147",
      "Immunopathology" = "830",
      "Genitourinary Medicine" = "360",
      "Child and Adolescent Psychiatry" = "711",
      "Tropical Medicine" = "352",
      "Sport and Exercise Medicine" = "325"
    ),
    tretspef_recoded = fct_recode(
      tretspef, "General Surgery Service" = "100", 
      "Urology Service" = "101", 
      "Urology Service" = "103", 
      "Upper Gastrointenstinal Surgery Service" = "106", 
      "Colorectal Surgery Service" = "104", 
      "Vascular Surgery Service" = "107",
      "Spinal Surgery Service" = "108",
      "Trauma and Orthopaedic Service" = "110", 
      "Ophthalmology Service" = "130", 
      "Oral Surgery Service" = "140", 
      "Maxillofacial Surgery Service" = "144", 
      "Neurosurgical Service" = "150", 
      "Plastic Surgery Service" = "160", 
      "Emergency Medicine Service" = "180", 
      "Anaesthetic Service" = "190", 
      "Pain Management Service" = "191",
      "Intensive Care Medicine Service" = "192",
      "Paediatric Trauma and Orthopaedic Service" = "214", 
      "General Internal Medicine Service" = "300", 
      "Gastroenterology Service" = "301", 
      "Clinical Haematology Service" = "303", 
      "Endocrinology Service" = "302", 
      "Hepatology Service" = "306", 
      "Cardiology Service" = "320",
      "Transient Ischaemic Attack Service" = "329", 
      "Dermatology Service" = "330", 
      "Respiratory Medicine Service" = "340", 
      "Respiratory Physiology Service" = "341",
      "Medical Oncology Service" = "370", 
      "Neurology Service" = "400", 
      "Rheumatology Service" = "410", 
      "Obstetrics Service" = "501", 
      "Gynaecology Service" = "502", 
      "Gynaecological Oncology Service" = "503",
      "Adult Mental Health Service" = "710",  
      "Clinical Oncology Service" = "800", 
      "Interventional Radiology Service" = "811",
      "Ear Nose and Throat Service" = "120",
      "Hepatobiliary and Pancreatic Surgery Service" = "105",
      "Bariatric Surgery Service" = "109",
      "Restorative Dentistry Service" = "141",
      "Orthodontic Service" = "143",
      "Oral and Maxillofacial Surgery Service"  = "145",
      "Paediatric Diabetes Service" = "263",
      "Clinical Physiology Service" = "304",
      "Diabetes Service" = "307",
      "Blood and Marrow Transplantation Service" = "308",
      "Audio Vestibular Medicine Service" = "310",
      "Clinical Genetics Service" = "311",
      "Clinical Immunology and Allergy Service" = "313",
      "Rehabilitation Medicine Service" = "314",
      "Allergy Service" = "317",
      "Anticoagulant Service" = "324",
      "Congenital Heart Disease Service" = "331",
      "Specialist Rehabilitation Service" = "345", 
      "Infectious Diseases Service" = "350",
      "Renal Medicine Service" = "361",
      "Nuclear Medicine Service" = "371",
      "Clinical Neurophysiology Service" = "401",
      "Elderly Medicine Service" = "430",
      "Medical Ophthalmology Service" = "460",
      "Fetal Medicine Service" = "505",
      "Midwifery Service" = "560",
      "Physiotherapy Service" = "650",
      "Occupational Therapy Service" = "651",
      "Speech and Language Therapy Service" = "652",
      "Podiatry Service" = "653",
      "Dietetics Service" = "654",
      "Orthoptics Service" = "655",
      "Clinical Psychology Service" = "656",
      "Orthotics Service" = "658",
      "Optometry Service" = "662",
      "Podiatric Surgery Service" = "663",
      "Cardiac Physiology Service" = "675",
      "Learning Disability Service" = "700",
      "Diagnostic Imaging Service" = "812",
      "Medical Virology Service" = "834",
      "Audiology Service" = "840",
      "Diabetic Education Service" = "920",
      "Intermediate Care Service" = "318",
      "Old Age Psychiatry Service" = "715",
      "Palliative Medicine Service" = "315",
      "Dental Medicine Service" = "450",
      "Cardiac Rehabilitation Service" = "327",
      "Liaison Psychiatry Service" = "722",
      "Chemical Pathology Service" = "822",
      "Stroke Medicine Service" = "328",
      "Cardiac Surgery Service" = "172",
      "Cardiothoracic Transplantation Service" = "174",
      "Cardiothoracic Surgery Service" = "170",
      "Burns Care Service" = "161",
      "Thoracic Surgery Service" = "173",
      "Clinical Immunology Service" = "316",
      "Spinal Injuries Service" = "323",
      "Transplant Surgery Service" = "102",
      "Clinical Pharmacology Service" = "305",
      "Paediatric Ophthalmology Service" = "216",
      "Haemophilia Service" = "309",
      "Clinical Microbiology Service" = "322",
      "Pulmonary Rehabilitation Service" = "342",
      "Genitourinary Medicine Service" = "360",
      "Prosthetics Service" = "657",
      "Medical Psychotherapy Service" = "713",
      "Sport and Exercise Medicine Service" = "325",
      "Complex Specialised Rehabilitation Service" = "344",
      "Genitourinary Medicine Service" = "360",
      "Tropical Medicine Service" = "352",
      "Child and Adolescent Psychiatry Service" = "711",
      "Paediatric Service" = "420",
      "Paediatric Ear Nose and Throat Service" = "215"
    )
  )

# Summary Stats ----

## Users ----
users_op_costs_df <- op_costs_final_data_recoded_raw |> 
  filter(user_status == "User")

overall_users_costs_df <- users_op_costs_df |> 
  group_by(patid) %>%                     # group data per patient
  summarise(total_patient_cost = sum(unit_cost)) %>%   # derive total number of appointments by picking max value of apptorder
  ungroup()

# Summary statistics
summary_users_appts_costs <- overall_users_costs_df %>%
  summarise(
    n            = n(),
    mean         = mean(total_patient_cost, na.rm = T),
    min          = min(total_patient_cost, na.rm = T),
    max          = max(total_patient_cost, na.rm = T),
    sd           = sd(total_patient_cost, na.rm = T),
    median       = quantile(total_patient_cost, 0.5, na.rm = T),
    lower_q      = quantile(total_patient_cost, 0.25, na.rm = T),
    upper_q      = quantile(total_patient_cost, 0.75, na.rm = T),
    se                 = sd / sqrt(n),
    ci_lower           = mean - qt(0.975, df = n - 1) * se,
    ci_upper           = mean + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))

users_overall_by_tretspef_c <- op_costs_final_data_recoded_raw |> 
  filter(user_status == "User") |> 
  group_by(patid, tretspef_recoded) %>%                     # group data per patient
  summarise(n_costs = sum(unit_cost), .groups = "drop") %>% 
  group_by(tretspef_recoded) %>%
  summarise(tot_costs = round(sum(n_costs, na.rm = TRUE), 2),
            mean_costs = round(mean(n_costs, na.rm = TRUE)),
            sd_costs = round(sd(n_costs, na.rm = TRUE), 2),
            min_costs = round(min(n_costs, na.rm = TRUE), 2),
            max_costs = round(max(n_costs, na.rm = TRUE), 2),
            num_subjects_visited = n_distinct(patid),
            .groups = "drop")

rio::export(users_overall_by_tretspef_c,
            here::here("CPRD Raw Analysis", "Results",
                       "SecondaryCareAnalysis","OP", "Overall","Output_costs_by_tretspef-Users_Raw.xlsx"))
users_overall_by_tretspef_v <- op_costs_final_data_recoded_raw %>%
  filter(user_status == "User") %>% 
  group_by(patid, tretspef_recoded) %>%                     # group data per patient
  summarise(n_appts = n(), .groups = "drop") %>% 
  group_by(tretspef_recoded) %>%
  summarise(tot_visits = round(sum(n_appts, na.rm = TRUE), 2),
            mean_visits = round(mean(n_appts, na.rm = TRUE)),
            sd_visits = round(sd(n_appts, na.rm = TRUE), 2),
            min_visit = round(min(n_appts, na.rm = TRUE), 2),
            max_visits = round(max(n_appts, na.rm = TRUE), 2),
            num_subjects_visited = n_distinct(patid),
            .groups = "drop")
rio::export(users_overall_by_tretspef_v,
            here::here("CPRD Raw Analysis", "Results",
                       "SecondaryCareAnalysis","OP", "Overall", "Output_appointments_by_tretspef-Users_Raw.xlsx"))



## All ----
overall_all_costs_df <- op_costs_final_data_recoded_raw |> 
  group_by(patid) %>%                     # group data per patient
  summarise(total_patient_cost = sum(unit_cost)) %>%   # derive total number of appointments by picking max value of apptorder
  ungroup()

# Summary statistics
summary_all_appts_costs <- overall_all_costs_df %>%
  summarise(
    n            = n(),
    mean         = mean(total_patient_cost, na.rm = T),
    min          = min(total_patient_cost, na.rm = T),
    max          = max(total_patient_cost, na.rm = T),
    sd           = sd(total_patient_cost, na.rm = T),
    median       = quantile(total_patient_cost, 0.5, na.rm = T),
    lower_q      = quantile(total_patient_cost, 0.25, na.rm = T),
    upper_q      = quantile(total_patient_cost, 0.75, na.rm = T),
    se                 = sd / sqrt(n),
    ci_lower           = mean - qt(0.975, df = n - 1) * se,
    ci_upper           = mean + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))


all_overall_by_tretspef_c <- op_costs_final_data_recoded_raw |> 
  group_by(patid, tretspef_recoded) %>%                     # group data per patient
  summarise(n_costs = sum(unit_cost), .groups = "drop") %>% 
  group_by(tretspef_recoded) %>%
  summarise(tot_costs = round(sum(n_costs, na.rm = TRUE), 2),
            mean_costs = round(mean(n_costs, na.rm = TRUE)),
            sd_costs = round(sd(n_costs, na.rm = TRUE), 2),
            min_costs = round(min(n_costs, na.rm = TRUE), 2),
            max_costs = round(max(n_costs, na.rm = TRUE), 2),
            num_subjects_visited = n_distinct(patid),
            .groups = "drop") |> 
  mutate(tretspef_recoded = if_else(is.na(tretspef_recoded), "Non-User", tretspef_recoded))

rio::export(all_overall_by_tretspef_c,
            here::here("CPRD Raw Analysis", "Results",
                       "SecondaryCareAnalysis", "OP", "Overall", "Output_costs_by_tretspef-All_Raw.xlsx"))
all_overall_by_tretspef_v <- op_costs_final_data_recoded_raw %>%
  group_by(patid, tretspef_recoded) %>%  
  mutate(tretspef_recoded = if_else(is.na(tretspef_recoded), "Non-User", tretspef_recoded)) |> # group data per patient
  summarise(n_appts = n(), .groups = "drop") %>%
  mutate(n_appts = if_else(tretspef_recoded == "Non-User", 0, n_appts)) |> 
  group_by(tretspef_recoded) %>%
  summarise(tot_visits = round(sum(n_appts, na.rm = TRUE), 2),
            mean_visits = round(mean(n_appts, na.rm = TRUE)),
            sd_visits = round(sd(n_appts, na.rm = TRUE), 2),
            min_visit = round(min(n_appts, na.rm = TRUE), 2),
            max_visits = round(max(n_appts, na.rm = TRUE), 2),
            num_subjects_visited = n_distinct(patid),
            .groups = "drop") |> 
  mutate_at("tretspef_recoded", as.factor) %>%
  mutate(tretspef_recoded = forcats::fct_infreq(tretspef_recoded))

rio::export(all_overall_by_tretspef_v,
            here::here("CPRD Raw Analysis", "Results",
                       "SecondaryCareAnalysis","OP", "Overall", "Output_appointments_by_tretspef-All_Raw.xlsx"))


# Export the file
library(writexl)

costs_list <- list(
  summary_users_appts_costs = summary_users_appts_costs,
  summary_all_appts_costs = summary_all_appts_costs
)

# Write to an Excel file
write_xlsx(costs_list, here::here("CPRD Raw Analysis", "Results",
                                  "SecondaryCareAnalysis", "OP","Overall", "Output_Costs_Raw.xlsx"))

# Bootstrap visit costs -----

# --- Function to compute bootstrap summary for a given dataset

bootstrap_summary_c <- function(data, varname = "total_patient_cost", R = 1000, seed = 626) {
  
  stat_fn <- function(d, i) {
    d_sample <- d[i, ]
    mean(d_sample[[varname]], na.rm = TRUE)
  }
  
  set.seed(seed)
  boot_result <- boot(data = data, statistic = stat_fn, R = R)
  boot_ci <- boot.ci(boot_result, type = c("norm", "basic", "perc"))
  
  boot_mean_costs     <- mean(boot_result$t)
  boot_values_costs   <- boot_result$t
  boot_variance_costs <- var(as.numeric(boot_values_costs))
  boot_sd_costs       <- sd(boot_values_costs)
  boot_se_costs       <- boot_sd_costs / sqrt(length(boot_values_costs))
  
  ci_norm  <- boot_ci$normal[2:3]
  ci_basic <- boot_ci$basic[4:5]
  ci_perc  <- boot_ci$perc[4:5]
  
  # Return in wide format
  tibble(
    mean     = boot_mean_costs,
    variance = boot_variance_costs,
    sd       = boot_sd_costs,
    se       = boot_se_costs,
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
boot_summary_all_patients_all_costs_encounters <- bootstrap_summary_c(overall_all_costs_df, varname = "total_patient_cost")

# --- Bootstrap: All Patients & Relevant Encounters
# boot_summary_all_patients_relevant_encounters <- bootstrap_summary(all_patients_relevant_encounters_counts, varname = "total_patient_encounters")

# --- Bootstrap: Users Only & All Encounters
boot_summary_users_all_costs_encounters <- bootstrap_summary_c(overall_users_costs_df, varname = "total_patient_cost")

# --- Bootstrap: Users Only & Relevant Encounters
# boot_summary_users_relevant_encounters <- bootstrap_summary(users_relevant_encounters_counts, varname = "total_patient_encounters")


(combined_bootstrap_summary_encounters_costs <- bind_rows(
  boot_summary_all_patients_all_costs_encounters %>%
    mutate(group = "All Patients & all encounters"),
  
  # boot_summary_all_patients_relevant_encounters %>%
  #   mutate(group = "All Patients & relevant encounters"),
  
  boot_summary_users_all_costs_encounters %>%
    mutate(group = "Users & all encounters"),
  
  # boot_summary_users_relevant_encounters %>%
  #   mutate(group = "Users & relevant encounters")
) %>%
    select(group, everything()))


# Export
export(combined_bootstrap_summary_encounters_costs, 
       here("CPRD Raw Analysis", "Results", "SecondaryCareAnalysis" , "OP",
            "Overall", "overall_bootstrap_summary_costs_encounters_SOURCE.xlsx"))
