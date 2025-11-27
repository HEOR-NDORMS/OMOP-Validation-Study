# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: OP Table 1: description of the patients
# Date: 01.09.2025
# Last date edited: 03.10.2025
# ****************************************************************************** #

library(stringdist)
# Prepare the OP appts data ----
## Filter visit_detail for outpatient visits within study period ----
# Sample 100 unique person_ids from the filtered outpatient visits

# sampled_ids <- cdm[["visit_detail"]] %>%
#   dplyr::distinct(person_id) %>%
#   dplyr::collect() %>%
#   dplyr::sample_n(100)
# 
# # Filter the visit_detail_filtered table to only include those 100 person_ids
# sampled_ids_db <- dplyr::copy_to(cdm$person$src, sampled_ids, name = "sampled_ids_temp", temporary = TRUE)

cdm[["visit_detail_filtered"]] <- cdm[["visit_detail"]] %>%
  # dplyr::semi_join(sampled_ids_db, by = "person_id") %>%
  dplyr::filter(
    visit_detail_concept_id == 9202,
    visit_detail_start_date >= study_start_date,
    visit_detail_start_date <= study_end_date
  ) %>%
  dplyr::select(
    visit_detail_id, person_id, provider_id,
    visit_detail_start_date, visit_detail_concept_id, preceding_visit_detail_id
  ) %>%
  dplyr::compute()

## Join with provider to get specialty info ----
cdm[["appt_data"]] <- cdm[["visit_detail_filtered"]] %>%
  dplyr::left_join(
    cdm[["provider"]] %>%
      dplyr::select(provider_id, specialty_concept_id, specialty_source_value),
    by = "provider_id"
  ) %>%
  dplyr::select(
    visit_detail_id, person_id, provider_id,
    specialty_concept_id, specialty_source_value,
    visit_detail_start_date, visit_detail_concept_id
  ) %>% dplyr::compute()

## Prepare diagnoses (all, with primary flag) ----
# all_diagnoses <- cdm[["condition_occurrence"]] %>%
#   mutate(
#     is_primary_diag = if_else(grepl("DIAG_01", condition_source_value), TRUE, FALSE)
#   ) %>%
#   select(
#     person_id, visit_occurrence_id,
#     condition_source_value, is_primary_diag
#   )

## Join diagnoses to appointment data ----
# cdm[["appt_with_diag"]] <- cdm[["appt_data"]] %>%
#   left_join(all_diagnoses, by = c("person_id")) %>%
#   compute()

## Prepare procedures (all, with primary flag) ----
# all_procedures <- cdm[["procedure_occurrence"]] %>%
#   mutate(
#     is_primary_proc = if_else(grepl("OPERTN_01", procedure_source_value), TRUE, FALSE)
#   ) %>%
#   select(
#     person_id, visit_occurrence_id,
#     procedure_concept_id, procedure_source_value, is_primary_proc
#   )

## Join procedures to appointment+diagnosis data ----
# cdm[["appt_with_diag_proc"]] <- cdm[["appt_with_diag"]] %>%
#   left_join(all_procedures, by = c("person_id", "visit_occurrence_id")) %>%
#   compute()

## Add the patient table ----
cdm[["appt_with_patient_info"]] <- cdm[["appt_data"]] %>%
  dplyr::left_join(
    cdm[["person"]] %>%
      dplyr::select(person_id, year_of_birth, gender_source_value),
    by = "person_id"
  ) %>%
  dplyr::compute()

## Merge cohort with visit data ----
cohort_appt_data <- cohort_table %>%
  dplyr::left_join(
    cdm[["appt_with_patient_info"]], 
    by = c("subject_id" = "person_id"),
    copy = TRUE
  ) %>%
  dplyr::filter(
    visit_detail_start_date >= index_date &
      visit_detail_start_date <= follow_up_end
  ) %>% 
  dplyr::collect()

## Minimise the data variables ----
table_one_op_omop_appt <- cohort_appt_data %>% 
  select(subject_id, index_date, follow_up_end,  follow_up_reason, 
         follow_up_time, visit_detail_id, provider_id,
         specialty_concept_id, specialty_source_value, 
         visit_detail_start_date, year_of_birth, gender_source_value)


# Merge back to the cohort table
table_one_op_omop_appt <- left_join(
  cohort_table %>% select(
    subject_id, index_date, follow_up_end, follow_up_end, 
    follow_up_reason, follow_up_time),
  table_one_op_omop_appt,
  by = c("subject_id", "index_date", "follow_up_end", "follow_up_reason", 
         "follow_up_time"))

# Recoding variables ----

## Specialties ----
validated_map <- read_excel("check_mainspef_tretspef_outpatient_GF.xlsx", 
                            sheet= "mapping_mainspef_tretspef") %>%
  mutate(
    specialty_source_value_modified = str_to_lower(
      str_trim(specialty_source_value_modified)
    ),
    mainspef = str_to_lower(str_trim(mainspef)),
    tretspef = str_to_lower(str_trim(tretspef))
  )

# Create small lookups for codes (for later re-join)
mainspef_codes <- validated_map %>%
  dplyr::select(mainspef, mainspef_code) %>%
  distinct() %>%
  filter(!is.na(mainspef))

tretspef_codes <- validated_map %>%
  dplyr::select(tretspef, tretspef_code) %>%
  distinct() %>%
  filter(!is.na(tretspef))

# Clean outpatient data
table_one_op_omop_appt_recoded <- table_one_op_omop_appt %>%
  mutate(
    specialty_source_value_modified = specialty_source_value %>%
      str_remove("\\s*\\([^\\)]+\\)") %>%
      str_replace_all("&", "and") %>%
      str_to_lower() %>%
      str_trim()
  )

# 3. Join validated mapping
table_one_op_omop_appt_recoded <- table_one_op_omop_appt_recoded %>%
  left_join(
    validated_map %>%
      dplyr::select(
        specialty_source_value_modified, mainspef, tretspef, 
        mainspef_code, tretspef_code) %>%
      distinct(),
    by = "specialty_source_value_modified"
  )


## Defining users ----

# Create the users status
table_one_op_omop_appt_recoded <- table_one_op_omop_appt_recoded %>% 
  mutate(
    user_status = if_else(is.na(visit_detail_start_date), "Non-User", "User")
  )

# List of summaries ----
summaries_op <- list(
  mainspef_summary = table_one_op_omop_appt_recoded %>% 
    select(subject_id, visit_detail_id, mainspef) %>% distinct() %>%
    group_by(mainspef) %>%
    summarise(count_of_appt = n(),
              .groups = "drop") %>%
    mutate(percent_appt = round(count_of_appt / sum(count_of_appt) * 100, 2),
           Variable = "Main Specialty",
           Category = mainspef) %>% 
    mutate(Category = if_else(percent_appt < 1, "Other Main Specialty", Category)) %>% 
    group_by(Variable, Category) %>%
    summarise(count_of_appt = sum(count_of_appt),
              percent_appt = round(sum(percent_appt), 2),
              .groups = "drop"),
  
  tretspef_summary = table_one_op_omop_appt_recoded %>% 
    select(subject_id, visit_detail_id, tretspef) %>% distinct() %>%
    group_by(tretspef) %>%
    summarise(count_of_appt = n(),
              .groups = "drop") %>%
    mutate(percent_appt = round(count_of_appt / sum(count_of_appt) * 100, 2),
           Variable = "Treatment Specialty",
           Category = tretspef) %>% 
    mutate(Category = if_else(percent_appt < 1, "Other Treatment Specialty", Category)) %>% 
    group_by(Variable, Category) %>%
    summarise(count_of_appt = sum(count_of_appt),
              percent_appt = round(sum(percent_appt), 2),
              .groups = "drop"),
  
  user_status_summary = table_one_op_omop_appt_recoded %>% 
    select(subject_id, user_status) %>% distinct() %>%
    group_by(user_status) %>%
    summarise(count_of_appt = n(),
              .groups = "drop") %>%
    mutate(percent_appt= round(count_of_appt / sum(count_of_appt) * 100, 2),
           Variable = "OP Health Care Service Status",
           Category = user_status)
)

# Combine them
table2_combined_summary_op <- bind_rows(summaries_op, .id = "summary_name") %>%
  select(Variable, Category, count_of_appt, percent_appt) %>%
  arrange(Variable, desc(count_of_appt)) %>% 
  mutate(Result = paste0(count_of_appt, " (", percent_appt, "%)")) %>% 
  select(!c(count_of_appt, percent_appt)) %>% 
  mutate(Category = if_else(is.na(Category), "Missing", Category))


# Export ----
rio::export(table2_combined_summary_op, 
            here::here("OMOP Analysis", "Results", "SecondaryCareAnalysis", 
                       "OP","Table1_OP_OMOP.xlsx"))
