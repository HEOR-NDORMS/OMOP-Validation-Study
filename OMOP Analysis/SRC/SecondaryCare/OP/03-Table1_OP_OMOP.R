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
###Specialties ----
specialty_mapping_data <- import("check_mainspef_tretspef_outpatient_GF.xlsx",
                         sheet = "mapping_mainspef_tretspef")

### Create unique lookup tables ----
mainspef_lookup <- specialty_mapping_data %>%
  filter(!is.na(mainspef)) %>%
  distinct(mainspef, mainspef_code)

tretspef_lookup <- specialty_mapping_data %>%
  filter(!is.na(tretspef)) %>%
  distinct(tretspef, tretspef_code)

### Extract only the vectors for fuzzy matching ----
main_specialties  <- mainspef_lookup$mainspef
treat_specialties <- tretspef_lookup$tretspef


### Clean outpatient data ----
table_one_op_omop_appt <- table_one_op_omop_appt %>%
  mutate(
    specialty_source_value_modified = specialty_source_value %>%
      str_remove("\\s*\\([^\\)]+\\)") %>%   # remove parentheses
      str_replace_all("&", "and") %>%
      str_to_lower() %>%
      str_trim()
  )


### Fuzzy matching function (Jaro–Winkler similarity) ----

# The function find_best_match() tries to find the closest match to a given
# word or phrase (value) from a list of possible options (lookup_vector).
# “fuzzy matching” function because it doesn’t require exact matches.
# Even if there are small spelling mistakes or differences, it can still find the best match.

# Jaro–Winkler similarity is a way to measure how similar two strings (words) are, 
# based on:
#  1. Matching characters (letters that appear in both words and 
# are close to the same position).
#  2. Number of transpositions (how many letters are swapped).
#  3. Common prefix bonus (if the start of the words is the same, 
# it scores slightly higher).
# It gives a score between:
#   0 = completely different
#   1 = exactly the same

find_best_match <- function(value, lookup_vector, min_similarity = 0.90) {
  # If the input is missing (NA), empty, or there’s nothing to compare to, 
  # it returns NA (nothing found).
  if (is.na(value) || value == "" || length(lookup_vector) == 0) return(NA_character_)
  
  # Calculates how similar the value is to every word in lookup_vector using 
  # the Jaro–Winkler method.
  # stringdist() gives a distance (0 = identical, 1 = very different),
  # so 1 - distance gives a similarity (1 = identical, 0 = completely different).
  sim <- 1 - stringdist(value, lookup_vector, method = "jw")
  
  # Finds the index of the most similar word (best_idx) and 
  # its similarity score (best_sim).
  best_idx <- which.max(sim)
  best_sim <- sim[best_idx]
  
  # If the best match is at least 90% similar (or the threshold you set), 
  # it returns that word.
  # Otherwise, it returns NA, meaning no good enough match was found.
  if (best_sim >= min_similarity) {
    return(lookup_vector[best_idx])
  } else {
    return(NA_character_)
  }
}

### Optimized fuzzy matching on unique specialties ----
unique_specialties <- unique(
  na.omit(
    table_one_op_omop_appt$specialty_source_value_modified
    )
  )

### Match only once per unique name ----
main_matches <- tibble(
  specialty_source_value_modified = unique_specialties,
  mainspef = sapply(unique_specialties, find_best_match,
                    lookup_vector = main_specialties,
                    min_similarity = 0.85)
)

treat_matches <- tibble(
  specialty_source_value_modified = unique_specialties,
  tretspef = sapply(unique_specialties, find_best_match,
                    lookup_vector = treat_specialties,
                    min_similarity = 0.85)
)

### Merge back to the full dataset ----
table_one_op_omop_appt <- table_one_op_omop_appt %>%
  left_join(main_matches,  by = "specialty_source_value_modified") %>%
  left_join(treat_matches, by = "specialty_source_value_modified")


###  Add back corresponding codes from the mapping ----
table_one_op_omop_appt <- table_one_op_omop_appt %>%
  left_join(mainspef_lookup, by = "mainspef") %>%
  left_join(tretspef_lookup, by = "tretspef")

## Defining users ----

# Create the users status
table_one_op_omop_appt <- table_one_op_omop_appt %>% 
  mutate(
    user_status = if_else(is.na(visit_detail_start_date), "Non-User", "User")
  )

# List of summaries ----
summaries_op <- list(
  mainspef_summary = table_one_op_omop_appt %>% 
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
  
  tretspef_summary = table_one_op_omop_appt %>% 
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
  
  user_status_summary = table_one_op_omop_appt %>% 
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
            here::here("OMOPAnalysis", "Results", "SecondaryCareAnalysis", 
                       "OP","Table1_OP_OMOP.xlsx"))
