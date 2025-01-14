# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Cleaning Cohort; clean the data grouped by subject_id based on 
# the remaining conditions in the table
# Date: 23.12.2024
# Last date edited: 23.12.2024
# ****************************************************************************** #

# Cleaning Cohort ----

# Starting with fracture_table_e ran from CleaningFractures_OMOP.R script...

## Only female ----
only_females_table <- fracture_table_e %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::filter(gender_source_value == "F")

# n_distinct(only_females_table$subject_id)
# 
# (sex_count <- only_females_table %>% 
#     dplyr::group_by(gender_source_value) %>% 
#     dplyr::summarise(Count = n_distinct(subject_id)))

# count of patients
n_distinct(only_females_table$subject_id)

# count of fractures
dim(only_females_table)

## Not female ----
not_females_table <- fracture_table_e %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::filter(gender_source_value != "F")

# count of patients
n_distinct(not_females_table$subject_id)

# count of fractures
dim(not_females_table)

## Have fractures ----
# Assuming missing values in condition_start_date variable indicates no fracture?
have_fracture_table <- only_females_table %>% 
  dplyr::filter(!is.na(condition_start_date))

# count of patients
n_distinct(have_fracture_table$subject_id)

# count of fractures
dim(have_fracture_table)

# (define index) - look from the SAP
# History of fx in the 730 prior to index
# Age <50 on index 
# Prior history of cancer
# Prior history of metabolic diseases
