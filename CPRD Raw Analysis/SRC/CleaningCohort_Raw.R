# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Cleaning Cohort; clean the data grouped by subject_id based on 
# the remaining conditions in the table
# Date: 13.01.2024
# Last date edited: 31.01.2025
# ****************************************************************************** #

# prior obs
prior_observation <- 730

# Cleaning Cohort ----

# Starting with trauma_fract_table_g ran from CleaningFractures_Raw.R script...
cohort_table <- fracture_table_g %>% 
  select(-c(fracture_year, trauma_present, number_sites))

## a) Only female ----

cohort_table_a <- cohort_table %>%
  group_by(patid) %>% 
  filter(!is.na(gender), gender == "F") %>% 
  ungroup()

AttritionReportCoh<- AttritionReportFrac_Raw[,1:3] %>% 
  union_all(  
    tibble(
      number_records = cohort_table_a %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_a %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding not female individuals"
    )
  ) 

## b) No fractures ----
# Assuming missing values in condition_start_date variable indicates no fracture?
cohort_table_b <- cohort_table_a %>% 
  group_by(patid) %>% 
  filter(!is.na(fx_date)) %>% 
  ungroup()

AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_b %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_b %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals without fracture records"
    )
  ) 

## Define index ----
# This has been done by Antonella

## c) Excluding individuals who has index date same as obs period end date ----
cohort_table_c <- cohort_table_b %>% 
  group_by(patid) %>% 
  filter(!(type == "index" & fx_date == aurum_min_accept_date)) %>% 
  ungroup()

AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_c %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_c %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding index fractures that happen on the last day of observation period"
    )
  )

## d) At least 730 days prior obs -----
# check if a fx_date where falls within the 730 days before another date (index_date).
# fa_index_dt is greater than or equal to index_date - 730 (this means it happened in the last 730 days).
# fa_index_dt is less than index_date (this makes sure it's before the index_date).
# filter(fa_index_dt >= (index_date - 730) & fa_index_dt < index_date)

cohort_table_d <- cohort_table_c %>% 
  mutate(days_prior_obs = fx_date - aurum_min_accept_date, days_after_obs = aurum_max_accept_date - fx_date) %>%
  filter(days_prior_obs >= prior_observation, days_after_obs >= 0) 

AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_d %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_d %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records with insufficient prior observation and/or outside of observation period"
    )
  )


## e) at least 50 -----
cohort_table_e <- cohort_table_d %>% 
  mutate(age_fracture = lubridate::year(fx_date) - yob) %>%
  filter(age_fracture >= 50)

AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_e %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_e %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records of index fractures where the subject has not turned 50"
    )
  )



## f) No records of cancer before or on the index date ----

# cancer columns
cancer_df <- all_patients %>% 
  select(patid, cancer_lpon_index_dt, cancer_fa_index_dt) 

# Pivot data to long format for `_index` columns
long_cancer_df <- cancer_df %>%
  pivot_longer(
    cols = -patid, 
    names_to = c("condition", "condition_date_type", "drop"), # "drop" captures `_index_dt`
    names_pattern = "(cancer)_(lpon|fa)_(index_dt)"
  ) %>%
  select(-drop) %>%  # Remove the extra "drop" column
  rename(cancer_date = value)  # Rename to `condition_date`

# merge with the fx df
cohort_table_e.1 <- inner_join(cohort_table_e, long_cancer_df, by = "patid") 

# remove cancer before yob
cohort_table_e.1 <- cohort_table_e.1 %>% 
  group_by(patid) %>% 
  mutate(cancer_year = lubridate::year(cancer_date)) %>%
  filter(cancer_year > yob | is.na(cancer_year)) %>% 
  ungroup()

cohort_table_f <-  cohort_table_e.1 %>% 
  group_by(patid) %>% 
  filter(fx_date <= cancer_date | is.na(cancer_date)) %>% 
  ungroup() %>% 
  select(!c(condition, condition_date_type, cancer_year)) %>% 
  distinct()

AttritionReportCoh <- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_f %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_f %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records happened after cancer"
    )
  ) 


## g) No records of metabolic bone disease ----
metabolic_df <- all_patients %>% 
  select(patid, paget_lpon_index_dt, paget_fa_index_dt) 

# Pivot data to long format for `_index` columns
long_metabolic_df <- metabolic_df %>%
  pivot_longer(
    cols = -patid, 
    names_to = c("condition", "condition_date_type", "drop"), # "drop" captures `_index_dt`
    names_pattern = "(paget)_(lpon|fa)_(index_dt)"
  ) %>%
  select(-drop) %>%  # Remove the extra "drop" column
  rename(paget_date = value)  # Rename to `condition_date`

# merge with the fx df
cohort_table_f.1 <- inner_join(cohort_table_f, long_metabolic_df, by = "patid") 


cohort_table_g <- cohort_table_f.1 %>%
  group_by(patid) %>% 
  filter(fx_date <= paget_date | is.na(paget_date)) %>%
  ungroup() %>% 
  select(!c(condition, condition_date_type)) %>% 
  distinct()

AttritionReportCoh <- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_g %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_g %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records happened after metabolic bone diseases"
    )
  ) 

# Finalise attrition ----
AttritionReportCoh <- AttritionReportCoh %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records)))

# rio::export(AttritionReportCoh, here::here("CPRD Raw Analysis", "Results", "AttritionReportCoh_Raw.xlsx"))

## Creating follow-up end ------
### 730 days after the index date ----
# for a fracture table, compute the index date for each person
addIndex <- function (fractureTable){
  fractureTable %>% 
    inner_join(fractureTable %>% 
                        filter(type == "index") %>%
                        group_by(patid) %>% 
                        summarise(index_date = min(fx_date)) %>% 
                        ungroup(),
                      by = "patid"
    )
}

cohort_table_g.1 <- addIndex(cohort_table_g)

# add an extra variable after_index which is two years after the index date
# addInTwoYearsAfter <- function (fractureTable){
#   fractureTable %>%
#     mutate(after_index = index_date + 730)
# }

# the rule is: follow-up end date is the earliest date of censoring event or index+730. Meaning that if there are no censoring events the follow-up will end 730 days after index date.
# cohort_table_g.2 <- cohort_table_g.1 %>%
#   group_by(patid) %>% 
#   mutate(after_index = if_else(type == "fa", fx_date, fx_date + 730)) %>%
#   ungroup()


cohort_table_g.2 <- cohort_table_g.1 %>%
  group_by(patid) %>%
  mutate(fracture_after_index = case_when(
    type == "fa" ~ fx_date,
    type == "index" & any(type == "fa") ~ NA_Date_,
    type == "index" & !any(type == "fa") ~ fx_date + 730,
    TRUE ~ NA_Date_
  )) 
# %>%
#   fill(after_index, .direction = "downup")

# add a column of next fracture after the index one
# addInNextFracture <- function(fractureTable){
#   fractureTable %>% 
#     left_join(fractureTable %>% 
#                        filter(fx_date > index_date) %>% 
#                        group_by(patid) %>% 
#                        summarise(fracture_after_index = min(fx_date, na.rm =  T)) %>% 
#                        ungroup(),
#                      by = "patid")
# }
# 
# cohort_table_g.3 <- addInNextFracture(cohort_table_g.2)

# Add in FOLLOWUPEND ----
# add in FOLLOWUPEND - only after the relevant columns are added
addInFollowUpEnd <- function(fractureTable){
  fractureTable %>% 
    mutate(follow_up_end = pmin(.data$aurum_max_accept_date, .data$cancer_date, .data$paget_date, .data$fracture_after_index, .data$death_date, na.rm = TRUE)) %>%
    mutate(follow_up_time = follow_up_end - index_date) %>%
    mutate(censoring_event = case_when(
      follow_up_end == .data$aurum_max_accept_date ~ "aurum_max_accept_date",
      follow_up_end == .data$cancer_date ~ "cancer_date",
      follow_up_end == .data$paget_date ~ "paget_date",
      follow_up_end == .data$fracture_after_index ~ "fracture_after_index",
      follow_up_end == .data$death_date ~ "death_date",
      TRUE ~ NA_character_
    ))
}
cohort_table_g.3 <- addInFollowUpEnd(cohort_table_g.2)


# Ok, so.. A censoring event is defined as the earliest occurrence among the following: an imminent fracture after the index date (FA), death, cancer, metabolic disease, end of data collection, or two years from the index date. Each patient will have at least one censoring event.
# The "follow-up end date" function identifies the earliest of these events, determining the end of follow-up. In some cases, multiple events may occur on the same day, for example, a diagnosis of cancer and MBD. However, in most cases, I expect each patient to have a single censoring event.
# Regarding the patient you mentioned (index date: 24-07-2013):
#   They experienced an NHNS fracture on 31-07-2013.
# They were diagnosed with cancer on 23-05-2018.
# This patient does not have two cancer diagnoses, only one. However, cancer is not the earliest censoring event. The NHNS fracture following the index date is the first censoring event, and this should be the event counted in Table 1 for this patient.

