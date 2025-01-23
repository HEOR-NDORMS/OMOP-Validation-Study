# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Cleaning Cohort; clean the data grouped by subject_id based on 
# the remaining conditions in the table
# Date: 13.01.2024
# Last date edited: 21.01.2025
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
  rename(condition_date = value)  # Rename to `condition_date`

# merge with the fx df
cohort_table_e.1 <- inner_join(cohort_table_e, long_cancer_df, by = "patid") 

# remove cancer before yob
cohort_table_e.1 <- cohort_table_e.1 %>% 
  group_by(patid) %>% 
  mutate(cancer_year = lubridate::year(condition_date)) %>%
  filter(cancer_year > yob | is.na(cancer_year)) %>% 
  ungroup()

cohort_table_f <-  cohort_table_e.1 %>% 
  group_by(patid) %>% 
  filter(fx_date <= condition_date | is.na(condition_date)) %>% 
  ungroup() %>% 
  select(!c(condition, condition_date, condition_date_type, cancer_year)) %>% 
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
  rename(condition_date = value)  # Rename to `condition_date`

# merge with the fx df
cohort_table_f.1 <- inner_join(cohort_table_f, long_metabolic_df, by = "patid") 


cohort_table_g <- cohort_table_f.1 %>%
  group_by(patid) %>% 
  filter(fx_date <= condition_date | is.na(condition_date)) %>%
  ungroup() %>% 
  select(!c(condition, condition_date, condition_date_type)) %>% 
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


