# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Cleaning Cohort; clean the data grouped by subject_id based on 
# the remaining conditions in the table
# Date: 13.01.2024
# Last date edited: 21.01.2025
# ****************************************************************************** #

# Cleaning Cohort ----

# Starting with trauma_fract_table_g ran from CleaningFractures_Raw.R script...
cohort_table <- fracture_table_g %>% 
  select(-c(fracture_year, trauma_present, number_sites))

## Only female ----

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

observations <- my_dm$`validation_observation` %>%  collect()

cohort_table_d <- cohort_table_c %>% 
  plyr::mutate(days_prior_obs = fx_date - aurum_min_accept_date, days_after_obs = aurum_max_accept_date - fx_date) %>%
  dplyr::filter(days_prior_obs >= prior_observation, days_after_obs >= 0) 

AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_d %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_d %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records with insufficient prior observation and/or outside of observation period"
    )
  )


## e) at least 50 -----

cohort_table_e <- cohort_table_c %>% 
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



## Remove fx Prior history of cancer ----
cancer_df <- fa_df %>% 
  select(patid, cancer_fa_index, cancer_fa_index_dt) 

# merge with the fx df
cancer_df.1 <- left_join(fracture_table_k, cancer_df, by = "patid")

fracture_table_l <-  cancer_df.1 %>% 
  filter(index_date < cancer_fa_index_dt | is.na(cancer_fa_index_dt)) %>% 
  filter(fa_index_dt < cancer_fa_index_dt | is.na(cancer_fa_index_dt))


# count of patients
n_distinct(fracture_table_l$patid)

# count of fractures
dim(fracture_table_l)

## Remove Prior history of metabolic diseases ----
metabolic_df <- fa_df %>%
  select(patid, paget_fa_index, paget_fa_index_dt)

# merge with the fx df
metabolic_df.1 <- left_join(fracture_table_l, metabolic_df, by = "patid")

fracture_table_m <- metabolic_df.1 %>% 
  filter(index_date < paget_fa_index_dt | is.na(paget_fa_index_dt)) %>% 
  filter(fa_index_dt < paget_fa_index_dt | is.na(paget_fa_index_dt))

# count of patients
n_distinct(fracture_table_m$patid)

# count of fractures
dim(fracture_table_m)



