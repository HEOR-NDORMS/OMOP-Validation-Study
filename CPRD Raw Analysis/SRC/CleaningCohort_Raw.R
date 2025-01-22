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
  dplyr::filter(days_prior_obs >= prior_observation, days_after_obs >= 0) %>%

# count of patients
n_distinct(fracture_table_j$patid)

# count of fractures
dim(fracture_table_j)


## Remove fx Age <50 on index -----

# as.numeric(format(index_date, "%Y")):
#   This takes the year from the index_date column (e.g., the year 2023 from "2023-01-20").
# 
# - yob:
#   Subtracts the year of birth (yob) from the year of index_date to get a rough estimate of the person's age.
# 
# if_else(format(index_date, "%m-%d") < "01-01", 1, 0):
# Here we check if the patient's birthday has already occurred in the current year.
# using January 1st specifically—it's just a placeholder to decide if the subtraction applies.
# It checks if the birth month and day come before the reference date's (index_date) month and day. In this case:
# If their birthday hasn't occurred yet in the year of the index_date, subtract 1 from the rough age to account for that.
# If their birthday has already occurred, the rough age is correct, so no adjustment is needed.
# # If the month and day of index_date are earlier than January 1st, it subtracts 1 from the age 
# # (because the birthday hasn't occurred yet in that year).


fracture_table_k <- fracture_table_j %>% 
  mutate(
    # Age using index_date
    age_index = as.numeric(format(index_date, "%Y")) - yob -
      if_else(format(index_date, "%m-%d") < "01-01", 1, 0),
    
    # Check if age < 50 for both reference dates
    age_lt_50_index = age_index < 50
  ) %>% 
  filter(age_lt_50_index == FALSE)

# sense check if the compare the month and day of the reference date (index_date) with the month and day of their birthdate. 
# If the birthdate hasn't occurred yet in the year, we adjust.
# fracture_table_k.1 <- fracture_table_j %>% 
#   mutate(
#     age_index = as.numeric(format(index_date, "%Y")) - yob -
#       if_else(format(index_date, "%m-%d") < format(as.Date(paste0(yob, "-01-01")), "%m-%d"), 1, 0)
#   )



# count of patients
n_distinct(fracture_table_k$patid)

# count of fractures
dim(fracture_table_k)

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



