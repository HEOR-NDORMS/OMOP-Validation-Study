# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Cleaning Cohort; clean the data grouped by subject_id based on 
# the remaining conditions in the table
# Date: 13.01.2024
# Last date edited: 15.01.2024
# ****************************************************************************** #

# Cleaning Cohort ----

# Starting with trauma_fract_table_f ran from CleaningFractures_Raw.R script...

## Only female ----
fracture_table_g <- trauma_fract_table_f %>% 
  filter(gender == "F")

# count of patients
n_distinct(fracture_table_g$patid)

# count of fractures
dim(fracture_table_g)

## Remove patients w/o having fractures ----
# Assuming missing values in condition_start_date variable indicates no fracture?
fracture_table_h <- fracture_table_g %>% 
  filter(!is.na(index_date)) %>% 
  filter(!is.na(fa_index_dt))

# count of patients
n_distinct(fracture_table_h$patid)

# count of fractures
dim(fracture_table_h)

# (define index)

## History of fx in the 730 prior to index -----
# check if a date (fa_index_dt) falls within the 730 days before another date (index_date).
# fa_index_dt is greater than or equal to index_date - 730 (this means it happened in the last 730 days).
# fa_index_dt is less than index_date (this makes sure it's before the index_date).
# filter(fa_index_dt >= (index_date - 730) & fa_index_dt < index_date)

fracture_table_j <- fracture_table_h %>% 
  mutate(history_of_fx = fa_index_dt >= (index_date - 730) & fa_index_dt < index_date) %>%
  filter(history_of_fx == FALSE)

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



