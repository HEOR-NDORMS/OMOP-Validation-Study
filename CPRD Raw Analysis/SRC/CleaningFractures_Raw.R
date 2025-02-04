# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Cleaning Fractures	
# Date: 24.12.2024
# Last date edited: 09.01.2025
# ****************************************************************************** #

# Notes From Antonella:

# From a population of 598,211 female individuals, 156,732 had at least one fx.
# Of these, 31,217 had their 1st fx >= 01/04/2012 and <= 31/03/2018 when they were > 50 years old. This is the number of patients in the cohort I prepared.
# 
# Please note the following:
#   1.	Among these 31,217 patients, 1,504 have index_date < aurum_min_accept_date
# According to the documentation for regstartdate = Registration start date” -> 
# “The date that the patient registered with the CPRD contributing practice.
# Most recent date the patient is recorded as having registered at the practice. 
# If a patient deregistered for a period of time and returned, the return date would be recorded.”
# For the same reason there is 1 patient with aurum_max_accept_date < aurum_min_accept_date
# Considering this peculiarity of Aurum, where we cannot choose between the First registration date and Current registration date (we can do that in GOLD and we always choose the First), 
# I would ignore the “aurum_min_accept_date” which is based only on regstartdate.
# 
# 2.	The data are available in 5 read-only tables as follows:
#   Database = “cdm_aurum_p22_002498_full”
# Schema = “validation”
# Table 1 “_validation” contains all the requested variables for the patients in the cohort
# Table 2 “validation_consultation” contains the requested fields from aurum.consultation for the patients in the cohort
# Table 3 “validation_drugissue” contains the requested fields from aurum.drugissue for the patients in the cohort
# Table 4 “validation_observation” contains the requested fields from aurum.observation for the patients in the cohort
# Table 5 “validation_staff” contains the requested fields from aurum.staff
# 
# 3.	The variable names contain the following patterns and abbreviations:
#   _lpon_ = last prior or on
# _fa_ = first after
# _faon = first after or on
# index = index_date
# mi = myocardial infarction
# ra = rheumatoid arthritis
# 
# 4.	Your analyses should be saved in the “results” schema, as for the rest of the project

# Load packages ----

library(tidyverse)
library(tidylog)
library(tidyr)

# Collect the table called “_validation” ----
all_patients <- my_dm$`_validation` %>%  collect()

### Start and end dates ----
study_start_date <- as.Date("2010-04-01")
study_end_date <- as.Date("2018-03-31")

# consulations <- my_dm$`validation_consultation` %>%  collect()
# 
# observations <- my_dm$`validation_observation` %>%  collect()
# 
# drug_issue <- my_dm$validation_drugissue %>%  collect()
# 
# staff <- my_dm$validation_staff %>%  collect()

# Data Cleaning ----

# Check the structure of the data
str(all_patients)

# Number of patients in all_patients
n_distinct(all_patients$patid)

# Dimension of the data
nrow(all_patients)
ncol(all_patients)

## Long format ----
# this data is in wide data, lets start by changing into long format based on 
# fracture site and associated date

# In wide format, each patient spans a single row, but with many columns. 
# Counting rows or aggregating data by condition can become cumbersome.
# Example: Counting patients with any _index = 1 across conditions requires a 
# complex operation like row-wise calculations, which is inefficient.

### Select columns containing _fa_ ----
fa_df <- all_patients %>% 
  select(patid, contains("_fa_")) 

# Pivot data to long format for `_index` columns
long_fa_df <- fa_df %>%
  pivot_longer(
    cols = -patid, # Keep `patid` as is
    names_to = c("condition", ".value"), # Split column names into `condition` and `_index` or `_dt`
    names_sep = "_fa_" # Use `_fa_` as the separator
  ) %>% 
  rename("fa_index" = "index",
         "fa_index_dt" = "index_dt",
         "fa_condition" =  "condition")

# The remaining columns so that we can combine the two dfs together
other_cols_df <- all_patients %>% 
  select(patid, gender, yob, death, death_date, aurum_min_accept_date, 
         aurum_max_accept_date, index_date, index_hip_fx, index_vert_fx,
         index_nhnv_fx, trauma_faon_index, trauma_faon_index_dt)

### Fracture table ----
# keep only fracture as the events
fracture_table <- full_join(
  other_cols_df, long_fa_df, by = "patid"
) %>% 
  # Remove conditions that aren't fracture
  filter(fa_condition %in% c("hip_fx", "vert_fx", "nhnv_fx"))

### De-duplication ----
# just check for duplicates in case of merging
fracture_table.1 <- fracture_table %>% 
  distinct()
# no rows removed, noice!

# a) Remove fractures before the birth year ----

fracture_table_a <- fracture_table %>% 
  dplyr::mutate(fracture_year_index = year(index_date)) %>% 
  dplyr::mutate(fracture_year_fa_index = year(fa_index_dt)) %>%
  dplyr::filter(fracture_year_index > yob) %>%
  dplyr::filter(fracture_year_fa_index > yob) 

# count of patients
n_distinct(fracture_table_a$patid)

# count of fractures
dim(fracture_table_a)

trauma_fract_table_a <- fracture_table_a %>%
  dplyr::mutate(trauma_year_faon = year(trauma_faon_index_dt)) %>%
  dplyr::filter(trauma_year_faon > yob)

# count of patients
n_distinct(trauma_fract_table_a$patid)

# count of trauma
dim(trauma_fract_table_a)

# b) Remove fracture outside of study period ----
fracture_table_b <- trauma_fract_table_a %>% 
  dplyr::filter(index_date >= study_start_date & index_date <= study_end_date) %>% 
  dplyr::filter(fa_index_dt >= study_start_date & fa_index_dt <= study_end_date)

# count of patients
n_distinct(fracture_table_b$patid)

# count of fractures
dim(fracture_table_b)

trauma_fract_table_b <- fracture_table_b %>%
  dplyr::filter(trauma_faon_index_dt >= study_start_date & trauma_faon_index_dt <= study_end_date)

# count of patients
n_distinct(trauma_fract_table_b$patid)

# count of trauma
dim(trauma_fract_table_b)

# c) Remove fractures on the same day as trauma codes ----
fracture_table_c <- trauma_fract_table_b %>% 
  dplyr::filter(index_date != trauma_faon_index_dt) %>% 
  dplyr::filter(fa_index_dt != trauma_faon_index_dt)

# count of patients
n_distinct(fracture_table_c$patid)

# count of fractures
dim(fracture_table_c)

# d) Remove fractures on the same day as death or after ----
fracture_table_d <- fracture_table_c %>% 
  group_by(patid) %>% 
  dplyr::filter(index_date < death_date) %>% 
  dplyr::filter(fa_index_dt < death_date) %>% 
  ungroup()

# count of patients
n_distinct(fracture_table_d$patid)

# count of fractures
dim(fracture_table_d)

trauma_fract_table_d <- fracture_table_d %>%
  group_by(patid) %>% 
  dplyr::filter(trauma_faon_index_dt < death_date) %>% 
  ungroup()

# count of patients
n_distinct(trauma_fract_table_d$patid)

# count of trauma
dim(trauma_fract_table_d)

# e) Remove fractures on the last day of observation period ("aurum_max_accept_date") ----
fracture_table_e <- trauma_fract_table_d %>% 
  group_by(patid) %>% 
  dplyr::filter(index_date != aurum_max_accept_date) %>% 
  dplyr:: filter(fa_index_dt != aurum_max_accept_date) %>% 
  ungroup()

# count of patients
n_distinct(fracture_table_e$patid)

# count of fractures
dim(fracture_table_e)

trauma_fract_table_e <- fracture_table_e %>%
  group_by(patid) %>% 
  dplyr::filter(trauma_faon_index_dt != aurum_max_accept_date) %>% 
  ungroup()

# count of patients
n_distinct(trauma_fract_table_e$patid)

# count of trauma
dim(trauma_fract_table_e)

# f) Remove fractures after the last day of observation period ("aurum_max_accept_date") ----

fracture_table_f <- trauma_fract_table_e %>% 
  group_by(patid) %>% 
  dplyr::filter(index_date < aurum_max_accept_date) %>% 
  dplyr:: filter(fa_index_dt < aurum_max_accept_date) %>% 
  ungroup()

# count of patients
n_distinct(fracture_table_f$patid)

# count of fractures
dim(fracture_table_f)

trauma_fract_table_f <- fracture_table_f %>%
  group_by(patid) %>% 
  dplyr::filter(trauma_faon_index_dt < aurum_max_accept_date) %>% 
  ungroup()

# count of patients
n_distinct(trauma_fract_table_f$patid)

# count of trauma
dim(trauma_fract_table_f)
