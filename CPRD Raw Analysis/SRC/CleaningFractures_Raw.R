# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Cleaning Fractures	
# Date: 24.12.2024
# Last date edited: 21.01.2025
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

## Remove columns not associated with fracture

all_patients.1 <- all_patients %>% 
  select(-c(contains("cardiovascular_lpon"), contains("heart_failure_lpon"), contains("mi_"),
            contains("osteoporosis_"), contains("ra_"), contains("cancer_"), contains("paget_"))) %>% 
  rename("index_dt" = "index_date") 

### Reshaped the Data for Indexes: ----
# I transformed the index_hip_fx, index_vert_fx, and index_nhnv_fx columns into a long format, creating rows for each site where the patient has an index fracture.
# Each of these rows gets a Type of "index" and the value is set to the index_dt.

reshaped_fracture_table <- all_patients.1 %>%
  pivot_longer(cols = c(index_hip_fx, index_vert_fx, index_nhnv_fx),
               names_to = "site_type", values_to = "index_flag") %>%
  mutate(site = case_when(
    site_type == "index_hip_fx" ~ "hip",
    site_type == "index_vert_fx" ~ "vert",
    site_type == "index_nhnv_fx" ~ "nhnv"
  ),
  Type = "index",
  value = index_dt) %>%
  select(patid, gender, yob, death, death_date, aurum_min_accept_date, aurum_max_accept_date, site, Type, value,index_flag)


### Added Rows for Fracture and Trauma Dates: ----
# I transformed the hip_fx_fa_index_dt, vert_fx_fa_index_dt, nhnv_fx_fa_index_dt, and trauma_faon_index_dt columns into a long format, creating rows for each site where the patient has a fracture or trauma date.
# Each of these rows gets a Type of "fa" for fractures and "0" for trauma, with the value set to the corresponding date.

additional_fracture_table <- all_patients.1 %>%
  pivot_longer(cols = c(hip_fx_fa_index_dt, vert_fx_fa_index_dt, nhnv_fx_fa_index_dt, trauma_faon_index_dt),
               names_to = "site_type", values_to = "value") %>%
  mutate(site = case_when(
    site_type == "hip_fx_fa_index_dt" ~ "hip",
    site_type == "vert_fx_fa_index_dt" ~ "vert",
    site_type == "nhnv_fx_fa_index_dt" ~ "nhnv",
    site_type == "trauma_faon_index_dt" ~ "0"
  ),
  Type = case_when(
    site_type == "trauma_faon_index_dt" ~ "trauma",
    TRUE ~ "fa"
  )) %>%
  select(patid, gender, yob, death, death_date, aurum_min_accept_date, aurum_max_accept_date, site, Type, value) %>% 
  filter(!is.na(value))

## Combine the reshaped data with the additional rows ----
fracture_data <- bind_rows(reshaped_fracture_table, additional_fracture_table)


final_fracture_data <- fracture_data %>% 
  filter(index_flag != 0 | is.na(index_flag)) %>% 
  select(-index_flag) %>% 
  rename(
     "type" = "Type",
     "fx_date" = "value"
  )

n_distinct(final_fracture_data$patid)
names(final_fracture_data)


### De-duplication ----
# just check for duplicates in case of merging
final_fracture_data.1 <- final_fracture_data %>% 
  distinct()
# no rows removed, noice!

rm(final_fracture_data.1)

# Attrition table

AttritionReportFrac_Raw <- tibble(
  cohort_definition_id = as.integer(1),
  number_records = final_fracture_data %>% dplyr::tally() %>% dplyr::pull(),
  number_subjects = final_fracture_data %>% distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
  reason = "Starting Population - Anyone With Fracture(s)"
)

# a) Remove fractures before the birth year ----

fracture_table_a <- final_fracture_data %>% 
  mutate(fracture_year = year(fx_date)) %>% 
  filter(fracture_year > yob)  

AttritionReportFrac_Raw <- AttritionReportFrac_Raw %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_a %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_a %>% distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding fracture happening before the birth year"
    )
  )

# b) Remove fracture outside of study period ----
fracture_table_b <- fracture_table_a %>% 
  filter(fx_date >= study_start_date & fx_date <= study_end_date)
  
AttritionReportFrac_Raw <- AttritionReportFrac_Raw %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_b %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_b %>% distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records that happen outside of study period"
    )
  )

# c) Remove fracture outside of observation period ----
fracture_table_c <- fracture_table_b %>% 
  filter(fx_date >= aurum_min_accept_date & fx_date <= aurum_max_accept_date)

AttritionReportFrac_Raw <- AttritionReportFrac_Raw %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_c %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_c %>% distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records that happen outside of observation period"
    )
  )

# d) Remove fractures on the same day as trauma codes or 3 distinct fracture sites on same day ----
# Remove fa and index if trauma exists on the same date
fracture_table_d <- fracture_table_c %>%
  group_by(patid, fx_date) %>% 
  mutate(trauma_present = any(site == 0)) %>% 
  filter(!(type %in% c("fa", "index") & trauma_present)) %>%  
  ungroup()

# only_index_d <- fracture_table_c %>%
#   group_by(patid, fx_date) %>% 
#   mutate(trauma_present = any(site == 0)) %>% 
#   filter((type %in% c("fa", "index") & trauma_present)) %>%  # Remove fa and index if trauma exists on the same date
#   ungroup() 
# # 1158 patients (with 1293 fx) will index date same date as the trauma
# n_distinct(fracture_table_d$patid)

# fracture_table_d.1 <- fracture_table_d %>%
#   group_by(patid, fx_date) %>% 
#   summarise(number_sites = n_distinct(site), .groups = "drop") %>% 
#   filter(number_sites == 3) %>% 
#   ungroup()

# Remove 3 distinct fracture sites on same day
fracture_table_d <- fracture_table_d %>% 
  inner_join(
    fracture_table_d %>%
      group_by(patid, fx_date) %>% 
      summarise(number_sites = n_distinct(site), .groups = "drop") %>% 
      filter(number_sites != 3) %>% 
      ungroup(),
    by = c("patid", "fx_date")
  )

AttritionReportFrac_Raw <- AttritionReportFrac_Raw %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_d %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_d %>% distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding trauma fractures (same day as trauma codes or 3 distinct fracture sites on the same date"
    )
  )

# e) Remove fractures on the same day as death or after ----
fracture_table_e <- fracture_table_d %>% 
  group_by(patid) %>% 
  filter(is.na(death_date)| death_date > fx_date) %>%
  ungroup()

AttritionReportFrac_Raw <- AttritionReportFrac_Raw %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_e %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_e %>% distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding fractures on the same day as death or after death"
    )
  )

# f) Washout ----
# Define the fracture sites
sites <- c("hip", "vert", "nhnv", "0")

# Initialise the backup table and the result container
fracture_table_back_up <- fracture_table_e
index_fractures <- tibble()

suppressWarnings(
  while (nrow(fracture_table_back_up) > 0) {
    fracture_correction <- list()
    
    # Process each site independently
    for (site in sites) {
      fracture_correction[[site]] <- fracture_table_back_up %>% 
        dplyr::filter(site == site) %>%  # Filter fractures for the current site
        dplyr::group_by(patid) %>% 
        dplyr::summarise(min_date = min(fx_date, na.rm = TRUE)) %>% 
        dplyr::mutate(site = site)
    }
    
    # Combine corrections for all sites
    fracture_correction_combined <- bind_rows(fracture_correction)
    
    # Update the fracture table with the washout logic
    fracture_table_back_up <- fracture_table_back_up %>% 
      dplyr::left_join(fracture_correction_combined, by = c("patid", "site")) %>% 
      dplyr::mutate(gap_to_min_date = fx_date - min_date) %>% 
      dplyr::filter(gap_to_min_date == 0 | gap_to_min_date > 90) %>% 
      dplyr::group_by(patid, fx_date, site) %>% 
      dplyr::arrange(patid, .by_group = TRUE) %>%
      dplyr::filter(row_number() == 1) %>% 
      dplyr::ungroup()
    
    # Append index fractures (those that meet the criteria)
    index_fractures <- bind_rows(
      index_fractures,
      fracture_table_back_up %>% 
        dplyr::filter(gap_to_min_date == 0) %>%
        dplyr::select(-min_date, -gap_to_min_date)
    )
    
    # Prepare the remaining fractures for the next loop
    fracture_table_back_up <- fracture_table_back_up %>% 
      dplyr::filter(!gap_to_min_date == 0) %>%
      dplyr::select(-min_date, -gap_to_min_date)
  }
)

# Final output: index fractures
fracture_table_f <- index_fractures

AttritionReportFrac_Raw <- AttritionReportFrac_Raw %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_f %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_f %>% distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Clean out using washout period"
    )
  ) 
rm(index_fractures)

# g) Applying Hierarchy (hip,vertebra,NHNV) to multiple records on the same day ----
fracture_table_f$site <- factor(fracture_table_f$site, levels = sites)

fracture_table_g <- fracture_table_f %>%
  group_by(patid, fx_date) %>%
  arrange(site, .by_group = TRUE) %>% 
  filter(row_number()==1) %>%
  ungroup()

AttritionReportFrac_Raw <- AttritionReportFrac_Raw %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_g %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_g %>% distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records on the same day for the same person according to hierarchy (Hip, Spine, NHNS)"
    )
  )


# Finalise attrition -----
AttritionReportFrac_Raw <- AttritionReportFrac_Raw %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records))) %>% 
  dplyr::select(-c("cohort_definition_id"))

# rio::export(AttritionReportFrac_Raw, here::here("Results", "AttritionReportFrac_Raw.xlsx"))
