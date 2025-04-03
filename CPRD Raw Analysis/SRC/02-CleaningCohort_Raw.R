# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Cleaning Cohort; clean the data grouped by subject_id based on 
# the remaining conditions in the table
# Date: 13.01.2025
# Last date edited: 17.03.2025
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
# Even though we had index defined by Antonella, since we have cleaned out some fractures (e.g., trauma and others), 
# we now have to redefined index because some patients don't have the one originally assigned anymore.
# For example if index was deleted because of being on the same date as trauma, then the fa fracture now becomes index.
# So, following the same logic as in the omop code, we now work here with just a dataframe containing just index and then we join back with the other fractures 
cohort_table_b_index <- cohort_table_b %>%
  dplyr::group_by(patid) %>% 
  dplyr::slice_min(fx_date) %>%  # Keep only the earliest fx_date per subject
  dplyr::ungroup() 


### c) Excluding individuals who have index date same as obs period end date ----
cohort_table_c_index <- cohort_table_b_index %>% 
  group_by(patid) %>% 
  filter(!(fx_date == aurum_min_accept_date)) %>% 
  ungroup()

AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_c_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_c_index %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding index fractures that happen on the last day of observation period"
    )
  )

### d) At least 730 days prior obs -----
# check if a fx_date where falls within the 730 days before another date (index_date).
# fa_index_dt is greater than or equal to index_date - 730 (this means it happened in the last 730 days).
# fa_index_dt is less than index_date (this makes sure it's before the index_date).
# filter(fa_index_dt >= (index_date - 730) & fa_index_dt < index_date)

cohort_table_d_index <- cohort_table_c_index %>% 
  mutate(days_prior_obs = fx_date - aurum_min_accept_date, days_after_obs = aurum_max_accept_date - fx_date) %>%
  filter(days_prior_obs >= prior_observation, days_after_obs >= 0) 

AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_d_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_d_index %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records with insufficient prior observation and/or outside of observation period"
    )
  )


### e) at least 50 -----
cohort_table_e_index <- cohort_table_d_index %>% 
  mutate(age_fracture = lubridate::year(fx_date) - yob) %>%
  filter(age_fracture >= 50)

AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_e_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_e_index %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records of index fractures where the subject has not turned 50"
    )
  )


### f) No records of cancer before or on the index date ----

# cancer columns
cancer_df <- all_patients %>% 
  select(patid, cancer_lpon_index_dt) 

# Pivot data to long format for `_index` columns
# long_cancer_df <- cancer_df %>%
#   pivot_longer(
#     cols = -patid,
#     names_to = c("condition", "condition_date_type", "drop"), # "drop" captures `_index_dt`
#     names_pattern = "(cancer)_(lpon|fa)_(index_dt)"
#   ) %>%
#   select(-drop) %>%  # Remove the extra "drop" column
#   rename(cancer_date = value) %>%   # Rename to `condition_date`
#   filter(condition_date_type != "lpon")

# merge with the fx df
cohort_table_e.1 <- left_join(cohort_table_e_index, cancer_df, by = "patid") 

# remove cancer before yob
cohort_table_e.1 <- cohort_table_e.1 %>% 
  group_by(patid) %>% 
  mutate(cancer_year = lubridate::year(cancer_lpon_index_dt)) %>%
  filter(cancer_year > yob | is.na(cancer_year)) %>% 
  ungroup()

# remove subjects who had cancer before or on index date.
cohort_table_f_index <-  cohort_table_e.1 %>% 
  group_by(patid) %>% 
  filter(is.na(cancer_lpon_index_dt)) %>% 
  ungroup() %>% 
  select(!cancer_year)

AttritionReportCoh <- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_f_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_f_index %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records happened after cancer"
    )
  ) 

### g) No records of metabolic bone disease ----
metabolic_df <- all_patients %>% 
  select(patid, paget_lpon_index_dt) 

# Pivot data to long format for `_index` columns
# long_metabolic_df <- metabolic_df %>%
#   pivot_longer(
#     cols = -patid, 
#     names_to = c("condition", "condition_date_type", "drop"), # "drop" captures `_index_dt`
#     names_pattern = "(paget)_(lpon|fa)_(index_dt)"
#   ) %>%
#   select(-drop) %>%  # Remove the extra "drop" column
#   rename(paget_date = value)  # Rename to `condition_date`

# merge with the fx df
cohort_table_f.1 <- left_join(cohort_table_f_index, metabolic_df, by = "patid") 

# remove subjects who had mbd before or on index date.
cohort_table_g_index <- cohort_table_f.1 %>%
  group_by(patid) %>% 
  filter(is.na(paget_lpon_index_dt)) %>%
  ungroup() 


AttritionReportCoh <- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_g_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_g_index %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records happened after metabolic bone diseases"
    )
  ) 


### Create unique dataset with both index and non index fractures ----
cohort_table_g_index <-cohort_table_g_index%>% 
  dplyr::mutate(class = "index") # create a variable to represent the new index (remember some that prviously where fa are now index)

# fracture_table_rq2 <- fracture_table
cohort_table_g_ids <- cohort_table_g_index %>% dplyr::pull(patid) 

cohort_table <- cohort_table %>%
  dplyr::left_join(cohort_table_g_index, by = colnames(cohort_table)) %>% 
  dplyr::filter(patid %in% cohort_table_g_ids) %>% 
  dplyr::mutate(
    class = ifelse(is.na(class) | class == "", type, class)
  ) %>% 
  dplyr::select(-type, -cancer_lpon_index_dt, -paget_lpon_index_dt) %>% 
  dplyr::rename(type = class)

# bring back cancer and mbd after index to be used as censoring events

cohort_table <- cohort_table %>% 
  dplyr::left_join(all_patients %>% dplyr::select(patid, cancer_fa_index_dt, paget_fa_index_dt), 
                   by = "patid")

### Creating follow-up end ------
# Run the functions script
source(here::here("CPRD Raw Analysis", "SRC", "Functions_Raw.R"))

# Create index date 
cohort_table <- addIndex(cohort_table)
# 730 days after the index date ----
cohort_table <- addInTwoYearsAfter(cohort_table)
# Add in first fracture date after the index dates
cohort_table <- addInNextFracture(cohort_table)
# Add in FOLLOWUPEND ----
cohort_table <- addInFollowUpEndGL(cohort_table)

# FINAL: Keep only one row per patient as we have fu end and info

cohort_table <- cohort_table %>% 
  dplyr:: filter(type=="index")

# Remove patients that have less one month contribution ----
cohort_table <- cohort_table %>% 
  mutate(exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days"))) %>% 
  mutate(
    excluded_flag = if_else(exposed_days < 30.74, "Exclude", "Include")
    ) %>% 
  filter(excluded_flag == "Include")

# Sense Check
(summary_mnth_exclusion <- cohort_table %>% 
  group_by(excluded_flag) %>% 
  summarise(count = n()))

# Count patients to be excluded
excluded_patients <- cohort_table %>%
  filter(excluded_flag == "Exclude") %>%
  distinct(patid) %>%
  tally() %>%
  pull()

## Attrition report
# Update attrition report
AttritionReportCoh <- AttritionReportCoh %>%
  dplyr::union_all(
    tibble(
      number_records = cohort_table %>% 
        filter(excluded_flag == "Include") %>%
        nrow(),
      number_subjects = cohort_table %>%
        filter(excluded_flag == "Include") %>%
        distinct(patid) %>%
        tally() %>%
        pull(),
      reason = "Exclude patients with less than one month of exposure"
    )
  )


### Finalise attrition ----
AttritionReportCoh <- AttritionReportCoh %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records)))

# rio::export(AttritionReportCoh, here::here("CPRD Raw Analysis", "Results", "AttritionReportCoh_Raw.xlsx"))

cohort_table_raw_ids <- as.data.frame(cohort_table %>% dplyr::pull(patid))
#rio::export(cohort_table_raw_ids, here::here("CPRD Raw Analysis", "Results", "cohort_table_raw_ids.csv"))
#rio::export(AttritionReportCoh, here::here("CPRD Raw Analysis", "Results", "AttritionReportCoh_OMOP.xlsx"))