# ****************************************************************************** #
#	Project name: Validation Study			    	
# Script: Cleaning Cohort; clean the data grouped by subject_id based on 
# the remaining conditions in the table
# ****************************************************************************** #

# Cleaning Cohort ----

# Starting with trauma_fract_table_g ran from CleaningFractures_Raw.R script.
cohort_table <- fracture_table_e %>% 
  select(-c(fracture_year))

## a) Only female ----

cohort_table_a <- cohort_table %>%
  group_by(patid) %>% 
  filter(!is.na(gender), gender == "F") %>% 
  ungroup()

# Update attrition report
AttritionReportCoh<- AttritionReportFrac_Raw[,1:3] %>% 
  union_all(  
    tibble(
      number_records = cohort_table_a %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_a %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding not female individuals"
    )
  ) 

## b) No fractures ----

cohort_table_b <- cohort_table_a %>% 
  group_by(patid) %>% 
  filter(!is.na(fx_date)) %>% 
  ungroup()

# Update attrition report
AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_b %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_b %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals without fracture records"
    )
  ) 


## c) Excluding individuals who have index date same as obs period end date ----

# Identify patients with index fracture after or on max observation date
patients_to_exclude <- cohort_table_b %>%
  filter(type == "index" & fx_date >= aurum_max_accept_date) %>%
  distinct(patid) %>%
  pull()

# Remove all rows for those patients
cohort_table_c <- cohort_table_b %>%
  filter(!patid %in% patients_to_exclude)

rm(patients_to_exclude)

# Update attrition report
AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_c %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_c %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals with index fracture after end of observation period"
    )
  )

## d) At least 730 days prior obs -----

# Identify patients with index fractures having less than 730 days prior observation
patients_to_exclude <- cohort_table_c %>%
  mutate(days_prior_obs = fx_date - aurum_min_accept_date) %>%
  filter(type == "index" & days_prior_obs < 730) %>%
  distinct(patid) %>%
  pull()

# Remove all rows for those patients
cohort_table_d <- cohort_table_c %>%
  filter(!patid %in% patients_to_exclude)

rm(patients_to_exclude)

# Update attrition report
AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_d %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_d %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals with less than 730 days observation time prior to index date"
    )
  )

## e) Older than 50 -----
# Identify patients with index fractures before age 50
patients_to_exclude <- cohort_table_d %>%
  mutate(age_fracture = lubridate::year(fx_date) - yob) %>%
  filter(type == "index" & age_fracture <= 50) %>%
  distinct(patid) %>%
  pull()

# Remove all rows for those patients
# rm(patients_to_exclude)

cohort_table_e <- cohort_table_d %>%
  {
    if (!is.null(patients_to_exclude) && length(patients_to_exclude) > 0) {
      filter(., !is.na(patid) & !patid %in% patients_to_exclude)
    } else {
      .
    }
  } %>%
  mutate(age_fracture = lubridate::year(fx_date) - yob)
rm(patients_to_exclude)
# Update attrition report
AttritionReportCoh <- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_e %>% tally() %>% pull(),
      number_subjects = cohort_table_e %>% distinct(patid) %>% tally() %>% pull(),
      reason = "Excluding individuals who had not turned 50 before index date"
    )
  )

## f) No records of cancer before or on the index date ----
cancer_df <- all_patients %>% 
  select(patid, cancer_lpon_index_dt) 
# Step 1: Join cancer data and calculate the cancer condition
cohort_with_cancer_flag <- cohort_table_e %>%
  left_join(cancer_df, by = "patid") %>%
  mutate(cancer_year = lubridate::year(cancer_lpon_index_dt)) %>%
  filter(cancer_year > yob | is.na(cancer_year)) %>%
  mutate(
    cancer_flag = (type == "index" & !is.na(cancer_lpon_index_dt) & cancer_lpon_index_dt <= fx_date)
  )

# Step 2: Find patients to exclude (who have cancer before or on index date)
patients_to_exclude <- cohort_with_cancer_flag %>%
  filter(cancer_flag) %>%
  distinct(patid) %>%
  pull(patid)

# Step 3: Exclude all records for those patients
cohort_table_f <- cohort_with_cancer_flag %>%
  filter(!patid %in% patients_to_exclude) %>%
  select(-cancer_flag, -cancer_year, -cancer_lpon_index_dt)  # clean-up columns if you want
rm(patients_to_exclude)

# Update attrition report
AttritionReportCoh <- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_f %>% tally() %>% pull(),
      number_subjects = cohort_table_f %>% distinct(patid) %>% tally() %>% pull(),
      reason = "Excluding individuals with cancer prior or on index date"
    )
  ) 

## g) No records of metabolic bone disease ----

metabolic_df <- all_patients %>% 
  select(patid, paget_lpon_index_dt) 
# Step 1: Join metabolic disease data and flag patients with MBD before or on index date
cohort_with_mbd_flag <- cohort_table_f %>%
  left_join(metabolic_df, by = "patid") %>%
  mutate(
    paget_flag = (type == "index" & !is.na(paget_lpon_index_dt) & paget_lpon_index_dt <= fx_date)
  )

# Step 2: Identify patients to exclude
patients_to_exclude_mbd <- cohort_with_mbd_flag %>%
  filter(paget_flag) %>%
  distinct(patid) %>%
  pull(patid)

# Step 3: Exclude all records of these patients
cohort_table_g <- cohort_with_mbd_flag %>%
  filter(!patid %in% patients_to_exclude_mbd) %>%
  select(-paget_flag, -paget_lpon_index_dt)  # Clean up if you want
rm(patients_to_exclude_mbd)

# Update attrition report
AttritionReportCoh <- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_g %>% tally() %>% pull(),
      number_subjects = cohort_table_g %>% distinct(patid) %>% tally() %>% pull(),
      reason = "Excluding individuals with metabolic bone diseases prior or on index date"
    )
  )

# Bring back cancer and mbd after index to be used as censoring events ----
cohort_table <- cohort_table_g %>%
  dplyr::left_join(all_patients %>% dplyr::select(patid, cancer_fa_index_dt, paget_fa_index_dt),
                   by = "patid")

## Creating follow-up end ------
# Run the functions script
source(here::here( "CPRD Raw Analysis", "SRC", "Functions_Raw.R"))

# create variable indicating 730 days after the index date ----
cohort_table <- cohort_table %>%
  dplyr::mutate(
    two_yrs_after_index = fx_date + 730
  )

# Identify first fracture date after the index date

fa_earliest <- cohort_table %>%
  filter(type == "fa") %>%
  group_by(patid) %>%
  slice_min(order_by = fx_date, n = 1) %>%
  ungroup()%>%
  select(patid, fx_date) %>%
  rename(fracture_after_index = fx_date)

# join index and first after index 
cohort_table <- cohort_table %>%
  left_join(fa_earliest, by = "patid") %>% 
  select(-c(gender, yob, age_fracture,type)) %>% 
  rename(index_date=fx_date)

# Add study end date
cohort_table <- cohort_table %>% 
  mutate(study_end = study_end_date)

## Add in FOLLOWUPEND ----

cohort_table <- addInFollowUpEndGL(cohort_table)

# Remove patients that have less one month contribution ----
cohort_table <- cohort_table %>%
  filter(follow_up_time >= 30.44)

# Update attrition report
AttritionReportCoh <- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table %>% dplyr::distinct(patid) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals with less than one month exposure"
    )
  ) 

# # Sense Check
# (summary_mnth_exclusion <- cohort_table %>%
#   group_by(excluded_flag) %>%
#   summarise(count = n()))
# # 
# # # Count patients to be excluded
# ## Attrition report
# # # Update attrition report


### Finalise attrition ----
AttritionReportCoh <- AttritionReportCoh %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records)))

rio::export(AttritionReportCoh, here::here("CPRD Raw Analysis", "Results", "Final", "Cohort", "AttritionReportCoh_Raw.xlsx"))

cohort_table_raw_ids <- as.data.frame(cohort_table %>% dplyr::pull(patid))
# end of 02-CleaningCohort_Raw_new.R
saveRDS(cohort_table, here::here("CPRD Raw Analysis", "Results", "Final", "Cohort", "cohort_table_raw.rds"))
