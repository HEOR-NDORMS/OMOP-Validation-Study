cohort_table <- fracture_table_g

### a) Not Female ----

cohort_table_a <- cohort_table %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr:: group_by(subject_id) %>% 
  dplyr:: filter(!is.na(gender_source_value), gender_source_value == "F") %>%
  dplyr::ungroup() %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportCoh<- AttritionReportFrac[,1:3] %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_a %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_a %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding not female individuals"
    )
  ) 

### b) No fractures ----

cohort_table_b <- cohort_table_a %>% 
  dplyr:: group_by(subject_id) %>% 
  dplyr:: filter(!is.na(condition_start_date)) %>%
  dplyr::ungroup()
                   
AttritionReportCoh<- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_b %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_b %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals without fracture records"
    )
  )

### Define index date ----

# cohort_table_b_index <- cohort_table_b %>%
#   dplyr::group_by(subject_id) %>%
#   dplyr::arrange(condition_start_date, .by_group = T) %>%
#   dplyr::mutate(gap = condition_start_date - lag(condition_start_date)) %>%
#   dplyr::filter(gap>730|is.na(gap)) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-gap)

# Defined Index as the first fracture (ever) -----

cohort_table_b_index <- cohort_table_b %>%
  dplyr::group_by(subject_id) %>% 
  dplyr::slice_min(condition_start_date) %>%  # Keep only the earliest condition_start_date per subject
  dplyr::ungroup()  

                    
### c) Excluding individuals who have index date same as obs period end date ----
cohort_table_c_index <- cohort_table_b_index %>%  
dplyr::group_by(subject_id) %>%
  dplyr::arrange(condition_start_date, .by_group = T) %>%
  dplyr::mutate(gap = condition_start_date - dplyr::lag(condition_start_date)) %>%
  dplyr::filter(gap>730|is.na(gap)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-gap)

### c) Excluding individuals who has index date same as obs period end date ----

cohort_table_c_index <- cohort_table_c_index %>%
  dplyr::anti_join(cdm[["observation_period"]], by = c("subject_id" = "person_id", "condition_start_date" = "observation_period_end_date"), copy = T)

AttritionReportCoh<- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_c_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_c_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding index fractures that happen on the last day of observation period"
    )
  ) 

### d) At least 730 days prior obs -----
cohort_table_d_index <-cohort_table_c_index %>% 
  dplyr::left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  dplyr::select(subject_id:fracture_site, observation_period_start_date, observation_period_end_date) %>%
  dplyr::mutate(days_prior_obs = condition_start_date - observation_period_start_date, days_after_obs = observation_period_end_date - condition_start_date) %>%
  dplyr::filter(days_prior_obs >= prior_observation, days_after_obs >= 0) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportCoh<- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_d_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_d_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records with insufficient prior observation and/or outside of observation period"
    )
  ) 


### e) at least 50 ----
cohort_table_e_index <- cohort_table_d_index %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(age_fracture = lubridate::year(condition_start_date) - year_of_birth) %>%
  dplyr::filter(age_fracture >= 50) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportCoh<- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_e_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_e_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records of index fractures where the subject has not turned 50"
    )
  )


### f) No records of cancer before or on the index date ----

# Loading exclusion criteria tables
cancer <- read_excel(paste0(here(), "/OMOP Analysis/cancer_codes.xlsx"))
cancer_codes <- cancer %>% dplyr::select(Id) %>% dplyr::pull()

cdm[["cancer"]] <- cdm[["denominator"]] %>% # remember to revert to condition_occurrence if you want to have both primary and secondary
  dplyr::left_join(cdm[["condition_occurrence_primary"]], by = c("subject_id" = "person_id")) %>%
  dplyr::filter(condition_concept_id %in% cancer_codes) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  dplyr::mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  dplyr::mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  dplyr::rename(cancer_date =condition_start_date) 

# Removing cancer records before the birth year
cdm[["cancer"]] <- cdm[["cancer"]] %>%
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(cancer_year = lubridate::year(cancer_date)) %>%
  dplyr::filter(cancer_year > year_of_birth) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, cancer_date) 


cohort_table_f_index <- 
  cohort_table_e_index %>% dplyr::anti_join(cohort_table_e_index %>% 
                                           dplyr::inner_join(cdm[["cancer"]] %>% dplyr::select(subject_id, cancer_date), 
                                                             by = "subject_id", copy = T, relationship = "many-to-many") %>%
                                           dplyr::filter(cancer_date<=condition_start_date) %>%
                                           dplyr::distinct() %>%
                                           dplyr::compute(), by = colnames(cohort_table_e_index)) 

AttritionReportCoh<- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_f_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_f_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records happened after cancer"
    )
  ) 

### g) No records of metabolic bone disease ----

mbd <- read_excel(paste0(here(), "/OMOP Analysis/mbd_codes.xlsx"))
mbd_codes <- mbd %>% dplyr::select(Id) %>% dplyr::pull()

cdm[["mbd"]] <- cdm[["denominator"]] %>% # # remember to revert to condition_occurrence if you want to have both primary and secondary
  dplyr::left_join(cdm[["condition_occurrence_primary"]], by = c("subject_id" = "person_id")) %>%
  dplyr::filter(condition_concept_id %in% mbd_codes) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  dplyr::mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  dplyr::mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  dplyr::rename(mbd_date =condition_start_date) 

cohort_table_g_index <- 
  cohort_table_f_index %>% dplyr::anti_join(cohort_table_f_index %>% 
                                           dplyr::inner_join(cdm[["mbd"]] %>% dplyr::select(subject_id, mbd_date), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                                           dplyr::filter(mbd_date<=condition_start_date) %>%
                                           dplyr::distinct() %>%
                                           dplyr::compute(), by = colnames(cohort_table_f_index)) %>% 
  dplyr::mutate(class = "index") # create a variable called class that is used later in the addIndex function

AttritionReportCoh<- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_g_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_g_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records happened after metabolic bone diseases"
    )
  )  



## Create unique dataset with both index and non index fractures -----

# fracture_table_rq2 <- fracture_table
cohort_table_g_index_ids <- cohort_table_g_index %>% dplyr::pull(subject_id)

cohort_table <- cohort_table %>%
  dplyr::left_join(cohort_table_g_index, by = colnames(cohort_table)) %>%
  dplyr::filter(subject_id %in% cohort_table_g_index_ids)

# Run the functions script
source(here::here("OMOP Analysis", "SRC", "Functions_OMOP.R"))

cohort_table <- addIndex(cohort_table)

### Creating follow-up end ------
# 730 days after the index date
cohort_table <- addInTwoYearsAfter(cohort_table)

# End of data collection (assuming each person has only one observation period)
cohort_table <- addInObsEndDate(cohort_table)

# Adding first cancer date after the index date
cohort_table <- addInCancerPostIndex(cohort_table)

# Adding first bone disease date after the index date
cohort_table <- addInBoneDiseasePostIndex(cohort_table)

# Add in first fracture date after the index dates
cohort_table <- addInNextFracture(cohort_table)

# Add in death date after the index date 
cohort_table <- addInDeath(cohort_table)

# Add in FOLLOWUPEND
#cohort_table <- addInFollowUpEnd(cohort_table)

# Add in FOLLOWUPENDGL - with reason for FU (censoring)
cohort_table <- addInFollowUpEndGL(cohort_table)

# FINAL: Keep only one row per patient as we have fu end and info

cohort_table <- cohort_table %>% 
  dplyr:: filter(class=="index")

# Remove patients that have less one month contribution
cohort_table <- cohort_table %>% 
  mutate(exposed_yrs = as.numeric(follow_up_end - index_date) / 360.25,
         exposed_months = exposed_yrs * 12,  # Convert years to months
         excluded_flag = if_else(exposed_months < 1, "Excluded", "Included"))
  

(summary_mnth_exclusion <- cohort_table %>% 
    group_by(excluded_flag) %>% 
    summarise(count = n()))

# Count patients to be excluded
excluded_patients <- cohort_table %>%
  filter(excluded_flag == "Excluded") %>%
  distinct(subject_id) %>%
  tally() %>%
  pull()

## Attrition report
# Update attrition report
AttritionReportCoh <- AttritionReportCoh %>%
  dplyr::union_all(
    tibble(
      number_records = cohort_table %>% 
        filter(excluded_flag == "Included") %>%
        nrow(),
      number_subjects = cohort_table %>%
        filter(excluded_flag == "Included") %>%
        distinct(subject_id) %>%
        tally() %>%
        pull(),
      reason = "Exclude patients with less than one month of exposure"
    )
  )


### Finalise attrition
AttritionReportCoh <- AttritionReportCoh %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-dplyr::lag(number_subjects)), records_excluded = -(number_records - dplyr::lag(number_records)))


cohort_table <- cohort_table %>% 
  filter(excluded_flag == "Included")


# rio::export(AttritionReportCoh, here::here("OMOP Analysis", "Results", "AttritionReportCoh_OMOP.xlsx"))
