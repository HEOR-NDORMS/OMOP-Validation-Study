# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki, Gianluca Fabiano	
# Do file name: Cleaning Cohort; clean the data grouped by subject_id based on 
# the remaining conditions in the table
# Date: 13.01.2025
# Last date edited: 19.08.2025
# ****************************************************************************** #

cohort_table <- fracture_table_e

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
  dplyr::mutate(
    type = ifelse(condition_start_date == min(condition_start_date), "index", NA_character_)
  ) %>%
  dplyr::ungroup()

### c) Excluding individuals who have index date same as obs period end date ----

# Identify patients with index fracture after or on max observation date
patients_to_exclude <- cohort_table_b_index %>%
  filter(type == "index" & condition_start_date >= cohort_end_date) %>%
  distinct(subject_id ) %>%
  pull()

# Remove all rows for those patients
cohort_table_c_index <- cohort_table_b_index %>%
  filter(!subject_id  %in% patients_to_exclude)

rm(patients_to_exclude)

AttritionReportCoh<- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_c_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_c_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals with index fracture after end of observation period"
    )
  ) 

### d) At least 730 days prior obs -----
# patients_to_exclude <-cohort_table_c_index %>% 
#   dplyr::left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>% 
#   dplyr::select(subject_id:fracture_site, observation_period_start_date, observation_period_end_date, type) %>%
#   dplyr::mutate(days_prior_obs = condition_start_date - observation_period_start_date) %>%
#   dplyr::filter(type == "index" & days_prior_obs >= prior_observation) %>%
#   dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

# test
patients_to_exclude <- cohort_table_c_index %>%
  mutate(days_prior_obs = condition_start_date - cohort_start_date) %>%
  filter(type == "index" & days_prior_obs < 730) %>%
  distinct(subject_id) %>%
  pull()
# Remove all rows for those patients
cohort_table_d_index <-cohort_table_c_index%>%
  filter(!subject_id %in% patients_to_exclude)
rm(patients_to_exclude)


AttritionReportCoh<- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_d_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_d_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals with less than 730 days observation time prior to index date"
    )
  ) 

### e) Older than 50 ----
patients_to_exclude <- cohort_table_d_index %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(age_fracture = lubridate::year(condition_start_date) - year_of_birth) %>%
  dplyr::filter(type == "index" & age_fracture > 50) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)


# Remove all rows for those patients
cohort_table_e_index <-cohort_table_d_index%>%
  filter(!subject_id %in% patients_to_exclude)
rm(patients_to_exclude)

AttritionReportCoh<- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_e_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_e_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals who had not turned 50 before index date"
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
  dplyr::rename(cancer_date =condition_start_date) %>% computeQuery()

# Removing cancer records before the birth year
cdm[["cancer"]] <- cdm[["cancer"]] %>%
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(cancer_year = lubridate::year(cancer_date)) %>%
  dplyr::filter(cancer_year > year_of_birth) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, cancer_date)


cohort_table_f_index <- cohort_table_e_index %>% dplyr::anti_join(cohort_table_e_index %>% 
                                           dplyr::inner_join(cdm[["cancer"]] %>% dplyr::select(subject_id, cancer_date), 
                                                             by = "subject_id", copy = T, relationship = "many-to-many") %>%
                                           dplyr::filter(type == "index" & cancer_date <= condition_start_date) %>%
                                           dplyr::distinct() %>%
                                           dplyr::compute(), by = colnames(cohort_table_e_index)) 
# Remove all rows for those patients
# cohort_table_f_index <-cohort_table_e_index%>%
#   filter(!subject_id %in% patients_to_exclude)
# rm(patients_to_exclude)

AttritionReportCoh<- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_f_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_f_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals with cancer prior or on index date"
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
  dplyr::rename(mbd_date =condition_start_date) %>% computeQuery()

cohort_table_g_index <- 
  cohort_table_f_index %>% dplyr::anti_join(cohort_table_f_index %>% 
                                           dplyr::inner_join(cdm[["mbd"]] %>% dplyr::select(subject_id, mbd_date), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                                           dplyr::filter(type == "index" & mbd_date<=condition_start_date) %>%
                                           dplyr::distinct() %>%
                                           dplyr::compute(), by = colnames(cohort_table_f_index)) 

# Remove all rows for those patients
# cohort_table_g_index <-cohort_table_f_index%>%
#   filter(!subject_id %in% patients_to_exclude)
# rm(patients_to_exclude)

AttritionReportCoh<- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table_g_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_g_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals with metabolic bone diseases prior or on index date"
    )
  )  


## Create unique dataset with both index and non index fractures -----

# fracture_table_rq2 <- fracture_table
# cohort_table_g_index_ids <- cohort_table_g_index %>% dplyr::pull(subject_id)
# 
# cohort_table <- cohort_table %>%
#   dplyr::left_join(cohort_table_g_index, by = colnames(cohort_table)) %>%
#   dplyr::filter(subject_id %in% cohort_table_g_index_ids)
# 

cohort_table<-cohort_table_g_index

# Run the functions script
source(here::here("OMOP Analysis", "SRC", "Functions_OMOP.R"))

### Creating follow-up end ------
cohort_table.1 <- cohort_table %>% filter(type=="index")


# 730 days after the index date
# create variable indicating 730 days after the index date ----
cohort_table.2 <- cohort_table.1 %>%
  dplyr::mutate(
    two_yrs_after_index = condition_start_date + 730
  )

# Adding first cancer date after the index date
cohort_table.3 <-cohort_table.2 %>% dplyr::left_join(cohort_table.2 %>% 
                                     dplyr::inner_join(cdm[["cancer"]] %>% rename(cancer_concept_id = condition_concept_id), 
                                                       by = c("subject_id", "cohort_start_date", "cohort_end_date"), 
                                                       copy = T, 
                                                       relationship = "many-to-many") %>%
                                     dplyr::filter(condition_start_date < cancer_date) %>%   
                                     dplyr::group_by(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, 
                                                     condition_start_date, fracture_site, type, two_yrs_after_index) %>%
                                     dplyr::arrange(cancer_date) %>%
                                     dplyr::filter(row_number()==1) %>%
                                     dplyr::ungroup(), by = c("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", 
                                                              "condition_start_date", "fracture_site",  "type", "two_yrs_after_index")) %>%
  dplyr::select(-cancer_concept_id) 


# Adding first bone disease date after the index date
cohort_table.4 <- cohort_table.3 %>% dplyr::left_join(cohort_table.3 %>% 
                                     dplyr::inner_join(cdm[["mbd"]] %>%
                                                         rename(bone_disease_concept_id = condition_concept_id), 
                                                       by = c("subject_id", "cohort_start_date", "cohort_end_date"),
                                                       copy = T, 
                                                       relationship = "many-to-many") %>%
                                     dplyr::filter(condition_start_date < mbd_date) %>%   
                                     dplyr::group_by(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, type, two_yrs_after_index) %>%
                                     dplyr::arrange(mbd_date) %>%
                                     dplyr::filter(row_number()==1) %>%
                                     dplyr::ungroup(), by = c("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", "condition_start_date", 
                                                              "fracture_site", "type",  "two_yrs_after_index", "cancer_date")) %>%
  dplyr::select(-bone_disease_concept_id) 


# Add in first fracture date after the index dates
fracture_after_index_dates <- cohort_table %>% filter(is.na(type))

fracture_after_index <- fracture_after_index_dates %>%
  dplyr::group_by(subject_id) %>%
  dplyr::summarise(fracture_after_index = min(condition_start_date, na.rm = TRUE))

cohort_table.5 <- cohort_table.4 %>%
  dplyr::left_join(fracture_after_index, by = "subject_id")

rm(fracture_after_index_dates, fracture_after_index)

# Add in death date after the index date 
cohort_table.6 <- cohort_table.5 %>% 
  dplyr::left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::filter(death_date > condition_start_date | is.na(death_date)) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, type, 
                 two_yrs_after_index, cancer_date, mbd_date, fracture_after_index, death_date)

# Add study end date
cohort_table.7 <- cohort_table.6 %>% 
  mutate(study_end = study_end_date)

# Add in FOLLOWUPENDGL - with reason for FU (censoring)
cohort_table.8 <- addInFollowUpEndGL(cohort_table.7)

# Remove patients that have less one month contribution ----

cohort_table <- cohort_table.8 %>%
  filter(follow_up_time >= 30.44)

# Update attrition report
# Update attrition report
AttritionReportCoh <- AttritionReportCoh %>% 
  dplyr::union_all(  
    tibble(
      number_records = cohort_table %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals with less than one month exposure"
    )
  ) 

# Final cleaning of names

cohort_table <- cohort_table %>% 
  rename(index_date = condition_start_date) %>% 
  select(-type)


### Finalise attrition
AttritionReportCoh <- AttritionReportCoh %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-dplyr::lag(number_subjects)), records_excluded = -(number_records - dplyr::lag(number_records)))


# rio::export(AttritionReportCoh, here::here("OMOP Analysis", "Results", "AttritionReportCoh_OMOP.xlsx"))
# 
# 
# rio::export(cohort_table, here::here("OMOP Analysis", "Results", "cohort_table_omop_gl.csv"))

