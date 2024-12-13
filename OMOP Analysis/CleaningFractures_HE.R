
study_start_date <- as.Date("2010-04-01")
study_end_date <- as.Date("2018-03-31")
### Create a cohort of women only, age above 50 between 2010/04/01 and 2018/03/31
info(logger, "CREATING DENOMINATOR - WOMEN WHO ARE ABOVE 50 WITHIN THE STUDY PERIOD")
print(paste0("Creating denominators at ", Sys.time()))
cdm <- generateDenominatorCohortSet(
  cdm,
  name = "denominator",
  cohortDateRange = c(study_start_date, study_end_date),
  sex = "Female",
  ageGroup = list(c(50, 150)))

denom_count <-cdm[["denominator"]] %>% dplyr::tally() %>% dplyr::pull()

#AttritionReportDenom<-cohortAttrition(cdm$denominator)

### Loading fracture codes
info(logger, "LOADING FRACTURE CODES")
print(paste0("Loading fracture codes at ", Sys.time()))
conditions_sheet1 <- read_excel(paste0(here(), "/1_InstantiateCohorts/fracture_sites_conditions_codes.xlsx"), sheet = 1) 
trauma <- read_excel(paste0(here(), "/1_InstantiateCohorts/trauma_codes.xlsx")) 
trauma_condition <- trauma %>% dplyr::filter(Domain == "Condition") %>% dplyr::select(Id) %>% dplyr::pull()
trauma_observation <- trauma %>% dplyr::filter(Domain == "Observation") %>% dplyr::select(Id) %>% dplyr::pull()

hip_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Hip") %>% dplyr::select(Id) %>% dplyr::pull()
femur_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Femur") %>% dplyr::select(Id) %>% dplyr::pull()
pelvic_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Pelvic") %>% dplyr::select(Id) %>% dplyr::pull()
vert_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Vertebra") %>% dplyr::select(Id) %>% dplyr::pull()
humerus_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Humerus") %>% dplyr::select(Id) %>% dplyr::pull()
forearm_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Forearm") %>% dplyr::select(Id) %>% dplyr::pull()
tib_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Tibia and Fibula") %>% dplyr::select(Id) %>% dplyr::pull()
rib_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Rib") %>% dplyr::select(Id) %>% dplyr::pull()
foot_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Foot") %>% dplyr::select(Id) %>% dplyr::pull()
nonspecific_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Nonspecific") %>% dplyr::select(Id) %>% dplyr::pull()

any_fracture_id <- conditions_sheet1 %>% dplyr::filter(!Site == "Exclude") %>% dplyr::select(Id) %>% dplyr::pull()

### cohort with all records of fracture 
info(logger, "COLLECTING ALL RECORDS OF FRACTURES FROM DENOMINATORS")
print(paste0("Collecting all records of fractures from the denominators at ", Sys.time()))
cdm[["fracture"]] <- cdm[["denominator"]] %>% 
  dplyr::left_join(cdm[["condition_occurrence"]], by = c("subject_id" = "person_id")) %>%
  dplyr::filter(condition_concept_id %in% any_fracture_id) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  dplyr::mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  dplyr::mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  dplyr::mutate(fracture_site = if (condition_concept_id %in% hip_fracture_id) {
    "Hip"
  }
  else if (condition_concept_id %in% femur_fracture_id) {
    "Femur"
  }
  else if (condition_concept_id %in% foot_fracture_id) {
    "Foot"
  }
  else if (condition_concept_id %in% tib_fracture_id) {
    "Tibia and Fibula"
  }
  else if (condition_concept_id %in% rib_fracture_id) {
    "Rib"
  }
  else if (condition_concept_id %in% forearm_fracture_id) {
    "Forearm"
  }
  else if (condition_concept_id %in% vert_fracture_id) {
    "Vertebra"
  }
  else if (condition_concept_id %in% pelvic_fracture_id) {
    "Pelvic"
  }  
  else if (condition_concept_id %in% humerus_fracture_id) {
    "Humerus"
  } 
  else if (condition_concept_id %in% nonspecific_fracture_id) {
    "Nonspecific"
  }
  )
rm(hip_fracture_id, femur_fracture_id, foot_fracture_id, tib_fracture_id, rib_fracture_id, forearm_fracture_id, vert_fracture_id, pelvic_fracture_id, humerus_fracture_id, nonspecific_fracture_id)
fracture_table <- cdm[["fracture"]] %>% dplyr::collect()

AttritionReportFrac<-tibble(
  cohort_definition_id = as.integer(1),
  number_records = fracture_table %>% dplyr::tally() %>% dplyr::pull(),
  number_subjects = fracture_table %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
  reason = "Starting Population - Anyone With Fracture(s)"
)


### a) Remove fractures before the birth year
fracture_table_a <- fracture_table %>%
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(fracture_year = as.numeric(format(condition_start_date, "%Y"))) %>%
  dplyr::filter(fracture_year > year_of_birth) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

### b) Remove fracture outside of study period
fracture_table_b <- fracture_table_a %>%
  dplyr::filter(condition_start_date >= study_start_date & condition_start_date <= study_end_date)

### c) Remove fractures on the same day as trauma codes
fracture_table_c <- fracture_table_b %>%
  anti_join(cdm[["condition_occurrence"]] %>% dplyr::filter(condition_concept_id %in% trauma_condition), by = c("subject_id" = "person_id", "condition_start_date"), copy = T)

fracture_table_c <- fracture_table_c %>% 
  anti_join(cdm[["observation"]] %>% dplyr::filter(observation_concept_id %in% trauma_observation), by = c("subject_id" = "person_id", "condition_start_date" = "observation_date"), copy = T) 


### d) Remove fractures on the same day as death or after

fracture_table_d <- fracture_table_c %>% 
  
  
  
  ### A) First fracture (index) >= 01/04/2012 and <= 31/03/2018 
  fracture_table_a <- fracture_table %>%
  group_by(subject_id) %>%
  arrange(condition_start_date) %>%  
  slice(1) %>%  # Select the first fracture for each subject
  filter(condition_start_date >= as.Date("2012-04-01") &
           condition_start_date <= as.Date("2018-03-31")) %>%
  ungroup()

### B) No history of fracture in the 2 years prior to index date
fracture_table_b <- fracture_table_a %>%
  dplyr::group_by(subject_id) %>%
  mutate(
    gap = condition_start_date - lag(cohort_start_date)) %>%
  filter(gap>=730|is.na(gap)) %>%
  dplyr::select(-gap) %>% 
  ungroup()

### C) At least 50 at time of fracture
fracture_table_c <- fracture_table_b %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(age_fracture = lubridate::year(condition_start_date) - year_of_birth) %>%
  dplyr::filter(age_fracture >= 50) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

### D)