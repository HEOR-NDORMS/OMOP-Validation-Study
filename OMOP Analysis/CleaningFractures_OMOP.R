# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Gianluca Fabiano & Njoki Njuki	
# Do file name: Cleaning Fractures (a to e in the attrition table)	
# Date: 20.12.2024
# Last date edited: 23.12.2024
# ****************************************************************************** #

# Run the CodeToRun.R first to connect to the database first

### Start and end dates ----
study_start_date <- as.Date("2010-04-01")
study_end_date <- as.Date("2018-03-31")

### Create a cohort of women only ----
# age above 50 between 2010/04/01 and 2018/03/31
# info(logger, "CREATING DENOMINATOR - WOMEN WHO ARE ABOVE 50 WITHIN THE STUDY PERIOD")
print(paste0("Creating denominators at ", Sys.time()))

library(CohortConstructor)

cdm <- generateDenominatorCohortSet(
  cdm,
  name = "denominator",
  cohortDateRange = c(study_start_date, study_end_date),
  sex = "Female",
  ageGroup = list(c(50, 150)))

denom_count <-cdm[["denominator"]] %>% dplyr::tally() %>% dplyr::pull()

#AttritionReportDenom<-cohortAttrition(cdm$denominator)

### Loading fracture codes ----
# info(logger, "LOADING FRACTURE CODES")
print(paste0("Loading fracture codes at ", Sys.time()))
conditions_sheet1 <- read_excel(paste0(here(), "/1_InstantiateCohorts/fracture_sites_conditions_codes.xlsx"), sheet = 1) 
trauma <- read_excel(paste0(here(), "/1_InstantiateCohorts/trauma_codes.xlsx")) 
# not to self: the code is filtering the domain condition then selecting the id variable
# then pull function is similar to $ so basically calling that variable
# its like doing unique() when you do the pull () at the end of the pipe!! (noice!)
trauma_condition <- trauma %>% 
  dplyr::filter(Domain == "Condition") %>%
  dplyr::select(Id) %>% dplyr::pull()
trauma_observation <- trauma %>% 
  dplyr::filter(Domain == "Observation") %>% 
  dplyr::select(Id) %>% dplyr::pull()

hip_fracture_id <- conditions_sheet1 %>% 
  dplyr::filter(Site == "Hip") %>% 
  dplyr::select(Id) %>% 
  dplyr::pull()

femur_fracture_id <- conditions_sheet1 %>% 
  dplyr::filter(Site == "Femur") %>% 
  dplyr::select(Id) %>% dplyr::pull()

pelvic_fracture_id <- conditions_sheet1 %>% 
  dplyr::filter(Site == "Pelvic") %>% 
  dplyr::select(Id) %>% dplyr::pull()

vert_fracture_id <- conditions_sheet1 %>% 
  dplyr::filter(Site == "Vertebra") %>% 
  dplyr::select(Id) %>% dplyr::pull()

humerus_fracture_id <- conditions_sheet1 %>% 
  dplyr::filter(Site == "Humerus") %>% 
  dplyr::select(Id) %>% dplyr::pull()
# n_distinct(humerus_fracture_id)

forearm_fracture_id <- conditions_sheet1 %>% 
  dplyr::filter(Site == "Forearm") %>% 
  dplyr::select(Id) %>% dplyr::pull()

tib_fracture_id <- conditions_sheet1 %>% 
  dplyr::filter(Site == "Tibia and Fibula") %>% 
  dplyr::select(Id) %>% dplyr::pull()

rib_fracture_id <- conditions_sheet1 %>% 
  dplyr::filter(Site == "Rib") %>% 
  dplyr::select(Id) %>% 
  dplyr::pull()

foot_fracture_id <- conditions_sheet1 %>% 
  dplyr::filter(Site == "Foot") %>% 
  dplyr::select(Id) %>% 
  dplyr::pull()

nonspecific_fracture_id <- conditions_sheet1 %>% 
  dplyr::filter(Site == "Nonspecific") %>% 
  dplyr::select(Id) %>% 
  dplyr::pull()

any_fracture_id <- conditions_sheet1 %>% 
  dplyr::filter(!Site == "Exclude") %>% 
  dplyr::select(Id) %>% 
  dplyr::pull()

### Cohort with all records of fracture ---- 
# info(logger, "COLLECTING ALL RECORDS OF FRACTURES FROM DENOMINATORS")
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

# The fracture table
fracture_table <- cdm[["fracture"]] %>% dplyr::collect()

AttritionReportFrac<-tibble(
  cohort_definition_id = as.integer(1),
  number_records = fracture_table %>% dplyr::tally() %>% dplyr::pull(),
  number_subjects = fracture_table %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
  reason = "Starting Population - Anyone With Fracture(s)"
)


### a) Remove fractures before the birth year ----
fracture_table_a <- fracture_table %>%
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(fracture_year = as.numeric(format(condition_start_date, "%Y"))) %>%
  dplyr::filter(fracture_year > year_of_birth) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

# count of patients
n_distinct(fracture_table_a$subject_id)

# count of fractures
dim(fracture_table_a)

### b) Remove fracture outside of study period ----
fracture_table_b <- fracture_table_a %>%
  dplyr::filter(condition_start_date >= study_start_date & condition_start_date <= study_end_date)

# count of patients
n_distinct(fracture_table_b$subject_id)

# count of fractures
dim(fracture_table_b)

### c) Remove fractures on the same day as trauma codes ----
fracture_table_c <- fracture_table_b %>%
  anti_join(cdm[["condition_occurrence"]] %>% dplyr::filter(condition_concept_id %in% trauma_condition), by = c("subject_id" = "person_id", "condition_start_date"), copy = T)

fracture_table_c <- fracture_table_c %>% 
  anti_join(cdm[["observation"]] %>% dplyr::filter(observation_concept_id %in% trauma_observation), by = c("subject_id" = "person_id", "condition_start_date" = "observation_date"), copy = T) 

# count of patients
n_distinct(fracture_table_c$subject_id)

# count of fractures
dim(fracture_table_c)

### d) Remove fractures on the same day as death or after ----
fracture_table_d <- fracture_table_c %>%
  dplyr::left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  dplyr::group_by(subject_id) %>%
  dplyr::filter(condition_start_date < death_date) %>%
  dplyr::ungroup()

# count of patients
n_distinct(fracture_table_d$subject_id)

# count of fractures
dim(fracture_table_d)

### e) Remove fractures on the last day of observation period ----
fracture_table_e <- fracture_table_d %>% 
  dplyr::group_by(subject_id) %>% 
  dplyr::filter(condition_start_date != cohort_end_date) %>% 
  dplyr::ungroup()

# count of patients
n_distinct(fracture_table_e$subject_id)

# count of fractures
dim(fracture_table_e)
