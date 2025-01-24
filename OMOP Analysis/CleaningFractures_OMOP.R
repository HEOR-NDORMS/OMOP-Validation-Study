# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Gianluca Fabiano & Njoki Njuki, Rafa	
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
#print(paste0("Creating denominators at ", Sys.time()))

#library(CohortConstructor)

cdm <- IncidencePrevalence::generateDenominatorCohortSet(
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
conditions_sheet1 <- read_excel(paste0(here(), "/OMOP Analysis/fracture_sites_conditions_codes.xlsx"), sheet = 1) 
trauma <- read_excel(paste0(here(), "/OMOP Analysis/trauma_codes.xlsx")) 
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
    "NHNS"
  }
  else if (condition_concept_id %in% foot_fracture_id) {
    "NHNS"
  }
  else if (condition_concept_id %in% tib_fracture_id) {
    "NHNS"
  }
  else if (condition_concept_id %in% rib_fracture_id) {
    "NHNS"
  }
  else if (condition_concept_id %in% forearm_fracture_id) {
    "NHNS"
  }
  else if (condition_concept_id %in% vert_fracture_id) {
    "Vertebra"
  }
  else if (condition_concept_id %in% pelvic_fracture_id) {
    "NHNS"
  }  
  else if (condition_concept_id %in% humerus_fracture_id) {
    "NHNS"
  } 
  else if (condition_concept_id %in% nonspecific_fracture_id) {
    "NHNS"
  }
  )
#rm(hip_fracture_id, femur_fracture_id, foot_fracture_id, tib_fracture_id, rib_fracture_id, forearm_fracture_id, vert_fracture_id, pelvic_fracture_id, humerus_fracture_id, nonspecific_fracture_id)

# The fracture table
fracture_table <- cdm[["fracture"]] %>% dplyr::collect()

AttritionReportFrac<-tibble(
  cohort_definition_id = as.integer(1),
  number_records = fracture_table %>% dplyr::tally() %>% dplyr::pull(),
  number_subjects = fracture_table %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
  reason = "Starting Population - Anyone With Fracture(s)"
)

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

AttritionReportFrac<- AttritionReportFrac %>% 
  dplyr::union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_a %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_a %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding fracture happening before the birth year"
    )
  ) 

### b) Remove fracture outside of study period ----
fracture_table_b <- fracture_table_a %>%
  dplyr::filter(condition_start_date >= study_start_date & condition_start_date <= study_end_date)

AttritionReportFrac<- AttritionReportFrac %>% 
  dplyr::union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_b %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_b %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records that happen outside of study period"
    )
  )

### c) Remove fracture outside of observation period ----

fracture_table_c <- fracture_table_b %>%
  dplyr::filter(condition_start_date >= cohort_start_date & condition_start_date <= cohort_end_date)

AttritionReportFrac<- AttritionReportFrac %>% 
  dplyr::union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_c %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_c %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records that happen outside of observation period"
    )
  )

### d) Remove fractures on the same day as trauma codes or 3 distinct fracture sites on same day ----
fracture_table_d <- fracture_table_c %>%
  anti_join(cdm[["condition_occurrence"]] %>% dplyr::filter(condition_concept_id %in% trauma_condition), by = c("subject_id" = "person_id", "condition_start_date"), copy = T)

fracture_table_d <- fracture_table_c %>% 
  dplyr::anti_join(cdm[["observation"]] %>% dplyr::filter(observation_concept_id %in% trauma_observation), by = c("subject_id" = "person_id", "condition_start_date" = "observation_date"), copy = T) 

fracture_table_d <-fracture_table_d %>%
  dplyr::anti_join(
    fracture_table_d %>% 
      #dplyr::filter(!fracture_site=="NHNS") %>% 
      dplyr::group_by(subject_id, condition_start_date) %>% 
      dplyr::summarise(number_site = dplyr::n_distinct(fracture_site), .groups = "drop") %>% 
      dplyr::filter(number_site==3),
    by = c("subject_id", "condition_start_date")
  )

AttritionReportFrac<- AttritionReportFrac %>% 
  dplyr::union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_d %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_d %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding trauma fractures (same day as trauma codes or 3 distinct fracture sites on the same date"
    )
  ) 


### e) Remove fractures on the same day as death or after ----
fracture_table_e <- fracture_table_d %>%
  dplyr::left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  dplyr::group_by(subject_id) %>%
  dplyr::filter(is.na(death_date)| death_date > condition_start_date) %>%
  dplyr::ungroup() %>% 
  dplyr::select("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", "condition_start_date", "fracture_site")

AttritionReportFrac<- AttritionReportFrac %>% 
  dplyr::union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_e %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_e %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding fractures on the same day as death or after death"
    )
  ) 

### e) Remove fractures on the last day of observation period ----
# fracture_table_e <- fracture_table_d %>% 
#   dplyr::group_by(subject_id) %>% 
#   dplyr::filter(condition_start_date != cohort_end_date) %>% 
#   dplyr::ungroup()
# 
# # count of patients
# n_distinct(fracture_table_e$subject_id)
# 
# # count of fractures
# dim(fracture_table_e)

### f) Washout ----

# info(logger, "APPLYING WASHOUT PERIOD")
# print(paste0("Applying a washout period of ", washout_period[[k]], " at ", Sys.time()))

# Define the fracture sites
sites <- c("Hip", "Vertebra", "NHNS")

# Initialise the backup table and the result container
fracture_table_back_up <- fracture_table_e
index_fractures <- tibble()

suppressWarnings(
  while (nrow(fracture_table_back_up) > 0) {
    fracture_correction <- list()
    
    # Process each site independently
    for (site in sites) {
      fracture_correction[[site]] <- fracture_table_back_up %>% 
        dplyr::filter(fracture_site == site) %>%  # Filter fractures for the current site
        dplyr::group_by(subject_id) %>% 
        dplyr::summarise(min_date = min(condition_start_date, na.rm = TRUE)) %>% 
        dplyr::mutate(site = site)
    }
    
    # Combine corrections for all sites
    fracture_correction_combined <- dplyr::bind_rows(fracture_correction)
    
    # Update the fracture table with the washout logic
    fracture_table_back_up <- fracture_table_back_up %>% 
      dplyr::left_join(fracture_correction_combined, by = c("subject_id", "fracture_site" = "site")) %>% 
      dplyr::mutate(gap_to_min_date = condition_start_date - min_date) %>% 
      dplyr::filter(gap_to_min_date == 0 | gap_to_min_date > 90) %>% 
      dplyr::group_by(subject_id, condition_start_date, fracture_site) %>% 
      dplyr::arrange(condition_concept_id, .by_group = TRUE) %>% 
      dplyr::filter(dplyr::row_number() == 1) %>% 
      dplyr::ungroup()
    
    # Append index fractures (those that meet the criteria)
    index_fractures <- dplyr::bind_rows(
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

AttritionReportFrac<- AttritionReportFrac %>% 
  dplyr::union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_f %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_f %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Clean out using washout period"
    )
  ) 
rm(index_fractures)

### Applying Hierarchy (hip,vertebra,NHNS) to multiple records on the same day ----
fracture_table_f$fracture_site<-factor(fracture_table_f$fracture_site, levels = sites)

fracture_table_g <- fracture_table_f %>%
  dplyr::group_by(subject_id, condition_start_date) %>%
  dplyr::arrange(fracture_site, .by_group = TRUE) %>% 
  dplyr::filter( dplyr::row_number()==1) %>%
  dplyr::ungroup()

AttritionReportFrac<- AttritionReportFrac %>% 
  dplyr::union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_g %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table_g %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records on the same day for the same person according to hierarchy (Hip, Spine, NHNS)"
    )
  )

### Finalise attrition -----
AttritionReportFrac <- AttritionReportFrac %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records))) %>% 
  dplyr::select(-c("cohort_definition_id"))

