cohort_table <- fracture_table_g

## a) Not Female ----

cohort_table_a <- cohort_table %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr:: group_by(subject_id) %>% 
  dplyr:: filter(!is.na(gender_source_value), gender_source_value == "F") %>%
  dplyr::ungroup() %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportCoh<- AttritionReportFrac[,1:3] %>% 
  union_all(  
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
  union_all(  
    tibble(
      number_records = cohort_table_b %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_b %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding individuals without fracture records"
    )
  )

### Define index date ----

cohort_table_b_index <- cohort_table_b %>%
  dplyr::group_by(subject_id) %>%
  dplyr::arrange(condition_start_date, .by_group = T) %>%
  dplyr::mutate(gap = condition_start_date - lag(condition_start_date)) %>%
  dplyr::filter(gap>730|is.na(gap)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-gap)


### c) Excluding individuals who has index date same as obs period end date ----

cohort_table_c_index <- cohort_table_b_index %>%
  dplyr::anti_join(cdm[["observation_period"]], by = c("subject_id" = "person_id", "condition_start_date" = "observation_period_end_date"), copy = T)

AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
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
  union_all(  
    tibble(
      number_records = cohort_table_d_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_d_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records with insufficient prior observation and/or outside of observation period"
    )
  ) 


# e) at least 50 ----
cohort_table_e_index <- cohort_table_d_index %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(age_fracture = lubridate::year(condition_start_date) - year_of_birth) %>%
  dplyr::filter(age_fracture >= 50) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
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

cdm[["cancer"]] <- cdm[["denominator"]] %>% 
  dplyr::left_join(cdm[["condition_occurrence"]], by = c("subject_id" = "person_id")) %>%
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
  cohort_table_e_index %>% anti_join(cohort_table_e_index %>% 
                                           dplyr::inner_join(cdm[["cancer"]] %>% dplyr::select(subject_id, cancer_date), 
                                                             by = "subject_id", copy = T, relationship = "many-to-many") %>%
                                           dplyr::filter(cancer_date<=condition_start_date) %>%
                                           dplyr::distinct() %>%
                                           dplyr::compute(), by = colnames(cohort_table_e_index)) 

AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_f_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_f_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records happened after cancer"
    )
  ) 

### g) No records of metabolic bone disease

mbd <- read_excel(paste0(here(), "/OMOP Analysis/mbd_codes.xlsx"))
mbd_codes <- mbd %>% dplyr::select(Id) %>% dplyr::pull()

cdm[["mbd"]] <- cdm[["denominator"]] %>% 
  dplyr::left_join(cdm[["condition_occurrence"]], by = c("subject_id" = "person_id")) %>%
  dplyr::filter(condition_concept_id %in% mbd_codes) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  dplyr::mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  dplyr::mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  dplyr::rename(mbd_date =condition_start_date) 

cohort_table_g_index <- 
  cohort_table_f_index %>% anti_join(cohort_table_f_index %>% 
                                           dplyr::inner_join(cdm[["mbd"]] %>% dplyr::select(subject_id, mbd_date), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                                           dplyr::filter(mbd_date<=condition_start_date) %>%
                                           dplyr::distinct() %>%
                                           dplyr::compute(), by = colnames(cohort_table_f_index)) %>% 
  dplyr::mutate(class = "index") # create a variable called class that is used later in the addIndex function

AttritionReportCoh<- AttritionReportCoh %>% 
  union_all(  
    tibble(
      number_records = cohort_table_g_index %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = cohort_table_g_index %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding records happened after metabolic bone diseases"
    )
  )  



### Finalise attrition
AttritionReportCoh <- AttritionReportCoh %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records)))

write.xlsx(AttritionReport, file = here::here(sub_output_folder, "AttritionReport.xlsx"))


## Create unique dataset with both index and non index fractures

# fracture_table_rq2 <- fracture_table
cohort_table_g_index_ids <- cohort_table_g_index %>% dplyr::pull(subject_id)

cohort_table <- cohort_table %>%
  dplyr::left_join(cohort_table_g_index, by = colnames(cohort_table)) %>%
  dplyr::filter(subject_id %in% cohort_table_g_index_ids)

cohort_table <- addIndex(cohort_table)


######### START TO GET LOST HERE #########


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
cohort_table <- addInFollowUpEnd(cohort_table)

# Add in immFracture
cohort_table <- immFracture(cohort_table)

cohort_table_back_up <- cohort_table

reverseEntryTable <- list()

while (nrow(cohort_table_back_up) > 0){
  reverseEntryTable[[nrow(cohort_table_back_up)]] <- cohort_table_back_up %>% dplyr::filter(follow_up_time > 0)
  cohort_table_back_up <- cohort_table_back_up %>% dplyr::filter(follow_up_time > 0)
  cohort_table_back_up <- nextFractureClean(cohort_table_back_up)
  cohort_table_back_up <- addIndex(cohort_table_back_up)
  cohort_table_back_up <- addInTwoYearsAfter(cohort_table_back_up)
  cohort_table_back_up <- addInObsEndDate(cohort_table_back_up)
  cohort_table_back_up <- addInCancerPostIndex(cohort_table_back_up)
  cohort_table_back_up <- addInBoneDiseasePostIndex(cohort_table_back_up)
  cohort_table_back_up <- addInNextFracture(cohort_table_back_up)
  cohort_table_back_up <- addInDeath(cohort_table_back_up)
  cohort_table_back_up <- addInFollowUpEnd(cohort_table_back_up)
  cohort_table_back_up <- cohort_table_back_up %>% ungroup()
  cohort_table_back_up <- immFracture(cohort_table_back_up)
}

reverseEntryTable <- reverseEntryTable[!sapply(reverseEntryTable, is.null)]

entryTable <- list()
for (i in (1:length(reverseEntryTable))){
  entryTable[[i]]<-reverseEntryTable[[length(reverseEntryTable)+1-i]]
}

stratifiedCohort <- list()
for (i in (1:(length(entryTable)-1))){
  stratifiedCohort[[i]] <- entryTable[[i]] %>% dplyr::anti_join(entryTable[[i+1]], by = "subject_id")
}

stratifiedCohort[[length(entryTable)]] <- entryTable[[length(entryTable)]]
info(logger, "CREATING FOLLOW UP TIME IS DONE")



## DO I NEED THIS?

### imminent fracture cohort
print(paste0("Creating imminent fracture cohort at ", Sys.time()))
imminentFractureCohort <- list()
for (i in (1:length(stratifiedCohort))){
  imminentFractureCohort[[i]] <- stratifiedCohort[[i]] %>% 
    dplyr::filter (imminentFracture==1) %>% 
    dplyr::select(subject_id)
}

imminentFractureCohortTotal <-data.frame()
for (i in 1:length(imminentFractureCohort)){
  imminentFractureCohortTotal<-rbind(imminentFractureCohortTotal, imminentFractureCohort[[i]])
}

print(paste0("Creating no-imminent fracture cohort at ", Sys.time()))
withoutImminentFractureCohortTotal <- entryTable[[1]] %>%
  dplyr::anti_join(imminentFractureCohortTotal, by = "subject_id") %>%
  dplyr::select(subject_id) %>%
  dplyr::distinct()