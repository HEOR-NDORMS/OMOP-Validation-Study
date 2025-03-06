# for a fracture table, compute the index date for each person
addIndex <- function (fractureTable){
  fractureTable %>% 
    dplyr::inner_join(fractureTable %>% 
                        dplyr::filter(class == "index") %>%
                        dplyr::group_by(subject_id) %>% 
                        dplyr::summarise(index_date = min(condition_start_date)),
                      by = "subject_id"
    )
}

# add an extra variable after_index which is two years after the index date
addInTwoYearsAfter <- function (fractureTable){
  fractureTable %>%
    dplyr::mutate(after_index = index_date + 730)
}

# add in the end of observation period for each individual
addInObsEndDate <- function (fractureTable){
  fractureTable %>% 
    dplyr::left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>%
    dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, class, index_date, after_index, observation_period_end_date)
}

# add in a column indicating the date of cancer after the index date
addInCancerPostIndex <- function (fractureTable){
  fractureTable %>% dplyr::left_join(fractureTable %>% 
                                       dplyr::inner_join(cdm[["cancer"]] %>% rename(cancer_concept_id = condition_concept_id), 
                                                         by = c("subject_id", "cohort_start_date", "cohort_end_date"), 
                                                         copy = T, 
                                                         relationship = "many-to-many") %>%
                                       dplyr::filter(index_date < cancer_date) %>%   
                                       dplyr::group_by(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, class, index_date, observation_period_end_date) %>%
                                       dplyr::arrange(cancer_date) %>%
                                       dplyr::filter(row_number()==1) %>%
                                       dplyr::ungroup(), by = c("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", "condition_start_date", "fracture_site", "class", "index_date", "after_index", "observation_period_end_date")) %>%
    dplyr::select(-cancer_concept_id) 
}

# add in a column indicating the date of bone disease after the index date (has to be done after the cancer one)
addInBoneDiseasePostIndex <- function (fractureTable){
  fractureTable %>% dplyr::left_join(fractureTable %>% 
                                       dplyr::inner_join(cdm[["mbd"]] %>%
                                                           rename(bone_disease_concept_id = condition_concept_id), 
                                                         by = c("subject_id", "cohort_start_date", "cohort_end_date"),
                                                         copy = T, 
                                                         relationship = "many-to-many") %>%
                                       dplyr::filter(index_date < mbd_date) %>%   
                                       dplyr::group_by(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, class, index_date, observation_period_end_date) %>%
                                       dplyr::arrange(mbd_date) %>%
                                       dplyr::filter(row_number()==1) %>%
                                       dplyr::ungroup(), by = c("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", "condition_start_date", "fracture_site", "class", "index_date", "after_index", "observation_period_end_date", "cancer_date")) %>%
    dplyr::select(-bone_disease_concept_id) 
}

# add a column of next fracture after the index one
addInNextFracture <- function(fractureTable){
  fractureTable %>% 
    dplyr::left_join(fractureTable %>% 
                       dplyr::filter(condition_start_date> index_date) %>% 
                       dplyr::group_by(subject_id) %>% 
                       dplyr::summarise(fracture_after_index = min(condition_start_date, na.rm =  T)),
                     by = "subject_id")
}

# add a column of death record date after the index date
addInDeath <- function(fractureTable){
  fractureTable %>% 
    dplyr::left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>%
    dplyr::filter(death_date > index_date | is.na(death_date)) %>%
    dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, class, index_date, after_index, observation_period_end_date, cancer_date, mbd_date, fracture_after_index, death_date)
}

# # add in FOLLOWUPEND - only after the relevant columns are added
# addInFollowUpEnd <- function(fractureTable){
#   fractureTable %>% 
#     dplyr::mutate(follow_up_end = pmin(.data$after_index, .data$observation_period_end_date, .data$cancer_date, .data$mbd_date, .data$fracture_after_index, .data$death_date, na.rm = T)) %>%
#     dplyr::mutate(follow_up_time = follow_up_end-index_date) 
# }


## new FOLLOW UP END GL
addInFollowUpEndGL <- function(fractureTable) {
  fractureTable %>%
    dplyr::mutate(
      follow_up_end = pmin(
        .data$after_index, 
        .data$observation_period_end_date, 
        .data$cancer_date, 
        .data$mbd_date, 
        .data$fracture_after_index, 
        .data$death_date, 
        na.rm = TRUE
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      follow_up_reason = paste(
        c(
          if (!is.na(after_index) & follow_up_end == after_index) "after_index",
          if (!is.na(observation_period_end_date) & follow_up_end == observation_period_end_date) "observation_period_end_date",
          if (!is.na(cancer_date) & follow_up_end == cancer_date) "cancer_date",
          if (!is.na(mbd_date) & follow_up_end == mbd_date) "mbd_date",
          if (!is.na(fracture_after_index) & follow_up_end == fracture_after_index) "fracture_after_index",
          if (!is.na(death_date) & follow_up_end == death_date) "death_date"
        ),
        collapse = ", "
      ),
      follow_up_time = follow_up_end - index_date
    ) %>%
    dplyr::ungroup()
}


# add in immFracture, a function that indicates which fracture is considered imminent in relation to the current index fracture
immFracture <- function (fractureTable){
  fractureTable %>%
    dplyr::mutate(imminentFracture = as.integer(condition_start_date == follow_up_end))
}


#clean out fractures based on their follow up period, used for further analysis 
nextFractureClean <- function (fractureTable){
  pre <- 
    fractureTable %>%
    dplyr::anti_join(fractureTable %>% dplyr::filter(imminentFracture==1|follow_up_time <730|(mbd_date == follow_up_end)|(cancer_date == follow_up_end)|(death_date == follow_up_end)|(observation_period_end_date == follow_up_end)), by = "subject_id") %>% 
    dplyr::group_by(subject_id) %>%
    dplyr::arrange(condition_start_date, .by_group = T) %>%
    dplyr::filter(condition_start_date>index_date) %>%
    dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, class) %>%
    dplyr::ungroup()
  
  index_ids <- pre %>% 
    dplyr::filter(class == "index") %>%
    pull(subject_id)
  
  pre <- pre %>%
    dplyr::filter(subject_id %in% index_ids)
  return(pre)
}


analyse_visits <- function(cohort_df, visit_data) {
  
  ### Filtering visits based on the cohort_df
  filtered_visits <- visit_data %>%
    dplyr::left_join(cohort_df, by = "subject_id", relationship = "many-to-many", copy = T) %>%
    dplyr::filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end) %>%
    dplyr::group_by(subject_id, index_date, specialty) %>% # we group by index date to ensure each visit is associated with an entry
    dplyr::summarise(visit_count = n(), .groups = "drop") %>%
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery()
  
  ### Pivot the data
  visits_count_wide <- filtered_visits %>%
    pivot_wider(names_from = specialty, values_from = visit_count, values_fill = NA) %>% 
    CDMConnector::computeQuery()
  
  ### Join the wide dataframe back to cohort_combined and count tot num visits
  visits_count_wide <- visit_data %>% 
    dplyr::left_join(visits_count_wide, by = c("subject_id", "index_date"), copy = T) %>% 
    CDMConnector::computeQuery()
  
  ### summary for user only (subjects/visit= NA, not counted)
  included_cols <- colnames(visits_count_wide)[(colnames(visits_count_wide)%in% specialty_source_value_ids)]
  tot_exposed_yrs_all <- visits_count_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    dplyr::summarise(tot_exposed_yrs = sum(exposed_yrs)) %>% 
    dplyr::pull(tot_exposed_yrs)
  
  tot_exposed_yrs_user <- visits_count_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits") %>% 
    dplyr::mutate(visits_per_year = visits / exposed_yrs) %>%
    dplyr::filter(visits > 0) %>% 
    dplyr::group_by(subject_id, index_date) %>% 
    dplyr::filter(row_number()==1) %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(tot_exposed_yrs = sum(exposed_yrs)) %>% 
    dplyr::pull(tot_exposed_yrs)
  
  user_only_summary <- visits_count_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits") %>% 
    dplyr::mutate(visits_per_year = visits / exposed_yrs) %>%
    dplyr::filter(visits > 0) %>%
    dplyr::group_by(specialty) %>%
    dplyr::summarise(
      tot_visits = sum(visits),
      mean_visits_per_year = signif(tot_visits / tot_exposed_yrs_user, 4), # Manual calculation of mean
      sd_visits_per_year = signif(sd(visits_per_year, na.rm = TRUE), 4),
      min_visits_per_year = signif(min(visits_per_year, na.rm = TRUE), 4),
      max_visits_per_year = signif(max(visits_per_year, na.rm = TRUE), 4),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
    ) %>% 
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery() 
  
  ### summary for all subjects (subjects/visits = NA, treated as zero)
  all_summary <- visits_count_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits") %>% 
    tidyr::complete(subject_id, specialty, fill = list(visits = 0)) %>% # filling missing visits with 0
    dplyr::mutate(visits_per_year = visits / exposed_yrs) %>%
    dplyr::group_by(specialty) %>%
    dplyr::summarise(
      tot_visits = sum(visits),
      mean_visits_per_year = signif(tot_visits / tot_exposed_yrs_all, 4), # Manual calculation of mean
      sd_visits_per_year = signif(sd(visits_per_year, na.rm = TRUE), 4),
      min_visits_per_year = signif(min(visits_per_year, na.rm = TRUE), 4),
      max_visits_per_year = signif(max(visits_per_year, na.rm = TRUE), 4),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
    ) %>% 
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery() 
  
  # Calculating non-service users
  visits_count_wide_test <- visits_count_wide[(colnames(visits_count_wide)%in% specialty_source_value_ids)]
  visits_count_wide_test[is.na(visits_count_wide_test)] <- 0
  visits_count_wide_test$row_sum <- rowSums(visits_count_wide_test)
  non_service_users <- visits_count_wide_test %>% 
    dplyr::filter(row_sum == 0) %>% 
    dplyr::tally() %>%
    dplyr::rename(nonservice=n)
  
  return(list(user_only_summary = user_only_summary, all_summary = all_summary, non_service_users=non_service_users, visits_count_wide=visits_count_wide, tot_exposed_yrs_all=tot_exposed_yrs_all, tot_exposed_yrs_user = tot_exposed_yrs_user))
}

# Estimate costs primary care visits 

analyse_visits_cost <- function(cohort_df, visit_data) {
  
  ### Filtering visits based on the cohort_combined
  filtered_visits <- visit_data %>%
    dplyr::left_join(cohort_df, by = "subject_id", relationship = "many-to-many", copy = T) %>%
    dplyr::filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end) %>%
    dplyr::group_by(subject_id, index_date, specialty, unit_cost) %>% # we group by index date to ensure each visit is associated with an entry
    dplyr::summarise(visit_count = n(), .groups = "drop") %>%
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery()
  
  ### Compute costs visits
  filtered_visits <- filtered_visits %>%  
    dplyr::mutate (visit_cost = visit_count * unit_cost) %>% 
    dplyr::select (-unit_cost, -visit_count) %>% 
    CDMConnector::computeQuery()
  
  ### Pivot the data
  visits_cost_wide <- filtered_visits %>%
    pivot_wider(names_from = specialty, values_from = visit_cost, values_fill = 0) %>% 
    CDMConnector::computeQuery()
  
  ### Join the wide dataframe back to cohort_combined and count tot num visits
  visits_cost_wide <- cohort_combined %>% 
    dplyr::left_join(visits_cost_wide, by = c("subject_id", "index_date"), copy = T) %>% 
    CDMConnector::computeQuery()
  
  ### summary for user only (subjects/visit= NA, not counted)
  included_cols <- colnames(visits_cost_wide)[(colnames(visits_cost_wide)%in% specialty_source_value_ids)]
  tot_exposed_yrs_all <- visits_cost_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    dplyr::summarise(tot_exposed_yrs = sum(exposed_yrs)) %>% 
    dplyr::pull(tot_exposed_yrs)
  
  tot_exposed_yrs_user <- visits_cost_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits") %>% 
    dplyr::mutate(visits_per_year = visits / exposed_yrs) %>%
    dplyr::filter(visits > 0) %>% 
    dplyr::group_by(subject_id, index_date) %>% 
    dplyr::filter(row_number()==1) %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(tot_exposed_yrs = sum(exposed_yrs)) %>% 
    dplyr::pull(tot_exposed_yrs)
  
  user_only_cost_summary <- visits_cost_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits_costs") %>% 
    dplyr::mutate(visits_costs_per_year = visits_costs / exposed_yrs) %>%
    dplyr::filter(visits_costs > 0) %>%
    dplyr::group_by(specialty) %>%
    dplyr::summarise(
      tot_visits_costs = sum(visits_costs),
      mean_cost_visits_per_year = signif(tot_visits_costs / tot_exposed_yrs_user, 4), # Manual calculation of mean
      sd_cost_visits_per_year = signif(sd(visits_costs_per_year, na.rm = TRUE), 4),
      min_cost_visits_per_year = signif(min(visits_costs_per_year, na.rm = TRUE), 4),
      max_cost_visits_per_year = signif(max(visits_costs_per_year, na.rm = TRUE), 4),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
    ) %>% 
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery()
  
  ### summary for all subjects (subjects/visits = NA, treated as zero)
  all_cost_summary <-  visits_cost_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits_costs") %>% 
    dplyr::mutate(visits_costs_per_year = visits_costs / exposed_yrs) %>%
    dplyr::group_by(specialty) %>%
    dplyr::mutate(visits_costs = signif(visits_costs, digits = 2)) %>% 
    dplyr::summarise(
      tot_visits_costs = sum(visits_costs, na.rm = T),
      mean_cost_visits_per_year = signif(tot_visits_costs / tot_exposed_yrs_all, 4), # Manual calculation of mean
      sd_cost_visits_per_year = signif(sd(visits_costs_per_year, na.rm = TRUE), 4),
      min_cost_visits_per_year = signif(min(visits_costs_per_year, na.rm = TRUE), 4),
      max_cost_visits_per_year = signif(max(visits_costs_per_year, na.rm = TRUE), 4),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
    )  %>% 
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery()
  
  return(list(user_only_cost_summary = user_only_cost_summary, all_cost_summary = all_cost_summary, visits_cost_wide=visits_cost_wide, tot_exposed_yrs_all=tot_exposed_yrs_all, tot_exposed_yrs_user=tot_exposed_yrs_user))
}

# Cohort summary

cohort_summary <- function(data) {
  entries_per_woman <- data %>% 
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    dplyr::group_by(subject_id) %>% 
    dplyr::summarise(entries_per_woman = n(), 
                     tot_exposed_yrs = sum(exposed_yrs),
                     .groups = "drop") # Drop groups after summarising) 
  
  summary <- tibble(
    num_distinct_women = n_distinct(data$subject_id),
    num_entries = nrow(data),
    tot_exposed_yrs = sum(entries_per_woman$tot_exposed_yrs),
    mean_entries_per_woman = signif(mean(entries_per_woman$entries_per_woman), 4),
    sd_entries_per_woman = signif(sd(entries_per_woman$entries_per_woman), 4),
    min_entries_per_woman = min(entries_per_woman$entries_per_woman),
    max_entries_per_woman = max(entries_per_woman$entries_per_woman),
    num_women_1_entry = sum(entries_per_woman$entries_per_woman == 1),
    num_women_2_entries = sum(entries_per_woman$entries_per_woman == 2),
    num_women_3plus_entries = sum(entries_per_woman$entries_per_woman >= 3)
  )
  
  return(summary)
}
