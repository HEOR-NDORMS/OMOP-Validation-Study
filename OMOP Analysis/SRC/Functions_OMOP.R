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

# Analyse primary care visits
analyse_visits <- function(visit_data) {
  tot_exposed_yrs_all <- visit_data %>%
    group_by(subject_id) %>%
    summarise(exposed_yrs = first(exposed_yrs)) %>%
    summarise(tot_exposed_yrs = sum(exposed_yrs, na.rm = TRUE)) %>%
    ungroup() %>% 
    pull(tot_exposed_yrs)
  
  tot_exposed_yrs_user <- visit_data %>%
    filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
    group_by(subject_id, index_date) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    summarise(tot_exposed_yrs = sum(exposed_yrs), .groups = "drop") %>%
    pull(tot_exposed_yrs)
  
  user_only_summary <- visit_data %>%
    # filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
    group_by(subject_id, specialty) %>%
    summarise(total_patient_visits = first(visit_count),
              total_exposed_yrs = first(exposed_yrs),
              visits_per_year = total_patient_visits / total_exposed_yrs,
              .groups = "drop") %>%
    group_by(specialty) %>%
    summarise(tot_visits = round(sum(total_patient_visits, na.rm = TRUE), 2),
              mean_visits_per_year = round(sum(total_patient_visits, na.rm = TRUE) / sum(total_exposed_yrs, na.rm = TRUE), 2),
              sd_visits_per_year = round(sd(visits_per_year, na.rm = TRUE), 2),
              min_visits_per_year = round(min(visits_per_year, na.rm = TRUE), 2),
              max_visits_per_year = round(max(visits_per_year, na.rm = TRUE), 2),
              num_subjects_visited = n_distinct(subject_id),
              .groups = "drop") 
  
  all_summary <- visit_data %>%
    group_by(subject_id, specialty) %>%
    summarise(total_patient_visits = first(visit_count),
              total_exposed_yrs = first(exposed_yrs),
              visits_per_year = total_patient_visits / total_exposed_yrs,
              user_status = first(user_status),
              .groups = "drop") %>%
    group_by(specialty) %>%
    summarise(tot_visits = round(sum(total_patient_visits, na.rm = TRUE), 2),
              mean_visits_per_year = round(sum(total_patient_visits, na.rm = TRUE) / sum(total_exposed_yrs, na.rm = TRUE), 2),
              sd_visits_per_year = round(sd(visits_per_year, na.rm = TRUE), 2),
              min_visits_per_year = round(min(visits_per_year, na.rm = TRUE), 2),
              max_visits_per_year = round(max(visits_per_year, na.rm = TRUE), 2),
              num_subjects_visited = n_distinct(subject_id),
              num_non_users = sum(user_status == "Non-User"),
              .groups = "drop") %>%
    mutate(num_subjects_visited = ifelse(specialty == "Not Clinically Relevant", num_non_users, num_subjects_visited)) %>%
    select(-num_non_users) 
  
  non_service_users <- visit_data %>%
    filter(user_status == "Non-User", visit_count == 0) %>%
    summarise(n = n_distinct(subject_id)) %>%
    rename(nonservice = n)
  
  return(list(
    user_only_summary = user_only_summary,
    all_summary = all_summary,
    non_service_users = non_service_users,
    tot_exposed_yrs_all = tot_exposed_yrs_all,
    tot_exposed_yrs_user = tot_exposed_yrs_user
  ))
}

# This function is as above but now using the reduced specialties
analyse_visits_reduced <- function(visit_data_reduced) {
  tot_exposed_yrs_all <- visit_data_reduced %>%
    group_by(subject_id) %>%
    summarise(exposed_yrs = first(exposed_yrs)) %>%
    summarise(tot_exposed_yrs = sum(exposed_yrs, na.rm = TRUE)) %>%
    ungroup() %>% 
    pull(tot_exposed_yrs)
  
  tot_exposed_yrs_user <- visit_data_reduced %>%
    filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
    group_by(subject_id, index_date) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    summarise(tot_exposed_yrs = sum(exposed_yrs), .groups = "drop") %>%
    pull(tot_exposed_yrs)
  
  user_only_summary_reduced <- visit_data_reduced %>%
    filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
    group_by(subject_id, specialty_reduced) %>%
    summarise(total_patient_visits = first(visit_count),
              total_exposed_yrs = first(exposed_yrs),
              visits_per_year = total_patient_visits / total_exposed_yrs,
              .groups = "drop") %>%
    group_by(specialty_reduced) %>%
    summarise(tot_visits = round(sum(total_patient_visits, na.rm = TRUE), 2),
              mean_visits_per_year = round(sum(total_patient_visits, na.rm = TRUE) / sum(total_exposed_yrs, na.rm = TRUE), 2),
              sd_visits_per_year = round(sd(visits_per_year, na.rm = TRUE), 2),
              min_visits_per_year = round(min(visits_per_year, na.rm = TRUE), 2),
              max_visits_per_year = round(max(visits_per_year, na.rm = TRUE), 2),
              num_subjects_visited = n_distinct(subject_id),
              .groups = "drop") 
  
  
  all_summary_reduced <- visit_data_reduced %>%
    group_by(subject_id, specialty_reduced) %>%
    summarise(total_patient_visits = first(visit_count),
              total_exposed_yrs = first(exposed_yrs),
              visits_per_year = total_patient_visits / total_exposed_yrs,
              user_status = first(user_status),
              .groups = "drop") %>%
    group_by(specialty_reduced) %>%
    summarise(tot_visits = round(sum(total_patient_visits, na.rm = TRUE), 2),
              mean_visits_per_year = round(sum(total_patient_visits, na.rm = TRUE) / sum(total_exposed_yrs, na.rm = TRUE), 2),
              sd_visits_per_year = round(sd(visits_per_year, na.rm = TRUE), 2),
              min_visits_per_year = round(min(visits_per_year, na.rm = TRUE), 2),
              max_visits_per_year = round(max(visits_per_year, na.rm = TRUE), 2),
              num_subjects_visited = n_distinct(subject_id),
              num_non_users = sum(user_status == "Non-User"),
              .groups = "drop") %>%
    mutate(num_subjects_visited = ifelse(specialty_reduced == "Not Clinically Relevant", num_non_users, num_subjects_visited)) %>%
    select(-num_non_users) 
  
  non_service_users_reduced <- visit_data_reduced %>%
    filter(user_status == "Non-User", visit_count == 0) %>%
    summarise(n = n_distinct(subject_id)) %>%
    rename(nonservice = n)
  
  return(list(
    user_only_summary_reduced = user_only_summary_reduced,
    all_summary_reduced = all_summary_reduced,
    non_service_users_reduced = non_service_users_reduced,
    tot_exposed_yrs_all = tot_exposed_yrs_all,
    tot_exposed_yrs_user = tot_exposed_yrs_user
  ))
}

# Estimate costs primary care visits 
analyse_visits_cost <- function(visit_data) {
  
  tot_exposed_yrs_all <- visit_data %>%
    group_by(subject_id) %>%
    summarise(exposed_yrs = first(exposed_yrs)) %>%
    summarise(tot_exposed_yrs = sum(exposed_yrs, na.rm = TRUE)) %>%
    ungroup() %>% 
    pull(tot_exposed_yrs)
  
  tot_exposed_yrs_user <- visit_data %>%
    filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
    group_by(subject_id, index_date) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    summarise(tot_exposed_yrs = sum(exposed_yrs), .groups = "drop") %>%
    pull(tot_exposed_yrs)
  
  user_only_cost_summary <- visit_data %>%
    filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
    filter(visit_cost > 0) %>%
    group_by(subject_id, specialty) %>% 
    summarise(
      total_patient_cost = first(visit_cost),
      total_exposed_yrs = first(exposed_yrs),
      visits_costs_per_year = total_patient_cost / total_exposed_yrs,
      .groups = "drop"
    ) %>% 
    group_by(specialty) %>%
    summarise(
      tot_visits_costs = round(sum(total_patient_cost), 2),
      mean_visits_per_year = round(sum(total_patient_cost, na.rm = TRUE) / sum(total_exposed_yrs, na.rm = TRUE), 2),
      sd_cost_visits_per_year = round(sd(visits_costs_per_year, na.rm = TRUE), 2),
      min_cost_visits_per_year = round(min(visits_costs_per_year, na.rm = TRUE), 2),
      max_cost_visits_per_year = round(max(visits_costs_per_year, na.rm = TRUE), 2),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
    ) %>% 
    ungroup() 
  
  all_cost_summary <-  visit_data %>%
    group_by(subject_id, specialty) %>% 
    summarise(
      total_patient_cost = first(visit_cost),
      total_exposed_yrs = first(exposed_yrs),
      visits_costs_per_year = total_patient_cost / total_exposed_yrs,
      user_status = first(user_status),
      .groups = "drop"
    ) %>% 
    group_by(specialty) %>%
    summarise(
      tot_visits_costs = round(sum(total_patient_cost, na.rm = TRUE), 2),
      mean_visits_per_year = round(sum(total_patient_cost, na.rm = TRUE) / sum(total_exposed_yrs, na.rm = TRUE), 2),
      sd_cost_visits_per_year = round(sd(visits_costs_per_year, na.rm = TRUE), 2),
      min_cost_visits_per_year = round(min(visits_costs_per_year, na.rm = TRUE), 2),
      max_cost_visits_per_year = round(max(visits_costs_per_year, na.rm = TRUE), 2),
      num_subjects_visited = n_distinct(subject_id),
      num_non_users = sum(user_status == "Non-User"),
      .groups = "drop"
    ) %>% 
    mutate(num_subjects_visited = ifelse(specialty == "Not Clinically Relevant", num_non_users, num_subjects_visited)) %>%
    select(!num_non_users) 
  
  return(list(user_only_cost_summary = user_only_cost_summary, 
              all_cost_summary = all_cost_summary, 
              tot_exposed_yrs_all = tot_exposed_yrs_all, 
              tot_exposed_yrs_user = tot_exposed_yrs_user))
}


analyse_visits_cost_reduced <- function(visit_data_reduced) {
  
  tot_exposed_yrs_all <- visit_data_reduced %>%
    group_by(subject_id) %>%
    summarise(exposed_yrs = first(exposed_yrs)) %>%
    summarise(tot_exposed_yrs = sum(exposed_yrs, na.rm = TRUE)) %>%
    ungroup() %>% 
    pull(tot_exposed_yrs)
  
  tot_exposed_yrs_user <- visit_data_reduced %>%
    filter(user_status == "User", specialty_reduced != "Not Clinically Relevant") %>%
    group_by(subject_id, index_date) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    summarise(tot_exposed_yrs = sum(exposed_yrs), .groups = "drop") %>%
    pull(tot_exposed_yrs)
  
  user_only_cost_summary_reduced <- visit_data_reduced %>%
    filter(user_status == "User", specialty_reduced != "Not Clinically Relevant") %>%
    filter(visit_cost > 0) %>%
    group_by(subject_id, specialty_reduced) %>% 
    summarise(
      total_patient_cost = first(visit_cost),
      total_exposed_yrs = first(exposed_yrs),
      visits_costs_per_year = total_patient_cost / total_exposed_yrs,
      .groups = "drop"
    ) %>% 
    group_by(specialty_reduced) %>%
    summarise(
      tot_visits_costs = round(sum(total_patient_cost), 2),
      mean_visits_per_year = round(sum(total_patient_cost, na.rm = TRUE) / sum(total_exposed_yrs, na.rm = TRUE), 2),
      sd_cost_visits_per_year = round(sd(visits_costs_per_year, na.rm = TRUE), 2),
      min_cost_visits_per_year = round(min(visits_costs_per_year, na.rm = TRUE), 2),
      max_cost_visits_per_year = round(max(visits_costs_per_year, na.rm = TRUE), 2),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
    ) %>% 
    ungroup() 
  
  all_cost_summary_reduced <-  visit_data_reduced %>%
    group_by(subject_id, specialty_reduced) %>% 
    summarise(
      total_patient_cost = first(visit_cost),
      total_exposed_yrs = first(exposed_yrs),
      visits_costs_per_year = total_patient_cost / total_exposed_yrs,
      user_status = first(user_status),
      .groups = "drop"
    ) %>% 
    group_by(specialty_reduced) %>%
    summarise(
      tot_visits_costs = round(sum(total_patient_cost, na.rm = TRUE), 2),
      mean_visits_per_year = round(sum(total_patient_cost, na.rm = TRUE) / sum(total_exposed_yrs, na.rm = TRUE), 2),
      sd_cost_visits_per_year = round(sd(visits_costs_per_year, na.rm = TRUE), 2),
      min_cost_visits_per_year = round(min(visits_costs_per_year, na.rm = TRUE), 2),
      max_cost_visits_per_year = round(max(visits_costs_per_year, na.rm = TRUE), 2),
      num_subjects_visited = n_distinct(subject_id),
      num_non_users = sum(user_status == "Non-User"),
      .groups = "drop"
    ) %>% 
    mutate(num_subjects_visited = ifelse(specialty_reduced == "Not Clinically Relevant", num_non_users, num_subjects_visited)) %>%
    select(!num_non_users) 
  
  return(list(user_only_cost_summary_reduced = user_only_cost_summary_reduced, 
              all_cost_summary_reduced = all_cost_summary_reduced, 
              tot_exposed_yrs_all = tot_exposed_yrs_all, 
              tot_exposed_yrs_user = tot_exposed_yrs_user))
}

# Cohort summary
cohort_summary <- function(data) {
  
  num_distinct_women <- as.numeric(n_distinct(data$subject_id))
  num_non_service_users <- as.numeric(non_service_users)  
  perc_non_service_users <- round((num_non_service_users / num_distinct_women * 100), 2)
  num_service_users <- num_distinct_women - num_non_service_users
  perc_service_users <- round(100 - perc_non_service_users, 2)
  
  summary <- tibble::tibble(
    num_distinct_women = num_distinct_women,
    num_non_service_users = num_non_service_users,
    perc_non_service_users = perc_non_service_users,
    num_service_users = num_service_users,
    perc_service_users = perc_service_users
  )
  
  return(summary)
}

