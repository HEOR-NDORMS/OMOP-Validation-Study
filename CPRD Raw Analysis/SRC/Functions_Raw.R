library(tidyverse)
library(tidylog)
# for a fracture table, compute the index date for each person
addIndex <- function (fractureTable){
  fractureTable %>% 
    dplyr::inner_join(fractureTable %>% 
                        dplyr::filter(type == "index") %>%
                        dplyr::group_by(patid) %>% 
                        dplyr::summarise(index_date = min(fx_date)),
                      by = "patid"
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
    dplyr::left_join(observations %>% group_by(patid) %>%
                       mutate(obsdate_end = max(obsdate)) %>%
                       select(patid, obsdate_end) %>%
                       distinct(patid, obsdate_end) %>%
                       ungroup(), by = "patid",
                     relationship = "many-to-many", copy = T) %>%
    dplyr::select(patid, aurum_min_accept_date, aurum_max_accept_date, fx_date,
                  site, type, index_date, after_index, obsdate_end)
}

# add in a column indicating the date of cancer after the index date
addInCancerPostIndex <- function (fractureTable){
  fractureTable %>% dplyr::left_join(fractureTable %>% 
                                       dplyr::inner_join(long_cancer_df %>% 
                                                           dplyr::rename("cancer_date" = "condition_date") %>% 
                                                           dplyr::select(!c(condition_date_type, condition)), 
                                                  by = "patid",
                                                  copy = T, 
                                                  relationship = "many-to-many") %>%
                                       dplyr::group_by(patid) %>% 
                                       dplyr::filter(fx_date < cancer_date | is.na(cancer_date)) %>% 
                                       dplyr::ungroup() %>% 
                                       dplyr::distinct() %>% 
                                       dplyr::group_by(patid, aurum_min_accept_date, aurum_max_accept_date, fx_date, 
                                                site, type, index_date, after_index,cancer_date) %>%
                                       dplyr::arrange(cancer_date) %>%
                                       dplyr::filter(row_number()==1) %>%
                                       dplyr::ungroup(),
                                     by = c("patid", "aurum_min_accept_date", "aurum_max_accept_date", "fx_date", 
                                            "site", "type", "index_date", "after_index"))
}

# add in a column indicating the date of bone disease after the index date (has to be done after the cancer one)
addInBoneDiseasePostIndex <- function (fractureTable){
  fractureTable %>% dplyr::left_join(fractureTable %>% 
                                       dplyr::inner_join(long_metabolic_df %>% 
                                                           rename("paget_date" = "condition_date") %>% 
                                                           dplyr::select(!c(condition_date_type, condition)), 
                                                         by = c("patid"),
                                                         copy = T, 
                                                         relationship = "many-to-many") %>%
                                       dplyr::group_by(patid) %>%
                                       dplyr::filter(fx_date < paget_date | is.na(paget_date)) %>% 
                                       dplyr::ungroup() %>% 
                                       dplyr::distinct() %>%
                                       dplyr::group_by(patid, aurum_min_accept_date, aurum_max_accept_date, fx_date, 
                                                       site, type, index_date, after_index) %>%
                                       dplyr::arrange(paget_date) %>%
                                       dplyr::filter(row_number()==1) %>%
                                       dplyr::ungroup(), by = c("patid", "aurum_min_accept_date", "aurum_max_accept_date", "fx_date", 
                                                                "site", "type", "index_date", "after_index"))
}

# add a column of next fracture after the index one
addInNextFracture <- function(fractureTable){
  fractureTable %>% 
    dplyr::left_join(fractureTable %>% 
                       dplyr::filter(fx_date> index_date) %>% 
                       dplyr::group_by(patid) %>% 
                       dplyr::summarise(fracture_after_index = min(fx_date, na.rm =  T)),
                     by = "patid")
}

# add a column of death record date after the index date
addInDeath <- function(fractureTable){
  fractureTable %>% 
    dplyr::filter(death_date > index_date | is.na(death_date)) %>%
    dplyr::select(patid, aurum_min_accept_date, aurum_max_accept_date, fx_date, 
                  site, type, index_date, after_index, cancer_date, paget_date, fracture_after_index, death_date)
}

# add in FOLLOWUPEND - only after the relevant columns are added
addInFollowUpEndGL <- function(fractureTable) {
  fractureTable %>%
    dplyr::mutate(
      follow_up_end = pmin( # follow-up end is the minimum date of possible censoring events being:
        .data$after_index, # 730 after index
        .data$aurum_max_accept_date, # end of data collection
        .data$cancer_fa_index_dt, # cancer
        .data$paget_fa_index_dt, # pagets or mbd
        .data$fracture_after_index, # fracture ensuing index
        .data$death_date, # death
        na.rm = TRUE
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      follow_up_reason = paste(
        c(
          if (!is.na(after_index) & follow_up_end == after_index) "after_index",
          if (!is.na(aurum_max_accept_date) & follow_up_end == aurum_max_accept_date) "observation_period_end_date",
          if (!is.na(cancer_fa_index_dt) & follow_up_end == cancer_fa_index_dt) "cancer_date",
          if (!is.na(paget_fa_index_dt) & follow_up_end == paget_fa_index_dt) "mbd_date",
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
    dplyr::mutate(imminentFracture = as.integer(fx_date == follow_up_end))
}


#clean out fractures based on their follow up period, used for further analysis 
nextFractureClean <- function (fractureTable){
  pre <- 
    fractureTable %>%
    dplyr::anti_join(fractureTable %>% dplyr::filter(imminentFracture==1|follow_up_time <730|(paget_date == follow_up_end)|(cancer_date == follow_up_end)|(death_date == follow_up_end)|(obsdate == follow_up_end)), by = "patid") %>% 
    dplyr::group_by(patid) %>%
    dplyr::arrange(fx_date, .by_group = T) %>%
    dplyr::filter(fx_date>index_date) %>%
    dplyr::select(patid, aurum_min_accept_date, aurum_max_accept_date, fx_date, 
                  site, type) %>%
    dplyr::ungroup()
  
  index_ids <- pre %>% 
    dplyr::filter(type == "index") %>%
    pull(patid)
  
  pre <- pre %>%
    dplyr::filter(patid %in% index_ids)
  return(pre)
}

# turn off the scientific numbers
options(scipen = 999)

# receive washout and trauma done (hence this will be commented out)
# , to avoid negative follow-up time, we will have a flag for each site

# Analyse primary care visits
analyse_visits <- function(visit_data) {
  tot_exposed_yrs_all <- visit_data %>%
    group_by(patid) %>%
    summarise(exposed_yrs = first(exposed_yrs)) %>%
    summarise(tot_exposed_yrs = sum(exposed_yrs, na.rm = TRUE)) %>%
    ungroup() %>% 
    pull(tot_exposed_yrs)
  
  tot_exposed_yrs_user <- visit_data %>%
    filter(user_status == "User", specialty != "Not_clinically_relevant") %>%
    group_by(patid, index_date) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    summarise(tot_exposed_yrs = sum(exposed_yrs), .groups = "drop") %>%
    pull(tot_exposed_yrs)
  
  user_only_summary <- visit_data %>%
    filter(user_status == "User", specialty != "Not_clinically_relevant") %>%
    group_by(patid, specialty) %>%
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
              num_subjects_visited = n_distinct(patid),
              .groups = "drop")
  
  all_summary <- visit_data %>%
    group_by(patid, specialty) %>%
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
              num_subjects_visited = n_distinct(patid),
              num_non_users = sum(user_status == "Non-User"),
              .groups = "drop") %>%
    mutate(num_subjects_visited = ifelse(specialty == "Not_clinically_relevant", num_non_users, num_subjects_visited)) %>%
    select(-num_non_users)
  
  non_service_users <- visit_data %>%
    filter(user_status == "Non-User", visit_count == 0) %>%
    summarise(n = n_distinct(patid)) %>%
    rename(nonservice = n)
  
  return(list(
    user_only_summary = user_only_summary,
    all_summary = all_summary,
    non_service_users = non_service_users,
    tot_exposed_yrs_all = tot_exposed_yrs_all,
    tot_exposed_yrs_user = tot_exposed_yrs_user
  ))
}

# Estimate costs primary care visits 
analyse_visits_cost <- function(visit_data) {
  
  tot_exposed_yrs_all <- visit_data %>%
    group_by(patid) %>%
    summarise(exposed_yrs = first(exposed_yrs)) %>%
    summarise(tot_exposed_yrs = sum(exposed_yrs, na.rm = TRUE)) %>%
    ungroup() %>% 
    pull(tot_exposed_yrs)
  
  tot_exposed_yrs_user <- visit_data %>%
    filter(user_status == "User", specialty != "Not_clinically_relevant") %>%
    group_by(patid, index_date) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    summarise(tot_exposed_yrs = sum(exposed_yrs), .groups = "drop") %>%
    pull(tot_exposed_yrs)
  
  user_only_cost_summary <- visit_data %>%
    filter(user_status == "User", specialty != "Not_clinically_relevant") %>%
    filter(visit_cost > 0) %>%
    group_by(patid, specialty) %>% 
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
      num_subjects_visited = n_distinct(patid),
      .groups = "drop"
    ) %>% 
    ungroup()
  
  all_cost_summary <-  visit_data %>%
    group_by(patid, specialty) %>% 
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
      num_subjects_visited = n_distinct(patid),
      num_non_users = sum(user_status == "Non-User"),
      .groups = "drop"
    ) %>% 
    mutate(num_subjects_visited = ifelse(specialty == "Not_clinically_relevant", num_non_users, num_subjects_visited)) %>%
    select(!num_non_users)
  
  return(list(user_only_cost_summary = user_only_cost_summary, 
              all_cost_summary = all_cost_summary, 
              tot_exposed_yrs_all = tot_exposed_yrs_all, 
              tot_exposed_yrs_user = tot_exposed_yrs_user))
}


# Cohort summary
cohort_summary <- function(data) {
 
  num_distinct_women <- as.numeric(n_distinct(data$patid))
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
