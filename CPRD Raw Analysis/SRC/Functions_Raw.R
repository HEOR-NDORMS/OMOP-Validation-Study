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


# Analyse primary care visits

analyse_visits <- function(visit_data) {
  
  ### Filtering visits based on the cohort data
  filtered_visits <- visit_data_final_min.1 %>%
    group_by(patid, index_date, specialty_source_value) %>% # we group by index date to ensure each visit is associated with an entry
    summarise(visit_count = n()) %>%
    ungroup() 
  
  ### Pivot the data
  visits_count_wide <- filtered_visits %>%
    pivot_wider(names_from = specialty_source_value, values_from = visit_count, values_fill = NA)
  
  ### Join the wide dataframe back to cohort_combined and count tot num visits
  visits_count_wide <- visit_data_final_min.1 %>% 
    dplyr::left_join(visits_count_wide, by = c("patid", "index_date"), copy = T) 
  
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
    dplyr::group_by(patid, index_date) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(tot_exposed_yrs = sum(exposed_yrs)) %>%
    dplyr::pull(tot_exposed_yrs)
  # 
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
      num_subjects_visited = n_distinct(patid),
      .groups = "drop"
    ) %>% 
    dplyr::ungroup()
  
  ### summary for all subjects (subjects/visits = NA, treated as zero)
  all_summary <- visits_count_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits") %>% 
    tidyr::complete(patid, specialty, fill = list(visits = 0)) %>% # filling missing visits with 0
    dplyr::mutate(visits_per_year = visits / exposed_yrs) %>%
    dplyr::group_by(specialty) %>%
    dplyr::summarise(
      tot_visits = sum(visits),
      mean_visits_per_year = signif(tot_visits / tot_exposed_yrs_all, 4), # Manual calculation of mean
      sd_visits_per_year = signif(sd(visits_per_year, na.rm = TRUE), 4),
      min_visits_per_year = signif(min(visits_per_year, na.rm = TRUE), 4),
      max_visits_per_year = signif(max(visits_per_year, na.rm = TRUE), 4),
      num_subjects_visited = n_distinct(patid),
      .groups = "drop"
    ) %>% 
    dplyr::ungroup() 
  
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

analyse_visits_cost <- function(visit_data) {
  
  ### Filtering visits based on the visit_data_final_min.1
  filtered_visits <- visit_data_final_min.1 %>%
    dplyr::group_by(patid, index_date, specialty_source_value, unit_cost) %>% # we group by index date to ensure each visit is associated with an entry
    dplyr::summarise(visit_count = n(), .groups = "drop") %>%
    dplyr::ungroup() 
  
  ### Compute costs visits
  filtered_visits <- filtered_visits %>%  
    dplyr::mutate(visit_cost = visit_count * unit_cost) %>% 
    dplyr::select(-unit_cost, -visit_count) 
  
  ### Pivot the data
  visits_cost_wide <- filtered_visits %>%
    pivot_wider(names_from = specialty_source_value, values_from = visit_cost, values_fill = 0) 
  
  ### Join the wide dataframe back to visit_data_final_min.1 and count tot num visits
  visits_cost_wide <- visit_data_final_min.1 %>% 
    dplyr::left_join(visits_cost_wide, by = c("patid", "index_date"), copy = T)
  
  ### summary for user only (subjects/visit= NA, not counted)
  included_cols <- colnames(visits_cost_wide)[(colnames(visits_cost_wide) %in% specialty_source_value_ids)]
  
  tot_exposed_yrs_all <- visits_cost_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    dplyr::summarise(tot_exposed_yrs = sum(exposed_yrs)) %>% 
    dplyr::pull(tot_exposed_yrs)
  
  tot_exposed_yrs_user <- visits_cost_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits") %>% 
    dplyr::mutate(visits_per_year = visits / exposed_yrs)  %>%
    dplyr::filter(visits > 0) %>% 
    dplyr::group_by(patid, index_date) %>% 
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
      num_subjects_visited = n_distinct(patid),
      .groups = "drop"
    ) %>% 
    dplyr::ungroup() 
  
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
      num_subjects_visited = n_distinct(patid),
      .groups = "drop"
    )  %>% 
    dplyr::ungroup()
  
  return(list(user_only_cost_summary = user_only_cost_summary, all_cost_summary = all_cost_summary, visits_cost_wide=visits_cost_wide, tot_exposed_yrs_all=tot_exposed_yrs_all, tot_exposed_yrs_user=tot_exposed_yrs_user))
}


# Cohort summary

cohort_summary <- function(data) {
  visits_per_woman <- visit_data_final_min.1 %>% 
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    dplyr::group_by(patid) %>% 
    dplyr::summarise(entries_per_woman = n(), 
                     tot_exposed_yrs = sum(exposed_yrs),
                     .groups = "drop") # Drop groups after summarising) 
  
  summary <- tibble(
    num_distinct_women = n_distinct(data$patid),
    # num_entries = nrow(data),
    # tot_exposed_yrs = sum(entries_per_woman$tot_exposed_yrs),
    # mean_entries_per_woman = signif(mean(entries_per_woman$entries_per_woman), 4),
    # sd_entries_per_woman = signif(sd(entries_per_woman$entries_per_woman), 4),
    # min_entries_per_woman = min(entries_per_woman$entries_per_woman),
    # max_entries_per_woman = max(entries_per_woman$entries_per_woman),
    num_non_service_users = non_service_users,
    perc_non_service_users = signif((non_service_users / num_distinct_women * 100), 4),
    num_service_users = num_distinct_women - num_non_service_users,
    perc_service_users = 100-perc_non_service_users,
    # num_women_1_entry = sum(entries_per_woman$entries_per_woman == 1),
    # num_women_2_entries = sum(entries_per_woman$entries_per_woman == 2),
    # num_women_3plus_entries = sum(entries_per_woman$entries_per_woman >= 3)
  )
  
  return(summary)
}
