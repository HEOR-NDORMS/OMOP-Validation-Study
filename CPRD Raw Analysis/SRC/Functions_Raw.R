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
