# Cohort summary
cohort_summary <- function(data) {
  entries_per_woman <- visit_data_final_min.1 %>% 
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    dplyr::group_by(patid) %>% 
    dplyr::summarise(entries_per_woman = n(), 
                     tot_exposed_yrs = sum(exposed_yrs),
                     .groups = "drop") # Drop groups after summarising) 
  
  summary <- tibble(
    # num_distinct_women = n_distinct(data$patid),
    # num_entries = nrow(data),
    tot_exposed_yrs = sum(entries_per_woman$tot_exposed_yrs),
    mean_entries_per_woman = signif(mean(entries_per_woman$entries_per_woman), 4),
    sd_entries_per_woman = signif(sd(entries_per_woman$entries_per_woman), 4),
    min_entries_per_woman = min(entries_per_woman$entries_per_woman),
    max_entries_per_woman = max(entries_per_woman$entries_per_woman),
    # num_non_service_users = unlist(non_service_users),
    # perc_non_service_users = unlist(signif((non_service_users / num_entries * 100), 4)),
    # num_service_users = num_entries - num_non_service_users,
    # perc_service_users = 100-perc_non_service_users,
    num_women_1_entry = sum(entries_per_woman$entries_per_woman == 1),
    num_women_2_entries = sum(entries_per_woman$entries_per_woman == 2),
    num_women_3plus_entries = sum(entries_per_woman$entries_per_woman >= 3)
  )
  
  return(summary)
}
