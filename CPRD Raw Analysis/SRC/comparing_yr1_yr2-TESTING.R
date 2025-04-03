# Sense check this patient: 965140120491
# cohort_table_t <- cohort_table %>% 
#   mutate(exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days"))) %>% 
#   mutate(
#     included_first_year = exposed_days >= 30.44 & exposed_days < 365.25, 
#     included_second_year = exposed_days >= 365.25 & (exposed_days - 365.25) >= 30.44 & exposed_days <= 730,
#     excluded_flag = case_when(
#       included_first_year & included_second_year ~ "Include Both Years",
#       included_first_year & !included_second_year ~ "Include First Year Only",
#       !included_first_year & included_second_year ~ "Include Second Year Only",
#       TRUE ~ "Exclude"
#     )
#   )
# Sense check this patient: 965140120491
cohort_table_t <- cohort_table %>% 
  mutate(exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days"))) %>% 
  mutate(
    included_first_year = exposed_days >= 30.44 & exposed_days <= 730,  
    included_second_year = exposed_days > 365.25 & (exposed_days - 365.25) >= 30.44 & exposed_days <= 730,  
    excluded_flag = case_when(
      included_first_year & included_second_year ~ "Include Both Years",
      included_first_year & !included_second_year ~ "Include First Year Only",
      !included_first_year & included_second_year ~ "Include Second Year Only",
      !included_first_year & !included_second_year ~ "Exclude"
    )
  ) 
n_distinct(cohort_table_t$patid)

# year1_cohort_data <- cohort_table %>% 
#   mutate(exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days"))) %>%
#   filter(exposed_days >= 30.44 & exposed_days <= 365.25) %>% 
#   mutate(exposed_years_status = 1)
# 
# year2_cohort_data <- cohort_table %>% 
#   mutate(exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days"))) %>%
#   filter(exposed_days > 365.25 & (exposed_days - 365.25) >= 30.44 & exposed_days <= 730) %>% 
#   mutate(exposed_years_status = 2)
# 
# cohort_data_revised = bind_rows(year1_cohort_data, year2_cohort_data)
# cohort_data_revised_ids <- unique(cohort_data_revised$patid)
# 
# temp_df <- cohort_table %>% 
#   filter(!(patid %in% cohort_data_revised_ids))


# 
# 
# 
# 
# year1_cohort_data.1 <- cohort_table %>% 
#   mutate(exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days"))) %>%
#   mutate(exclude_flag = if_else(exposed_days >= 30.44 & exposed_days <= 365.25, "Include", "Exclude")) %>% 
#   mutate(exposed_years_status = 1)
# 
# year2_cohort_data.2 <- cohort_table %>% 
#   mutate(exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days"))) %>%
#   mutate(exclude_flag = if_else(exposed_days > 365.25 & (exposed_days - 365.25) >= 30.44 & exposed_days <= 730,
#                                 "Include", "Exclude")) %>% 
#   mutate(exposed_years_status = 2)
# 
# cohort_data_revised = bind_rows(year1_cohort_data, year2_cohort_data)
# 
# n_distinct(cohort_data_revised$patid)
# 
# cohort_table_t <- cohort_table %>% 
#   mutate(exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days"))) %>% 
#   mutate(
#     included_first_year = exposed_days >= 30.44 & exposed_days <= 730,  
#     included_second_year = exposed_days > 365.25 & (exposed_days - 365.25) >= 30.44 & exposed_days <= 730,  
#     excluded_flag = case_when(
#       included_first_year & included_second_year ~ "Include Both Years",
#       included_first_year & !included_second_year ~ "Include First Year Only",
#       !included_first_year & included_second_year ~ "Include Second Year Only",
#       TRUE ~ "Exclude"
#     )
#   ) %>%
#   mutate(excluded_flag = case_when(
#     excluded_flag == "Include First Year Only" & (exposed_days - 365.25) < 30.44 & exposed_days > 365.25 ~ "Include First Year, Exclude Second Year",
#     TRUE ~ excluded_flag
#   )) %>% 
#   # mutate(exposed_months = exposed_days / 30.44) %>% 
#   mutate(year1_exclude = if_else(included_first_year == FALSE, "exclude", "include"),
#          year2_exclude = if_else(included_second_year == FALSE, "exclude", "include"))
# 
# 
