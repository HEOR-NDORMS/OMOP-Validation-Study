# Visits ----
visit_data_reduced_MIN <- visit_data_reduced %>% select(
  patid, index_date, consdate, user_status,
  specialty_reduced, follow_up_end, death_date
)
## Unweighted ----
# Monthly summary stats for visits (no filtering for follow-up)
# Prepare data without filtering on follow_up_end
(monthly_visit_summary_unweighted <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>%
   mutate(month_after_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = n(), .groups = "drop") %>%
  group_by(month_after_index) %>%
   # check why we hace NA in the output??, could be in the report
  summarise(
    mean_visits = round(sum(total_visits)/n_distinct(patid),2),
    min = round(min(total_visits),2),
    max = round(max(total_visits),2),
    sd_visits = round(sd(total_visits),2),
    q1 = round(quantile(total_visits, 0.25),2),
    median = round(median(total_visits, 0.50),2),
    q3 = round(quantile(total_visits, 0.75),2),
    n_pts = n_distinct(patid)
  ))

## Weighted ----
# Monthly summary stats for visits (filtered)
# Prepare data with filtering on follow_up_end
(monthly_visit_summary_weighted <- visit_data_reduced_MIN %>%
   filter(specialty_reduced != "Not Clinically Relevant",
          user_status == "User") %>%
   group_by(patid) %>% 
   mutate(
     # Calculate month after index using 30.44 days/month approximation
     month_after_index = trunc((as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1),
     
     # Define month start and end using as.difftime to handle decimal days
     month_start = index_date + as.difftime((month_after_index - 1) * 30.44, units = "days"),
     month_end = index_date + as.difftime(month_after_index * 30.44, units = "days"),
     
     # Calculate exposure details
     exposure_end = as.Date(follow_up_end),
     days_exposed_in_month = as.numeric(pmin(exposure_end, month_end) - pmax(index_date, month_start) + 1),
     
     # Flags for contribution and death
     fully_contributed = days_exposed_in_month >= 30.44,
     half_contributed = days_exposed_in_month >= 15.22,
     died_in_month = !is.na(death_date) & death_date >= month_start & death_date <= month_end
   ) %>%
   ungroup() %>%   
   group_by(patid, month_after_index) %>%
   summarise(
     visit_count = n(),
     days_exposed_in_month = first(days_exposed_in_month),
     fully_contributed = first(fully_contributed),
     half_contributed = first(half_contributed),
     died_in_month = first(died_in_month),
     .groups = "drop"
   ) %>%
   filter(half_contributed == TRUE) %>%
   mutate(
     adjusted_visits = case_when(
       fully_contributed ~ visit_count,
       died_in_month ~ visit_count,  # No adjustment if patient died that month
       TRUE ~ (visit_count / days_exposed_in_month) / 30.44
     )
   ) %>% 
   group_by(month_after_index) %>%
   summarise(
     mean_visits = round(sum(adjusted_visits)/n_distinct(patid),2),
     min = round(min(adjusted_visits),4),
     max = round(max(adjusted_visits),4),
     sd_visits = round(sd(adjusted_visits),2),
     q1 = round(quantile(adjusted_visits, 0.25),2),
     median = round(median(adjusted_visits, 0.50),2),
     q3 = round(quantile(adjusted_visits, 0.75),2),
     n_pts = n_distinct(patid)
   ))


# Export
monthly_visits_list <- list(
  monthly_visit_unweighted = monthly_visit_summary_unweighted,
  monthly_visit_weighted = monthly_visit_summary_weighted
)
# Write to an Excel file
write_xlsx(monthly_visits_list, here::here("CPRD Raw Analysis", "Results", "Output_monthly_visits_Raw.xlsx"))


# Costs ----

visit_data_reduced_MIN_c <- visit_data_reduced %>% 
  select(
    patid, index_date, consdate, user_status,
    specialty_reduced, follow_up_end, unit_cost, death_date
  )

## Unweighted ----
# Monthly summary stats for visit costs (no filtering for follow-up)
(monthly_summary_costs_unweighted <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>%
   mutate(month_after_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
   group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
   distinct(patid, month_after_index, specialty_reduced, visit_costs, 
            .keep_all = TRUE) %>% 
   group_by(patid, month_after_index) %>%
   summarise(total_costs = sum(visit_costs), .groups = "drop") %>% 
  group_by(month_after_index) %>%
  summarise(
    mean_cost = round(sum(total_costs)/n_distinct(patid), 2),
    min_cost = round(min(total_costs), 2),
    max_cost = round(max(total_costs), 2),
    sd_cost = round(sd(total_costs), 2),
    q1_cost = round(quantile(total_costs, 0.25), 2),
    median_cost = round(median(total_costs), 4),
    q3_cost = round(quantile(total_costs, 0.75), 2),
    n_pts = n_distinct(patid),
    .groups = "drop"
  ))

# sense check
# (month_12 <- (monthly_summary_costs_unweighted) %>% 
#     filter(month_after_index ==12) %>% 
#     summarise(median = median(total_costs)))

## Weighted ----
# Monthly summary stats for visit costs (filtered)
(monthly_summary_costs_weighted <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
   mutate(month_after_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
   mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44))) %>% 
   group_by(patid) %>% 
   filter(full_month_after_index >= month_after_index) %>%
   ungroup() %>% 
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct(patid, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE) %>% 
  group_by(patid, month_after_index) %>%
  summarise(
    total_costs = sum(visit_costs),  
    .groups = "drop"
  ) %>%
  group_by(month_after_index) %>%
  summarise(
    mean_cost = round(sum(total_costs)/n_distinct(patid), 2),
    min_cost = round(min(total_costs), 2),
    max_cost = round(max(total_costs), 2),
    sd_cost = round(sd(total_costs), 2),
    q1_cost = round(quantile(total_costs, 0.25), 2),
    median_cost = round(median(total_costs), 4),
    q3_cost = round(quantile(total_costs, 0.75), 2),
    n_pts = n_distinct(patid),
    .groups = "drop"
  ))

# Export
monthly_costs_list <- list(
  monthly_costs_unweighted = monthly_summary_costs_unweighted,
  monthly_costs_weighted = monthly_summary_costs_weighted
)
# Write to an Excel file
write_xlsx(monthly_costs_list, here::here("CPRD Raw Analysis", "Results", "Output_monthly_costs_Raw.xlsx"))
