# Visits ----
visit_data_reduced_MIN <- visit_data_reduced %>% select(
  patid, index_date, consdate, user_status,
  specialty_reduced, follow_up_end
)
## Unweighted ----
# Monthly summary stats for visits (no filtering for follow-up)
# Prepare data without filtering on follow_up_end
(monthly_visit_summary_unweighted <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>%
  mutate(month_after_index = interval(index_date, consdate) %/% months(1)) %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = n(), .groups = "drop") %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visits = round(sum(total_visits)/n_distinct(patid),2),
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
  mutate(
    month_after_index = interval(index_date, consdate) %/% months(1),
    month_start = index_date %m+% months(month_after_index),
    month_end = ceiling_date(month_start, unit = "month") - days(1)
  ) %>%
    # Only fully contributing patients
    # only include patients who were still being followed up for the entire month.
    # If a patient left the study early (e.g., died, etc.), 
    # they might have only contributed a few days of data in that month
    # best to calculate month_end based on calendar months, not a fixed number of days like 30.44.
    # Calendar months vary in length (28–31 days).
    # We usually group data by calendar months (e.g., Jan, Feb, Mar), not arbitrary 30.44-day chunks.
    # Using actual start and end dates of calendar months ensures alignment with how healthcare data (like visits or costs) is typically tracked and reported.
  filter(follow_up_end >= month_end) %>%  
  group_by(patid, month_after_index) %>%
  summarise(total_visits = n(), .groups = "drop") %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visits = round(sum(total_visits)/n_distinct(patid),2),
    sd_visits = round(sd(total_visits),2),
    q1 = round(quantile(total_visits, 0.25),2),
    median = round(median(total_visits, 0.50),2),
    q3 = round(quantile(total_visits, 0.75),2),
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
    specialty_reduced, follow_up_end, unit_cost
  )

## Unweighted ----
# Monthly summary stats for visit costs (no filtering for follow-up)
(monthly_summary_costs_unweighted <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>%
   mutate(
     month_after_index = interval(index_date, consdate) %/% months(1),
     month_start = index_date %m+% months(month_after_index),
     month_end = ceiling_date(month_start, unit = "month") - days(1)
   ) %>%
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
  mutate(
    month_after_index = interval(index_date, consdate) %/% months(1),
    month_start = index_date %m+% months(month_after_index),
    month_end = ceiling_date(month_start, unit = "month") - days(1)
  ) %>%
  filter(follow_up_end >= month_end) %>%
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
    sd_cost = round(sd(total_costs), 2),
    q1_cost = round(quantile(total_costs, 0.25), 2),
    median_cost = round(median(total_costs), 2),
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
