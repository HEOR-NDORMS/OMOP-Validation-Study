# Visits ----
visit_data_reduced_MIN <- visit_data_reduced %>% select(
  subject_id, index_date, visit_detail_start_date, user_status,
  specialty_reduced, follow_up_end
)

## Unweighted ----
# Monthly summary stats for visits (no filtering for follow-up)
# Prepare data without filtering on follow_up_end
(monthly_visit_summary_unweighted <- visit_data_reduced_MIN %>%
   filter(specialty_reduced != "Not Clinically Relevant", 
          user_status == "User") %>%
   mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
   group_by(subject_id, month_after_index) %>%
   summarise(total_visits = n(), .groups = "drop") %>%
   group_by(month_after_index) %>%
   summarise(
     mean_visits = round(sum(total_visits)/n_distinct(subject_id),2),
     min = round(min(total_visits),2),
     max = round(max(total_visits),2),
     sd_visits = round(sd(total_visits),2),
     q1 = round(quantile(total_visits, 0.25),2),
     median = round(median(total_visits, 0.50),2),
     q3 = round(quantile(total_visits, 0.75),2),
     n_pts = n_distinct(subject_id)
   ))

## Weighted ----
# Monthly summary stats for visits (filtered)
# Prepare data with filtering on follow_up_end
(monthly_visit_summary_weighted <- visit_data_reduced_MIN %>%
   filter(specialty_reduced != "Not Clinically Relevant", 
          user_status == "User") %>%
   mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
   mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44))) %>% 
   group_by(subject_id) %>% 
   filter(full_month_after_index >= month_after_index) %>%  # Fully contributing patients only
   # removed 29,293 rows
   ungroup() %>% 
   group_by(subject_id, month_after_index) %>%
   summarise(total_visits = n(), .groups = "drop") %>%
   group_by(month_after_index) %>%
   summarise(
     mean_visits = round(sum(total_visits)/n_distinct(subject_id),2),
     min = round(min(total_visits),2),
     max = round(max(total_visits),2),
     sd_visits = round(sd(total_visits),2),
     q1 = round(quantile(total_visits, 0.25),2),
     median = round(median(total_visits, 0.50),2),
     q3 = round(quantile(total_visits, 0.75),2),
     n_pts = n_distinct(subject_id)
   ))

# Export
monthly_visits_list <- list(
  monthly_visit_unweighted = monthly_visit_summary_unweighted,
  monthly_visit_weighted = monthly_visit_summary_weighted
)
library(writexl)

# Write to an Excel file
write_xlsx(monthly_visits_list, here::here("OMOP Analysis", "Results", "Output_monthly_visits_OMOP.xlsx"))


# Costs ----

visit_data_reduced_MIN_c <- visit_data_reduced %>% 
  select(
    subject_id, index_date, visit_detail_start_date, user_status,
    specialty_reduced, follow_up_end, unit_cost
  )

## Unweighted ----
# Monthly summary stats for visit costs (no filtering for follow-up)
(monthly_summary_costs_unweighted <- visit_data_reduced_MIN_c %>%
   filter(specialty_reduced != "Not Clinically Relevant", 
          user_status == "User") %>%
   mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
   group_by(subject_id, month_after_index, specialty_reduced) %>%
   summarise(
     visit_count = n(),
     visit_costs = visit_count * unit_cost,
     .groups = "drop"
   ) %>% 
   distinct(subject_id, month_after_index, specialty_reduced, visit_costs, 
            .keep_all = TRUE) %>% 
   group_by(subject_id, month_after_index) %>%
   summarise(total_costs = sum(visit_costs), .groups = "drop") %>% 
   group_by(month_after_index) %>%
   summarise(
     mean_cost = round(sum(total_costs)/n_distinct(subject_id), 2),
     min_cost = round(min(total_costs), 2),
     max_cost = round(max(total_costs), 2),
     sd_cost = round(sd(total_costs), 2),
     q1_cost = round(quantile(total_costs, 0.25), 2),
     median_cost = round(median(total_costs), 4),
     q3_cost = round(quantile(total_costs, 0.75), 2),
     n_pts = n_distinct(subject_id),
     .groups = "drop"
   ))

## Weighted ----
# Monthly summary stats for visit costs (filtered)
(monthly_summary_costs_weighted <- visit_data_reduced_MIN_c %>%
   filter(specialty_reduced != "Not Clinically Relevant") %>%
   mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
   mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44))) %>% 
   group_by(subject_id) %>% 
   filter(full_month_after_index >= month_after_index) %>%  # Fully contributing patients only
   # removed 29,293 rows
   ungroup() %>%
   group_by(subject_id, month_after_index, specialty_reduced) %>%
   summarise(
     visit_count = n(),
     visit_costs = visit_count * unit_cost,
     .groups = "drop"
   ) %>% 
   distinct(subject_id, month_after_index, specialty_reduced, visit_costs, 
            .keep_all = TRUE) %>% 
   group_by(subject_id, month_after_index) %>%
   summarise(
     total_costs = sum(visit_costs),  
     .groups = "drop"
   ) %>%
   group_by(month_after_index) %>%
   summarise(
     mean_cost = round(sum(total_costs)/n_distinct(subject_id), 2),
     min_cost = round(min(total_costs), 2),
     max_cost = round(max(total_costs), 2),
     sd_cost = round(sd(total_costs), 2),
     q1_cost = round(quantile(total_costs, 0.25), 2),
     median_cost = round(median(total_costs), 4),
     q3_cost = round(quantile(total_costs, 0.75), 2),
     n_pts = n_distinct(subject_id),
     .groups = "drop"
   ))

# Export
monthly_costs_list <- list(
  monthly_costs_unweighted = monthly_summary_costs_unweighted,
  monthly_costs_weighted = monthly_summary_costs_weighted
)

# Write to an Excel file
write_xlsx(monthly_costs_list, here::here("OMOP Analysis", "Results", "Output_monthly_costs_OMOP.xlsx"))

