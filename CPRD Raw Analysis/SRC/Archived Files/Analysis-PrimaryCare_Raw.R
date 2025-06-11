# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Primary care analysis	
# Date: 10.02.2025
# Last date edited: 13.02.2025
# ****************************************************************************** #

# Service users are defined as patients with records of healthcare encounters 
# with the NHS and their resource use and associated costs will be estimated 
# separately for primary care (hence excluding patients with no record of primary care visits with any relevant healthcare staff),

# Table 5 “validation_staff” contains the requested fields from aurum.staff

# Starting with table_one_raw

cohort_table_raw <- table_one_raw

# HRCU & Cost Analysis ----
## Import the staff and consultation tables ----
# this is to be used for the specialty
validation_staff <- my_dm$`validation_staff` %>%  collect()
validation_consultation <- my_dm$`validation_consultation` %>%  collect()

## Merge with validation_consultation ----
# Filter from index date to follow up end date
visit_data  <- inner_join(cohort_table_raw, validation_consultation ,  
                               by = "patid") %>%
  # in order to count all the visits from index date until follow-up end date.
  filter(consdate >= index_date & consdate <= follow_up_end) 
# left_join it back to the cohort data - so this 13 data will be non-users
# NOTE: 13 patients are being dropped when we filter to have consultation date be
# after or on index date and before or on follow-up end date.

# merge back to cohort data
visit_data.1 <- left_join(
  cohort_table_raw, visit_data,
  by = c("patid", "gender", "yob", "death", "death_date", 
         "aurum_min_accept_date", "aurum_max_accept_date", "site",
         "fx_date", "days_prior_obs", "days_after_obs", "age_fracture",
         "type", "cancer_fa_index_dt", "paget_fa_index_dt", "index_date",
         "after_index", "fracture_after_index", "follow_up_end", 
         "follow_up_reason", "follow_up_time"))
n_distinct(visit_data.1$patid) # sense check, same as the cohort data

## Merge with validation_staff ----
visit_data_final <- left_join(visit_data.1, validation_staff,
                           by = "staffid")

## Keep only distinct rows ----
# - eliminate all visits in the same day with the same specialist by the same patient

(visits_specialist_day <- visit_data_final %>%
   distinct(consdate, patid, description) %>% tally())

visit_data_final <- visit_data_final %>% 
  distinct(consdate, patid, description, .keep_all = TRUE)

## Select columns needed ----
visit_data_final_min <- visit_data_final %>% 
  select(!c(gender, yob, death, death_date,
            fx_date, cancer_fa_index_dt, paget_fa_index_dt)) %>% 
  rename("fracture_site" = "site")

# Remove the visits that are not clinically relevance ----
# the costs are to be updated
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.1.xlsx") %>% 
  janitor::clean_names() %>% 
  # include those with 1 which are clinically relevant
  filter(include == 1)

specialty_source_value_ids <- unique(provider_cost_inputs$specialty_source_value)

# visit_data_final_min.1 <- visit_data_final_min %>% 
#   rename("specialty_source_value" = "description") %>% 
#   mutate(specialty_source_value = case_when(
#     specialty_source_value %in% specialty_source_value_ids ~ specialty_source_value,
#     TRUE ~ NA_character_
#   )) %>% 
#   left_join(provider_cost_inputs,
#             by = "specialty_source_value")  %>% 
#   # Identify if the visit is clinically relevant
#   mutate(is_clinically_relevant = if_else(!is.na(specialty_source_value), 1, 0)) %>%
#   # Determine user status per patient
#   group_by(patid) %>%
#   mutate(user_status = if_else(any(is_clinically_relevant == 1, na.rm = TRUE), "User", "Non-User")) %>%
#   ungroup()

visit_data_final_min.1 <- visit_data_final_min %>% 
  rename("specialty_source_value" = "description") %>% 
  mutate(specialty_source_value = case_when(
    specialty_source_value %in% specialty_source_value_ids ~ specialty_source_value,
    TRUE ~ "Not_clinically_relevant"
  )) %>% 
  left_join(provider_cost_inputs,
            by = "specialty_source_value")  %>% 
  # Identify if the visit is clinically relevant
  mutate(is_clinically_relevant = if_else(
    specialty_source_value == "Not_clinically_relevant", 0, 1)) %>%
  # Determine user status per patient
  group_by(patid) %>%
  mutate(user_status = if_else(any(is_clinically_relevant == 1, na.rm = TRUE), "User", "Non-User")) %>%
  ungroup()

# create summary df of the visits
summary_visits <- visit_data_final_min.1 %>%
  group_by(patid, index_date, specialty_source_value) %>% # we group by index date to ensure each visit is associated with an entry
  summarise(visit_count = n()) %>%
  ungroup()


visit_data_final_min.2 <- left_join(visit_data_final_min.1, summary_visits,
                                    by = c("patid", "index_date", "specialty_source_value")) %>% 
  
  # distinct(patid, specialty_source_value, visit_count, .keep_all = TRUE)
  # Non users are patients with no contacts with the NHS (i.e., no visits at all) or 
  # visits with staff that are not clinically relevant
  # sense check this pt: 3275789521160
  mutate(visit_count = if_else(
    specialty_source_value == "Not_clinically_relevant", 
    0, visit_count)) %>% 
  mutate(unit_cost = if_else(
    specialty_source_value == "Not_clinically_relevant", 
    0, unit_cost)) %>% 
  mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
  mutate(visits_per_year = visit_count / exposed_yrs)  %>%
  # in R 0/0 results to NaN, so I converted this to 0
  mutate(visits_per_year = replace_na(visits_per_year, 0)) %>% 
  mutate(visit_cost = visit_count * unit_cost) %>% 
  mutate(visits_costs_per_year = visit_cost / exposed_yrs) %>%
  mutate(visits_costs_per_year = replace_na(visits_costs_per_year, 0)) %>%
  rename("specialty" = "specialty_source_value")
 
# count of users/non-users
(u_n_summary <- visit_data_final_min.2 %>%
  group_by(user_status) %>%
  summarise(count_of_pts = n_distinct(patid)) %>%
  ungroup())

## Visits - Primary care ----
# NOTE:
# users as those with at least one visit, regardless of who they had it with. 
# So if one had a GP visit and another patient had one with cardiologist they are both considered users.
# Non-user would be someone with a overall count = 0

visits_results_raw <- analyse_visits(visit_data=visit_data_final_min.2)

# Get the output
user_only_summary <- visits_results_raw$user_only_summary
all_summary <- visits_results_raw$all_summary
(non_service_users <- visits_results_raw$non_service_users)
(tot_exposed_yrs_all_v <- visits_results_raw$tot_exposed_yrs_all)
(tot_exposed_yrs_user_v <- visits_results_raw$tot_exposed_yrs_user)


### Overall visits summaries ----
(overall_mean_visits <- mean(visit_data_final_min.2$visit_count))
(overall_median_visits <- median(visit_data_final_min.2$visit_count))
(overall_summary_visits <- summary(visit_data_final_min.2$visit_count, na.rm = TRUE))

(overall_summary_visits_pa <- summary(visit_data_final_min.2$visits_per_year))
overall_summary_visits_pa_df <- data.frame(Statistic = names(overall_summary_visits_pa), 
                                           Value = as.numeric(overall_summary_visits_pa))
# Export the output
library(writexl)

visits_list <- list(
  overall_summary_visits_pa_df = overall_summary_visits_pa_df,
  user_only_summary = user_only_summary,
  all_summary = all_summary
  
)

# Write to an Excel file
# write_xlsx(visits_list, here::here("CPRD Raw Analysis", "Results", "Output_visits_Raw.xlsx"))

## Cost - Primary care ----
cost_results_raw <- analyse_visits_cost(visit_data_final_min.1)

### Get the output grouped by specialty ----
user_only_cost_summary <- cost_results_raw$user_only_cost_summary 
all_cost_summary <- cost_results_raw$all_cost_summary
(tot_exposed_yrs_all_c <- cost_results_raw$tot_exposed_yrs_all)
(tot_exposed_yrs_user_c <- cost_results_raw$tot_exposed_yrs_user)

### Overall cost summaries ----
(overall_mean_cost <- mean(visit_data_final_min.2$visit_cost, na.rm = TRUE))
(overall_median_cost <- median(visit_data_final_min.2$visit_cost, na.rm = TRUE))
(overall_summary_cost <- summary(visit_data_final_min.2$visit_cost, na.rm = TRUE))

(overall_summary_costs_pa <- summary(visit_data_final_min.2$visits_costs_per_year))
overall_summary_costs_pa_df <- data.frame(Statistic = names(overall_summary_costs_pa), 
                                           Value = as.numeric(overall_summary_costs_pa))

costs_list <- list(
  overall_summary_costs_pa_df = overall_summary_costs_pa_df,
  user_only_cost_summary = user_only_cost_summary,
  all_cost_summary = all_cost_summary
  
)

# Write to an Excel file
write_xlsx(costs_list, here::here("CPRD Raw Analysis", "Results", "Output_costs_Raw.xlsx"))

## Cohort summary ----
cohort_summary_raw <- cohort_summary(visit_data_final_min.1)

visits_list <- list(
  overall_summary_visits_pa_df = overall_summary_visits_pa_df,
  user_only_summary = user_only_summary,
  all_summary = all_summary
  
)

# Write to an Excel file
# write_xlsx(visits_list, here::here("CPRD Raw Analysis", "Results", "Output_visits_Raw.xlsx"))
