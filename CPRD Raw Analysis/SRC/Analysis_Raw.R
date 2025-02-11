# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Primary care and Secondary care analysis	
# Date: 10.02.2025
# Last date edited: 10.02.2025
# ****************************************************************************** #

# Service users are defined as patients with records of healthcare encounters 
# with the NHS and their resource use and associated costs will be estimated 
# separately for primary care (hence excluding patients with no record of primary care visits with any relevant healthcare staff),

# Table 5 “validation_staff” contains the requested fields from aurum.staff

# Starting with table_one_raw

cohort_table_raw <- table_one_raw

# HRCU Analysis ----
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

## Merge with validation_staff ----
visit_data_final <- left_join(visit_data, validation_staff,
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

visit_data_final_min.1 <- visit_data_final_min %>% 
  rename("specialty_source_value" = "description") %>% 
  mutate(specialty_source_value = case_when(
    specialty_source_value %in% specialty_source_value_ids ~ specialty_source_value,
    TRUE ~ NA_character_
  )) %>% 
  left_join(provider_cost_inputs,
            by = "specialty_source_value")  %>% 
# Identify if the visit is clinically relevant
mutate(is_clinically_relevant = if_else(!is.na(specialty_source_value), 1, 0)) %>%
  
  # Determine user status per patient
  group_by(patid) %>%
  mutate(user_status = if_else(any(is_clinically_relevant == 1, na.rm = TRUE), "User", "Non-User")) %>%
  ungroup()
# Note: filtering for the clinically relevant specialty, we drop 504 patients, 
# therefore we are left with 2462 patients whom we look at their visits and cost
  

## Visits - Primary care ----
# NOTE:
# users as those with at least one visit, regardless of who they had it with. 
# So if one had a GP visit and another patient had one with cardiologist they are both considered users.
# Non-user would be someone with a overall count = 0
visits_results_raw <- analyse_visits(visit_data_final_min.1)

# Get the output
user_only_summary <- visits_results_raw$user_only_summary
all_summary <- visits_results_raw$all_summary
(non_service_users <- visits_results_raw$non_service_users)
(tot_exposed_yrs_all_v <- visits_results_raw$tot_exposed_yrs_all)
(tot_exposed_yrs_user_v <- visits_results_raw$tot_exposed_yrs_user)


# Overall
summary_visits <- visit_data_final_min.1 %>%
  group_by(patid, index_date, specialty_source_value) %>% # we group by index date to ensure each visit is associated with an entry
  summarise(visit_count = n()) %>%
  ungroup()

# summary_visits_t <- visit_data_final_min.1 %>%
#   group_by(patid) %>% # we group by index date to ensure each visit is associated with an entry
#   summarise(visit_count = n()) %>%
#   ungroup()

# create a temporary df in order to get the overall summary stats for visits
df_temp_visits <- visit_data_final_min.1 %>%
  dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
  left_join(summary_visits, by = c("patid", "specialty_source_value", "index_date")) %>% 
  dplyr::mutate(visits_per_year = visit_count / exposed_yrs)

    
(overall_mean_visits <- mean(df_temp_visits$visit_count))
(overall_median_visits <- median(df_temp_visits$visit_count))
(overall_summary_visits <- summary(df_temp_visits$visit_count, na.rm = TRUE))

(overall_summary_visits_pa <- mean(df_temp_visits$visits_per_year, na.rm = TRU))
rm(df_temp_visits)

## Cost - Primary care ----
cost_results_raw <- analyse_visits_cost(visit_data_final_min.1)

### Get the output grouped by specialty ----
user_only_cost_summary <- cost_results_raw$user_only_cost_summary
all_cost_summary <- cost_results_raw$all_cost_summary
(tot_exposed_yrs_all_c <- cost_results_raw$tot_exposed_yrs_all)
(tot_exposed_yrs_user_c <- cost_results_raw$tot_exposed_yrs_user)

### Overall cost summaries ----
# create a temporary df in order to get the overall summary stats for visits
df_temp_cost <- visit_data_final_min.1 %>%
  left_join(summary_visits, by = c("patid", "specialty_source_value", "index_date")) %>% 
  dplyr::mutate(visit_cost = visit_count * unit_cost) %>% 
  dplyr::select(-unit_cost, -visit_count)


(overall_mean_cost <- mean(df_temp_cost$visit_cost, na.rm = TRUE))
(overall_median_cost <- median(df_temp_cost$visit_cost, na.rm = TRUE))
(overall_summary_cost <- summary(df_temp_cost$visit_cost, na.rm = TRUE))

## Cohort summary ----
cohort_summary_raw <- cohort_summary(visit_data_final_min.1)
