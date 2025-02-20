# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Primary care analysis	
# Date: 10.02.2025
# Last date edited: 20.02.2025
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

# Merge cohort data
# merge back to cohort data
visit_data <- left_join(
  cohort_table_raw, visit_data,
  by = c("patid", "gender", "yob", "death", "death_date", 
         "aurum_min_accept_date", "aurum_max_accept_date", "site",
         "fx_date", "days_prior_obs", "days_after_obs", "age_fracture",
         "type", "cancer_fa_index_dt", "paget_fa_index_dt", "index_date",
         "after_index", "fracture_after_index", "follow_up_end", 
         "follow_up_reason", "follow_up_time"))

## Merge with validation_staff
visit_data <- left_join(visit_data, validation_staff, by = "staffid") %>%
  ## Keep only distinct rows
  distinct(consdate, patid, description, .keep_all = TRUE) %>% 
  ## Select columns needed
  select(!c(gender, yob, death, death_date,
            fx_date, cancer_fa_index_dt, paget_fa_index_dt)) %>% 
  rename("fracture_site" = "site")

# the costs are to be updated
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.1.xlsx") %>% 
  janitor::clean_names() %>% 
  # include those with 1 which are clinically relevant
  filter(include == 1)
 
# count of users/non-users
(u_n_summary <- visit_data_final_min.2 %>%
  group_by(user_status) %>%
  summarise(count_of_pts = n_distinct(patid)) %>%
  ungroup())

# Remove the visits that are not clinically relevant
specialty_source_value_ids <- unique(provider_cost_inputs$specialty_source_value)

visit_data <- visit_data %>% 
  rename("specialty_source_value" = "description") %>% 
  mutate(specialty_source_value = case_when(
    specialty_source_value %in% specialty_source_value_ids ~ specialty_source_value,
    TRUE ~ "Not_clinically_relevant"
  )) %>% 
  left_join(provider_cost_inputs, by = "specialty_source_value")  %>% 
  mutate(is_clinically_relevant = if_else(specialty_source_value == "Not_clinically_relevant", 0, 1)) %>%
  group_by(patid) %>%
  mutate(user_status = if_else(any(is_clinically_relevant == 1, na.rm = TRUE), "User", "Non-User")) %>%
  ungroup()

# Create summary visits
summary_visits <- visit_data %>%
  group_by(patid, index_date, specialty_source_value) %>%
  summarise(visit_count = n(), .groups = "drop")

visit_data <- left_join(visit_data, summary_visits,
                        by = c("patid", "index_date", "specialty_source_value")) %>%
  group_by(patid) %>% 
  mutate(visit_count = if_else(specialty_source_value == "Not_clinically_relevant", 0, visit_count),
         unit_cost = if_else(specialty_source_value == "Not_clinically_relevant", 0, unit_cost),
         exposed_yrs = as.numeric(follow_up_end - index_date) / 365.25,
         visits_per_year = visit_count / exposed_yrs,
         visits_per_year = replace_na(visits_per_year, 0),
         visit_cost = visit_count * unit_cost,
         visits_costs_per_year = visit_cost / exposed_yrs,
         visits_costs_per_year = replace_na(visits_costs_per_year, 0)) %>%
  rename("specialty" = "specialty_source_value") %>% 
  ungroup()

## Visits - Primary care ----
# NOTE:
# users as those with at least one visit, regardless of who they had it with. 
# So if one had a GP visit and another patient had one with cardiologist they are both considered users.
# Non-user would be someone with a overall count = 0
visits_results_raw <- analyse_visits(
  visit_data = visit_data
   )

# Get the output
user_only_summary <- visits_results_raw$user_only_summary
all_summary <- visits_results_raw$all_summary
(non_service_users <- visits_results_raw$non_service_users)
(tot_exposed_yrs_all_v <- visits_results_raw$tot_exposed_yrs_all)
(tot_exposed_yrs_user_v <- visits_results_raw$tot_exposed_yrs_user)


### Overall visits summaries ----
user_only_summary_statistics_df <- visit_data %>%
  filter(user_status == "User", specialty != "Not_clinically_relevant") %>%
  group_by(patid, specialty) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop") %>% 
  group_by(patid) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop")


# mean(user_only_summary_statistics_df$tot_visits_per_yr)

user_only_summary_statistics <- tibble(
  mean_visit_per_year=(sum(user_only_summary_statistics_df$tot_visits)/sum(user_only_summary_statistics_df$tot_exposed_yrs)),
  min_visit_per_year = min(user_only_summary_statistics_df$tot_visits_per_yr),
  max_visit_per_year = max(user_only_summary_statistics_df$tot_visits_per_yr),
  sd_visit_per_year = signif(sd(user_only_summary_statistics_df$tot_visits_per_yr), 4),
  median_visit_per_year = signif(quantile(user_only_summary_statistics_df$tot_visits_per_yr, probs = (.5)), 4),
  lower_q_visits_per_year = signif(quantile(user_only_summary_statistics_df$tot_visits_per_yr, probs = (.25)), 4),
  upper_q_visits_per_year = signif(quantile(user_only_summary_statistics_df$tot_visits_per_yr, probs = (.75)), 4)
)


all_summary_statistics_df <- visit_data %>%
  group_by(patid, specialty) %>%
  summarise(total_patient_visits = first(visit_count),
            total_exposed_yrs = first(exposed_yrs),
            visits_per_year = total_patient_visits / total_exposed_yrs,
            user_status = first(user_status),
            .groups = "drop") %>% 
  group_by(patid) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop") %>% 
  mutate(tot_visits_per_yr = replace_na(tot_visits_per_yr, 0))

all_summary_statistics <- tibble(
  mean_visit_per_year=(sum(all_summary_statistics_df$tot_visits)/sum(all_summary_statistics_df$tot_exposed_yrs)),
  min_visit_per_year = min(all_summary_statistics_df$tot_visits_per_yr),
  max_visit_per_year = max(all_summary_statistics_df$tot_visits_per_yr),
  sd_visit_per_year = signif(sd(all_summary_statistics_df$tot_visits_per_yr),4),
  median_visit_per_year = signif(quantile(all_summary_statistics_df$tot_visits_per_yr, probs = (.5)), 4),
  lower_q_visits_per_year = signif(quantile(all_summary_statistics_df$tot_visits_per_yr, probs = (.25)), 4),
  upper_q_visits_per_year = signif(quantile(all_summary_statistics_df$tot_visits_per_yr, probs = (.75)), 4)
)

# Export the output
library(writexl)

visits_list <- list(
  user_only_summary_statistics = user_only_summary_statistics,
  all_summary_statistics = all_summary_statistics,
  user_only_summary = user_only_summary,
  all_summary = all_summary
  
)

# Write to an Excel file
# write_xlsx(visits_list, here::here("CPRD Raw Analysis", "Results", "Output_visits_Raw.xlsx"))

## Cost - Primary care ----
cost_results_raw <- analyse_visits_cost(
  visit_data = visit_data
  )

### Get the output grouped by specialty ----
user_only_cost_summary <- cost_results_raw$user_only_cost_summary 
all_cost_summary <- cost_results_raw$all_cost_summary
(tot_exposed_yrs_all_c <- cost_results_raw$tot_exposed_yrs_all)
(tot_exposed_yrs_user_c <- cost_results_raw$tot_exposed_yrs_user)

### Overall cost summaries ----
user_cost_summary_statistics_df <- visit_data %>%
   filter(user_status == "User", specialty != "Not_clinically_relevant") %>%
   filter(visit_cost > 0) %>%
   group_by(patid, specialty) %>% 
   summarise(
     total_patient_cost = first(visit_cost),
     total_exposed_yrs = first(exposed_yrs),
     visits_costs_per_year = total_patient_cost / total_exposed_yrs,
     .groups = "drop"
   ) %>% 
   group_by(patid) %>%
   summarise(
     tot_costs = sum(total_patient_cost, na.rm = TRUE),
     tot_exposed_yrs = first(total_exposed_yrs),
     tot_costs_per_yr = tot_costs / tot_exposed_yrs,
     .groups = "drop")
 
user_cost_summary_statistics <- tibble(
   mean_visit_per_year=(sum(user_cost_summary_statistics_df$tot_costs)/sum(user_cost_summary_statistics_df$tot_exposed_yrs)),
   min_visit_per_year = min(user_cost_summary_statistics_df$tot_costs_per_yr),
   max_visit_per_year = max(user_cost_summary_statistics_df$tot_costs_per_yr),
   sd_visit_per_year = signif(sd(user_cost_summary_statistics_df$tot_costs_per_yr), 4),
   median_visit_per_year = signif(quantile(user_cost_summary_statistics_df$tot_costs_per_yr, probs = (.5)), 4),
   lower_q_visits_per_year = signif(quantile(user_cost_summary_statistics_df$tot_costs_per_yr, probs = (.25)), 4),
   upper_q_visits_per_year = signif(quantile(user_cost_summary_statistics_df$tot_costs_per_yr, probs = (.75)), 4)
 )
 
 
all_cost_summary_statistics_df <-  visit_data %>%
   group_by(patid, specialty) %>% 
   summarise(
     total_patient_cost = first(visit_cost),
     total_exposed_yrs = first(exposed_yrs),
     visits_costs_per_year = total_patient_cost / total_exposed_yrs,
     user_status = first(user_status),
     .groups = "drop"
   ) %>% 
   group_by(patid) %>%
   summarise(
     tot_costs = sum(total_patient_cost, na.rm = TRUE),
     tot_exposed_yrs = first(total_exposed_yrs),
     tot_costs_per_yr = tot_costs / tot_exposed_yrs,
     .groups = "drop") %>% 
   mutate(tot_costs_per_yr = replace_na(tot_costs_per_yr, 0))
 
 all_cost_summary_statistics <- tibble(
   mean_visit_per_year=(sum(all_cost_summary_statistics_df$tot_costs)/sum(all_cost_summary_statistics_df$tot_exposed_yrs)),
   min_visit_per_year = min(all_cost_summary_statistics_df$tot_costs_per_yr),
   max_visit_per_year = max(all_cost_summary_statistics_df$tot_costs_per_yr),
   sd_visit_per_year = signif(sd(all_cost_summary_statistics_df$tot_costs_per_yr), 4),
   median_visit_per_year = signif(quantile(all_cost_summary_statistics_df$tot_costs_per_yr, probs = (.5)), 4),
   lower_q_visits_per_year = signif(quantile(all_cost_summary_statistics_df$tot_costs_per_yr, probs = (.25)), 4),
   upper_q_visits_per_year = signif(quantile(all_cost_summary_statistics_df$tot_costs_per_yr, probs = (.75)), 4)
 )
 
costs_list <- list(
  user_cost_summary_statistics = user_cost_summary_statistics,
  all_cost_summary_statistics = all_cost_summary_statistics,
  user_only_cost_summary = user_only_cost_summary,
  all_cost_summary = all_cost_summary
  
)

# Write to an Excel file
write_xlsx(costs_list, here::here("CPRD Raw Analysis", "Results", "Output_costs_Raw.xlsx"))

## Cohort summary ----
cohort_summary_raw <- cohort_summary(visit_data_final_min.1)

# Write to an Excel file
# write_xlsx(cohort_summary_raw, here::here("CPRD Raw Analysis", "Results", "cohort_summary_raw.xlsx"))
