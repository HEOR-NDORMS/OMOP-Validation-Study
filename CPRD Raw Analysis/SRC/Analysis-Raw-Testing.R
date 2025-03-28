## Merge with validation_consultation ----
# Filter from index date to follow up end date
visit_data <- inner_join(cohort_table_raw, validation_consultation,
                         by = "patid"
) %>%
  # in order to count all the visits from index date until follow-up end date.
  filter(consdate >= index_date & consdate <= follow_up_end)
# left_join it back to the cohort data - so this 13 data will be non-users
# NOTE: 13 patients are being dropped when we filter to have consultation date be
# after or on index date and before or on follow-up end date.

## Merge with validation_staff
visit_data_t <- left_join(visit_data, validation_staff, by = "staffid") %>%
  ## Keep only distinct rows
  distinct(consdate, patid, description, .keep_all = TRUE) %>%
  ## Select columns needed
  select(!c(
    gender, yob, death, death_date,
    fx_date, cancer_fa_index_dt, paget_fa_index_dt
  )) %>%
  rename("fracture_site" = "site",
         "specialty" = "description") %>% 
  mutate(exposed_yrs = as.numeric(follow_up_end - index_date) / 365.25)

# Provider unit costs
# the costs are to be updated
# file changed to include the specialty source values that have been grouped 
# into 3: GP, Nurse, Nurse
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.3.xlsx") %>%
  janitor::clean_names() %>%
  # include those with 1 which are clinically relevant
  filter(include == 1) %>%
  select(!notes)

# Remove the visits that are not clinically relevant
specialty_source_value_ids <- unique(provider_cost_inputs$specialty_source_value)


# should be the same as the summary of users by specialty
visit_count_user <- visit_data_t %>%
  filter(specialty %in% specialty_source_value_ids) %>% 
  group_by(patid, specialty) %>% 
  summarise(
    exposed_yrs = first(exposed_yrs),
    visit_count = n(),
    .groups = "drop") %>% 
  mutate(visits_per_yr = visit_count / exposed_yrs)

user_only_summary_statistics_overall <- tibble(
  mean_visit_per_year = round(sum(visit_count_user$visit_count) / sum(visit_count_user$exposed_yrs), 2),
  min_visit_per_year = round(min(visit_count_user$visits_per_yr), 2),
  max_visit_per_year = round(max(visit_count_user$visits_per_yr), 2),
  sd_visit_per_year = round(sd(visit_count_user$visits_per_yr), 2),
  median_visit_per_year = round(quantile(visit_count_user$visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(visit_count_user$visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(visit_count_user$visits_per_yr, probs = (.75)), 2)
)

visit_count_all <- cohort_table_raw %>% select(!exposed_yrs) %>% 
  left_join(visit_count_user, by = c("patid")) %>% 
  select(patid, index_date, follow_up_end, exposed_yrs, specialty, visit_count) %>% 
  mutate(visit_count = replace_na(visit_count, 0),
         exposed_yrs = replace_na(exposed_yrs, 0)) %>% 
  mutate(visits_per_yr = visit_count/ exposed_yrs) %>% 
  mutate(visits_per_yr = replace_na(visits_per_yr, 0))

all_summary_statistics_overall <- tibble(
  mean_visit_per_year = round(sum(visit_count_all$visit_count) / sum(visit_count_all$exposed_yrs), 2),
  min_visit_per_year = round(min(visit_count_all$visits_per_yr), 2),
  max_visit_per_year = round(max(visit_count_all$visits_per_yr), 2),
  sd_visit_per_year = round(sd(visit_count_all$visits_per_yr), 2),
  median_visit_per_year = round(quantile(visit_count_all$visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(visit_count_all$visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(visit_count_all$visits_per_yr, probs = (.75)), 2)
)


# visit_data_t.1 <- visit_data_t %>% 
#   mutate(follow_up_time_n = as.numeric(follow_up_time)) %>% 
#   group_by(patid) %>% 
#   mutate(years_follow_up_end = case_when(
#     follow_up_time_n <= 365.25 ~ 1,
#     follow_up_time_n > 365.25 & follow_up_time_n <= 730 ~ 2)) %>% 
#   ungroup()
