# Visits ----
## Users ----
# user_visits_yr <- visit_data %>%
#   filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
#   mutate(
#     visit_yr_from_index = as.numeric(difftime(follow_up_end, index_date, units = "days")) / 365.25,
#     year_after_index = case_when(
#       visit_yr_from_index < 1 ~ 1,
#       visit_yr_from_index > 1 & visit_yr_from_index < 2 ~ 2,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   group_by(subject_id, year_after_index, specialty) %>%
#   summarise(visit_count = n(), .groups = "drop")
# 
# # Total visits per patient per year
# user_total_visits_per_patient_yr <- user_visits_yr %>%
#   group_by(subject_id, year_after_index) %>%
#   summarise(
#     total_visits = sum(visit_count),
#     visits_per_yr = total_visits / year_after_index,  # One full year assumed per year_after_index
#     .groups = "drop"
#   ) %>%
#   distinct()
# 
# # Summary statistics for each year
# user_summary_visits_per_year <- user_total_visits_per_patient_yr %>%
#   group_by(year_after_index) %>%
#   summarise(
#     mean_visit_count = round(mean(visits_per_yr), 2),
#     min_visits = round(min(visits_per_yr), 2),
#     max_visits = round(max(visits_per_yr), 2),
#     sd_visits = round(sd(visits_per_yr), 2),
#     median_visits = round(median(visits_per_yr), 2),
#     q1_visits = round(quantile(visits_per_yr, 0.25), 2),
#     q3_visits = round(quantile(visits_per_yr, 0.75), 2),
#     .groups = "drop"
#   )
# Identify patients who qualify for at least 30.44 days in the second year
eligible_patients <- cohort_table %>% 
  mutate(exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days"))) %>% 
  mutate(
    included_second_year = exposed_days > 365.25 & (exposed_days - 365.25) >= 30.44 & exposed_days <= 730
  ) %>%
  select(subject_id, included_second_year)

# Adjust year_after_index calculation and merge with eligible patients
user_visits_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    raw_interval = interval(index_date, visit_detail_start_date),
    year_after_index = pmin(2, pmax(1, floor(interval(index_date, visit_detail_start_date) / years(1)) + 1))  # Add 1, cap at 2
  ) %>%
  inner_join(eligible_patients, by = "subject_id") %>%  # Keep only eligible patients
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%  # Ensure valid second-year patients
  group_by(subject_id, year_after_index, specialty) %>%
  summarise(visit_count = n(), .groups = "drop")

# Total visits per patient per year
user_total_visits_per_patient_yr <- user_visits_yr %>%
  group_by(subject_id, year_after_index) %>%
  summarise(total_visits = sum(visit_count),
            visits_per_yr = total_visits/year_after_index,
            .groups = "drop") %>% 
  distinct()

# Summary statistics for visits per year
user_summary_visits_per_year <- user_total_visits_per_patient_yr %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visit_count = round(sum(total_visits) /sum(year_after_index), 2),
    min_visits = round(min(visits_per_yr), 2),
    max_visits = round(max(visits_per_yr), 2),
    sd_visits = round(sd(visits_per_yr), 2),
    median_visits = round(median(visits_per_yr), 2),
    q1_visits = round(quantile(visits_per_yr, 0.25), 2),
    q3_visits = round(quantile(visits_per_yr, 0.75), 2),
    .groups = "drop"
  )

## All Patients ---- 

# all_visits_yr <- visit_data %>%
#   mutate(
#     visit_yr_from_index = as.numeric(difftime(follow_up_end, index_date, units = "days")) / 365.25,
#     year_after_index = case_when(
#       visit_yr_from_index < 1 ~ 1,
#       visit_yr_from_index > 1 & visit_yr_from_index < 2 ~ 2,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   group_by(subject_id, year_after_index, specialty) %>%
#   summarise(visit_count = n(), .groups = "drop") %>% 
#   mutate(visit_count = if_else(specialty == "Not Clinically Relevant", 0, visit_count))

all_visits_yr <- visit_data %>%
  mutate(
    raw_interval = interval(index_date, visit_detail_start_date),
    year_after_index = pmin(2, pmax(1, floor(interval(index_date, visit_detail_start_date) / years(1)) + 1))  # Add 1, cap at 2
  ) %>%
  inner_join(eligible_patients, by = "subject_id") %>%  # Keep only eligible patients
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%  # Ensure valid second-year patients
  group_by(subject_id, year_after_index, specialty) %>%
  summarise(visit_count = n(), .groups = "drop") %>% 
  mutate(visit_count = if_else(specialty == "Not Clinically Relevant", 0, visit_count))

# Total visits per patient per year
all_total_visits_per_patient_yr <- all_visits_yr %>%
  group_by(subject_id, year_after_index) %>%
  summarise(total_visits = sum(visit_count),
            visits_per_yr = total_visits/year_after_index,
            .groups = "drop") %>% 
  distinct()

# Total visits per patient per year
all_total_visits_per_patient_yr <- all_visits_yr %>%
  group_by(subject_id, year_after_index) %>%
  summarise(
    total_visits = sum(visit_count),
    visits_per_yr = total_visits / year_after_index,  # One full year assumed per year_after_index
    .groups = "drop"
  ) %>%
  distinct()

# Summary statistics for each year
all_summary_visits_per_year <- all_total_visits_per_patient_yr %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visit_count = round(mean(visits_per_yr), 2),
    min_visits = round(min(visits_per_yr), 2),
    max_visits = round(max(visits_per_yr), 2),
    sd_visits = round(sd(visits_per_yr), 2),
    median_visits = round(median(visits_per_yr), 2),
    q1_visits = round(quantile(visits_per_yr, 0.25), 2),
    q3_visits = round(quantile(visits_per_yr, 0.75), 2),
    .groups = "drop"
  )

# Export
yearly_visits_list <- list(
  user_summary_visits = user_summary_visits_per_year,
  all_summary_visits = all_summary_visits_per_year
)

# Write to an Excel file
write_xlsx(yearly_visits_list, here::here("OMOP Analysis", "Results", "Output_yearly_visits_OMOP.xlsx"))

# Costs ----
## Users ----
# user_costs_yr <- visit_data %>%
#   filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
#   mutate(
#     visit_yr_from_index = as.numeric(difftime(follow_up_end, index_date, units = "days")) / 365.25,
#     year_after_index = case_when(
#       visit_yr_from_index < 1 ~ 1,
#       visit_yr_from_index > 1 & visit_yr_from_index < 2 ~ 2,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   group_by(subject_id, year_after_index, specialty) %>%
#   summarise(visit_count = n(),
#             unit_cost = first(unit_cost),
#             visits_cost = visit_count * unit_cost,
#             .groups = "drop")
user_costs_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    raw_interval = interval(index_date, visit_detail_start_date),
    year_after_index = pmin(2, pmax(1, floor(interval(index_date, visit_detail_start_date) / years(1)) + 1))  # Add 1, cap at 2
  ) %>%
  inner_join(eligible_patients, by = "subject_id") %>%  # Keep only eligible patients
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%  # Ensure valid second-year patients
  group_by(subject_id, year_after_index, specialty) %>%
  summarise(visit_count = n(),
            unit_cost = first(unit_cost),
            visits_cost = visit_count * unit_cost,
            .groups = "drop")

# Total visit costs per patient per year
user_total_costs_per_patient_yr <- user_costs_yr %>%
  group_by(subject_id, year_after_index) %>%
  summarise(
    total_costs = sum(visits_cost),
    visit_costs_per_yr = total_costs / year_after_index,  # One full year assumed per year_after_index
    .groups = "drop"
  ) %>%
  distinct()

# Summary statistics for each year
user_summary_costs_per_year <- user_total_costs_per_patient_yr %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visits_cost = round(sum(total_costs) / sum(year_after_index), 2),
    min_visits_cost = round(min(visit_costs_per_yr), 2),
    max_visits_cost = round(max(visit_costs_per_yr), 2),
    sd_visits_cost = round(sd(visit_costs_per_yr), 2),
    median_visits_cost = round(median(visit_costs_per_yr), 2),
    q1_visits_cost = round(quantile(visit_costs_per_yr, 0.25), 2),
    q3_visits_cost = round(quantile(visit_costs_per_yr, 0.75), 2),
    .groups = "drop"
  )

## All Patients ----
# all_costs_yr <- visit_data %>%
#   mutate(
#     visit_yr_from_index = as.numeric(difftime(follow_up_end, index_date, units = "days")) / 365.25,
#     year_after_index = case_when(
#       visit_yr_from_index < 1 ~ 1,
#       visit_yr_from_index > 1 & visit_yr_from_index < 2 ~ 2,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   group_by(subject_id, year_after_index, specialty) %>%
#   summarise(visit_count = n(),
#             unit_cost = first(unit_cost),
#             .groups = "drop") %>% 
#   mutate(visit_count = if_else(specialty == "Not Clinically Relevant", 0, visit_count)) %>% 
#   group_by(subject_id, year_after_index, specialty) %>%
#   summarise(visits_cost = visit_count * unit_cost,
#             .groups = "drop") %>% 
#   mutate(visits_cost = if_else(specialty == "Not Clinically Relevant", 0, visits_cost))

all_costs_yr <- visit_data %>%
  mutate(
    raw_interval = interval(index_date, visit_detail_start_date),
    year_after_index = pmin(2, pmax(1, floor(interval(index_date, visit_detail_start_date) / years(1)) + 1))  # Add 1, cap at 2
  ) %>%
  inner_join(eligible_patients, by = "subject_id") %>%  # Keep only eligible patients
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%  # Ensure valid second-year patients
  group_by(subject_id, year_after_index, specialty) %>%
  summarise(visit_count = n(),
            unit_cost = first(unit_cost),
            .groups = "drop") %>% 
  mutate(visit_count = if_else(specialty == "Not Clinically Relevant", 0, visit_count)) %>% 
  group_by(subject_id, year_after_index, specialty) %>%
  summarise(visits_cost = visit_count * unit_cost,
            .groups = "drop") %>% 
  mutate(visits_cost = if_else(specialty == "Not Clinically Relevant", 0, visits_cost))

# Total visit costs per patient per year
all_total_costs_per_patient_yr <- all_costs_yr %>%
  group_by(subject_id, year_after_index) %>%
  summarise(
    total_costs = sum(visits_cost),
    visit_costs_per_yr = total_costs / year_after_index,  # One full year assumed per year_after_index
    .groups = "drop"
  ) %>%
  distinct()

# Summary statistics for each year
all_summary_costs_per_year <- all_total_costs_per_patient_yr %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visits_cost = round(sum(total_costs) / sum(year_after_index), 2),
    min_visits_cost = round(min(visit_costs_per_yr), 2),
    max_visits_cost = round(max(visit_costs_per_yr), 2),
    sd_visits_cost = round(sd(visit_costs_per_yr), 2),
    median_visits_cost = round(median(visit_costs_per_yr), 2),
    q1_visits_cost = round(quantile(visit_costs_per_yr, 0.25), 2),
    q3_visits_cost = round(quantile(visit_costs_per_yr, 0.75), 2),
    .groups = "drop"
  )

# Export
yearly_costs_list <- list(
  user_summary_costs = user_summary_costs_per_year,
  all_summary_costs = all_summary_costs_per_year
)
# Write to an Excel file
write_xlsx(yearly_costs_list, here::here("OMOP Analysis", "Results", "Output_yearly_costs_OMOP.xlsx"))
