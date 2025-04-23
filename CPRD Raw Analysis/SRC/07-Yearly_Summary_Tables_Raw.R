# Visits ----
## Users ----
## For years 1 and 2 users
# Step 1: Identify eligible patients and calculate year-specific exposure
eligible_patients <- cohort_table %>% 
  mutate(
    exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days")),
    exposed_yrs = exposed_days / 365.25,
    included_second_year = exposed_days > 365.25 & (exposed_days - 365.25) >= 30.44 & exposed_days <= 730,
    year1_exposure = pmin(1, exposed_yrs),
    year2_exposure = case_when(
      exposed_yrs > 1 ~ pmin(1, exposed_yrs - 1),
      TRUE ~ 0
    )
  ) %>%
  select(patid, index_date, included_second_year, year1_exposure, year2_exposure)

# Step 2: Assign visits to year 1 or year 2 based on consult date
user_visits_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  inner_join(eligible_patients, by = c("patid", "index_date")) %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")) / 365.25,
    year_after_index = case_when(
      visit_yr_from_index < 1 ~ 1,
      visit_yr_from_index >= 1 & visit_yr_from_index < 2 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%
  group_by(patid, year_after_index, specialty) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 3: Total visits per patient per year
user_total_visits_per_patient_yr <- user_visits_yr %>%
  group_by(patid, year_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop") %>%
  left_join(
    eligible_patients %>%
      pivot_longer(cols = c(year1_exposure, year2_exposure),
                   names_to = "exposure_label",
                   values_to = "exposure_time") %>%
      mutate(year_after_index = ifelse(exposure_label == "year1_exposure", 1, 2)) %>%
      select(patid, year_after_index, exposure_time),
    by = c("patid", "year_after_index")
  ) %>%
  mutate(visits_per_yr = total_visits / exposure_time)

# Step 4: Summary statistics for visits per year
user_summary_visits_per_year <- user_total_visits_per_patient_yr %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visit_count = round(mean(visits_per_yr, na.rm = TRUE), 2),
    min_visits = round(min(visits_per_yr, na.rm = TRUE), 2),
    max_visits = round(max(visits_per_yr, na.rm = TRUE), 2),
    sd_visits = round(sd(visits_per_yr, na.rm = TRUE), 2),
    median_visits = round(median(visits_per_yr, na.rm = TRUE), 2),
    q1_visits = round(quantile(visits_per_yr, 0.25, na.rm = TRUE), 2),
    q3_visits = round(quantile(visits_per_yr, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  )

# user_visits_yr.2 <- visit_data %>%
#   filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
#   mutate(
#     visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")) / 365.25,
#     year_after_index = case_when(
#       visit_yr_from_index < 1 ~ 1,
#       visit_yr_from_index > 1 & visit_yr_from_index < 2 ~ 2,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   group_by(patid, year_after_index, specialty) %>%
#   summarise(visit_count = n(), .groups = "drop")

# Total visits per patient per year
# user_total_visits_per_patient_yr <- user_visits_yr %>%
#   group_by(patid, year_after_index) %>%
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

## All Patients ---- 
# Assign year per visit and join with exposure info
all_visits_yr <- visit_data %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")) / 365.25,
    year_after_index = case_when(
      visit_yr_from_index < 1 ~ 1,
      visit_yr_from_index >= 1 & visit_yr_from_index < 2 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  inner_join(eligible_patients, by = "patid") %>%  # add exposure flags
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>% 
  group_by(patid, year_after_index, specialty) %>%
  summarise(visit_count = n(), .groups = "drop") %>%
  mutate(
    visit_count = if_else(specialty == "Not Clinically Relevant", 0L, visit_count)
  )

# Total visits per patient per year, merged with exposure time
all_total_visits_per_patient_yr <- all_visits_yr %>%
  group_by(patid, year_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop") %>%
  left_join(
    eligible_patients %>%
      pivot_longer(cols = c(year1_exposure, year2_exposure),
                   names_to = "exposure_label",
                   values_to = "exposure_time") %>%
      mutate(year_after_index = ifelse(exposure_label == "year1_exposure", 1, 2)) %>%
      select(patid, year_after_index, exposure_time),
    by = c("patid", "year_after_index")
  ) %>%
  mutate(visits_per_yr = total_visits / exposure_time)

# Summary statistics per year
all_summary_visits_per_year <- all_total_visits_per_patient_yr %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visit_count = round(mean(visits_per_yr, na.rm = TRUE), 2),
    min_visits = round(min(visits_per_yr, na.rm = TRUE), 2),
    max_visits = round(max(visits_per_yr, na.rm = TRUE), 2),
    sd_visits = round(sd(visits_per_yr, na.rm = TRUE), 2),
    median_visits = round(median(visits_per_yr, na.rm = TRUE), 2),
    q1_visits = round(quantile(visits_per_yr, 0.25, na.rm = TRUE), 2),
    q3_visits = round(quantile(visits_per_yr, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Export
yearly_visits_list <- list(
  user_summary_visits = user_summary_visits_per_year,
  all_summary_visits = all_summary_visits_per_year
)
# Write to an Excel file
write_xlsx(yearly_visits_list, here::here("CPRD Raw Analysis", "Results", "Output_yearly_visits_Raw.xlsx"))

# Costs ----

## Users ----
user_costs_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")) / 365.25,
    year_after_index = case_when(
      visit_yr_from_index < 1 ~ 1,
      visit_yr_from_index >= 1 & visit_yr_from_index < 2 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  inner_join(eligible_patients, by = "patid") %>%
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%
  group_by(patid, year_after_index, specialty) %>%
  summarise(
    visit_count = n(),
    unit_cost = first(unit_cost),
    visits_cost = visit_count * unit_cost,
    .groups = "drop"
  )

# Total cost per user per year, adjusted by exposure
user_total_costs_per_patient_yr <- user_costs_yr %>%
  group_by(patid, year_after_index) %>%
  summarise(total_costs = sum(visits_cost), .groups = "drop") %>%
  left_join(
    eligible_patients %>%
      pivot_longer(cols = c(year1_exposure, year2_exposure),
                   names_to = "exposure_label",
                   values_to = "exposure_time") %>%
      mutate(year_after_index = ifelse(exposure_label == "year1_exposure", 1, 2)) %>%
      select(patid, year_after_index, exposure_time),
    by = c("patid", "year_after_index")
  ) %>%
  mutate(visit_costs_per_yr = total_costs / exposure_time)

# Summary stats for user cost per year
user_summary_costs_per_year <- user_total_costs_per_patient_yr %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visits_cost = round(mean(visit_costs_per_yr, na.rm = TRUE), 2),
    min_visits_cost = round(min(visit_costs_per_yr, na.rm = TRUE), 2),
    max_visits_cost = round(max(visit_costs_per_yr, na.rm = TRUE), 2),
    sd_visits_cost = round(sd(visit_costs_per_yr, na.rm = TRUE), 2),
    median_visits_cost = round(median(visit_costs_per_yr, na.rm = TRUE), 2),
    q1_visits_cost = round(quantile(visit_costs_per_yr, 0.25, na.rm = TRUE), 2),
    q3_visits_cost = round(quantile(visit_costs_per_yr, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  )

## All Patients ----
all_costs_yr <- visit_data %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")) / 365.25,
    year_after_index = case_when(
      visit_yr_from_index < 1 ~ 1,
      visit_yr_from_index >= 1 & visit_yr_from_index < 2 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  inner_join(eligible_patients, by = "patid") %>%
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%
  group_by(patid, year_after_index, specialty) %>%
  summarise(
    visit_count = n(),
    unit_cost = first(unit_cost),
    visits_cost = visit_count * unit_cost,
    .groups = "drop"
  ) %>%
  mutate(
    visit_count = if_else(specialty == "Not Clinically Relevant", 0L, visit_count),
    visits_cost = if_else(specialty == "Not Clinically Relevant", 0, visits_cost)
  )

# Total cost per patient per year, adjusted by exposure
all_total_costs_per_patient_yr <- all_costs_yr %>%
  group_by(patid, year_after_index) %>%
  summarise(total_costs = sum(visits_cost), .groups = "drop") %>%
  left_join(
    eligible_patients %>%
      pivot_longer(cols = c(year1_exposure, year2_exposure),
                   names_to = "exposure_label",
                   values_to = "exposure_time") %>%
      mutate(year_after_index = ifelse(exposure_label == "year1_exposure", 1, 2)) %>%
      select(patid, year_after_index, exposure_time),
    by = c("patid", "year_after_index")
  ) %>%
  mutate(visit_costs_per_yr = total_costs / exposure_time)

# Summary stats for all-patient cost per year
all_summary_costs_per_year <- all_total_costs_per_patient_yr %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visits_cost = round(mean(visit_costs_per_yr, na.rm = TRUE), 2),
    min_visits_cost = round(min(visit_costs_per_yr, na.rm = TRUE), 2),
    max_visits_cost = round(max(visit_costs_per_yr, na.rm = TRUE), 2),
    sd_visits_cost = round(sd(visit_costs_per_yr, na.rm = TRUE), 2),
    median_visits_cost = round(median(visit_costs_per_yr, na.rm = TRUE), 2),
    q1_visits_cost = round(quantile(visit_costs_per_yr, 0.25, na.rm = TRUE), 2),
    q3_visits_cost = round(quantile(visit_costs_per_yr, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Export
yearly_costs_list <- list(
  user_summary_costs = user_summary_costs_per_year,
  all_summary_costs = all_summary_costs_per_year
)
# Write to an Excel file
write_xlsx(yearly_costs_list, here::here("CPRD Raw Analysis", "Results", "Output_yearly_costs_Raw.xlsx"))
