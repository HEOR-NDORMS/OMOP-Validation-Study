# Visits ----
## Users ----
## For years 1 and 2 users
# Step 1: Identify eligible patients and calculate year-specific exposure
eligible_patients <- cohort_table_raw %>% 
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

# Unadjusted: Count total visits per year without exposure adjustment
# user_summary_unadjusted <- user_visits_yr %>%
#   group_by(year_after_index) %>%
#   summarise(
#     mean_visit_count = round(mean(visit_count, na.rm = TRUE), 2),
#     min_visits = round(min(visit_count, na.rm = TRUE), 2),
#     max_visits = round(max(visit_count, na.rm = TRUE), 2),
#     sd_visits = round(sd(visit_count, na.rm = TRUE), 2),
#     median_visits = round(median(visit_count, na.rm = TRUE), 2),
#     q1_visits = round(quantile(visit_count, 0.25, na.rm = TRUE), 2),
#     q3_visits = round(quantile(visit_count, 0.75, na.rm = TRUE), 2),
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


## Unadjusted ----

# Step 2: Assign visits to year 1 or year 2 based on consult date
unadjusted_user_visits_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")),
    year_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 365.25 + 1)) %>% 
  group_by(patid, year_after_index) %>%
  summarise(total_visits = n(), .groups = "drop")
# for the patient with the no exposure in year two, then the visits should be 0
# same applied in months

# # Step 1: Get the full list of users and the years you want to analyze
# all_users <- visit_data %>%
#   filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
#   distinct(patid)
# 
# # Define the years you care about (e.g., 1 and 2)
# years <- tibble(year_after_index = 1:2)
# 
# # Create full combination
# user_years <- crossing(all_users, years)
# 
# # Step 2: Your current summary
# visit_summary <- visit_data %>%
#   filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
#   mutate(
#     visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")),
#     year_after_index = trunc(visit_yr_from_index / 365.25 + 1)
#   ) %>% 
#   group_by(patid, year_after_index) %>%
#   summarise(total_visits = n(), .groups = "drop")
# 
# # Step 3: Merge and fill missing visits with 0
# unadjusted_user_visits_yr <- user_years %>%
#   left_join(visit_summary, by = c("patid", "year_after_index")) %>%
#   mutate(total_visits = replace_na(total_visits, 0))


# Create a complete dataset with all combinations of patient_id and year (1, 2)
unadjusted_all_combinations <- expand.grid(
  patid = unique(unadjusted_user_visits_yr$patid),
  year_after_index = 1:2
)
# Explanation: This creates a new data frame (all_combinations) containing every combination of each unique patient_id and the years 1 and 2. So, even if a patient is missing Year 2 in the original data, this ensures they appear in both years.

# Merge with the original data
unadjusted_final_df <- merge(unadjusted_all_combinations, unadjusted_user_visits_yr, by = c("patid", "year_after_index"), all.x = TRUE)
# Explanation: This merges all_combinations with the original df on both patient_id and year. The all.x = TRUE makes it a left join, meaning all rows from all_combinations will be kept, even if they don’t exist in the original data (with missing values filled as NA).
# Replace NA visits_per_year with 0
unadjusted_final_df$total_visits[is.na(unadjusted_final_df$total_visits)] <- 0

# %>% 
#   group_by(patid) %>% 
#   mutate(total_visits = if_else(
#     year_after_index == 1 & year_after_index != 2, 0, total_visits
#   )) %>% 
#   mutate(year_after_index = if_else(
#     total_visits == 0, 2, year_after_index
#   )) %>% 
#   ungroup()

# Step 3: Total visits per patient per year
# unadjusted_user_total_visits_per_patient_yr <- unadjusted_user_visits_yr %>%
#   group_by(patid, year_after_index) %>%
#   summarise(total_visits = sum(visit_count), .groups = "drop") 

# Step 4: Summary statistics for visits per year
unadjusted_user_summary_visits_per_year <-  unadjusted_final_df %>%
  group_by(year_after_index) %>%
  summarise(
    sum_visits =  sum(total_visits),
    mean_visit_count = round(mean(total_visits, na.rm = TRUE), 2),
    min_visits = round(min(total_visits, na.rm = TRUE), 2),
    max_visits = round(max(total_visits, na.rm = TRUE), 2),
    sd_visits = round(sd(total_visits, na.rm = TRUE), 2),
    median_visits = round(median(total_visits, na.rm = TRUE), 2),
    q1_visits = round(quantile(total_visits, 0.25, na.rm = TRUE), 2),
    q3_visits = round(quantile(total_visits, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Assign year per visit and join with exposure info
unadjusted_all_visits_yr <- visit_data %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")),
    year_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 365.25 + 1)) %>%
  group_by(patid, year_after_index, specialty) %>%
  summarise(visit_count = n(), .groups = "drop") %>%
  mutate(
    visit_count = if_else(specialty == "Not Clinically Relevant", 0L, visit_count)
  )

# Total visits per patient per year, merged with exposure time
unadjusted_all_total_visits_per_patient_yr <- unadjusted_all_visits_yr %>%
  group_by(patid, year_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")
  
# Create a complete dataset with all combinations of patient_id and year (1, 2)
unadjusted_all_pts_combinations <- expand.grid(
  patid = unique(unadjusted_all_total_visits_per_patient_yr$patid),
  year_after_index = 1:2
)
# Explanation: This creates a new data frame (all_combinations) containing every combination of each unique patient_id and the years 1 and 2. So, even if a patient is missing Year 2 in the original data, this ensures they appear in both years.

# Merge with the original data
unadjusted_all_pts_final_df <- merge(unadjusted_all_pts_combinations, unadjusted_all_total_visits_per_patient_yr, by = c("patid", "year_after_index"), all.x = TRUE)
# Explanation: This merges all_combinations with the original df on both patient_id and year. The all.x = TRUE makes it a left join, meaning all rows from all_combinations will be kept, even if they don’t exist in the original data (with missing values filled as NA).
# Replace NA visits_per_year with 0
unadjusted_all_pts_final_df$total_visits[is.na(unadjusted_all_pts_final_df$total_visits)] <- 0


# Summary statistics per year
unadjusted_all_summary_visits_per_year <- unadjusted_all_pts_final_df %>%
  group_by(year_after_index) %>%
  summarise(
    sum_visits = sum(total_visits),
    mean_visit_count = round(mean(total_visits, na.rm = TRUE), 2),
    min_visits = round(min(total_visits, na.rm = TRUE), 2),
    max_visits = round(max(total_visits, na.rm = TRUE), 2),
    sd_visits = round(sd(total_visits, na.rm = TRUE), 2),
    median_visits = round(median(total_visits, na.rm = TRUE), 2),
    q1_visits = round(quantile(total_visits, 0.25, na.rm = TRUE), 2),
    q3_visits = round(quantile(total_visits, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  )


unadjusted_yearly_visits_list <- list(
  unadjusted_user_summary = unadjusted_user_summary_visits_per_year,
  unadjusted_all_summary = unadjusted_all_summary_visits_per_year
)
# Write to an Excel file
write_xlsx(unadjusted_yearly_visits_list, here::here("CPRD Raw Analysis", "Results", "Output_yearly_visits_Raw-Unadjusted.xlsx"))


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


## Unadjusted ----
unadjusted_user_costs_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")),
    year_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 365.25 + 1)) %>%
  group_by(patid, year_after_index, specialty) %>%
  summarise(
    visit_count = n(),
    unit_cost = unit_cost,
    visits_cost = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct() %>% 
  group_by(patid, year_after_index) %>% 
  summarise(
    total_costs = sum(visits_cost),
    .groups = "drop"
  )


# Create a complete dataset with all combinations of patient_id and year (1, 2)
unadjusted_user_costs_combinations <- expand.grid(
  patid = unique(unadjusted_user_costs_yr$patid),
  year_after_index = 1:2
)
# Explanation: This creates a new data frame (all_combinations) containing every combination of each unique patient_id and the years 1 and 2. So, even if a patient is missing Year 2 in the original data, this ensures they appear in both years.

# Merge with the original data
unadjusted_user_costs_final_df <- merge(unadjusted_user_costs_combinations, unadjusted_user_costs_yr, by = c("patid", "year_after_index"), all.x = TRUE)
# Explanation: This merges all_combinations with the original df on both patient_id and year. The all.x = TRUE makes it a left join, meaning all rows from all_combinations will be kept, even if they don’t exist in the original data (with missing values filled as NA).
# Replace NA visits_per_year with 0
unadjusted_user_costs_final_df$total_costs[is.na(unadjusted_user_costs_final_df$total_costs)] <- 0


# Summary stats for user cost per year
unadjusted_user_summary_costs_per_year <- unadjusted_user_costs_final_df %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visits_cost = round(mean(total_costs, na.rm = TRUE), 2),
    min_visits_cost = round(min(total_costs, na.rm = TRUE), 2),
    max_visits_cost = round(max(total_costs, na.rm = TRUE), 2),
    sd_visits_cost = round(sd(total_costs, na.rm = TRUE), 2),
    median_visits_cost = round(median(total_costs, na.rm = TRUE), 2),
    q1_visits_cost = round(quantile(total_costs, 0.25, na.rm = TRUE), 2),
    q3_visits_cost = round(quantile(total_costs, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  )

# All Pts
unadjusted_all_costs_yr <- visit_data %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")),
    year_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 365.25 + 1)) %>%
  group_by(patid, year_after_index, specialty) %>%
  summarise(
    visit_count = n(),
    unit_cost = unit_cost,
    visits_cost = visit_count * unit_cost,
    .groups = "drop"
  ) %>%
  mutate(
    visit_count = if_else(specialty == "Not Clinically Relevant", 0L, visit_count),
    visits_cost = if_else(specialty == "Not Clinically Relevant", 0, visits_cost)
  ) %>% 
  distinct() %>% 
  group_by(patid, year_after_index) %>% 
  summarise(
    total_costs = sum(visits_cost),
    .groups = "drop"
  )
  

# Total cost per patient per year, adjusted by exposure
# unadjusted_all_total_costs_per_patient_yr <- unadjusted_all_costs_yr %>%
#   group_by(patid, year_after_index) %>%
#   summarise(total_costs = sum(visits_cost), .groups = "drop")

# Create a complete dataset with all combinations of patient_id and year (1, 2)
unadjusted_all_costs_combinations <- expand.grid(
  patid = unique(unadjusted_all_costs_yr$patid),
  year_after_index = 1:2
)
# Explanation: This creates a new data frame (all_combinations) containing every combination of each unique patient_id and the years 1 and 2. So, even if a patient is missing Year 2 in the original data, this ensures they appear in both years.

# Merge with the original data
unadjusted_all_costs_final_df <- merge(unadjusted_all_costs_combinations, unadjusted_all_costs_yr, by = c("patid", "year_after_index"), all.x = TRUE)
# Explanation: This merges all_combinations with the original df on both patient_id and year. The all.x = TRUE makes it a left join, meaning all rows from all_combinations will be kept, even if they don’t exist in the original data (with missing values filled as NA).
# Replace NA visits_per_year with 0
unadjusted_all_costs_final_df$total_costs[is.na(unadjusted_all_costs_final_df$total_costs)] <- 0


# Summary stats for all-patient cost per year
unadjusted_all_summary_costs_per_year <- unadjusted_all_costs_final_df %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visits_cost = round(mean(total_costs, na.rm = TRUE), 2),
    min_visits_cost = round(min(total_costs, na.rm = TRUE), 2),
    max_visits_cost = round(max(total_costs, na.rm = TRUE), 2),
    sd_visits_cost = round(sd(total_costs, na.rm = TRUE), 2),
    median_visits_cost = round(median(total_costs, na.rm = TRUE), 2),
    q1_visits_cost = round(quantile(total_costs, 0.25, na.rm = TRUE), 2),
    q3_visits_cost = round(quantile(total_costs, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Visits per specialty
(visits_per_specialty <- visit_data %>% 
    filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
    group_by(specialty_reduced) %>% 
    summarise(n_counts = n(), .groups = "drop") %>% 
    mutate(n_percent = round(n_counts/ sum(n_counts) * 100, 4)))
sum(visits_per_specialty$n_percent)

