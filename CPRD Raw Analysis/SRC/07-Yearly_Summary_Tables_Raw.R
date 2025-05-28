# Visits ----
## Users ----
## For years 1 and 2 users
# Step 1: Identify eligible patients and calculate year-specific exposure
# eligible_patients <- cohort_table_raw %>% 
#   mutate(
#     exposed_days = as.numeric(difftime(follow_up_end, index_date, units = "days")),
#     exposed_yrs = exposed_days / 365.25,
#     included_second_year = exposed_days > 365.25 & (exposed_days - 365.25) >= 30.44 & exposed_days <= 730,
#     year1_exposure = pmin(1, exposed_yrs),
#     year2_exposure = case_when(
#       exposed_yrs > 1 ~ pmin(1, exposed_yrs - 1),
#       TRUE ~ 0
#     )
#   ) %>%
#   select(patid, index_date, included_second_year, year1_exposure, year2_exposure)
# 
# # Step 2: Assign visits to year 1 or year 2 based on consult date
# user_visits_yr <- visit_data %>%
#   filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
#   inner_join(eligible_patients, by = c("patid", "index_date")) %>%
#   mutate(
#     visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")) / 365.25,
#     year_after_index = case_when(
#       visit_yr_from_index < 1 ~ 1,
#       visit_yr_from_index >= 1 & visit_yr_from_index < 2 ~ 2,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%
#   group_by(patid, year_after_index, specialty) %>%
#   summarise(visit_count = n(), .groups = "drop")
# 
# # Step 3: Total visits per patient per year
# user_total_visits_per_patient_yr <- user_visits_yr %>%
#   group_by(patid, year_after_index) %>%
#   summarise(total_visits = sum(visit_count), .groups = "drop") %>%
#   left_join(
#     eligible_patients %>%
#       pivot_longer(cols = c(year1_exposure, year2_exposure),
#                    names_to = "exposure_label",
#                    values_to = "exposure_time") %>%
#       mutate(year_after_index = ifelse(exposure_label == "year1_exposure", 1, 2)) %>%
#       select(patid, year_after_index, exposure_time),
#     by = c("patid", "year_after_index")
#   ) %>%
#   mutate(visits_per_yr = total_visits / exposure_time)
# 
# # Step 4: Summary statistics for visits per year
# user_summary_visits_per_year <- user_total_visits_per_patient_yr %>%
#   group_by(year_after_index) %>%
#   summarise(
#     mean_visit_count = round(mean(visits_per_yr, na.rm = TRUE), 2),
#     min_visits = round(min(visits_per_yr, na.rm = TRUE), 2),
#     max_visits = round(max(visits_per_yr, na.rm = TRUE), 2),
#     sd_visits = round(sd(visits_per_yr, na.rm = TRUE), 2),
#     median_visits = round(median(visits_per_yr, na.rm = TRUE), 2),
#     q1_visits = round(quantile(visits_per_yr, 0.25, na.rm = TRUE), 2),
#     q3_visits = round(quantile(visits_per_yr, 0.75, na.rm = TRUE), 2),
#     .groups = "drop"
#   )
# 
# # Unadjusted: Count total visits per year without exposure adjustment
# # user_summary_unadjusted <- user_visits_yr %>%
# #   group_by(year_after_index) %>%
# #   summarise(
# #     mean_visit_count = round(mean(visit_count, na.rm = TRUE), 2),
# #     min_visits = round(min(visit_count, na.rm = TRUE), 2),
# #     max_visits = round(max(visit_count, na.rm = TRUE), 2),
# #     sd_visits = round(sd(visit_count, na.rm = TRUE), 2),
# #     median_visits = round(median(visit_count, na.rm = TRUE), 2),
# #     q1_visits = round(quantile(visit_count, 0.25, na.rm = TRUE), 2),
# #     q3_visits = round(quantile(visit_count, 0.75, na.rm = TRUE), 2),
# #     .groups = "drop"
# #   )
# 
# 
# 
# 
# ## All Patients ---- 
# # Assign year per visit and join with exposure info
# all_visits_yr <- visit_data %>%
#   mutate(
#     visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")) / 365.25,
#     year_after_index = case_when(
#       visit_yr_from_index < 1 ~ 1,
#       visit_yr_from_index >= 1 & visit_yr_from_index < 2 ~ 2,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   inner_join(eligible_patients, by = "patid") %>%  # add exposure flags
#   filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>% 
#   group_by(patid, year_after_index, specialty) %>%
#   summarise(visit_count = n(), .groups = "drop") %>%
#   mutate(
#     visit_count = if_else(specialty == "Not Clinically Relevant", 0L, visit_count)
#   )
# 
# # Total visits per patient per year, merged with exposure time
# all_total_visits_per_patient_yr <- all_visits_yr %>%
#   group_by(patid, year_after_index) %>%
#   summarise(total_visits = sum(visit_count), .groups = "drop") %>%
#   left_join(
#     eligible_patients %>%
#       pivot_longer(cols = c(year1_exposure, year2_exposure),
#                    names_to = "exposure_label",
#                    values_to = "exposure_time") %>%
#       mutate(year_after_index = ifelse(exposure_label == "year1_exposure", 1, 2)) %>%
#       select(patid, year_after_index, exposure_time),
#     by = c("patid", "year_after_index")
#   ) %>%
#   mutate(visits_per_yr = total_visits / exposure_time)
# 
# # Summary statistics per year
# all_summary_visits_per_year <- all_total_visits_per_patient_yr %>%
#   group_by(year_after_index) %>%
#   summarise(
#     mean_visit_count = round(mean(visits_per_yr, na.rm = TRUE), 2),
#     min_visits = round(min(visits_per_yr, na.rm = TRUE), 2),
#     max_visits = round(max(visits_per_yr, na.rm = TRUE), 2),
#     sd_visits = round(sd(visits_per_yr, na.rm = TRUE), 2),
#     median_visits = round(median(visits_per_yr, na.rm = TRUE), 2),
#     q1_visits = round(quantile(visits_per_yr, 0.25, na.rm = TRUE), 2),
#     q3_visits = round(quantile(visits_per_yr, 0.75, na.rm = TRUE), 2),
#     .groups = "drop"
#   )
# 
# # Export
# yearly_visits_list <- list(
#   user_summary_visits = user_summary_visits_per_year,
#   all_summary_visits = all_summary_visits_per_year
# )
# # Write to an Excel file
# write_xlsx(yearly_visits_list, here::here("CPRD Raw Analysis", "Results", "Output_yearly_visits_Raw.xlsx"))


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


# Step 4: Summary statistics for visits per year
unadjusted_user_summary_visits_per_year <- unadjusted_final_df %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visit_per_year = mean(total_visits),
    min_visit_per_year = min(total_visits),
    max_visit_per_year = max(total_visits),
    sd_visit_per_year = sd(total_visits),
    median_visit_per_year = quantile(total_visits, 0.5),
    lower_q_visits_per_year = quantile(total_visits, 0.25),
    upper_q_visits_per_year = quantile(total_visits, 0.75),
    n = n()
  ) %>%
  mutate(
    se = sd_visit_per_year / sqrt(n),
    lower = mean_visit_per_year - qt(0.975, df = n - 1) * se,
    upper = mean_visit_per_year + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))

# All

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
    mean_visit_per_year = mean(total_visits),
    min_visit_per_year = min(total_visits),
    max_visit_per_year = max(total_visits),
    sd_visit_per_year = sd(total_visits),
    median_visit_per_year = quantile(total_visits, 0.5),
    lower_q_visits_per_year = quantile(total_visits, 0.25),
    upper_q_visits_per_year = quantile(total_visits, 0.75),
    n = n()
  ) %>%
  mutate(
    se = sd_visit_per_year / sqrt(n),
    lower = mean_visit_per_year - qt(0.975, df = n - 1) * se,
    upper = mean_visit_per_year + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))



unadjusted_yearly_visits_list <- list(
  unadjusted_user_summary = unadjusted_user_summary_visits_per_year,
  unadjusted_all_summary = unadjusted_all_summary_visits_per_year
)
# Write to an Excel file
write_xlsx(unadjusted_yearly_visits_list, here::here("CPRD Raw Analysis", "Results", "Output_yearly_visits_Raw-Unadjusted.xlsx"))

## Adjusted ----

# Calculate visits, exposure, and apply rules
# If a patient died during the year → Do not adjust visits.
# If a patient was censored for other reasons (e.g., end of follow-up, other events) → Adjust visits based on the number of days they were exposed.
# If a patient was followed up for less than half a year and did not die → Exclude that year.

# Sense check: 23520018 - dropped, less than 6 months of exposed days
# adjusted_user_visits_yr <- visit_data %>%
#   filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
#   mutate(
#     year_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 365.25 + 1)
#   ) %>%
#   group_by(patid, year_after_index) %>%
#   summarise(
#     total_visits = n(),
#     index_date = first(index_date),
#     follow_up_end = as.Date(first(follow_up_end)),
#     death_date = as.Date(first(death_date)),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     year_start = index_date + years(year_after_index - 1),
#     year_end = index_date + years(year_after_index),
#     
#     # How many days were they observed during this year?
#     exposed_days = as.numeric(pmin(follow_up_end, year_end) - pmax(index_date, year_start) + 1),
#     
#     # Logic flags
#     died_in_year = !is.na(death_date) & death_date >= year_start & death_date <= year_end,
#     fully_contributed = exposed_days >= 365.25,
#     half_contributed = exposed_days >= 182.6
#   ) %>%
#   filter(died_in_year | half_contributed) %>%
#   mutate(
#     adjusted_visits = case_when(
#       died_in_year ~ total_visits,  # Rule 1: no adjustment if died
#       fully_contributed ~ total_visits,  # Full follow-up: no adjustment needed
#       TRUE ~ (total_visits / exposed_days) * 365.25  # Rule 2: adjust for other censoring
#     )
#   )
# 
# # Include missing year combinations (set adjusted visits = 0)
# adjusted_combinations_users <- expand.grid(
#   patid = unique(adjusted_user_visits_yr$patid),
#   year_after_index = 1:2
# )
# 
# adjusted_final_df_users <- merge(adjusted_combinations_users, adjusted_user_visits_yr, by = c("patid", "year_after_index"), all.x = TRUE)
# adjusted_final_df_users$adjusted_visits[is.na(adjusted_final_df_users$adjusted_visits)] <- 0
# 
# # Summary statistics on adjusted visits per year
# adjusted_user_summary_visits_per_year <- adjusted_final_df_users %>%
#   group_by(year_after_index) %>%
#   summarise(
#     mean_visit_per_year = mean(adjusted_visits),
#     min_visit_per_year = min(adjusted_visits),
#     max_visit_per_year = max(adjusted_visits),
#     sd_visit_per_year = sd(adjusted_visits),
#     median_visit_per_year = quantile(adjusted_visits, 0.5),
#     lower_q_visits_per_year = quantile(adjusted_visits, 0.25),
#     upper_q_visits_per_year = quantile(adjusted_visits, 0.75),
#     n = n()
#   ) %>%
#   mutate(
#     se = sd_visit_per_year / sqrt(n),
#     lower = mean_visit_per_year - qt(0.975, df = n - 1) * se,
#     upper = mean_visit_per_year + qt(0.975, df = n - 1) * se
#   ) %>%
#   mutate(across(everything(), round, 2))

library(dplyr)
library(lubridate)

adjusted_user_visits_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  
  # Step 1: Calculate year index of each visit
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_year_after_index = ceiling(days_exposed_total / 365.25),
    year_after_index = floor(as.numeric(difftime(consdate, index_date, units = "days")) / 365.25) + 1,
    died_in_final_year = !is.na(death_date) &
      floor(as.numeric(death_date - index_date) / 365.25) + 1 == final_year_after_index,
    days_into_final_year = days_exposed_total - 365.25 * (final_year_after_index - 1)
  ) %>%
  
  # Step 2: Count visits per year per patient
  group_by(patid, year_after_index) %>%
  summarise(
    total_visits = n(),
    final_year_after_index = first(final_year_after_index),
    days_into_final_year = first(days_into_final_year),
    died_in_final_year = first(died_in_final_year),
    .groups = "drop"
  ) %>%
  
  # Step 3: Adjust visits
  mutate(
    adjusted_visits = case_when(
      year_after_index != final_year_after_index ~ total_visits,  # not final year: no adjustment
      died_in_final_year ~ total_visits,                         # final year + died: no adjustment
      days_into_final_year >= 365.25 ~ total_visits,             # full year: no adjustment
      days_into_final_year >= 182.6 ~ (total_visits / days_into_final_year) * 365.25,  # adjust
      TRUE ~ 0  # drop if < 6 months and not died
    )
  ) %>%
  filter(adjusted_visits > 0)  # drop years with <6 months and not adjusted

# Step 4: Add missing years (adjusted visits = 0)
adjusted_combinations_users <- expand.grid(
  patid = unique(adjusted_user_visits_yr$patid),
  year_after_index = 1:2
)

adjusted_final_df_users <- adjusted_combinations_users %>%
  left_join(adjusted_user_visits_yr, by = c("patid", "year_after_index")) %>%
  mutate(adjusted_visits = ifelse(is.na(adjusted_visits), 0, adjusted_visits))

# Step 5: Summary statistics
adjusted_user_summary_visits_per_year <- adjusted_final_df_users %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visit_per_year = mean(adjusted_visits),
    min_visit_per_year = min(adjusted_visits),
    max_visit_per_year = max(adjusted_visits),
    sd_visit_per_year = sd(adjusted_visits),
    median_visit_per_year = quantile(adjusted_visits, 0.5),
    lower_q_visits_per_year = quantile(adjusted_visits, 0.25),
    upper_q_visits_per_year = quantile(adjusted_visits, 0.75),
    n = n()
  ) %>%
  mutate(
    se = sd_visit_per_year / sqrt(n),
    lower = mean_visit_per_year - qt(0.975, df = n - 1) * se,
    upper = mean_visit_per_year + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))

# # All
# adjusted_all_visits_yr <- visit_data %>%
#   # Assign consultation year relative to index
#   mutate(
#     year_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 365.25 + 1),
#     
#     # Set visit count to 0 if specialty is Not Clinically Relevant
#     visit_flag = if_else(specialty == "Not Clinically Relevant", 0L, 1L)
#   ) %>%
#   
#   # Sum visits per patient per year
#   group_by(patid, year_after_index) %>%
#   summarise(
#     total_visits = sum(visit_flag),  # total relevant visits; NCR visits = 0
#     index_date = first(index_date),
#     follow_up_end = as.Date(first(follow_up_end)),
#     death_date = as.Date(first(death_date)),
#     .groups = "drop"
#   ) %>%
#   
#   # Define calendar boundaries for year N post index
#   mutate(
#     year_start = index_date + years(year_after_index - 1),
#     year_end = index_date + years(year_after_index),
#     
#     # How many days patient was observed in this year window
#     exposed_days = as.numeric(pmin(follow_up_end, year_end) - pmax(index_date, year_start) + 1),
#     
#     # Rules
#     died_in_year = !is.na(death_date) & death_date >= year_start & death_date <= year_end,
#     fully_contributed = exposed_days >= 365.25,
#     half_contributed = exposed_days >= 182.6
#   ) %>%
#   
#   # Drop patients with <6 months follow-up unless they died
#   filter(died_in_year | half_contributed) %>%
#   
#   # Apply adjustment logic
#   mutate(
#     adjusted_visits = case_when(
#       died_in_year ~ total_visits,  # Rule 1: No adjustment if death
#       fully_contributed ~ total_visits,  # Rule 2: Full follow-up, no change
#       TRUE ~ (total_visits / exposed_days) * 365.25  # Rule 3: Adjust for partial
#     )
#   )
# 
# 
# # Include missing year combinations (set adjusted visits = 0)
# adjusted_combinations_all <- expand.grid(
#   patid = unique(adjusted_all_visits_yr$patid),
#   year_after_index = 1:2
# )
# 
# adjusted_final_df_all <- merge(adjusted_combinations_all, adjusted_all_visits_yr, by = c("patid", "year_after_index"), all.x = TRUE)
# adjusted_final_df_all$adjusted_visits[is.na(adjusted_final_df_all$adjusted_visits)] <- 0
# 
# # Summary statistics on adjusted visits per year
# adjusted_all_summary_visits_per_year <- adjusted_final_df_all %>%
#   group_by(year_after_index) %>%
#   summarise(
#     mean_visit_per_year = mean(adjusted_visits),
#     min_visit_per_year = min(adjusted_visits),
#     max_visit_per_year = max(adjusted_visits),
#     sd_visit_per_year = sd(adjusted_visits),
#     median_visit_per_year = quantile(adjusted_visits, 0.5),
#     lower_q_visits_per_year = quantile(adjusted_visits, 0.25),
#     upper_q_visits_per_year = quantile(adjusted_visits, 0.75),
#     n = n()
#   ) %>%
#   mutate(
#     se = sd_visit_per_year / sqrt(n),
#     lower = mean_visit_per_year - qt(0.975, df = n - 1) * se,
#     upper = mean_visit_per_year + qt(0.975, df = n - 1) * se
#   ) %>%
#   mutate(across(everything(), round, 2))

library(dplyr)
library(lubridate)

# Step 1: Assign year since index and define visit flag
adjusted_all_visits_yr <- visit_data %>%
  mutate(
    year_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 365.25 + 1),
    visit_flag = if_else(specialty == "Not Clinically Relevant", 0L, 1L)
  ) %>%
  
  # Step 2: Sum relevant visits per patient-year
  group_by(patid, year_after_index) %>%
  summarise(
    total_visits = sum(visit_flag),
    index_date = first(index_date),
    follow_up_end = as.Date(first(follow_up_end)),
    death_date = as.Date(first(death_date)),
    .groups = "drop"
  ) %>%
  
  # Step 3: Calculate follow-up exposure during each year
  mutate(
    days_since_index = as.numeric(follow_up_end - index_date),
    days_since_death = as.numeric(death_date - index_date),
    
    year_start_days = 365.25 * (year_after_index - 1),
    year_end_days = 365.25 * year_after_index,
    
    exposed_days = pmin(days_since_index, year_end_days) - pmax(0, year_start_days) + 1,
    
    died_in_year = !is.na(death_date) & days_since_death >= year_start_days & days_since_death < year_end_days,
    fully_contributed = exposed_days >= 365.25,
    half_contributed = exposed_days >= 182.6
  ) %>%
  
  # Step 4: Drop patients who contributed <6 months and didn’t die
  filter(died_in_year | half_contributed) %>%
  
  # Step 5: Adjust visits
  mutate(
    adjusted_visits = case_when(
      died_in_year ~ total_visits,
      fully_contributed ~ total_visits,
      TRUE ~ (total_visits / exposed_days) * 365.25
    )
  )

# Step 6: Fill missing (patid, year) combinations
adjusted_combinations_all <- expand.grid(
  patid = unique(adjusted_all_visits_yr$patid),
  year_after_index = 1:2
)

adjusted_final_df_all <- adjusted_combinations_all %>%
  left_join(adjusted_all_visits_yr, by = c("patid", "year_after_index")) %>%
  mutate(adjusted_visits = ifelse(is.na(adjusted_visits), 0, adjusted_visits))

# Step 7: Summarise stats per year
adjusted_all_summary_visits_per_year <- adjusted_final_df_all %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visit_per_year = mean(adjusted_visits),
    min_visit_per_year = min(adjusted_visits),
    max_visit_per_year = max(adjusted_visits),
    sd_visit_per_year = sd(adjusted_visits),
    median_visit_per_year = quantile(adjusted_visits, 0.5),
    lower_q_visits_per_year = quantile(adjusted_visits, 0.25),
    upper_q_visits_per_year = quantile(adjusted_visits, 0.75),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_visit_per_year / sqrt(n),
    lower = mean_visit_per_year - qt(0.975, df = n - 1) * se,
    upper = mean_visit_per_year + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))

adjusted_yearly_visits_list <- list(
  adjusted_user_summary = adjusted_user_summary_visits_per_year,
  adjusted_all_summary = adjusted_all_summary_visits_per_year
)

# Write to an Excel file
write_xlsx(adjusted_yearly_visits_list, here::here("CPRD Raw Analysis", "Results", "Output_yearly_visits_Raw-Adjusted.xlsx"))


##### Boostrap ----
###### Unadjusted User ----
# Define the bootstrap function
fc_grouped <- function(d, i) {
  sampled <- d[i, ]
  mean(sampled$total_visits)
}

set.seed(626)

# Group by year and perform bootstrap per group
boot_user_summary_statistics_per_year <- unadjusted_final_df %>%
  group_by(year_after_index) %>%
  group_split() %>%
  map_df(function(df_group) {
    # Run bootstrap
    boot_result <- boot(df_group, statistic = fc_grouped, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "norm")
    
    # Extract year value
    year_val <- unique(df_group$year_after_index)
    
    # Assemble the results
    tibble(
      year_after_index = year_val,
      mean_boot_visits = round(as.numeric(boot_result$t0), 3),
      lower_ci = round(as.numeric(boot_ci$normal[2]), 3),
      upper_ci = round(as.numeric(boot_ci$normal[3]), 3)
    )
  })

###### Adjusted User ----
# Define the bootstrap function
fc_grouped_adj <- function(d, i) {
  sampled <- d[i, ]
  mean(sampled$adjusted_visits)
}

set.seed(626)

# Group by year and perform bootstrap per group
adj_boot_user_summary_statistics_per_year <- adjusted_final_df_users %>%
  group_by(year_after_index) %>%
  group_split() %>%
  map_df(function(df_group) {
    # Run bootstrap
    boot_result <- boot(df_group, statistic = fc_grouped_adj, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "norm")
    
    # Extract year value
    year_val <- unique(df_group$year_after_index)
    
    # Assemble the results
    tibble(
      year_after_index = year_val,
      mean_boot_visits = round(as.numeric(boot_result$t0), 3),
      lower_ci = round(as.numeric(boot_ci$normal[2]), 3),
      upper_ci = round(as.numeric(boot_ci$normal[3]), 3)
    )
  })

###### Unadjusted All ----
# Define the statistic function
# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')

# Set a seed for reproducibility
set.seed(42)

# Group by year and perform bootstrap per group
boot_all_summary_statistics_per_year <- unadjusted_all_pts_final_df %>%
  group_by(year_after_index) %>%
  group_split() %>%
  map_df(function(df_group) {
    # Run bootstrap
    boot_result <- boot(df_group, statistic = fc_grouped, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "norm")
    
    # Extract year value
    year_val <- unique(df_group$year_after_index)
    
    # Assemble the results
    tibble(
      year_after_index = year_val,
      mean_boot_visits = round(as.numeric(boot_result$t0), 3),
      lower_ci = round(as.numeric(boot_ci$normal[2]), 3),
      upper_ci = round(as.numeric(boot_ci$normal[3]), 3)
    )
  })

###### Adjusted All ----
# Define the statistic function
# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')

# Set a seed for reproducibility
set.seed(42)

# Group by year and perform bootstrap per group
adj_boot_all_summary_statistics_per_year <- adjusted_final_df_all %>%
  group_by(year_after_index) %>%
  group_split() %>%
  map_df(function(df_group) {
    # Run bootstrap
    boot_result <- boot(df_group, statistic = fc_grouped_adj, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "norm")
    
    # Extract year value
    year_val <- unique(df_group$year_after_index)
    
    # Assemble the results
    tibble(
      year_after_index = year_val,
      mean_boot_visits = round(as.numeric(boot_result$t0), 3),
      lower_ci = round(as.numeric(boot_ci$normal[2]), 3),
      upper_ci = round(as.numeric(boot_ci$normal[3]), 3)
    )
  })

boot_visits_list_per_year <- list(
  unadj_boot_all_summary = boot_all_summary_statistics_per_year,
  unadj_boot_user_summary = boot_user_summary_statistics_per_year,
  adj_boot_all_summary = adj_boot_all_summary_statistics_per_year,
  adj_boot_user_summary = adj_boot_user_summary_statistics_per_year
)

write_xlsx(boot_visits_list_per_year, here::here("CPRD Raw Analysis", "Results", "Output_yearly_visits_Raw-Boot.xlsx"))


# Costs ----

## Users ----
# user_costs_yr <- visit_data %>%
#   filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
#   mutate(
#     visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")) / 365.25,
#     year_after_index = case_when(
#       visit_yr_from_index < 1 ~ 1,
#       visit_yr_from_index >= 1 & visit_yr_from_index < 2 ~ 2,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   inner_join(eligible_patients, by = "patid") %>%
#   filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%
#   group_by(patid, year_after_index, specialty) %>%
#   summarise(
#     visit_count = n(),
#     unit_cost = first(unit_cost),
#     visits_cost = visit_count * unit_cost,
#     .groups = "drop"
#   )
# 
# # Total cost per user per year, adjusted by exposure
# user_total_costs_per_patient_yr <- user_costs_yr %>%
#   group_by(patid, year_after_index) %>%
#   summarise(total_costs = sum(visits_cost), .groups = "drop") %>%
#   left_join(
#     eligible_patients %>%
#       pivot_longer(cols = c(year1_exposure, year2_exposure),
#                    names_to = "exposure_label",
#                    values_to = "exposure_time") %>%
#       mutate(year_after_index = ifelse(exposure_label == "year1_exposure", 1, 2)) %>%
#       select(patid, year_after_index, exposure_time),
#     by = c("patid", "year_after_index")
#   ) %>%
#   mutate(visit_costs_per_yr = total_costs / exposure_time)
# 
# # Summary stats for user cost per year
# user_summary_costs_per_year <- user_total_costs_per_patient_yr %>%
#   group_by(year_after_index) %>%
#   summarise(
#     mean_visits_cost = round(mean(visit_costs_per_yr, na.rm = TRUE), 2),
#     min_visits_cost = round(min(visit_costs_per_yr, na.rm = TRUE), 2),
#     max_visits_cost = round(max(visit_costs_per_yr, na.rm = TRUE), 2),
#     sd_visits_cost = round(sd(visit_costs_per_yr, na.rm = TRUE), 2),
#     median_visits_cost = round(median(visit_costs_per_yr, na.rm = TRUE), 2),
#     q1_visits_cost = round(quantile(visit_costs_per_yr, 0.25, na.rm = TRUE), 2),
#     q3_visits_cost = round(quantile(visit_costs_per_yr, 0.75, na.rm = TRUE), 2),
#     .groups = "drop"
#   )
# 
# ## All Patients ----
# all_costs_yr <- visit_data %>%
#   mutate(
#     visit_yr_from_index = as.numeric(difftime(consdate, index_date, units = "days")) / 365.25,
#     year_after_index = case_when(
#       visit_yr_from_index < 1 ~ 1,
#       visit_yr_from_index >= 1 & visit_yr_from_index < 2 ~ 2,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   inner_join(eligible_patients, by = "patid") %>%
#   filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%
#   group_by(patid, year_after_index, specialty) %>%
#   summarise(
#     visit_count = n(),
#     unit_cost = first(unit_cost),
#     visits_cost = visit_count * unit_cost,
#     .groups = "drop"
#   ) %>%
#   mutate(
#     visit_count = if_else(specialty == "Not Clinically Relevant", 0L, visit_count),
#     visits_cost = if_else(specialty == "Not Clinically Relevant", 0, visits_cost)
#   )
# 
# # Total cost per patient per year, adjusted by exposure
# all_total_costs_per_patient_yr <- all_costs_yr %>%
#   group_by(patid, year_after_index) %>%
#   summarise(total_costs = sum(visits_cost), .groups = "drop") %>%
#   left_join(
#     eligible_patients %>%
#       pivot_longer(cols = c(year1_exposure, year2_exposure),
#                    names_to = "exposure_label",
#                    values_to = "exposure_time") %>%
#       mutate(year_after_index = ifelse(exposure_label == "year1_exposure", 1, 2)) %>%
#       select(patid, year_after_index, exposure_time),
#     by = c("patid", "year_after_index")
#   ) %>%
#   mutate(visit_costs_per_yr = total_costs / exposure_time)
# 
# # Summary stats for all-patient cost per year
# all_summary_costs_per_year <- all_total_costs_per_patient_yr %>%
#   group_by(year_after_index) %>%
#   summarise(
#     mean_visits_cost = round(mean(visit_costs_per_yr, na.rm = TRUE), 2),
#     min_visits_cost = round(min(visit_costs_per_yr, na.rm = TRUE), 2),
#     max_visits_cost = round(max(visit_costs_per_yr, na.rm = TRUE), 2),
#     sd_visits_cost = round(sd(visit_costs_per_yr, na.rm = TRUE), 2),
#     median_visits_cost = round(median(visit_costs_per_yr, na.rm = TRUE), 2),
#     q1_visits_cost = round(quantile(visit_costs_per_yr, 0.25, na.rm = TRUE), 2),
#     q3_visits_cost = round(quantile(visit_costs_per_yr, 0.75, na.rm = TRUE), 2),
#     .groups = "drop"
#   )
# 
# # Export
# yearly_costs_list <- list(
#   user_summary_costs = user_summary_costs_per_year,
#   all_summary_costs = all_summary_costs_per_year
# )
# # Write to an Excel file
# write_xlsx(yearly_costs_list, here::here("CPRD Raw Analysis", "Results", "Output_yearly_costs_Raw.xlsx"))
# 

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
    n = n()
  ) %>%
  mutate(
    se = sd_visits_cost / sqrt(n),
    lower = mean_visits_cost - qt(0.975, df = n - 1) * se,
    upper = mean_visits_cost + qt(0.975, df = n - 1) * se
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
    n = n()
  ) %>%
  mutate(
    se = sd_visits_cost / sqrt(n),
    lower = mean_visits_cost - qt(0.975, df = n - 1) * se,
    upper = mean_visits_cost + qt(0.975, df = n - 1) * se
  ) 

unadjusted_yearly_costs_list <- list(
  unadjusted_user_summary = unadjusted_user_summary_costs_per_year,
  unadjusted_all_summary = unadjusted_all_summary_costs_per_year
)
# Write to an Excel file
write_xlsx(unadjusted_yearly_costs_list, here::here("CPRD Raw Analysis", "Results", "Output_yearly_costs_Raw-Unadjusted.xlsx"))


## Adjusted ----
adjusted_user_costs_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  
  # Assign year
  mutate(
    year_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 365.25 + 1)
  ) %>%
  
  # Group per patient-year-specialty
  group_by(patid, year_after_index, specialty) %>%
  summarise(
    total_visits = n(),
    unit_cost = first(unit_cost),
    index_date = first(index_date),
    follow_up_end = first(follow_up_end),
    death_date = first(death_date),
    .groups = "drop"
  ) %>%
  
  # Time window per year after index
  mutate(
    year_start = index_date + years(year_after_index - 1),
    year_end = index_date + years(year_after_index),
    exposed_days = as.numeric(pmin(follow_up_end, year_end) - pmax(index_date, year_start) + 1),
    
    # Adjustment logic
    died_in_year = !is.na(death_date) & death_date >= year_start & death_date <= year_end,
    fully_contributed = exposed_days >= 365.25,
    half_contributed = exposed_days >= 182.6
  ) %>%
  
  # Apply rules
  filter(died_in_year | half_contributed) %>%
  mutate(
    adjusted_visits = case_when(
      died_in_year ~ total_visits,
      fully_contributed ~ total_visits,
      TRUE ~ (total_visits / exposed_days) * 365.25
    ),
    
    # Specialty-specific cost
    adjusted_costs = adjusted_visits * unit_cost
  ) %>% 
  group_by(patid, year_after_index) %>% 
  summarise(adjusted_costs = sum(adjusted_costs),
            .groups = "drop")

# Create a complete dataset with all combinations of patient_id and year (1, 2)
adjusted_user_costs_combinations <- expand.grid(
  patid = unique(adjusted_user_costs_yr$patid),
  year_after_index = 1:2
)
# Explanation: This creates a new data frame (all_combinations) containing every combination of each unique patient_id and the years 1 and 2. So, even if a patient is missing Year 2 in the original data, this ensures they appear in both years.

# Merge with the original data
adjusted_user_costs_final_df <- merge(adjusted_user_costs_combinations, adjusted_user_costs_yr, by = c("patid", "year_after_index"), all.x = TRUE)
# Explanation: This merges all_combinations with the original df on both patient_id and year. The all.x = TRUE makes it a left join, meaning all rows from all_combinations will be kept, even if they don’t exist in the original data (with missing values filled as NA).
# Replace NA visits_per_year with 0
adjusted_user_costs_final_df$adjusted_costs[is.na(adjusted_user_costs_final_df$adjusted_costs)] <- 0


# Summary stats for user cost per year
adjusted_user_summary_costs_per_year <- adjusted_user_costs_final_df %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visits_cost = round(mean(adjusted_costs, na.rm = TRUE), 2),
    min_visits_cost = round(min(adjusted_costs, na.rm = TRUE), 2),
    max_visits_cost = round(max(adjusted_costs, na.rm = TRUE), 2),
    sd_visits_cost = round(sd(adjusted_costs, na.rm = TRUE), 2),
    median_visits_cost = round(median(adjusted_costs, na.rm = TRUE), 2),
    q1_visits_cost = round(quantile(adjusted_costs, 0.25, na.rm = TRUE), 2),
    q3_visits_cost = round(quantile(adjusted_costs, 0.75, na.rm = TRUE), 2),
    n = n()
  ) %>%
  mutate(
    se = sd_visits_cost / sqrt(n),
    lower = mean_visits_cost - qt(0.975, df = n - 1) * se,
    upper = mean_visits_cost + qt(0.975, df = n - 1) * se
  ) 

# All patients
adjusted_all_costs_yr <- visit_data %>%
  
  # Assign year
  mutate(
    year_after_index = trunc(as.numeric(difftime(consdate, index_date, units = "days")) / 365.25 + 1)
  ) %>%
  
  # Group per patient-year-specialty
  group_by(patid, year_after_index, specialty) %>%
  summarise(
    total_visits = n(),
    unit_cost = first(unit_cost),
    index_date = first(index_date),
    follow_up_end = first(follow_up_end),
    death_date = first(death_date),
    .groups = "drop"
  ) %>%
  mutate(
    total_visits = if_else(specialty == "Not Clinically Relevant", 0L, total_visits)
  ) %>% 
  # Time window per year after index
  mutate(
    year_start = index_date + years(year_after_index - 1),
    year_end = index_date + years(year_after_index),
    exposed_days = as.numeric(pmin(follow_up_end, year_end) - pmax(index_date, year_start) + 1),
    
    # Adjustment logic
    died_in_year = !is.na(death_date) & death_date >= year_start & death_date <= year_end,
    fully_contributed = exposed_days >= 365.25,
    half_contributed = exposed_days >= 182.6
  ) %>%
  
  # Apply rules
  filter(died_in_year | half_contributed) %>%
  mutate(
    adjusted_visits = case_when(
      died_in_year ~ total_visits,
      fully_contributed ~ total_visits,
      TRUE ~ (total_visits / exposed_days) * 365.25
    ),
    
    # Specialty-specific cost
    adjusted_costs = adjusted_visits * unit_cost
  ) %>% 
  group_by(patid, year_after_index) %>% 
  summarise(adjusted_costs = sum(adjusted_costs),
            .groups = "drop")

# Create a complete dataset with all combinations of patient_id and year (1, 2)
adjusted_all_costs_combinations <- expand.grid(
  patid = unique(adjusted_all_costs_yr$patid),
  year_after_index = 1:2
)
# Explanation: This creates a new data frame (all_combinations) containing every combination of each unique patient_id and the years 1 and 2. So, even if a patient is missing Year 2 in the original data, this ensures they appear in both years.

# Merge with the original data
adjusted_all_costs_final_df <- merge(adjusted_all_costs_combinations, adjusted_all_costs_yr, by = c("patid", "year_after_index"), all.x = TRUE)
# Explanation: This merges all_combinations with the original df on both patient_id and year. The all.x = TRUE makes it a left join, meaning all rows from all_combinations will be kept, even if they don’t exist in the original data (with missing values filled as NA).
# Replace NA visits_per_year with 0
adjusted_all_costs_final_df$adjusted_costs[is.na(adjusted_all_costs_final_df$adjusted_costs)] <- 0


# Summary stats for all cost per year
adjusted_all_summary_costs_per_year <- adjusted_all_costs_final_df %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visits_cost = round(mean(adjusted_costs, na.rm = TRUE), 2),
    min_visits_cost = round(min(adjusted_costs, na.rm = TRUE), 2),
    max_visits_cost = round(max(adjusted_costs, na.rm = TRUE), 2),
    sd_visits_cost = round(sd(adjusted_costs, na.rm = TRUE), 2),
    median_visits_cost = round(median(adjusted_costs, na.rm = TRUE), 2),
    q1_visits_cost = round(quantile(adjusted_costs, 0.25, na.rm = TRUE), 2),
    q3_visits_cost = round(quantile(adjusted_costs, 0.75, na.rm = TRUE), 2),
    n = n()
  ) %>%
  mutate(
    se = sd_visits_cost / sqrt(n),
    lower = mean_visits_cost - qt(0.975, df = n - 1) * se,
    upper = mean_visits_cost + qt(0.975, df = n - 1) * se
  ) 
adjusted_yearly_costs_list <- list(
  adjusted_user_summary = adjusted_all_summary_costs_per_year,
  adjusted_all_summary = adjusted_user_summary_costs_per_year
)
# Write to an Excel file
write_xlsx(adjusted_yearly_costs_list, here::here("CPRD Raw Analysis", "Results", "Output_yearly_costs_Raw-Adjusted.xlsx"))

##### Boostrap ----
###### Unadjusted User ----
# Define the bootstrap function
fc_grouped_c <- function(d, i) {
  sampled <- d[i, ]
  mean(sampled$total_costs)
}

set.seed(626)

# Group by year and perform bootstrap per group
boot_user_summary_statistics_costs_per_year <- unadjusted_user_costs_final_df %>%
  group_by(year_after_index) %>%
  group_split() %>%
  map_df(function(df_group) {
    # Run bootstrap
    boot_result <- boot(df_group, statistic = fc_grouped_c, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "norm")
    
    # Extract year value
    year_val <- unique(df_group$year_after_index)
    
    # Assemble the results
    tibble(
      year_after_index = year_val,
      mean_boot_visits = round(as.numeric(boot_result$t0), 3),
      lower_ci = round(as.numeric(boot_ci$normal[2]), 3),
      upper_ci = round(as.numeric(boot_ci$normal[3]), 3)
    )
  })

###### Adjusted User ----
# Define the bootstrap function
fc_grouped_c_adj <- function(d, i) {
  sampled <- d[i, ]
  mean(sampled$adjusted_costs)
}

set.seed(626)

# Group by year and perform bootstrap per group
adj_boot_user_summary_statistics_costs_per_year <- adjusted_user_costs_final_df %>%
  group_by(year_after_index) %>%
  group_split() %>%
  map_df(function(df_group) {
    # Run bootstrap
    boot_result <- boot(df_group, statistic = fc_grouped_c_adj, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "norm")
    
    # Extract year value
    year_val <- unique(df_group$year_after_index)
    
    # Assemble the results
    tibble(
      year_after_index = year_val,
      mean_boot_visits = round(as.numeric(boot_result$t0), 3),
      lower_ci = round(as.numeric(boot_ci$normal[2]), 3),
      upper_ci = round(as.numeric(boot_ci$normal[3]), 3)
    )
  })

###### Unadjusted All ----
# Define the statistic function
# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')

# Set a seed for reproducibility
set.seed(42)

# Group by year and perform bootstrap per group
boot_all_summary_statistics_costs_per_year <- unadjusted_all_costs_final_df %>%
  group_by(year_after_index) %>%
  group_split() %>%
  map_df(function(df_group) {
    # Run bootstrap
    boot_result <- boot(df_group, statistic = fc_grouped_c, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "norm")
    
    # Extract year value
    year_val <- unique(df_group$year_after_index)
    
    # Assemble the results
    tibble(
      year_after_index = year_val,
      mean_boot_visits = round(as.numeric(boot_result$t0), 3),
      lower_ci = round(as.numeric(boot_ci$normal[2]), 3),
      upper_ci = round(as.numeric(boot_ci$normal[3]), 3)
    )
  })

###### Adjusted All ----
# Define the statistic function
# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')

# Set a seed for reproducibility
set.seed(42)

# Group by year and perform bootstrap per group
adj_boot_all_summary_statistics_costs_per_year <- adjusted_all_costs_final_df %>%
  group_by(year_after_index) %>%
  group_split() %>%
  map_df(function(df_group) {
    # Run bootstrap
    boot_result <- boot(df_group, statistic = fc_grouped_c_adj, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "norm")
    
    # Extract year value
    year_val <- unique(df_group$year_after_index)
    
    # Assemble the results
    tibble(
      year_after_index = year_val,
      mean_boot_visits = round(as.numeric(boot_result$t0), 3),
      lower_ci = round(as.numeric(boot_ci$normal[2]), 3),
      upper_ci = round(as.numeric(boot_ci$normal[3]), 3)
    )
  })

boot_costs_list_per_year <- list(
  unadj_boot_all_summary = boot_all_summary_statistics_costs_per_year,
  unadj_boot_user_summary = boot_user_summary_statistics_costs_per_year,
  adj_boot_all_summary = adj_boot_all_summary_statistics_costs_per_year,
  adj_boot_user_summary = adj_boot_user_summary_statistics_costs_per_year
)

write_xlsx(boot_costs_list_per_year, here::here("CPRD Raw Analysis", "Results", "Output_yearly_costs_Raw-Boot.xlsx"))


## Visits per specialty ----
(visits_per_specialty <- visit_data %>% 
    filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
    group_by(specialty_reduced) %>% 
    summarise(n_counts = n(), .groups = "drop") %>% 
    mutate(n_percent = round(n_counts/ sum(n_counts) * 100, 4)))
sum(visits_per_specialty$n_percent)

rio::export(visits_per_specialty, here::here("CPRD Raw Analysis", "Results", "Output_visits_per_specialty_Raw.xlsx"))
