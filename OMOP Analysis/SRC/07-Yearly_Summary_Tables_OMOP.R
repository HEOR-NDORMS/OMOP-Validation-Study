# Visits ----
## Users ----

# Identify patients who qualify for at least 30.44 days in the second year
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
  select(subject_id, index_date, included_second_year, year1_exposure, year2_exposure)


# Step 2: Assign visits to year 1 or year 2 based on consult date
user_visits_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  inner_join(eligible_patients, by = c("subject_id", "index_date")) %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25,
    year_after_index = case_when(
      visit_yr_from_index < 1 ~ 1,
      visit_yr_from_index >= 1 & visit_yr_from_index < 2 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%
  group_by(subject_id, year_after_index, specialty) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 3: Total visits per patient per year
user_total_visits_per_patient_yr <- user_visits_yr %>%
  group_by(subject_id, year_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop") %>%
  left_join(
    eligible_patients %>%
      pivot_longer(cols = c(year1_exposure, year2_exposure),
                   names_to = "exposure_label",
                   values_to = "exposure_time") %>%
      mutate(year_after_index = ifelse(exposure_label == "year1_exposure", 1, 2)) %>%
      select(subject_id, year_after_index, exposure_time),
    by = c("subject_id", "year_after_index")
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

## All Patients ---- 

# Assign year per visit and join with exposure info
all_visits_yr <- visit_data %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25,
    year_after_index = case_when(
      visit_yr_from_index < 1 ~ 1,
      visit_yr_from_index >= 1 & visit_yr_from_index < 2 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  inner_join(eligible_patients, by = "subject_id") %>%  # add exposure flags
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>% 
  group_by(subject_id, year_after_index, specialty) %>%
  summarise(visit_count = n(), .groups = "drop") %>%
  mutate(
    visit_count = if_else(specialty == "Not Clinically Relevant", 0L, visit_count)
  )

# Total visits per patient per year, merged with exposure time
all_total_visits_per_patient_yr <- all_visits_yr %>%
  group_by(subject_id, year_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop") %>%
  left_join(
    eligible_patients %>%
      pivot_longer(cols = c(year1_exposure, year2_exposure),
                   names_to = "exposure_label",
                   values_to = "exposure_time") %>%
      mutate(year_after_index = ifelse(exposure_label == "year1_exposure", 1, 2)) %>%
      select(subject_id, year_after_index, exposure_time),
    by = c("subject_id", "year_after_index")
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
write_xlsx(yearly_visits_list, here::here("OMOP Analysis", "Results", "Output_yearly_visits_OMOP.xlsx"))


## Unadjusted ----

# Step 2: Assign visits to year 1 or year 2 based on consult date
unadjusted_user_visits_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(visit_detail_start_date, index_date, units = "days")),
    year_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25 + 1)) %>% 
  group_by(subject_id, year_after_index) %>%
  summarise(total_visits = n(), .groups = "drop")
# for the patient with the no exposure in year two, then the visits should be 0
# same applied in months



# Create a complete dataset with all combinations of patient_id and year (1, 2)
unadjusted_all_combinations <- expand.grid(
  subject_id = unique(unadjusted_user_visits_yr$subject_id),
  year_after_index = 1:2
)
# Explanation: This creates a new data frame (all_combinations) containing every combination of each unique patient_id and the years 1 and 2. So, even if a patient is missing Year 2 in the original data, this ensures they appear in both years.

# Merge with the original data
unadjusted_users_final_df <- merge(unadjusted_all_combinations, unadjusted_user_visits_yr, by = c("subject_id", "year_after_index"), all.x = TRUE)
# Explanation: This merges all_combinations with the original df on both patient_id and year. The all.x = TRUE makes it a left join, meaning all rows from all_combinations will be kept, even if they don’t exist in the original data (with missing values filled as NA).
# Replace NA visits_per_year with 0
unadjusted_users_final_df$total_visits[is.na(unadjusted_users_final_df$total_visits)] <- 0

# Step 4: Summary statistics for visits per year
unadjusted_user_summary_visits_per_year <- unadjusted_users_final_df %>%
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
    visit_yr_from_index = as.numeric(difftime(visit_detail_start_date, index_date, units = "days")),
    year_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25 + 1)) %>%
  group_by(subject_id, year_after_index, specialty) %>%
  summarise(visit_count = n(), .groups = "drop") %>%
  mutate(
    visit_count = if_else(specialty == "Not Clinically Relevant", 0L, visit_count)
  )

# Total visits per patient per year, merged with exposure time
unadjusted_all_total_visits_per_patient_yr <- unadjusted_all_visits_yr %>%
  group_by(subject_id, year_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Create a complete dataset with all combinations of patient_id and year (1, 2)
unadjusted_all_pts_combinations <- expand.grid(
  subject_id = unique(unadjusted_all_total_visits_per_patient_yr$subject_id),
  year_after_index = 1:2
)
# Explanation: This creates a new data frame (all_combinations) containing every combination of each unique patient_id and the years 1 and 2. So, even if a patient is missing Year 2 in the original data, this ensures they appear in both years.

# Merge with the original data
unadjusted_all_pts_final_df <- merge(unadjusted_all_pts_combinations, unadjusted_all_total_visits_per_patient_yr, by = c("subject_id", "year_after_index"), all.x = TRUE)
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
write_xlsx(unadjusted_yearly_visits_list, here::here("OMOP Analysis", "Results", "Output_yearly_visits_OMOP-Unadjusted.xlsx"))

##### Boostrap ----
###### Unadjusted User ----
# Define the bootstrap function
fc_grouped <- function(d, i) {
  sampled <- d[i, ]
  mean(sampled$total_visits)
}

set.seed(626)

# Group by year and perform bootstrap per group
boot_user_summary_statistics_per_year <- unadjusted_users_final_df %>%
  group_by(year_after_index) %>%
  group_split() %>%
  map_df(function(df_group) {
    # Run bootstrap
    boot_result <- boot(df_group, statistic = fc_grouped, R = 1000)
    boot_ci <- boot.ci(boot.out = boot_result, type = "perc")
    
    # Extract year value
    year_val <- unique(df_group$year_after_index)
    
    # Assemble the results
    tibble(
      year_after_index = year_val,
      mean_boot_visits = round(as.numeric(boot_result$t0), 3),
      lower_ci = round(as.numeric(boot_ci$percent[4]), 3),
      upper_ci = round(as.numeric(boot_ci$percent[5]), 3)
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
    boot_ci <- boot.ci(boot.out = boot_result, type = "perc")
    
    # Extract year value
    year_val <- unique(df_group$year_after_index)
    
    # Assemble the results
    tibble(
      year_after_index = year_val,
      mean_boot_visits = round(as.numeric(boot_result$t0), 3),
      lower_ci = round(as.numeric(boot_ci$percent[4]), 3),
      upper_ci = round(as.numeric(boot_ci$percent[5]), 3)
    )
  })

boot_visits_list_per_year <- list(
  boot_all_summary_stats = boot_all_summary_statistics_per_year,
  boot_user_summary_stats = boot_user_summary_statistics_per_year
)

write_xlsx(boot_visits_list_per_year, here::here("OMOP Analysis", "Results", "Output_yearly_visits_OMOP-Unadjusted_Boot.xlsx"))


# Costs ----
## Users ----
user_costs_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25,
    year_after_index = case_when(
      visit_yr_from_index < 1 ~ 1,
      visit_yr_from_index >= 1 & visit_yr_from_index < 2 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  inner_join(eligible_patients, by = "subject_id") %>%
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%
  group_by(subject_id, year_after_index, specialty) %>%
  summarise(
    visit_count = n(),
    unit_cost = first(unit_cost),
    visits_cost = visit_count * unit_cost,
    .groups = "drop"
  )

# Total cost per user per year, adjusted by exposure
user_total_costs_per_patient_yr <- user_costs_yr %>%
  group_by(subject_id, year_after_index) %>%
  summarise(total_costs = sum(visits_cost), .groups = "drop") %>%
  left_join(
    eligible_patients %>%
      pivot_longer(cols = c(year1_exposure, year2_exposure),
                   names_to = "exposure_label",
                   values_to = "exposure_time") %>%
      mutate(year_after_index = ifelse(exposure_label == "year1_exposure", 1, 2)) %>%
      select(subject_id, year_after_index, exposure_time),
    by = c("subject_id", "year_after_index")
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
    visit_yr_from_index = as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25,
    year_after_index = case_when(
      visit_yr_from_index < 1 ~ 1,
      visit_yr_from_index >= 1 & visit_yr_from_index < 2 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  inner_join(eligible_patients, by = "subject_id") %>%
  filter(year_after_index == 1 | (year_after_index == 2 & included_second_year)) %>%
  group_by(subject_id, year_after_index, specialty) %>%
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
  group_by(subject_id, year_after_index) %>%
  summarise(total_costs = sum(visits_cost), .groups = "drop") %>%
  left_join(
    eligible_patients %>%
      pivot_longer(cols = c(year1_exposure, year2_exposure),
                   names_to = "exposure_label",
                   values_to = "exposure_time") %>%
      mutate(year_after_index = ifelse(exposure_label == "year1_exposure", 1, 2)) %>%
      select(subject_id, year_after_index, exposure_time),
    by = c("subject_id", "year_after_index")
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
write_xlsx(yearly_costs_list, here::here("OMOP Analysis", "Results", "Output_yearly_costs_OMOP.xlsx"))

## Unadjusted ----
unadjusted_user_costs_yr <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(visit_detail_start_date, index_date, units = "days")),
    year_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25 + 1)) %>%
  group_by(subject_id, year_after_index, specialty) %>%
  summarise(
    visit_count = n(),
    unit_cost = unit_cost,
    visits_cost = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct() %>% 
  group_by(subject_id, year_after_index) %>% 
  summarise(
    total_costs = sum(visits_cost),
    .groups = "drop"
  )


# Create a complete dataset with all combinations of patient_id and year (1, 2)
unadjusted_user_costs_combinations <- expand.grid(
  subject_id = unique(unadjusted_user_costs_yr$subject_id),
  year_after_index = 1:2
)
# Explanation: This creates a new data frame (all_combinations) containing every combination of each unique patient_id and the years 1 and 2. So, even if a patient is missing Year 2 in the original data, this ensures they appear in both years.

# Merge with the original data
unadjusted_user_costs_final_df <- merge(unadjusted_user_costs_combinations, unadjusted_user_costs_yr, by = c("subject_id", "year_after_index"), all.x = TRUE)
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
  ) %>%
  mutate(across(everything(), round, 2))

# All Pts
unadjusted_all_costs_yr <- visit_data %>%
  mutate(
    visit_yr_from_index = as.numeric(difftime(visit_detail_start_date, index_date, units = "days")),
    year_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25 + 1)) %>%
  group_by(subject_id, year_after_index, specialty) %>%
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
  group_by(subject_id, year_after_index) %>% 
  summarise(
    total_costs = sum(visits_cost),
    .groups = "drop"
  )



# Create a complete dataset with all combinations of patient_id and year (1, 2)
unadjusted_all_costs_combinations <- expand.grid(
  subject_id = unique(unadjusted_all_costs_yr$subject_id),
  year_after_index = 1:2
)
# Explanation: This creates a new data frame (all_combinations) containing every combination of each unique patient_id and the years 1 and 2. So, even if a patient is missing Year 2 in the original data, this ensures they appear in both years.

# Merge with the original data
unadjusted_all_costs_final_df <- merge(unadjusted_all_costs_combinations, unadjusted_all_costs_yr, by = c("subject_id", "year_after_index"), all.x = TRUE)
# Explanation: This merges all_combinations with the original df on both patient_id and year. The all.x = TRUE makes it a left join, meaning all rows from all_combinations will be kept, even if they don’t exist in the original data (with missing values filled as NA).
# Replace NA visits_per_year with 0
unadjusted_all_costs_final_df$total_costs[is.na(unadjusted_all_costs_final_df$total_costs)] <- 0


# Summary stats for all-patient cost per year
unadjusted_all_summary_costs_per_year <- unadjusted_all_costs_final_df %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visits_cost = round(mean(total_costs, na.rm = TRUE), 4),
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
  ) %>%
  mutate(across(everything(), round, 2))

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
    boot_ci <- boot.ci(boot.out = boot_result, type = "perc")
    
    # Extract year value
    year_val <- unique(df_group$year_after_index)
    
    # Assemble the results
    tibble(
      year_after_index = year_val,
      mean_boot_visits = round(as.numeric(boot_result$t0), 3),
      lower_ci = round(as.numeric(boot_ci$percent[4]), 3),
      upper_ci = round(as.numeric(boot_ci$percent[5]), 3)
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
    boot_ci <- boot.ci(boot.out = boot_result, type = "perc")
    
    # Extract year value
    year_val <- unique(df_group$year_after_index)
    
    # Assemble the results
    tibble(
      year_after_index = year_val,
      mean_boot_visits = round(as.numeric(boot_result$t0), 3),
      lower_ci = round(as.numeric(boot_ci$percent[4]), 3),
      upper_ci = round(as.numeric(boot_ci$percent[5]), 3)
    )
  })

boot_costs_list_per_year <- list(
  boot_all_summary_stats = boot_all_summary_statistics_costs_per_year,
  boot_user_summary_stats = boot_user_summary_statistics_costs_per_year
)

write_xlsx(boot_costs_list_per_year, here::here("OMOP Analysis", "Results", "Output_yearly_costs_OMOP-Unadjusted_Boot.xlsx"))


# Visits per specialty
(visits_per_specialty <- visit_data %>% 
    filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
    group_by(specialty_reduced) %>% 
    summarise(n_counts = n(), .groups = "drop") %>% 
    mutate(n_percent = round(n_counts/ sum(n_counts) * 100, 4)))
sum(visits_per_specialty$n_percent)

rio::export(visits_per_specialty, here::here("OMOP Analysis", "Results", "Output_visits_per_specialty_OMOP.xlsx"))
