# Visits ----

## Unadjusted ----
# Assign visits to year 1 or year 2 based on consult date
unadjusted_user_visits_yr <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
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

# Summary statistics for visits per year
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

## Adjusted ----
# Filter for users
adjusted_user_visits_yr <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  
  # Calculate year index of each visit
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_year_after_index = ceiling(days_exposed_total / 365.25),
    year_after_index = floor(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25) + 1,
    died_in_final_year = !is.na(death_date) &
      floor(as.numeric(death_date - index_date) / 365.25) + 1 == final_year_after_index,
    days_into_final_year = days_exposed_total - 365.25 * (final_year_after_index - 1)
  ) %>%
  
  # Count visits per year per patient
  group_by(subject_id, year_after_index) %>%
  summarise(
    total_visits = n(),
    final_year_after_index = first(final_year_after_index),
    days_into_final_year = first(days_into_final_year),
    died_in_final_year = first(died_in_final_year),
    .groups = "drop"
  ) %>%
  
  # Adjust visits
  # If a patient died during the year → Do not adjust visits.
  # If a patient was censored for other reasons (e.g., end of follow-up, other events) → Adjust visits based on the number of days they were exposed.
  # If a patient was followed up for less than half a year and did not die → Exclude that year.
    mutate(
    adjusted_visits = case_when(
      year_after_index != final_year_after_index ~ total_visits,  # not final year: no adjustment
      died_in_final_year ~ total_visits,                         # final year + died: no adjustment
      days_into_final_year >= 365.25 ~ total_visits,             # full year: no adjustment
      days_into_final_year >= 182.6 ~ (total_visits / days_into_final_year) * 365.25,  # adjust
      TRUE ~ 0  # drop if < 6 months and not died
    )
  ) %>%
  # filter(!(year_after_index == final_year_after_index & adjusted_visits == 0))
  filter(adjusted_visits > 0)  # drop years with <6 months and not adjusted
  # filter(!(year_after_index == final_year_after_index & days_into_final_year == 0))

# Add missing years (adjusted visits = 0)
adjusted_combinations_users <- expand.grid(
  subject_id = unique(adjusted_user_visits_yr$subject_id),
  year_after_index = 1:2
)

adjusted_final_df_users <- adjusted_combinations_users %>%
  left_join(adjusted_user_visits_yr, by = c("subject_id", "year_after_index")) %>%
  mutate(adjusted_visits = ifelse(is.na(adjusted_visits), 0, adjusted_visits))

# Summary statistics
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



# Sense check: 23520018 - dropped, less than 6 months of exposed days
# All
adjusted_all_visits_yr <- visit_data %>%
  
  # Calculate year index of each visit
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_year_after_index = ceiling(days_exposed_total / 365.25),
    year_after_index = floor(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25) + 1,
    died_in_final_year = !is.na(death_date) &
      floor(as.numeric(death_date - index_date) / 365.25) + 1 == final_year_after_index,
    days_into_final_year = days_exposed_total - 365.25 * (final_year_after_index - 1)
  ) %>%
  
  # Count visits per year per patient
  group_by(subject_id, year_after_index, specialty) %>%
  summarise(
    total_visits = n(),
    final_year_after_index = first(final_year_after_index),
    days_into_final_year = first(days_into_final_year),
    died_in_final_year = first(died_in_final_year),
    .groups = "drop"
  ) %>%
  mutate(total_visits = if_else(specialty == "Not Clinically Relevant", 0, total_visits)) %>% 
  
# Adjust visits
# If a patient died during the year → Do not adjust visits.
# If a patient was censored for other reasons (e.g., end of follow-up, other events) → Adjust visits based on the number of days they were exposed.
# If a patient was followed up for less than half a year and did not die → Exclude that year.
mutate(
  adjusted_visits = case_when(
    year_after_index != final_year_after_index ~ total_visits,  # not final year: no adjustment
    died_in_final_year ~ total_visits,                         # final year + died: no adjustment
    days_into_final_year >= 365.25 ~ total_visits,             # full year: no adjustment
    days_into_final_year >= 182.6 ~ (total_visits / days_into_final_year) * 365.25,  # adjust
    TRUE ~ 0  # drop if < 6 months and not died
  )
)   %>% 
  mutate(
  adjusted_visits = if_else(specialty == "Not Clinically Relevant", 0L, adjusted_visits)
) %>% 
  # filter(!(year_after_index == final_year_after_index & adjusted_visits == 0| specialty == "Not Clinically Relevant"))
  filter(adjusted_visits > 0 | specialty == "Not Clinically Relevant") # drop years with <6 months and not adjusted

# Sense check using this patient: 1016820018
adjusted_all_visits_yr <- adjusted_all_visits_yr %>% 
  group_by(subject_id, year_after_index) %>% 
  summarise(adjusted_visits = sum(adjusted_visits),
            .groups = "drop")

#  Add missing years (adjusted visits = 0)
adjusted_combinations_all <- expand.grid(
  subject_id = unique(adjusted_all_visits_yr$subject_id),
  year_after_index = 1:2
)

adjusted_final_df_all <- adjusted_combinations_all %>%
  left_join(adjusted_user_visits_yr, by = c("subject_id", "year_after_index")) %>%
  mutate(adjusted_visits = ifelse(is.na(adjusted_visits), 0, adjusted_visits))

# Summary statistics
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
    n = n()
  ) %>%
  mutate(
    se = sd_visit_per_year / sqrt(n),
    lower = mean_visit_per_year - qt(0.975, df = n - 1) * se,
    upper = mean_visit_per_year + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(where(is.numeric), round, 2))

adjusted_yearly_visits_list <- list(
  adjusted_user_summary = adjusted_user_summary_visits_per_year,
  adjusted_all_summary = adjusted_all_summary_visits_per_year
)

# Write to an Excel file
write_xlsx(adjusted_yearly_visits_list, here::here("OMOP Analysis", "Results", "Output_yearly_visits_OMOP-Adjusted.xlsx"))

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

boot_visits_list_per_year <- list(
  boot_all_summary_stats = boot_all_summary_statistics_per_year,
  boot_user_summary_stats = boot_user_summary_statistics_per_year
)

write_xlsx(boot_visits_list_per_year, here::here("OMOP Analysis", "Results", "Output_yearly_visits_OMOP-Unadjusted_Boot.xlsx"))


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

# Costs ----

## Unadjusted ----
unadjusted_user_costs_yr <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
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
    .groups = "drop"
  ) %>%
  distinct() %>%
  mutate(visits_cost = visit_count * unit_cost) %>% 
  mutate(
    visit_count = if_else(specialty == "Not Clinically Relevant", 0L, visit_count),
    visits_cost = if_else(specialty == "Not Clinically Relevant", 0, visits_cost)
  ) %>% 
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

unadjusted_yearly_costs_list <- list(
  unadjusted_user_summary = unadjusted_user_summary_costs_per_year,
  unadjusted_all_summary = unadjusted_all_summary_costs_per_year
)
# Write to an Excel file
write_xlsx(unadjusted_yearly_costs_list, here::here("OMOP Analysis", "Results", "Output_yearly_costs_OMOP-Unadjusted.xlsx"))

## Adjusted ----
# Filter for users
adjusted_user_costs_yr <- visit_data %>%
  filter(user_status == "User", is_clinically_relevant == 1) %>%
  
  # Calculate year index of each visit
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_year_after_index = ceiling(days_exposed_total / 365.25),
    year_after_index = floor(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25) + 1,
    died_in_final_year = !is.na(death_date) &
      floor(as.numeric(death_date - index_date) / 365.25) + 1 == final_year_after_index,
    days_into_final_year = days_exposed_total - 365.25 * (final_year_after_index - 1)
  ) %>%
  
  # Count visits per year per patient
  group_by(subject_id, year_after_index, specialty_reduced) %>%
  summarise(
    total_visits = n(),
    final_year_after_index = first(final_year_after_index),
    days_into_final_year = first(days_into_final_year),
    died_in_final_year = first(died_in_final_year),
    unit_cost = unit_cost,
    .groups = "drop"
  ) %>%
  distinct() %>% 
  
  # Adjust visits
  # If a patient died during the year → Do not adjust visits.
  # If a patient was censored for other reasons (e.g., end of follow-up, other events) → Adjust visits based on the number of days they were exposed.
  # If a patient was followed up for less than half a year and did not die → Exclude that year.
  mutate(
    adjusted_visits = case_when(
      year_after_index != final_year_after_index ~ total_visits,  # not final year: no adjustment
      died_in_final_year ~ total_visits,                         # final year + died: no adjustment
      days_into_final_year >= 365.25 ~ total_visits,             # full year: no adjustment
      days_into_final_year >= 182.6 ~ (total_visits / days_into_final_year) * 365.25,  # adjust
      TRUE ~ 0  # drop if < 6 months and not died
    )
  ) %>%
  # filter(!(year_after_index == final_year_after_index & adjusted_visit_count == 0)) %>%
  filter(adjusted_visits > 0) # drop years with <6 months and not adjusted

# Sense check using this patient: 1016820018
adjusted_user_costs_yr <- adjusted_user_costs_yr %>% 
  group_by(subject_id, year_after_index) %>% 
  summarise(
    adjusted_visit_cost = adjusted_visits * unit_cost,
    .groups = "drop"
  ) %>% 
  group_by(subject_id, year_after_index) %>% 
  summarise(total_cost = sum(adjusted_visit_cost),
            .groups = "drop")

# Add missing years (adjusted visits = 0)
adjusted_combinations_users_costs <- expand.grid(
  subject_id = unique(adjusted_user_costs_yr$subject_id),
  year_after_index = 1:2
)

adjusted_final_df_users_costs <- adjusted_combinations_users_costs %>%
  left_join(adjusted_user_costs_yr, by = c("subject_id", "year_after_index")) %>%
  mutate(total_cost = ifelse(is.na(total_cost), 0, total_cost))

# Summary stats for user cost per year
adjusted_user_summary_costs_per_year <- adjusted_final_df_users_costs %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visits_cost = round(mean(total_cost, na.rm = TRUE), 2),
    min_visits_cost = round(min(total_cost, na.rm = TRUE), 2),
    max_visits_cost = round(max(total_cost, na.rm = TRUE), 2),
    sd_visits_cost = round(sd(total_cost, na.rm = TRUE), 2),
    median_visits_cost = round(median(total_cost, na.rm = TRUE), 2),
    q1_visits_cost = round(quantile(total_cost, 0.25, na.rm = TRUE), 2),
    q3_visits_cost = round(quantile(total_cost, 0.75, na.rm = TRUE), 2),
    n = n()
  ) %>%
  mutate(
    se = sd_visits_cost / sqrt(n),
    lower = mean_visits_cost - qt(0.975, df = n - 1) * se,
    upper = mean_visits_cost + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2))

# All patients
adjusted_all_costs_yr <- visit_data %>%
  
  # Calculate year index of each visit
  mutate(
    days_exposed_total = as.numeric(follow_up_end - index_date),
    final_year_after_index = ceiling(days_exposed_total / 365.25),
    year_after_index = floor(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25) + 1,
    died_in_final_year = !is.na(death_date) &
      floor(as.numeric(death_date - index_date) / 365.25) + 1 == final_year_after_index,
    days_into_final_year = days_exposed_total - 365.25 * (final_year_after_index - 1)
  ) %>%
  
  # Count visits per year per patient
  group_by(subject_id, year_after_index, specialty) %>%
  summarise(
    total_visits = n(),
    final_year_after_index = first(final_year_after_index),
    days_into_final_year = first(days_into_final_year),
    died_in_final_year = first(died_in_final_year),
    unit_cost = unit_cost,
    .groups = "drop"
  ) %>%
  distinct() %>% 
  
  # Adjust visits
  # If a patient died during the year → Do not adjust visits.
  # If a patient was censored for other reasons (e.g., end of follow-up, other events) → Adjust visits based on the number of days they were exposed.
  # If a patient was followed up for less than half a year and did not die → Exclude that year.
  mutate(
    adjusted_visits = case_when(
      year_after_index != final_year_after_index ~ total_visits,  # not final year: no adjustment
      died_in_final_year ~ total_visits,                         # final year + died: no adjustment
      days_into_final_year >= 365.25 ~ total_visits,             # full year: no adjustment
      days_into_final_year >= 182.6 ~ (total_visits / days_into_final_year) * 365.25,  # adjust
      TRUE ~ 0  # drop if < 6 months and not died
    )
  ) %>%
  mutate(
    adjusted_visits = if_else(specialty == "Not Clinically Relevant", 0L, adjusted_visits)
  ) %>% 
  filter(adjusted_visits > 0 | specialty == "Not Clinically Relevant") # drop years with <6 months and not adjusted

# Sense check using this patient: 1016820018
adjusted_all_costs_yr <- adjusted_all_costs_yr %>% 
  group_by(subject_id, year_after_index) %>% 
  summarise(
    adjusted_visit_cost = adjusted_visits * unit_cost,
    .groups = "drop"
  ) %>% 
  group_by(subject_id, year_after_index) %>% 
  summarise(total_cost = sum(adjusted_visit_cost),
            .groups = "drop")

# Add missing years (adjusted visits = 0)
adjusted_combinations_all_costs <- expand.grid(
  subject_id = unique(adjusted_all_costs_yr$subject_id),
  year_after_index = 1:2
)

adjusted_final_df_all_costs <- adjusted_combinations_all_costs %>%
  left_join(adjusted_all_costs_yr, by = c("subject_id", "year_after_index")) %>%
  mutate(total_cost = ifelse(is.na(total_cost), 0, total_cost))

# Summary stats for user cost per year
adjusted_all_summary_costs_per_year <- adjusted_final_df_all_costs %>%
  group_by(year_after_index) %>%
  summarise(
    mean_visits_cost = round(mean(total_cost, na.rm = TRUE), 2),
    min_visits_cost = round(min(total_cost, na.rm = TRUE), 2),
    max_visits_cost = round(max(total_cost, na.rm = TRUE), 2),
    sd_visits_cost = round(sd(total_cost, na.rm = TRUE), 2),
    median_visits_cost = round(median(total_cost, na.rm = TRUE), 2),
    q1_visits_cost = round(quantile(total_cost, 0.25, na.rm = TRUE), 2),
    q3_visits_cost = round(quantile(total_cost, 0.75, na.rm = TRUE), 2),
    n = n()
  ) %>%
  mutate(
    se = sd_visits_cost / sqrt(n),
    lower = mean_visits_cost - qt(0.975, df = n - 1) * se,
    upper = mean_visits_cost + qt(0.975, df = n - 1) * se
  ) %>%
  mutate(across(everything(), round, 2)) 

adjusted_yearly_costs_list <- list(
  adjusted_user_summary = adjusted_all_summary_costs_per_year,
  adjusted_all_summary = adjusted_user_summary_costs_per_year
)
# Write to an Excel file
write_xlsx(adjusted_yearly_costs_list, here::here("OMOP Analysis", "Results", "Output_yearly_costs_OMOP-Adjusted.xlsx"))



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
  mean(sampled$total_cost)
}

set.seed(123)

# Group by year and perform bootstrap per group
adj_boot_user_summary_statistics_costs_per_year <- adjusted_final_df_users_costs %>%
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
adj_boot_all_summary_statistics_costs_per_year <- adjusted_final_df_all_costs %>%
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

write_xlsx(boot_costs_list_per_year, here::here("OMOP Analysis", "Results", "Output_yearly_costs_OMOP-Boot.xlsx"))


boot_costs_list_per_year <- list(
  boot_all_summary_stats = boot_all_summary_statistics_costs_per_year,
  boot_user_summary_stats = boot_user_summary_statistics_costs_per_year
)

write_xlsx(boot_costs_list_per_year, here::here("OMOP Analysis", "Results", "Output_yearly_costs_OMOP-Unadjusted_Boot.xlsx"))


# Visits per specialty ----
(visits_per_specialty <- visit_data %>% 
    filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
    group_by(specialty_reduced) %>% 
    summarise(n_counts = n(), .groups = "drop") %>% 
    mutate(n_percent = round(n_counts/ sum(n_counts) * 100, 4)))
sum(visits_per_specialty$n_percent)

rio::export(visits_per_specialty, here::here("OMOP Analysis", "Results", "Output_visits_per_specialty_OMOP.xlsx"))


# Specialty count ----
yearly_category_count <- visit_data %>% 
  mutate(year_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25 + 1)) %>% 
  group_by(year_after_index, specialty_reduced) %>% 
  summarise(count_category = n(),
            .groups = "drop") %>% 
  mutate(percent_category = round(count_category / sum(count_category) * 100, 2))

yearly_specific_role_count <- visit_data %>% 
  mutate(year_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25 + 1)) %>%
  group_by(year_after_index, specialty_reduced, specialty) %>% 
  summarise(count_specific_role = n(),
            .groups = "drop") %>% 
  mutate(percent_specific_role = count_specific_role / sum(count_specific_role) * 100) 


yearly_healthcare_role_list <- list(
  yearly_category_count = yearly_category_count,
  yearly_specific_role_count = yearly_specific_role_count
)

write_xlsx(yearly_healthcare_role_list, here::here("OMOP Analysis", "Results", "Output_yearly_healthcare_roles-OMOP.xlsx"))

library(gtsummary)

(gt_yearly_healthcare_roles_count <- visit_data %>%
    mutate(year_after_index = trunc(as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 365.25 + 1)) %>%
    tbl_summary(
      by = year_after_index,
      include = c(specialty_reduced, specialty),
      label = list(
        specialty_reduced = "Healthcare Category", 
        specialty = "Healthcare Specific Role"
      ),
      statistic = all_categorical() ~ "{n} ({p}%)",
      sort = all_categorical() ~ "frequency"
    ) %>%
    modify_header(label ~ "**Table of Healthcare Roles**") %>%
    bold_labels())

gt_yearly_healthcare_roles_count_df <- as_tibble(gt_yearly_healthcare_roles_count)

rio::export(gt_yearly_healthcare_roles_count_df, here::here("OMOP Analysis", "Results", "Output_GT_yearly_healthcare_roles-OMOP.xlsx"))
