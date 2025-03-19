# Filter user visits and relevant specialties
user_visits <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    raw_exposure_interval = interval(index_date, pmin(consdate, follow_up_end)),
    month_after_index = interval(index_date, consdate) %/% months(1),
    year_after_index = case_when(
      month_after_index %in% c(1:12) ~ 1,
      TRUE ~ 2
    )
  )

# We then compute visits per year per patient
user_yearly_visits <- user_visits %>%
  group_by(patid, specialty) %>%
  summarise(visits_count = first(visit_count),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(patid, year_after_index) %>%
  summarise(
    total_patient_visits = sum(visits_count, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / year_after_index,
    .groups = "drop"
  )

# Then,compute total visits over full exposure period
user_total_visits <- visit_data %>%
  filter(user_status == "User",
         specialty != "Not Clinically Relevant") %>%
  group_by(patid, specialty) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop"
  )

# Merge yearly and overall exposure stats
user_summary_df <- user_yearly_visits %>%
  left_join(user_total_visits, by = "patid") %>% 
  distinct()

user_only_summary_statistics_overall <- tibble(
  mean_visit_per_year = round(sum(user_summary_df$tot_visits) / sum(user_summary_df$tot_exposed_yrs), 2),
  min_visit_per_year = round(min(user_summary_df$tot_visits_per_yr), 2),
  max_visit_per_year = round(max(user_summary_df$tot_visits_per_yr), 2),
  sd_visit_per_year = round(sd(user_summary_df$tot_visits_per_yr), 2),
  median_visit_per_year = round(quantile(user_summary_df$tot_visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(user_summary_df$tot_visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(user_summary_df$tot_visits_per_yr, probs = (.75)), 2)
)

user_summary_df_yr1 <- user_summary_df %>% 
  filter(year_after_index == 1)

user_only_summary_statistics_yr1 <- tibble(
  mean_visit_per_year = round(sum(user_summary_df_yr1$visits_per_year) / sum(user_summary_df_yr1$year_after_index), 2),
  min_visit_per_year = round(min(user_summary_df_yr1$visits_per_year), 2),
  max_visit_per_year = round(max(user_summary_df_yr1$visits_per_year), 2),
  sd_visit_per_year = round(sd(user_summary_df_yr1$visits_per_year), 2),
  median_visit_per_year = round(quantile(user_summary_df_yr1$visits_per_year, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(user_summary_df_yr1$visits_per_year, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(user_summary_df_yr1$visits_per_year, probs = (.75)), 2)
)

# Sense check
max_users_df <- user_summary_df %>% 
  group_by(patid, year_after_index) %>% 
  summarise(
    max_visits = max(total_patient_visits)
  )

# Regarding the max in year 1 with 209 visits, indeed the patient (1156291120220)
# has these number of visits

user_summary_df_yr2 <- user_summary_df %>% 
  filter(year_after_index == 2)

user_only_summary_statistics_yr2 <- tibble(
  mean_visit_per_year = round(sum(user_summary_df_yr2$visits_per_year) / sum(user_summary_df_yr2$year_after_index), 2),
  min_visit_per_year = round(min(user_summary_df_yr2$visits_per_year), 2),
  max_visit_per_year = round(max(user_summary_df_yr2$visits_per_year), 2),
  sd_visit_per_year = round(sd(user_summary_df_yr2$visits_per_year), 2),
  median_visit_per_year = round(quantile(user_summary_df_yr2$visits_per_year, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(user_summary_df_yr2$visits_per_year, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(user_summary_df_yr2$visits_per_year, probs = (.75)), 2)
)



# All patients
all_visits <- visit_data %>%
  mutate(
    raw_exposure_interval = interval(index_date, pmin(consdate, follow_up_end)),
    month_after_index = interval(index_date, consdate) %/% months(1),
    year_after_index = case_when(
      month_after_index %in% c(1:12) ~ 1,
      TRUE ~ 2
    )
  )

# We then compute visits per year per patient
all_yearly_visits <- all_visits %>%
  group_by(patid, specialty) %>%
  summarise(visits_count = first(visit_count),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(patid, year_after_index) %>%
  summarise(
    total_patient_visits = sum(visits_count, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / year_after_index,
    .groups = "drop"
  )

# Then,compute total visits over full exposure period
all_total_visits <- visit_data %>%
  group_by(patid, specialty) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop"
  )

# Merge yearly and overall exposure stats
all_summary_df <- all_yearly_visits %>%
  left_join(all_total_visits, by = "patid") %>% 
  distinct()

all_only_summary_statistics_overall <- tibble(
  mean_visit_per_year = round(sum(all_summary_df$tot_visits) / sum(all_summary_df$tot_exposed_yrs), 2),
  min_visit_per_year = round(min(all_summary_df$tot_visits_per_yr), 2),
  max_visit_per_year = round(max(all_summary_df$tot_visits_per_yr), 2),
  sd_visit_per_year = round(sd(all_summary_df$tot_visits_per_yr), 2),
  median_visit_per_year = round(quantile(all_summary_df$tot_visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(all_summary_df$tot_visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(all_summary_df$tot_visits_per_yr, probs = (.75)), 2)
)

all_summary_df_yr1 <- all_summary_df %>% 
  filter(year_after_index == 1)

all_only_summary_statistics_yr1 <- tibble(
  mean_visit_per_year = round(sum(all_summary_df_yr1$visits_per_year) / sum(all_summary_df_yr1$year_after_index), 2),
  min_visit_per_year = round(min(all_summary_df_yr1$visits_per_year), 2),
  max_visit_per_year = round(max(all_summary_df_yr1$visits_per_year), 2),
  sd_visit_per_year = round(sd(all_summary_df_yr1$visits_per_year), 2),
  median_visit_per_year = round(quantile(all_summary_df_yr1$visits_per_year, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(all_summary_df_yr1$visits_per_year, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(all_summary_df_yr1$visits_per_year, probs = (.75)), 2)
)

# Sense check
max_all_df <- all_summary_df %>% 
  group_by(patid, year_after_index) %>% 
  summarise(
    max_visits = max(total_patient_visits)
  )

# Regarding the max in year 1 with 209 visits, indeed the patient (1156291120220)
# has these number of visits

all_summary_df_yr2 <- all_summary_df %>% 
  filter(year_after_index == 2)

all_only_summary_statistics_yr2 <- tibble(
  mean_visit_per_year = round(sum(all_summary_df_yr2$visits_per_year) / sum(all_summary_df_yr2$year_after_index), 2),
  min_visit_per_year = round(min(all_summary_df_yr2$visits_per_year), 2),
  max_visit_per_year = round(max(all_summary_df_yr2$visits_per_year), 2),
  sd_visit_per_year = round(sd(all_summary_df_yr2$visits_per_year), 2),
  median_visit_per_year = round(quantile(all_summary_df_yr2$visits_per_year, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(all_summary_df_yr2$visits_per_year, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(all_summary_df_yr2$visits_per_year, probs = (.75)), 2)
)


user_costs <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    # Exposure interval from index_date to follow_up_end
    raw_exposure_interval = interval(index_date, follow_up_end),
    
    # Months since index_date, ensuring it doesn’t exceed follow-up end
    month_after_index = interval(index_date, follow_up_end) %/% months(1),
    
    # Assign Year 1 or Year 2 based on the corrected month index
    year_after_index = case_when(
      month_after_index <= 12 ~ 1,
      TRUE ~ 2
    )
  )
# user_costs <- visit_data %>%
#   filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
#   mutate(
#     raw_exposure_interval = interval(index_date, pmin(consdate, follow_up_end)),
#     month_after_index = interval(index_date, consdate) %/% months(1),
#     year_after_index = case_when(
#       month_after_index %in% c(1:12) ~ 1,
#       TRUE ~ 2
#     )
#   )
# We then compute visits per year per patient
user_yearly_costs <- user_costs %>%
  group_by(patid, specialty) %>%
  summarise(visit_cost = first(visit_cost),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(patid, year_after_index) %>%
  summarise(
    total_patient_visits_cost = sum(visit_cost, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_cost_per_year = total_patient_visits_cost / year_after_index,
    .groups = "drop"
  )

# Then,compute total visits over full exposure period
user_total_costs <- visit_data %>%
  filter(user_status == "User",
         specialty != "Not Clinically Relevant") %>%
  group_by(patid, specialty) %>%
  summarise(
    total_patient_visits_costs = first(visit_cost),
    total_exposed_yrs = first(exposed_yrs),
    visits_costs_per_year = total_patient_visits_costs / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_visits_cost = sum(total_patient_visits_costs, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_cost_per_yr = tot_visits_cost / tot_exposed_yrs,
    .groups = "drop"
  )

# Merge yearly and overall exposure stats
user_costs_summary_df <- user_yearly_costs %>%
  left_join(user_total_costs, by = "patid") %>% 
  distinct()

user_only_costs_summary_statistics_overall <- tibble(
  mean_visit_cost_per_year = round(sum(user_costs_summary_df$tot_visits_cost) / sum(user_costs_summary_df$tot_exposed_yrs), 2),
  min_visit_cost_per_year = round(min(user_costs_summary_df$tot_visits_cost_per_yr), 2),
  max_visit_cost_per_year = round(max(user_costs_summary_df$tot_visits_cost_per_yr), 2),
  sd_visit_cost_per_year = round(sd(user_costs_summary_df$tot_visits_cost_per_yr), 2),
  median_visit_cost_per_year = round(quantile(user_costs_summary_df$tot_visits_cost_per_yr, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(user_costs_summary_df$tot_visits_cost_per_yr, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(user_costs_summary_df$tot_visits_cost_per_yr, probs = (.75)), 2)
)

user_costs_summary_df_yr1 <- user_costs_summary_df %>% 
  filter(year_after_index == 1)

user_only_costs_summary_statistics_yr1 <- tibble(
  mean_visit_cost_per_year = round(sum(user_costs_summary_df_yr1$visits_cost_per_year) / sum(user_costs_summary_df_yr1$year_after_index), 2),
  min_visit_cost_per_year = round(min(user_costs_summary_df_yr1$visits_cost_per_year), 2),
  max_visit_cost_per_year = round(max(user_costs_summary_df_yr1$visits_cost_per_year), 2),
  sd_visit_cost_per_year = round(sd(user_costs_summary_df_yr1$visits_cost_per_year), 2),
  median_visit_cost_per_year = round(quantile(user_costs_summary_df_yr1$visits_cost_per_year, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(user_costs_summary_df_yr1$visits_cost_per_year, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(user_costs_summary_df_yr1$visits_cost_per_year, probs = (.75)), 2)
)

# Sense check
max_users_cost_costs_df <- user_costs_summary_df_yr1 %>% 
  group_by(patid, year_after_index) %>% 
  summarise(
    max_visits_cost = max(total_patient_visits_cost)
  )

# Regarding the max in year 1 with 209 visits, indeed the patient (1156291120220)
# has these number of visits

user_costs_summary_df_yr2 <- user_costs_summary_df %>% 
  filter(year_after_index == 2)

user_only_costs_summary_statistics_yr2 <- tibble(
  mean_visit_cost_per_year = round(sum(user_costs_summary_df_yr2$visits_cost_per_year) / sum(user_costs_summary_df_yr2$year_after_index), 2),
  min_visit_cost_per_year = round(min(user_costs_summary_df_yr2$visits_cost_per_year), 2),
  max_visit_cost_per_year = round(max(user_costs_summary_df_yr2$visits_cost_per_year), 2),
  sd_visit_cost_per_year = round(sd(user_costs_summary_df_yr2$visits_cost_per_year), 2),
  median_visit_cost_per_year = round(quantile(user_costs_summary_df_yr2$visits_cost_per_year, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(user_costs_summary_df_yr2$visits_cost_per_year, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(user_costs_summary_df_yr2$visits_cost_per_year, probs = (.75)), 2)
)


all_costs <- visit_data %>%
  mutate(
    # Exposure interval from index_date to follow_up_end
    raw_exposure_interval = interval(index_date, follow_up_end),
    
    # Months since index_date, ensuring it doesn’t exceed follow-up end
    month_after_index = interval(index_date, follow_up_end) %/% months(1),
    
    # Assign Year 1 or Year 2 based on the corrected month index
    year_after_index = case_when(
      month_after_index <= 12 ~ 1,
      TRUE ~ 2
    )
  )
# all_costs <- visit_data %>%
#   filter(all_status == "all", specialty != "Not Clinically Relevant") %>%
#   mutate(
#     raw_exposure_interval = interval(index_date, pmin(consdate, follow_up_end)),
#     month_after_index = interval(index_date, consdate) %/% months(1),
#     year_after_index = case_when(
#       month_after_index %in% c(1:12) ~ 1,
#       TRUE ~ 2
#     )
#   )
# We then compute visits per year per patient
all_yearly_costs <- all_costs %>%
  group_by(patid, specialty) %>%
  summarise(visit_cost = first(visit_cost),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(patid, year_after_index) %>%
  summarise(
    total_patient_visits_cost = sum(visit_cost, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_cost_per_year = total_patient_visits_cost / year_after_index,
    .groups = "drop"
  )

# Then,compute total visits over full exposure period
all_total_costs <- visit_data %>%
  group_by(patid, specialty) %>%
  summarise(
    total_patient_visits_costs = first(visit_cost),
    total_exposed_yrs = first(exposed_yrs),
    visits_costs_per_year = total_patient_visits_costs / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_visits_cost = sum(total_patient_visits_costs, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_cost_per_yr = tot_visits_cost / tot_exposed_yrs,
    .groups = "drop"
  )

# Merge yearly and overall exposure stats
all_costs_summary_df <- all_yearly_costs %>%
  left_join(all_total_costs, by = "patid") %>% 
  distinct()

all_only_costs_summary_statistics_overall <- tibble(
  mean_visit_cost_per_year = round(sum(all_costs_summary_df$tot_visits_cost) / sum(all_costs_summary_df$tot_exposed_yrs), 2),
  min_visit_cost_per_year = round(min(all_costs_summary_df$tot_visits_cost_per_yr), 2),
  max_visit_cost_per_year = round(max(all_costs_summary_df$tot_visits_cost_per_yr), 2),
  sd_visit_cost_per_year = round(sd(all_costs_summary_df$tot_visits_cost_per_yr), 2),
  median_visit_cost_per_year = round(quantile(all_costs_summary_df$tot_visits_cost_per_yr, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(all_costs_summary_df$tot_visits_cost_per_yr, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(all_costs_summary_df$tot_visits_cost_per_yr, probs = (.75)), 2)
)

all_costs_summary_df_yr1 <- all_costs_summary_df %>% 
  filter(year_after_index == 1)

all_only_costs_summary_statistics_yr1 <- tibble(
  mean_visit_cost_per_year = round(sum(all_costs_summary_df_yr1$visits_cost_per_year) / sum(all_costs_summary_df_yr1$year_after_index), 2),
  min_visit_cost_per_year = round(min(all_costs_summary_df_yr1$visits_cost_per_year), 2),
  max_visit_cost_per_year = round(max(all_costs_summary_df_yr1$visits_cost_per_year), 2),
  sd_visit_cost_per_year = round(sd(all_costs_summary_df_yr1$visits_cost_per_year), 2),
  median_visit_cost_per_year = round(quantile(all_costs_summary_df_yr1$visits_cost_per_year, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(all_costs_summary_df_yr1$visits_cost_per_year, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(all_costs_summary_df_yr1$visits_cost_per_year, probs = (.75)), 2)
)

# Sense check
max_alls_cost_costs_df <- all_costs_summary_df_yr1 %>% 
  group_by(patid, year_after_index) %>% 
  summarise(
    max_visits_cost = max(total_patient_visits_cost)
  )

# Regarding the max in year 1 with 209 visits, indeed the patient (1156291120220)
# has these number of visits

all_costs_summary_df_yr2 <- all_costs_summary_df %>% 
  filter(year_after_index == 2)

all_only_costs_summary_statistics_yr2 <- tibble(
  mean_visit_cost_per_year = round(sum(all_costs_summary_df_yr2$visits_cost_per_year) / sum(all_costs_summary_df_yr2$year_after_index), 2),
  min_visit_cost_per_year = round(min(all_costs_summary_df_yr2$visits_cost_per_year), 2),
  max_visit_cost_per_year = round(max(all_costs_summary_df_yr2$visits_cost_per_year), 2),
  sd_visit_cost_per_year = round(sd(all_costs_summary_df_yr2$visits_cost_per_year), 2),
  median_visit_cost_per_year = round(quantile(all_costs_summary_df_yr2$visits_cost_per_year, probs = (.5)), 2),
  lower_q_visits_cost_per_year = round(quantile(all_costs_summary_df_yr2$visits_cost_per_year, probs = (.25)), 2),
  upper_q_visits_cost_per_year = round(quantile(all_costs_summary_df_yr2$visits_cost_per_year, probs = (.75)), 2)
)

