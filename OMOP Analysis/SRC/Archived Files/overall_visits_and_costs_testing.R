# Filter user visits and relevant specialties
user_visits <- visit_data %>%
  filter(user_status == "User", specialty != "Not Clinically Relevant") %>%
  mutate(
    raw_exposure_interval = interval(index_date, pmin(visit_detail_start_date, follow_up_end)),
    month_after_index = interval(index_date, visit_detail_start_date) %/% months(1),
    year_after_index = case_when(
      month_after_index %in% c(1:12) ~ 1,
      TRUE ~ 2
    )
  )

# We then compute visits per year per patient
user_yearly_visits <- user_visits %>%
  group_by(subject_id, specialty) %>%
  summarise(visits_count = first(visit_count),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(subject_id, year_after_index) %>%
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
  group_by(subject_id, specialty) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(subject_id) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop"
  )

# Merge yearly and overall exposure stats
user_summary_df <- user_yearly_visits %>%
  left_join(user_total_visits, by = "subject_id") %>% 
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
  group_by(subject_id, year_after_index) %>% 
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
    raw_exposure_interval = interval(index_date, pmin(visit_detail_start_date, follow_up_end)),
    month_after_index = interval(index_date, visit_detail_start_date) %/% months(1),
    year_after_index = case_when(
      month_after_index %in% c(1:12) ~ 1,
      TRUE ~ 2
    )
  )

# We then compute visits per year per patient
all_yearly_visits <- all_visits %>%
  group_by(subject_id, specialty) %>%
  summarise(visits_count = first(visit_count),
            exposed_yrs = first(exposed_yrs),
            year_after_index = first(year_after_index),
            .groups = "drop") %>%
  group_by(subject_id, year_after_index) %>%
  summarise(
    total_patient_visits = sum(visits_count, na.rm = TRUE),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / year_after_index,
    .groups = "drop"
  )

# Then,compute total visits over full exposure period
all_total_visits <- visit_data %>%
  group_by(subject_id, specialty) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(subject_id) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop"
  )

# Merge yearly and overall exposure stats
all_summary_df <- all_yearly_visits %>%
  left_join(all_total_visits, by = "subject_id") %>% 
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
  group_by(subject_id, year_after_index) %>% 
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
