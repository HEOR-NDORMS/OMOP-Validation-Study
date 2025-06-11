# Sense check: 55196220200

adjusted_visits_data_r <- visit_data_reduced %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  group_by(patid) %>%

# Computes the number of months since the index_date for each consultation (consdate).
# Uses 30.44 days/month for approximation.
# +1 ensures the first month is 1 (not 0).

# Example:
# index_date = "2020-01-01"
# consdate = "2020-02-14" → 44 days later
# month_after_index = trunc((44 / 30.44) + 1) = trunc(2.44) = 2
# So this consultation falls in month 2
  
# month_start and month_end:
# Why this is done: Since real calendar months vary in length (28–31 days), we use 30.44-day “months” to simplify time-based calculations.
# 
# What they do:
#   month_start = the start of the patient's n-th month after the index.
#   month_end = the end of that n-th month.
# 
# Example:
# Let's say:
#   index_date = "2020-01-01"; month_after_index = 2
# 
# Then:
# month_start = 2020-01-01 + (2 - 1) * 30.44 days = 2020-01-01 + 30.44 days ≈ 2020-01-31
# month_end = 2020-01-01 + 2 * 30.44 days ≈ 2020-03-02
# So month 2 spans roughly from Jan 31 to Mar 2, using the 30.44-day approximation.

# month_after_index = 1:
# month_start = index_date + (1 - 1) * 30.44 days = 2020-01-01
# month_end   = index_date + 1 * 30.44 days ≈ 2020-01-01 + 30.44 ≈ 2020-01-31
# month 1 spans from Jan 1 to Jan 31 (approx).

# month_after_index = 2:
# month_start = 2020-01-01 + 30.44 ≈ 2020-01-31
# month_end   = 2020-01-01 + 60.88 ≈ 2020-03-02
# So month 2 starts exactly where month 1 ended — at Jan 31 — and goes to about Mar 2.

# In brief
# month_start, month_end ->	Define 30.44-day “months” post-index.
# days_exposed_in_month ->	Days the patient was followed up in that artificial month.
# adjusted_visits	Visit -> count adjusted for partial follow-up months (unless they died).

  mutate(
    # Calculate month after index using 30.44 days/month approximation
    month_after_index = trunc((as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1),
    
    # Define month start and end using as.difftime to handle decimal days
    month_start = index_date + as.difftime((month_after_index - 1) * 30.44, units = "days"),
    month_end = index_date + as.difftime(month_after_index * 30.44, units = "days"),
    
    # Calculate exposure details
    # Calculates how many days the patient was under observation during that month.
    # Takes the overlap between [month_start, month_end] and [index_date, follow_up_end].
    exposure_end = as.Date(follow_up_end),
    days_exposed_in_month = as.numeric(pmin(exposure_end, month_end) - pmax(index_date, month_start) + 1),
    
    # Flags for contribution and death
    fully_contributed = days_exposed_in_month >= 30.44,
    half_contributed = days_exposed_in_month >= 15.22,
    died_in_month = !is.na(death_date) & death_date >= month_start & death_date <= month_end
  ) %>%
  ungroup() %>% 
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    days_exposed_in_month = first(days_exposed_in_month),
    fully_contributed = first(fully_contributed),
    half_contributed = first(half_contributed),
    died_in_month = first(died_in_month),
    .groups = "drop"
  ) %>%
  filter(half_contributed == TRUE) %>%
  mutate(
    adjusted_visits = case_when(
      fully_contributed ~ visit_count,
      died_in_month ~ visit_count,  # No adjustment if patient died that month
      TRUE ~ (visit_count / days_exposed_in_month) / 30.44
    )
  )
# Sense check: 
# 1834920545; 132453320129 -> adjusted visits due to less than a month but more than half
# 178237820354; 97310720507 -> died so no adjustment done



# Prepare data with filtering on follow_up_end

# Step 1: Compute total visits per patient per month
total_visits_per_patient_month.2 <- adjusted_visits_data_r %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(adjusted_visits), .groups = "drop")

# Step 2: Compute mean number of visits per month (no weighting)
mean_visits_per_month.2 <- total_visits_per_patient_month.2 %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = sum(total_visits) / n_distinct(patid), .groups = "drop")

# Step 3: Compute total visits per specialty per month
visits_per_specialty_month.2 <- adjusted_visits_data_r %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_visits_specialty = sum(adjusted_visits), .groups = "drop")

# Step 4: Merge and compute specialty proportions
plots_users_mean_visits_specialty.2 <- visits_per_specialty_month.2 %>%
  left_join(mean_visits_per_month.2, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE)) %>%
  mutate(prop_mean = mean_visit_count * proportion)

# Step 5: Plot
(plot3 <- ggplot(
  plots_users_mean_visits_specialty.2,
  aes(x = factor(month_after_index), y = prop_mean, fill = specialty_reduced)
) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
    scale_fill_manual(values = c(
      "General Practitioner" = "#B4CDCD",
      "Nurse" = "#377eb8",
      "Other" = "#253494"
    )) +
    labs(
      x = "Month After Index",
      y = "Mean Visits per Healthcare Service User",
      fill = "Specialty",
      caption = "Visit counts were adjusted for partial month exposure when patients contributed ≥15.22 days. \nCounts were scaled by exposure days, except for full-month contributions or if the patient died during the month. \nRecords with <15.22 days were excluded."
    ) +
    scale_y_continuous(limits = c(0, 4)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 15),
      legend.position = "top",
      plot.caption = element_text(size = 15),
      panel.spacing = unit(2, "lines")
    ))


(monthly_summary_costs_weighted <- visit_data_reduced_MIN_c %>%
    filter(specialty_reduced != "Not Clinically Relevant") %>%
    group_by(patid) %>%
    mutate(
      # Calculate month index
      month_after_index = trunc((as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1),
      
      # Fix class conflict with as.Date()
      month_start = as.Date(index_date) + as.difftime((month_after_index - 1) * 30.44, units = "days"),
      month_end   = as.Date(index_date) + as.difftime(month_after_index * 30.44, units = "days"),
      
      # Exposure calculations
      exposure_end = as.Date(follow_up_end),
      days_exposed_in_month = as.numeric(pmin(exposure_end, month_end) - pmax(as.Date(index_date), month_start) + 1),
      
      # Contribution flags
      fully_contributed = days_exposed_in_month >= 30.44,
      half_contributed  = days_exposed_in_month >= 15.22,
      died_in_month     = !is.na(death_date) & death_date >= month_start & death_date <= month_end
    ) %>%
    ungroup() %>%
    group_by(patid, month_after_index, specialty_reduced) %>%
    summarise(
      visit_count = n(),
      days_exposed_in_month = first(days_exposed_in_month),
      fully_contributed = first(fully_contributed),
      half_contributed  = first(half_contributed),
      died_in_month     = first(died_in_month),
      unit_cost         = first(unit_cost),
      .groups = "drop"
    ) %>%
    filter(half_contributed == TRUE) %>%
    mutate(
      adjusted_visits = case_when(
        fully_contributed ~ visit_count,
        died_in_month     ~ visit_count,
        TRUE              ~ (visit_count / days_exposed_in_month) / 30.44
      ),
      visit_costs = adjusted_visits * unit_cost
    )
  %>% 
   group_by(patid, month_after_index) %>%
    summarise(
      total_costs = sum(visit_costs),  
      .groups = "drop"
    ) %>%
    group_by(month_after_index) %>%
    summarise(
      mean_cost = round(sum(total_costs)/n_distinct(patid), 2),
      min_cost = round(min(total_costs), 2),
      max_cost = round(max(total_costs), 2),
      sd_cost = round(sd(total_costs), 2),
      q1_cost = round(quantile(total_costs, 0.25), 2),
      median_cost = round(median(total_costs), 4),
      q3_cost = round(quantile(total_costs, 0.75), 2),
      n_pts = n_distinct(patid),
      .groups = "drop"
    ))

adjusted_costs_data_r <- visit_data_reduced_MIN_c %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(
    # Calculate month index
    month_after_index = trunc((as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1),
    
    # Fix class conflict with as.Date()
    month_start = as.Date(index_date) + as.difftime((month_after_index - 1) * 30.44, units = "days"),
    month_end   = as.Date(index_date) + as.difftime(month_after_index * 30.44, units = "days"),
    
    # Exposure calculations
    exposure_end = as.Date(follow_up_end),
    days_exposed_in_month = as.numeric(pmin(exposure_end, month_end) - pmax(as.Date(index_date), month_start) + 1),
    
    # Contribution flags
    fully_contributed = days_exposed_in_month >= 30.44,
    half_contributed  = days_exposed_in_month >= 15.22,
    died_in_month     = !is.na(death_date) & death_date >= month_start & death_date <= month_end
  ) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    days_exposed_in_month = first(days_exposed_in_month),
    fully_contributed = first(fully_contributed),
    half_contributed  = first(half_contributed),
    died_in_month     = first(died_in_month),
    unit_cost         = first(unit_cost),
    .groups = "drop"
  ) %>%
  filter(half_contributed == TRUE) %>%
  mutate(
    adjusted_visits = case_when(
      fully_contributed ~ visit_count,
      died_in_month     ~ visit_count,
      TRUE              ~ (visit_count / days_exposed_in_month) / 30.44
    ),
    visit_costs = adjusted_visits * unit_cost
  ) %>% 
  group_by(patid, month_after_index) %>%
  summarise(
    total_costs = sum(visit_costs),
    days_exposed_in_month = first(days_exposed_in_month),
    fully_contributed = first(fully_contributed),
    half_contributed  = first(half_contributed),
    died_in_month     = first(died_in_month),
    .groups = "drop"
  )
