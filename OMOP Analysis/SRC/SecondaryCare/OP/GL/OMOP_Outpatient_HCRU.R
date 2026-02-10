
# --- 1) Outpatient appointments in study period -------------------------

cdm[["outpatient"]] <- cdm$visit_occurrence %>%
  filter(
    visit_concept_id == 9202,               # outpatient
    visit_start_date >= study_start_date,
    visit_end_date <= study_end_date
  ) %>%
  compute(name = "outpatient", overwrite = TRUE)

outpatient <- cdm[["outpatient"]] %>% collect()
provider <- cdm[["provider"]] %>% collect()

# check OP without provider
outpatient %>%
  filter(!is.na(provider_id)) %>%
  tally()

# --- 2) Join with cohort data ----------------------
cohort_outpatient_data_user <- cohort_table %>%
  left_join(outpatient, by = c("subject_id" = "person_id")) %>%
  filter(
    visit_start_date >= index_date, # this will keep only patients with OP
    visit_start_date <= follow_up_end 
  ) 


# --- 3) Append specialty----------------------
cohort_outpatient_data_user <- cohort_outpatient_data_user %>%
  left_join(
    provider %>% 
      dplyr::select(provider_id, specialty_source_value, specialty_concept_id),
    by = "provider_id"
  )

# drop paediatric and baby 
# cohort_outpatient_data_user %>%
#   filter(str_detect(str_to_lower(specialty_source_value), 
#                     "paediatric|babies")) %>% nrow()
# 
# cohort_outpatient_data_user <- cohort_outpatient_data_user %>%
#   filter(!str_detect(str_to_lower(specialty_source_value), "paediatric|babies")) %>%
#   filter(!(is.na(specialty_source_value)))

# --- 4) OP per patient --------------------------------------------
cohort_outpatient_user <- cohort_outpatient_data_user %>%
  summarise(total_op = n_distinct(visit_occurrence_id), .by = subject_id)

# --- 5) Merge back to cohort; assign 0 to patients without OP
cohort_outpatient_all <- cohort_table %>%
  left_join(cohort_outpatient_user, by = "subject_id") %>%
  mutate(
    total_op = coalesce(total_op, 0L), 
    user_status_outpatient  = if_else(total_op > 0, "User", "Non-user") # define user status
  )

cohort_table <- cohort_table %>%
  left_join(
    cohort_outpatient_all %>%
      dplyr::select(subject_id, user_status_outpatient),
    by = "subject_id"
  )

# =====================================================
# Outpatient analysis
# =====================================================

# --- A) OP visits per patient ------------------------------

# Helper summary function
summarise_counts <- function(df_counts, varname = "total_op") {
  df_counts %>%
    dplyr::summarise(
      n_pts    = n(),
      n_op     = sum (.data[[varname]], na.rm = TRUE),
      mean     = mean(.data[[varname]], na.rm = TRUE),
      min      = min(.data[[varname]], na.rm = TRUE),
      max      = max(.data[[varname]], na.rm = TRUE),
      sd       = sd(.data[[varname]], na.rm = TRUE),
      median   = quantile(.data[[varname]], 0.5, na.rm = TRUE),
      lower_q  = quantile(.data[[varname]], 0.25, na.rm = TRUE),
      upper_q  = quantile(.data[[varname]], 0.75, na.rm = TRUE),
      se       = sd / sqrt(n_pts),
      ci_lower = mean - qt(0.975, df = n_pts - 1) * se,
      ci_upper = mean + qt(0.975, df = n_pts - 1) * se,
      .groups  = "drop"
    ) %>%
    mutate(across(where(is.numeric) & !any_of("n_pts"), ~ round(.x, 2)))
}

## All patients (users + non-users, zeros included)
op_counts_all <- cohort_outpatient_all %>%
  summarise_counts(varname = "total_op") %>%
  mutate(group = "All Patients") %>%
  relocate(group)

## Users only
op_counts_users <- cohort_outpatient_all %>%
  filter(user_status_outpatient == "User") %>%
  summarise_counts(varname = "total_op") %>%
  mutate(group = "Users") %>%
  relocate(group)

# Combine summaries
combined_summary_op <- bind_rows(
  op_counts_all,
  op_counts_users
) %>%
  arrange(group)

# Save outputs
write.csv(combined_summary_op,
          "OMOP Analysis/Results/SecondaryCareAnalysis/OP/GL/combined_summary_op.csv",
          row.names = FALSE)


# --- B) Bootstrap summaries for OP visits ------------------------------

bootstrap_summary <- function(data, varname = "total_op", R = 1000, seed = 626) {
  
  stat_fn <- function(d, i) {
    d_sample <- d[i, ]
    mean(d_sample[[varname]], na.rm = TRUE)
  }
  
  set.seed(seed)
  boot_result <- boot(data = data, statistic = stat_fn, R = R)
  boot_ci <- boot.ci(boot_result, type = c("norm", "basic", "perc"))
  
  boot_mean     <- mean(boot_result$t)
  boot_values   <- boot_result$t
  boot_variance <- var(as.numeric(boot_values))
  boot_sd       <- sd(boot_values)
  
  ci_norm  <- boot_ci$normal[2:3]
  ci_basic <- boot_ci$basic[4:5]
  ci_perc  <- boot_ci$perc[4:5]
  
  tibble(
    mean     = boot_mean,
    variance = boot_variance,
    sd       = boot_sd,
    ci_norm_lower = ci_norm[1],
    ci_norm_upper = ci_norm[2],
    ci_basic_lower = ci_basic[1],
    ci_basic_upper = ci_basic[2],
    ci_perc_lower = ci_perc[1],
    ci_perc_upper = ci_perc[2]
  ) %>%
    mutate(across(everything(), ~ round(.x, 3)))
}

## All patients
boot_op_all <- bootstrap_summary(cohort_outpatient_all, varname = "total_op") %>%
  mutate(group = "All Patients")


## Users only
boot_op_users <- bootstrap_summary(
  cohort_outpatient_all %>% 
    filter(user_status_outpatient == "User"),
  varname = "total_op"
) %>%
  mutate(group = "Users")

# Combine bootstrap results
combined_bt_summary_op <- bind_rows(
  boot_op_all,
  boot_op_users
) %>%
  dplyr::select(group, everything())

# Save bootstrap outputs
write.csv(combined_bt_summary_op,
          "OMOP Analysis/Results/SecondaryCareAnalysis/OP/GL/combined_bt_summary_op.csv",
          row.names = FALSE)

