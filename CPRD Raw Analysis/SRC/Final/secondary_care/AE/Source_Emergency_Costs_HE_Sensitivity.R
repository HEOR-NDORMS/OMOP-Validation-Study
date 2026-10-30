# --- 1) AE admissions in study period -------------------------

emergency <- my_dm$validation_hesae_attendance  %>%
  filter(
    arrivaldate >= study_start_date,
    arrivaldate <= study_end_date
  ) %>% collect()

# --- 2) Join with cohort data ----------------------

cohort_emergency_data_user <- cohort_table %>%
  dplyr::select(patid, index_date, follow_up_end) %>%
  left_join(emergency, by = "patid") %>%
  filter(
    arrivaldate >= index_date, # this will keep only patients with AE
    arrivaldate <= follow_up_end 
  ) 

# --- 3) AE per patient --------------------------------------------
cohort_emergency_user_sensitivity <- cohort_emergency_data_user %>%
  summarise(total_ae = n_distinct(aekey), .by = patid) %>%
  mutate(total_ae_cost = total_ae * 273)   # assign flat weighted unit cost per AE (2023/4 NHS cost collection)

# --- 4) Merge back to cohort; assign 0 to patients without AE
cohort_emergency_all_sensitivity <- cohort_table %>%
  left_join(cohort_emergency_user_sensitivity, by = "patid") %>%
  mutate(
    total_ae = coalesce(total_ae, 0L), 
    total_ae_cost  = coalesce(total_ae_cost, 0),
    user_status_emergency  = if_else(total_ae > 0, "User", "Non-user") # define user status
  )



# =====================================================
# AE (Emergency) cost analysis // Sentitivity analysis
# =====================================================

# --- A) AE cost per patient  ------------------------------

# Helper summary function (adapted to AE cost)
summarise_costs <- function(df_counts, varname = "total_ae_cost") {
  df_counts %>%
    summarise(
      n_pts    = n(),
      n_costed   = sum(!is.na(.data[[varname]])),
      total_cost = sum(.data[[varname]], na.rm = TRUE),
      mean     = mean(.data[[varname]], na.rm = TRUE),
      min      = min(.data[[varname]], na.rm = TRUE),
      max      = max(.data[[varname]], na.rm = TRUE),
      sd       = sd(.data[[varname]], na.rm = TRUE),
      median   = quantile(.data[[varname]], 0.5, na.rm = TRUE),
      lower_q  = quantile(.data[[varname]], 0.25, na.rm = TRUE),
      upper_q  = quantile(.data[[varname]], 0.75, na.rm = TRUE),
      se       = sd / sqrt(n_costed),
      ci_lower = mean - qt(0.975, df = n_costed - 1) * se,
      ci_upper = mean + qt(0.975, df = n_costed - 1) * se,
      .groups  = "drop"
    ) %>%
    mutate(across(where(is.numeric) & !any_of("n_pts"), ~ round(.x, 2)))
}

## All patients (users + non-users, zeros included)
ae_costs_all_sensitivity <- cohort_emergency_all_sensitivity %>%
  summarise_costs(varname = "total_ae_cost") %>%
  mutate(group = "All Patients") %>%
  relocate(group)

## Users only
ae_costs_users_sensitivity <- cohort_emergency_all_sensitivity %>%
  filter(user_status_emergency == "User") %>%
  summarise_costs(varname = "total_ae_cost") %>%
  mutate(group = "Users") %>%
  relocate(group)

# Combine summaries
combined_summary_ae_costs_sensitivity <- bind_rows(
  ae_costs_all_sensitivity,
  ae_costs_users_sensitivity
) %>%
  arrange(group)

# Save outputs
write.csv(combined_summary_ae_costs_sensitivity,
          "CPRD Raw Analysis/Results/Final/Secondary care/AE/combined_summary_ae_costs_sensitivity.csv",
          row.names = FALSE)


# --- B) Bootstrap summaries for AE costs ------------------------------

bootstrap_summary_costs <- function(data, varname = "total_ae_cost", R = 1000, seed = 626) {
  
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
    n_pts    = nrow(data),
    n_costed = sum(!is.na(data[[varname]])),
    mean     = boot_mean,
    variance = boot_variance,
    sd       = boot_sd,
    ci_norm_lower  = ci_norm[1],
    ci_norm_upper  = ci_norm[2],
    ci_basic_lower = ci_basic[1],
    ci_basic_upper = ci_basic[2],
    ci_perc_lower  = ci_perc[1],
    ci_perc_upper  = ci_perc[2]
  ) %>%
    mutate(across(everything(), ~ round(.x, 3)))
}

## All patients 
boot_ae_costs_all_sensitivity <- bootstrap_summary_costs(
  cohort_emergency_all_sensitivity,
  varname = "total_ae_cost"
) %>%
  mutate(group = "All Patients")

## Users only 
boot_ae_costs_users_sensitivity <- cohort_emergency_all_sensitivity %>%
  filter(user_status_emergency == "User") %>%
  bootstrap_summary_costs(varname = "total_ae_cost") %>%
  mutate(group = "Users")

## Combine bootstrap results
combined_bt_summary_ae_costs_sensitivity <- bind_rows(
  boot_ae_costs_all_sensitivity,
  boot_ae_costs_users_sensitivity
) %>%
  dplyr::select(group, everything())

# Save bootstrap outputs
write.csv(
  combined_bt_summary_ae_costs_sensitivity,
  "CPRD Raw Analysis/Results/Final/Secondary care/AE/combined_bt_summary_ae_costs_sensitivity.csv",
  row.names = FALSE
)
