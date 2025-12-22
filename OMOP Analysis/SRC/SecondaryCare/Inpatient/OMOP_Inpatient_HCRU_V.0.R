library(dplyr)
library(boot)
library(purrr)

# --- 1) Spells in study period -------------------------
cdm[["spells"]] <- cdm$visit_occurrence %>%
  filter(
    visit_concept_id == 9201,               # inpatient spells
    visit_start_date >= study_start_date,
    visit_end_date <= study_end_date
  ) %>%
  compute(name = "spells", overwrite = TRUE)

# --- 2) Join with cohort data ----------------------
cohort_hospital_data_user <- cohort_table %>%
  dplyr::select(subject_id, index_date, follow_up_end) %>%
  left_join(cdm[["spells"]] %>% collect(), by = c("subject_id" = "person_id")) %>%
  filter(
    visit_start_date >= index_date, # this will keep only patients with hospitalisations
    visit_start_date <= follow_up_end # we include spells that start within the study period, even if they after study end
  ) %>%
  mutate(
    los = as.numeric((visit_end_date - visit_start_date) + 1) # derive los per spell - same day admission/discharge are counted as 1
  )

# --- 3) Spells per patient --------------------------------------------
cohort_hospital_user <- cohort_hospital_data_user %>%
  summarise(total_spells = n_distinct(visit_occurrence_id), .by = subject_id)

# Merge back to cohort; assign 0 to patients without spells
cohort_hospital_all <- cohort_table %>%
  left_join(cohort_hospital_user, by = "subject_id") %>%
  mutate(
    total_spells = coalesce(total_spells, 0L), 
    user_status_inpatient  = if_else(total_spells > 0, "User", "Non-user") # define user status
  )

cohort_table <- cohort_table %>%
  left_join(
    cohort_hospital_all %>%
      dplyr::select(subject_id, user_status_inpatient),
    by = "subject_id"
  )

# --- 4) Analysis
# Helper summary function
summarise_counts <- function(df_counts, varname = "total_spells") {
  df_counts %>%
    dplyr::summarise(
      n_pts    = n(),
      n_spells = sum (.data[[varname]], na.rm = TRUE),
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

# --- A) Spells per patient  --------------------------------------

## All patients (users + non-users)
spell_counts_all <- cohort_hospital_all %>%
  summarise_counts(varname = "total_spells") %>%
  mutate(group = "All Patients") %>%
  relocate(group)

## Users only
spell_counts_users <- cohort_hospital_all %>%
  filter(user_status_inpatient == "User") %>%
  summarise_counts(varname = "total_spells") %>%
  mutate(group = "Users") %>%
  relocate(group)

## --- Combine outputs ----
combined_summary_hospitalisation <- bind_rows(
  spell_counts_all,
  spell_counts_users
) %>%
  arrange(group)

# Bootstrap spells -----

# --- Function to compute bootstrap summary----
bootstrap_summary <- function(data, varname = "total_spells", R = 1000, seed = 626) {
  
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

# --- Bootstrap summaries for inpatient spells ----

## All patients (users + non-users, zeros included)
boot_all <- bootstrap_summary(cohort_hospital_all, varname = "total_spells") %>%
  mutate(group = "All Patients")

## Users only
boot_users <- bootstrap_summary(
  cohort_hospital_all %>% 
    filter(user_status_inpatient == "User"),
  varname = "total_spells"
) %>%
  mutate(group = "Users")

# --- Combine bootstrap outputs
combined_bt_summary_hospitalisation <- bind_rows(
  boot_all,
  boot_users
) %>%
  dplyr::select(group, everything())

## Save outputs
write.csv(combined_summary_hospitalisation, "OMOP Analysis/Results/SecondaryCareAnalysis/Inpatient/combined_summary_hospitalisation.csv", row.names = FALSE)
write.csv(combined_bt_summary_hospitalisation,"OMOP Analysis/Results/SecondaryCareAnalysis/Inpatient/combined_bt_summary_hospitalisation.csv", row.names = FALSE)

# --- Plot inpatient spells  ----
plot_inpatient_spells <- ggplot(
  combined_bt_summary_hospitalisation,
  aes(x = group, y = mean, fill = group)
) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_perc_lower, ymax = ci_perc_upper),
                width = 0.15, color = "black", linewidth = 0.7) +
  labs(
    x = "",
    y = "Hospitalisations per patient (mean)",
    title = "Bootstrap mean hospitalisations with 95% percentile CI"
  ) +
  scale_x_discrete(labels = c("All Patients", "Users")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("OMOP Analysis/Results/SecondaryCareAnalysis/Inpatient/plot_inpatient_spells.png", 
       plot = plot_inpatient_spells, 
       width = 10, height = 6, dpi = 300)


# --- B) LOS summaries (spell level, users only) -----------------
los_summary <- cohort_hospital_data_user %>%
  summarise(
    n_spells = n(),
    mean_los = mean(los, na.rm = TRUE),
    sd       = sd(los, na.rm = TRUE),
    median_los   = quantile(los, 0.5, na.rm = TRUE),
    lower_q  = quantile(los, 0.25, na.rm = TRUE),
    upper_q  = quantile(los, 0.75, na.rm = TRUE),
    se       = sd / sqrt(n_spells),
    ci_lower = mean_los - qt(0.975, df = n_spells - 1) * se,
    ci_upper = mean_los + qt(0.975, df = n_spells - 1) * se,
    .groups  = "drop"
  ) %>%
  mutate(across(where(is.numeric) & !any_of("n_spells"), ~ round(.x, 2)))


# --- Function for bootstrap mean + CI ----
bootstrap_summary <- function(data, varname = "los", R = 1000, seed = 626) {
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

# --- Bootstrap LOS (users only, spell level) ----
boot_los <- bootstrap_summary(cohort_hospital_data_user, varname = "los") 

## Save outputs

write.csv(los_summary, "OMOP Analysis/Results/SecondaryCareAnalysis/Inpatient/los_summary.csv", row.names = FALSE)
write.csv(boot_los,"OMOP Analysis/Results/SecondaryCareAnalysis/Inpatient/boot_los.csv",row.names = FALSE)




