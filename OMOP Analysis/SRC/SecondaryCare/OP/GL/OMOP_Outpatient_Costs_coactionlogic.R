library(dplyr)
library(stringr)
library(readxl)

# ============================================================
# 1. Load validated specialty mapping
# ============================================================
validated_map <- rio::import("OMOP Analysis/SRC/SecondaryCare/OP/GL/check_mainspef_tretspef_inpatient_outpatient_ValidationStudy_coactionlogic_GF2.xlsx") %>% 
  mutate(
    # use the column with the concept_name
    concept_name = str_to_lower(str_trim(concept_name)),
    specialty_source_value = str_to_lower(str_trim(specialty_source_value)),
    mainspef = str_to_lower(str_trim(mainspef)),
    tretspef = str_to_lower(str_trim(tretspef)),
    specialty_source_value_modified = specialty_source_value %>%
      str_remove("\\s*\\([^\\)]+\\)") %>%
      str_replace_all("&", "and") %>%
      str_to_lower() %>%
      str_trim()
  )

# Create small lookups for codes (for later re-join)
mainspef_codes <- validated_map %>%
  dplyr::select(mainspef, mainspef_code) %>%
  distinct() %>%
  filter(!is.na(mainspef))

tretspef_codes <- validated_map %>%
  dplyr::select(tretspef, tretspef_code) %>%
  distinct() %>%
  filter(!is.na(tretspef))

# ============================================================
# 2. Clean outpatient data
# ============================================================
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user %>%
  mutate(
    specialty_source_value_modified = specialty_source_value %>%
      str_remove("\\s*\\([^\\)]+\\)") %>%
      str_replace_all("&", "and") %>%
      str_to_lower() %>%
      str_trim()
  )

# ============================================================
# 3. Join validated mapping
# ============================================================
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  left_join(
    validated_map %>%
      dplyr::select(specialty_source_value_modified, mainspef, tretspef, mainspef_code, tretspef_code) %>%
      distinct(),
    by = "specialty_source_value_modified")


# ============================================================
# 4. Count + Drop records where both mainspef & tretspef are NA
# ============================================================
dropped_n <- cohort_outpatient_data_user_cost %>%
  filter(is.na(mainspef) & is.na(tretspef)) %>%
  nrow()

cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  filter(!(is.na(mainspef) & is.na(tretspef)))

# ============================================================
# 9. Check if there are still missing values
# ============================================================

# Count how many records still have missing mainspef or tretspef
missing_summary <- cohort_outpatient_data_user_cost %>%
  summarise(
    total_records = n(),
    missing_mainspef = sum(is.na(mainspef)),
    missing_tretspef = sum(is.na(tretspef)),
    missing_both = sum(is.na(mainspef) & is.na(tretspef))
  )

missing_summary

# missing mainspef
# missing_mainspef <- cohort_outpatient_data_user_cost %>%
#   filter(is.na(mainspef)) %>%
#   count(specialty_source_value, sort = TRUE)
# 
# # missing tretspef
# missing_tretspef <- cohort_outpatient_data_user_cost %>%
#   filter(is.na(tretspef)) %>%
#   count(specialty_source_value, sort = TRUE)

# ============================================================
# 10. Bring in age and gender
# ============================================================
person_lookup <- cdm$person %>%
  dplyr::select(person_id, year_of_birth, gender_concept_id) %>%
  collect()
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  left_join(
    person_lookup,
    by = c("subject_id" = "person_id")
  )

# cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>% 
#   left_join(cdm$person %>% 
#               dplyr::select(person_id, year_of_birth, gender_concept_id),
#             by = c("subject_id" = "person_id"), copy = TRUE)

# create age at appointment variable and sex variables
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  mutate(
    age_op = year(visit_start_date) - year_of_birth,
    sex = case_when(
      gender_concept_id == 8507 ~ 1,  # Male
      gender_concept_id == 8532 ~ 2,  # Female
      TRUE ~ NA_real_                
    )
  )

# ============================================================
# 11. Firstatt
# ============================================================

# Since first att is not mapped I assign the same value to all appointments, and then will assign costs based on first 4 letters of HRG, using weighted mean unit costs.
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>% 
  dplyr::select (visit_occurrence_id, age_op, sex, mainspef_code, tretspef_code) %>% 
  mutate (firstatt = 1) %>% 
  rename (startage = age_op,
          mainspef = mainspef_code,
          tretspef = tretspef_code)

cohort_outpatient_data_user_cost_grouper <- cohort_outpatient_data_user_cost %>%
  dplyr:: select(-visit_occurrence_id)

# ============================================================
# Save outputs for grouper
# ============================================================

rio::export(cohort_outpatient_data_user_cost_grouper, here::here("OMOP Analysis", "Results",
                                          "SecondaryCareAnalysis", "OP",
                                          "cohort_outpatient_data_user_cost_grouper.csv"))

# ============================================================
# 12. Prepare NHS cost collection
# ============================================================

nhs_op_cost <- read_excel("OMOP Analysis/SRC/SecondaryCare/OP/GL/NHS_Collection_Costs_2023_2024.xlsx", sheet = "OP")

nhs_op_cost_weighted <- nhs_op_cost %>%
  filter(!is.na (`Currency Code`)) %>% 
  mutate(
    Currency_Group = str_sub(`Currency Code`, 1, 4),
    
    # --- Clean numeric columns ---
    # Replace '*' with '3' BEFORE removing commas and converting to numeric
    n_attendances = str_replace_all(`Number of attendances`, "\\*", "3"),
    n_attendances = as.numeric(str_remove_all(n_attendances, "[,]")),
    
    cost = as.numeric(str_remove_all(Cost, "[£,]")),
    service_description = str_to_lower(str_trim(`Service Description`))
  ) %>%
  group_by(service_description, Currency_Group) %>%
  summarise(
    total_cost = sum(cost, na.rm = TRUE),
    total_attendances = sum(n_attendances, na.rm = TRUE),
    weighted_avg_unit_cost = total_cost / total_attendances,
    .groups = "drop"
  )


# View result
print(nhs_op_cost_weighted)

# ============================================================
# 13. Append cost 
# ============================================================

hrg_output <- read.csv("OMOP Analysis/SRC/SecondaryCare/OP/GL/output_outpatient_coaxtionlogic_2324_attend_rel.csv")

# in the grouper we could not run the visit_occurrence_id but the output is in the same order, let's make some checks anyway

#check n columns is the same
stopifnot(nrow(hrg_output) == nrow(cohort_outpatient_data_user_cost))

# check that these variables are the same for each row
check_vars <- c("mainspef", "tretspef", "startage", "sex")
# Compare only shared variables
matching <- all(mapply(function(x, y) all(x == y, na.rm = TRUE),
                       cohort_outpatient_data_user_cost[check_vars],
                       hrg_output[check_vars]))

if (!matching) {
  warning("⚠️ The order or content of rows does not match between the two datasets!")
} else {
  message("✅ Variables match perfectly by row order — safe to append HRG.")
}

# now append hrg
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  mutate(HRG = hrg_output$NAC_HRG)
#now let's create a 4 letter HRG

cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  mutate(HRG4 = substr(HRG, 1, 4))

# ============================================================
# 14. Calculate cost 
# ============================================================

# bring back info about patients
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>% 
  left_join(cohort_outpatient_data_user, by ="visit_occurrence_id") %>% 
  dplyr::select (-startage) # that was the age at op appointment, not needed anymore

# let's also bring back the tretspef name (now we have code), as we need it to assign costs
tretspef_codes <- tretspef_codes %>%
  rename(tretspef_name = tretspef)

cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>% 
  left_join(tretspef_codes, by = c("tretspef" = "tretspef_code"))

# now let's attach costs based on hrg and tretspef
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  left_join(
    nhs_op_cost_weighted %>%
      dplyr::select(service_description, Currency_Group, weighted_avg_unit_cost),
    by = c("tretspef_name" = "service_description",
           "HRG4" = "Currency_Group")
  )

# check how many OP appointments have no cost
test <-cohort_outpatient_data_user_cost %>% filter(is.na (weighted_avg_unit_cost)) %>% distinct(tretspef_name)

# create cost per patient
cohort_outpatient_user_cost <- cohort_outpatient_data_user_cost %>%
  summarise(total_op_cost = sum(weighted_avg_unit_cost), .by = subject_id)

cohort_outpatient_all_cost <- cohort_outpatient_all %>%
  left_join(cohort_outpatient_user_cost, by = "subject_id", dplyr:: select (subject_id, weighted_avg_unit_cost)) %>%
  mutate(
    total_op_cost = coalesce(total_op_cost, 0L))

# =====================================================
# Outpatient COST analysis
# =====================================================

library(dplyr)
library(boot)
library(ggplot2)
library(scales)
library(MetBrewer)

# =====================================================
# A) Outpatient costs per patient
# =====================================================

# Helper summary function
summarise_costs <- function(df_costs, varname = "total_op_cost") {
  df_costs %>%
    summarise(
      n_pts    = n(),
      total_cost = sum(.data[[varname]], na.rm = TRUE),
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
op_costs_all <- cohort_outpatient_all_cost %>%
  summarise_costs(varname = "total_op_cost") %>%
  mutate(group = "All Patients") %>%
  relocate(group)

## Users only
op_costs_users <- cohort_outpatient_all_cost %>%
  filter(user_status_outpatient == "User") %>%
  summarise_costs(varname = "total_op_cost") %>%
  mutate(group = "Users") %>%
  relocate(group)

# Combine summaries
combined_summary_op_costs <- bind_rows(
  op_costs_all,
  op_costs_users
) %>%
  arrange(group)

# Save outputs
write.csv(combined_summary_op_costs,
          "OMOP Analysis/Results/SecondaryCareAnalysis/OP/GL/combined_summary_op_costs_coactionlogic.csv",
          row.names = FALSE)


# =====================================================
# B) Bootstrap summaries for outpatient costs
# =====================================================

bootstrap_summary_cost <- function(data, varname = "total_op_cost", R = 1000, seed = 626) {
  
  stat_fn <- function(d, i) {
    d_sample <- d[i, ]
    mean(d_sample[[varname]], na.rm = TRUE)
  }
  
  set.seed(seed)
  boot_result <- boot(data = data, statistic = stat_fn, R = R)
  boot_ci <- boot.ci(boot_result, type = c("norm", "basic", "perc"))
  
  boot_mean     <- mean(boot_result$t)
  boot_values   <- as.numeric(boot_result$t)
  boot_variance <- var(boot_values)
  boot_sd       <- sd(boot_values)
  
  ci_norm  <- boot_ci$normal[2:3]
  ci_basic <- boot_ci$basic[4:5]
  ci_perc  <- boot_ci$perc[4:5]
  
  tibble(
    mean = boot_mean,
    variance = boot_variance,
    sd = boot_sd,
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
boot_cost_all <- cohort_outpatient_all_cost %>%
  bootstrap_summary_cost(varname = "total_op_cost") %>%
  mutate(group = "All Patients") %>%
  relocate(group)

## Users only
boot_cost_users <- cohort_outpatient_all_cost %>%
  filter(user_status_outpatient == "User") %>%
  bootstrap_summary_cost(varname = "total_op_cost") %>%
  mutate(group = "Users") %>%
  relocate(group)

# Combine bootstrap results
combined_bt_summary_op_costs <- bind_rows(
  boot_cost_all,
  boot_cost_users
)

# Save bootstrap outputs
write.csv(
  combined_bt_summary_op_costs,
  "OMOP Analysis/Results/SecondaryCareAnalysis/OP/GL/combined_bt_summary_op_costs_coactionlogic.csv",
  row.names = FALSE
)

