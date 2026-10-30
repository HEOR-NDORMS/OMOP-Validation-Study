library(dplyr)
library(stringr)
library(readxl)

# ============================================================
# 1. Load validated specialty mapping
# ============================================================
validated_map <- rio::import("OMOP-Validation-Study/OMOP Analysis/SRC/secondary_care/check_mainspef_tretspef_inpatient_outpatient_ValidationStudy_omop_standard_final.xlsx") %>% 
  mutate(
    # use the column with the concept_name
    concept_name = str_to_lower(
      str_trim(concept_name)
    ),
    mainspef = str_to_lower(str_trim(mainspef)),
    tretspef = str_to_lower(str_trim(tretspef))
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
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user 

# %>%
#   mutate(
#     specialty_source_value_modified = specialty_source_value %>%
#       str_remove("\\s*\\([^\\)]+\\)") %>%
#       str_replace_all("&", "and") %>%
#       str_to_lower() %>%
#       str_trim()
#   )

# ============================================================
# 3. Join validated mapping
# ============================================================
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  left_join(
    validated_map %>%
      dplyr::select(specialty_concept_id, mainspef, tretspef, mainspef_code, tretspef_code) %>%
      distinct(),
    by = "specialty_concept_id")


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
# # missing tretspef

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

# First attendance is not preserved as a source value in OMOP-standard, so it
# is derived: a patient's earliest outpatient visit within a given treatment
# specialty (tretspef) is treated as the first attendance (1); every
# subsequent visit to that same specialty is a follow-up (2). This replaces
# the previous "every row = 1" assumption, which priced 100% of appointments
# as first attendances and inflated cost relative to Source.
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  dplyr::select(visit_occurrence_id, subject_id, visit_start_date, age_op, sex,
                mainspef_code, tretspef_code) %>%
  arrange(subject_id, tretspef_code, visit_start_date, visit_occurrence_id) %>%
  group_by(subject_id, tretspef_code) %>%
  mutate(firstatt = if_else(row_number() == 1, 1, 2)) %>%
  ungroup() %>%
  dplyr::select(-subject_id, -visit_start_date) %>%
  rename(startage = age_op,
         mainspef = mainspef_code,
         tretspef = tretspef_code)

cohort_outpatient_data_user_cost_grouper <- cohort_outpatient_data_user_cost %>%
  dplyr:: select(-visit_occurrence_id)


# Save outputs for grouper
rio::export(cohort_outpatient_data_user_cost_grouper, here::here("OMOP-Validation-Study", "OMOP Analysis", "SRC",
                                                                 "secondary_care", "OP",
                                          "cohort_outpatient_data_user_cost_grouper_omop_standard_after_review.csv"))
# ============================================================
# 12. Prepare NHS cost collection
# ============================================================

nhs_op_cost <- read_excel("OMOP-Validation-Study/OMOP Analysis/SRC/secondary_care/NHS_Collection_Costs_2023_2024.xlsx", sheet = "OP")


nhs_op_cost_weighted <- nhs_op_cost %>%
  filter(!is.na (`Currency Code`)) %>% 
  mutate(
    # full-length Currency Code — truncating to 4 characters previously
    # collapsed genuinely distinct currencies sharing a prefix (e.g. WF01A
    # "first attendance" vs WF01B "follow-up" were being averaged together;
    # same issue for procedure currencies like IVF01 vs IVF02)
    hrg = `Currency Code`,
    
    n_attendances = str_replace_all(`Number of attendances`, "\\*", "3"),
    n_attendances = as.numeric(str_remove_all(n_attendances, "[,]")),
    
    cost = as.numeric(str_remove_all(Cost, "[£,]")),
    service_description = str_to_lower(str_trim(`Service Description`))
  ) %>%
  group_by(service_description, hrg) %>%
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

hrg_output <- read.csv("OMOP-Validation-Study/OMOP Analysis/SRC/secondary_care/OP/output_outpatient_omop_standard_after_review_attend_rel.csv")

# in the grouper we could not run the visit_occurrence_id but the output is in the same order, let's make some checks anyway

#check n columns is the same
stopifnot(nrow(hrg_output) == nrow(cohort_outpatient_data_user_cost))

# check that these variables are the same for each row
check_vars <- c("mainspef", "tretspef", "startage", "sex", "firstatt")
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
# now let's attach costs based on hrg and tretspef
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  left_join(
    nhs_op_cost_weighted %>%
      dplyr::select(service_description, hrg, weighted_avg_unit_cost),
    by = c("tretspef_name" = "service_description",
           "HRG"  = "hrg")
  )

cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  mutate(
    weighted_avg_unit_cost = case_when(
      tretspef_name == "learning disability service" ~ 39,
      TRUE ~ weighted_avg_unit_cost
    )
  )

# #TEST to fix clinical physiology

# value used to test with 24/25 = 89.98686


# check how many OP appointments have no cost
test <-cohort_outpatient_data_user_cost %>% filter(is.na (weighted_avg_unit_cost)) %>% distinct(tretspef_name)


# create cost per patient — sum only the costed/groupable attendances;
# track how many of each patient's attendances couldn't be costed
cohort_outpatient_user_cost <- cohort_outpatient_data_user_cost %>%
  summarise(
    total_op_cost      = sum(weighted_avg_unit_cost, na.rm = TRUE),
    n_uncosted_op_visits = sum(is.na(weighted_avg_unit_cost)),
    .by = subject_id
  )

cohort_outpatient_all_cost <- cohort_outpatient_all %>%
  left_join(cohort_outpatient_user_cost, by = "subject_id") %>%
  mutate(
    n_uncosted_op_visits = coalesce(n_uncosted_op_visits, 0L),
    total_op_cost = case_when(
      total_op == 0                          ~ 0,          # true non-user
      n_uncosted_op_visits == total_op       ~ NA_real_,    # every visit uncostable -> excluded from cost stats
      TRUE                                    ~ total_op_cost  # >=1 costed visit -> real partial cost
    )
  )

# check number of uncosted spells
cohort_outpatient_all_cost %>%
  summarise(
    n_users_with_any_uncosted   = sum(n_uncosted_op_visits > 0),
    n_users_fully_uncosted      = sum(total_op > 0 & n_uncosted_op_visits == total_op),
    total_uncosted_op       = sum(n_uncosted_op_visits),
    total_op                = sum(total_op),
    pct_op_uncosted         = round(100 * total_uncosted_op / total_op, 2)
  )

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
      n_pts      = n(),
      n_costed   = sum(!is.na(.data[[varname]])),
      total_cost = sum(.data[[varname]], na.rm = TRUE),
      mean       = mean(.data[[varname]], na.rm = TRUE),
      min        = min(.data[[varname]], na.rm = TRUE),
      max        = max(.data[[varname]], na.rm = TRUE),
      sd         = sd(.data[[varname]], na.rm = TRUE),
      median     = quantile(.data[[varname]], 0.5, na.rm = TRUE),
      lower_q    = quantile(.data[[varname]], 0.25, na.rm = TRUE),
      upper_q    = quantile(.data[[varname]], 0.75, na.rm = TRUE),
      se         = sd / sqrt(n_costed),
      ci_lower   = mean - qt(0.975, df = n_costed - 1) * se,
      ci_upper   = mean + qt(0.975, df = n_costed - 1) * se,
      .groups    = "drop"
    ) %>%
    mutate(across(where(is.numeric) & !any_of(c("n_pts", "n_costed")), ~ round(.x, 2)))
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
          "OMOP-Validation-Study/OMOP Analysis/Results/Final/Secondary care/OP/combined_summary_op_costs_omop_standard.csv",
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
    n_pts    = nrow(data),
    n_costed = sum(!is.na(data[[varname]])),
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
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Secondary care/OP/combined_bt_summary_op_costs_omop_standard.csv",
  row.names = FALSE
)


# --- Count of categories ---
op_categories <- rio::import("OMOP-Validation-Study/OMOP Analysis/SRC/secondary_care/OP/outpatient_specialty_categories_hes_omop_final.xlsx", sheet = "OP_OMOP_standard") %>% select(specialty_concept_id, Category)

op_categories <- op_categories %>%
  distinct()

cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>% 
  left_join(op_categories, by="specialty_concept_id")

overall_category_count_op <- cohort_outpatient_data_user_cost %>% 
  group_by(Category) %>% 
  summarise(count_category = n(), .groups = "drop") %>% 
  mutate(percent_category = round(count_category / sum(count_category) * 100, 2)) %>% 
  select(Category, count_category, percent_category)

write.csv(
  overall_category_count_op,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Secondary care/OP/overall_category_count_op_omop_standard.csv",
  row.names = FALSE
)
