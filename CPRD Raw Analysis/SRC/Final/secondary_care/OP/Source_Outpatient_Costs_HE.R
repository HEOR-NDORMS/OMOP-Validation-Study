library(dplyr)
library(stringr)
library(readxl)

# ============================================================
# 1. Join clinical information needed for grouper
# ============================================================
validation_clinical <- my_dm$validation_hesop_clinical %>% collect()

cohort_outpatient_data_user_cost <- left_join(cohort_outpatient_data_user, 
                                   validation_clinical,
                                   by = c("patid", "attendkey"))

# clean OPERT01 variable reporting & - values
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>% 
  mutate(
    opertn_01 = if_else(opertn_01 %in% c("&", "-"), NA_character_, opertn_01),
    mainspef = if_else(mainspef %in% c("&", "-"), NA_character_, mainspef),
    tretspef = if_else(tretspef %in% c("&", "-"), NA_character_, tretspef),
    
  )

# ============================================================
# 2. Cleaning mainspef/tretspef
# ============================================================

# 2.1) Remove invalid codes
invalid_mainspef <- c("812","654","318","652","191","650","651","653","722","324","307","144","658")
invalid_tretspef <- c("950","900","810","960")

cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  mutate(
    mainspef = if_else(mainspef %in% invalid_mainspef, NA_character_, mainspef),
    tretspef = if_else(tretspef %in% invalid_tretspef, NA_character_, tretspef)
  )

# 2.2) If only MAINSPEF missing, copy from TRETSPEF (and vice versa) ----
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  mutate(
    mainspef = if_else(
      is.na(mainspef) & !is.na(tretspef) & !tretspef %in% invalid_mainspef,
      tretspef,
      mainspef
    ),
    tretspef = if_else(
      is.na(tretspef) & !is.na(mainspef) & !mainspef %in% invalid_tretspef,
      mainspef,
      tretspef
    )
  )

# 2.3) If mainspef/tretspef or both are still missing we replace with the mode across all appointments with same tretspef/mainspef or global mode if that does not exist

##

get_mode <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# global mode
global_mode_mainspef <- get_mode(cohort_outpatient_data_user_cost$mainspef)
global_mode_tretspef <- get_mode(cohort_outpatient_data_user_cost$tretspef)

# conditional modes
# TRETSPEF mode by MAINSPEF
tretspef_mode_by_mainspef <- cohort_outpatient_data_user_cost %>%
  group_by(mainspef) %>%
  summarise(
    mode_tretspef = get_mode(tretspef),
    .groups = "drop"
  )

# MAINSPEF mode by TRETSPEF
mainspef_mode_by_tretspef <- cohort_outpatient_data_user_cost %>%
  group_by(tretspef) %>%
  summarise(
    mode_mainspef = get_mode(mainspef),
    .groups = "drop"
  )

cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  left_join(tretspef_mode_by_mainspef, by = "mainspef") %>%
  left_join(mainspef_mode_by_tretspef, by = "tretspef") %>%
  mutate(
    tretspef = if_else(
      is.na(tretspef),
      coalesce(mode_tretspef, global_mode_tretspef),
      tretspef
    ),
    mainspef = if_else(
      is.na(mainspef),
      coalesce(mode_mainspef, global_mode_mainspef),
      mainspef
    )
  ) %>%
  select(-mode_tretspef, -mode_mainspef)


# ============================================================
# 3. Bring in age and gender
# ============================================================
person <- my_dm$`_validation` %>% collect()
# Merge with the above to get the sex and age which are required in the grouper
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>% 
  left_join(
    person %>% 
      select(patid, gender, yob),
    by = "patid"
  ) 

cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  mutate (
    sex = if_else(gender == "F", 2, 1),
    startage = year(apptdate) - yob,
  ) %>% select(-gender)

# ============================================================
# 4. Firstatt (when is "X" we replace with 4 which is least intensive)
# ============================================================
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>% 
  mutate(firstatt = case_when(
    firstatt == "X" ~ "4",
    TRUE ~ firstatt
  ))

# ============================================================
# 5. Save outputs for grouper
# ============================================================

cohort_outpatient_data_user_cost_grouper <- cohort_outpatient_data_user_cost %>% 
  select(startage,sex,mainspef,tretspef,firstatt)

write.csv(cohort_outpatient_data_user_cost_grouper,
          "CPRD Raw Analysis/SRC/Final/secondary_care/OP/cohort_outpatient_data_user_cost_grouper.csv",
          row.names = FALSE)

# ============================================================
# 6. Prepare NHS cost collection
# ============================================================

nhs_op_cost <- read_excel("NHS_Collection_Costs_2023_2024.xlsx", sheet = "OP")

nhs_op_cost_weighted <- nhs_op_cost %>%
  rename(currency_group = `Currency Code`) %>%
  mutate(
    n_attendances = str_replace_all(`Number of attendances`, "\\*", "3"),
    n_attendances = as.numeric(str_remove_all(n_attendances, "[,]")),
    
    cost = as.numeric(str_remove_all(Cost, "[£,]")),
    service_description = str_to_lower(str_trim(`Service Description`))
  ) %>%
  group_by(service_description, currency_group) %>%
  summarise(
    total_cost = sum(cost, na.rm = TRUE),
    total_attendances = sum(n_attendances, na.rm = TRUE),
    weighted_avg_unit_cost = total_cost / total_attendances,
    .groups = "drop"
  )


# View result
print(nhs_op_cost_weighted)

# ============================================================
# 7. Append cost 
# ============================================================

hrg_output <- read.csv("CPRD Raw Analysis/SRC/Final/secondary_care/OP/grouper_outpatient_source_output_attend_rel.csv")

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

# #now let's create a 4 letter HRG

# ============================================================
# 14. Calculate cost 
# ============================================================

# bring back info about patients

# let's also bring back the tretspef name (now we have code), as we need it to assign costs
mainspef_tretspef_dictionary <- rio::import("CPRD Raw Analysis/SRC/Final/secondary_care/OP/mainspef_tretspef_dictionary.xlsx") 

tretspef_codes <- mainspef_tretspef_dictionary %>%
  mutate(
    tretspef = str_to_lower(str_trim(tretspef))
  ) %>%
  select(tretspef_name = tretspef, tretspef_code) %>%
  distinct() %>%
  filter(!is.na(tretspef_name)) %>%
  mutate(
    tretspef_code = as.character(tretspef_code)
  )

cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>% 
  left_join(tretspef_codes, by = c("tretspef" = "tretspef_code"))

# check if there is any mismatch
sum(is.na(cohort_outpatient_data_user_cost$tretspef_name) &
      !is.na(cohort_outpatient_data_user_cost$tretspef))

# now let's attach costs based on hrg and tretspef
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  left_join(
    nhs_op_cost_weighted %>%
      dplyr::select(service_description, currency_group, weighted_avg_unit_cost),
    by = c("tretspef_name" = "service_description",
           "HRG" = "currency_group")
  )

# ## TEMPORRARY FIX ########
cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  mutate(
    weighted_avg_unit_cost = case_when(
      tretspef_name == "learning disability service" ~ 39,
      TRUE ~ weighted_avg_unit_cost
    )
  )


# check how many OP appointments have no cost
cohort_outpatient_data_user_cost %>% filter(is.na (weighted_avg_unit_cost)) %>% distinct(tretspef_name)


# create cost per patient — sum only the costed/groupable attendances;
# track how many of each patient's attendances couldn't be costed
cohort_outpatient_user_cost <- cohort_outpatient_data_user_cost %>%
  summarise(
    total_op_cost      = sum(weighted_avg_unit_cost, na.rm = TRUE),
    n_uncosted_op_visits = sum(is.na(weighted_avg_unit_cost)),
    .by = patid
  )

cohort_outpatient_all_cost <- cohort_outpatient_all %>%
  left_join(cohort_outpatient_user_cost, by = "patid") %>%
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

# =====================================================
# A) Outpatient costs per patient
# =====================================================

# Helper summary function
summarise_costs <- function(df_costs, varname = "total_op_cost") {
  df_costs %>%
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
      ci_lower   = mean - qt(0.975, df = n_costed - 1) * se,
      ci_upper   = mean + qt(0.975, df = n_costed - 1) * se,
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
          "CPRD Raw Analysis/Results/Final/Secondary care/OP/combined_summary_op_costs.csv",
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
  "CPRD Raw Analysis/Results/Final/Secondary care/OP/combined_bt_summary_op_costs.csv",
  row.names = FALSE
)


# specialty categories by tretspef (matches the OMOP provider specialty, which the ETL takes from tretspef)
op_categories <- rio::import("CPRD Raw Analysis/SRC/Final/secondary_care/OP/outpatient_specialty_categories_hes_omop_final.xlsx",
                             sheet = "OP_Source-HES") %>%
  mutate(tretspef_name = str_to_lower(str_trim(Description))) %>%
  select(tretspef_name, Category) %>%
  distinct()

cohort_outpatient_data_user_cost <- cohort_outpatient_data_user_cost %>%
  left_join(op_categories, by = "tretspef_name")

# check nothing is left uncategorised
cohort_outpatient_data_user_cost %>%
  filter(is.na(Category)) %>%
  group_by(tretspef, tretspef_name) %>%
  tally(sort = TRUE)

# --- Count of categories ---
overall_category_count_op <- cohort_outpatient_data_user_cost %>% 
  group_by(Category) %>% 
  summarise(count_category = n(), .groups = "drop") %>% 
  mutate(percent_category = round(count_category / sum(count_category) * 100, 2))

write.csv(overall_category_count_op,
          "CPRD Raw Analysis/Results/Final/Secondary care/OP/overall_category_count_op.csv",
          row.names = FALSE)