library(dplyr)
library(stringr)
library(readxl)

# ============================================================
# 1. Clean Invest/Treat
# ============================================================
ae_investigation <- my_dm$validation_hesae_investigation %>% collect()
ae_treatment <- my_dm$validation_hesae_treatment %>% collect()

## 1.) Investigation
ae_investigation <- ae_investigation %>%
  mutate(
    invest = substr(invest, 1, 2),
  )
# create invest order
ae_investigation <- ae_investigation %>%
  arrange(patid, aekey, invest) %>%        # set order of codes inside each group
  group_by(patid, aekey) %>%
  mutate(invest_seq = row_number()) %>%               # 1,2,3... per patient+aekey
  ungroup() %>%
  pivot_wider(
    id_cols = c(patid, aekey),
    names_from = invest_seq,
    values_from = invest,
    # make zero-padded column names like treat_01, treat_02, ...
    names_glue = "INV_{sprintf('%02d', invest_seq)}",
    values_fill = ""
  )
## clean invalid codes
# INV: 25 is not accepted - so we use 24 that stands for None 
ae_investigation <- ae_investigation %>%
  mutate(across(
    matches("^INV_"),
    ~ ifelse(.x == "25", "24", .x) 
  )) 


####  2.) Treatment
# Most treatment codes are truncated to 2 characters.
# For selected prefixes, 3 characters are retained when available;
# if only 2 characters are present, the modal 3-character code for that prefix is used.
# Prefixes without a reliable mode are mapped to 99 (not specified).

treat_assumed_3char <- c(
  "05" = "051",
  "22" = "222",
  "29" = "291",
  "51" = "511",
  "23" = "232",
  "10" = "101",
  "01" = "011",
  "52" = "522",
  "03" = "031",
  "04" = "041",
  "09" = "092",
  "24" = "243",
  "55" = "554",
  "28" = "282",
  "18" = "181"
)

treat_force_99 <- c("91", "92", "00", "26")

ae_treatment <- ae_treatment %>%
  mutate(
    treat = trimws(treat),
    
    treat_2 = substr(treat, 1, 2),
    treat_3 = substr(treat, 1, 3),
    
    treat = case_when(
      # special prefixes: keep 3 if available
      treat_2 %in% names(treat_assumed_3char) & nchar(treat) >= 3 ~
        treat_3,
      
      # special prefixes: only 2 chars → assume default
      treat_2 %in% names(treat_assumed_3char) & nchar(treat) == 2 ~
        treat_assumed_3char[treat_2],
      
      # force to 99 when only 2 chars
      treat_2 %in% treat_force_99 & nchar(treat) == 2 ~
        "99",
      
      # default: truncate to 2
      TRUE ~ treat_2
    )
  ) %>%
  select(-treat_2, -treat_3)


ae_treatment <- ae_treatment %>%
  arrange(patid, aekey, treat) %>%
  group_by(patid, aekey) %>%
  mutate(treat_seq = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(patid, aekey),
    names_from = treat_seq,
    values_from = treat,
    names_glue = "TREAT_{sprintf('%02d', treat_seq)}",
    values_fill = ""
  )


# ============================================================
# 2. Join clinical information needed for grouper
# ============================================================

cohort_emergency_data_user_cost <- left_join(cohort_emergency_data_user, 
                                             ae_investigation,
                                             by = c("patid", "aekey"))


cohort_emergency_data_user_cost <- left_join(cohort_emergency_data_user_cost, 
                                             ae_treatment,
                                             by = c("patid", "aekey"))

# ============================================================
# 3. Bring in age and gender
# ============================================================
# Merge with the above to get the sex and age which are required in the grouper
cohort_emergency_data_user_cost <- cohort_emergency_data_user_cost %>% 
  left_join(
    person %>% 
      select(patid, gender, yob),
    by = "patid"
  ) 

cohort_emergency_data_user_cost <- cohort_emergency_data_user_cost %>%
  mutate (
    sex = if_else(gender == "F", 2, 1),
    startage = year(arrivaldate) - yob,
  ) %>% select(-gender)

#---------------------------------------------
# lets clean and prepare for grouper

cohort_emergency_data_user_cost_grouper <- cohort_emergency_data_user_cost %>%
  mutate (
    AEPATIENTGROUP = "") %>% 
  dplyr::select(
    startage,
    AEPATIENTGROUP,
    paste0("INV_", sprintf("%02d", 1:12)), # grouper takes up to 12
    paste0("TREAT_", sprintf("%02d", 1:12)),
  ) %>% 
  rename(AGE = startage)

cohort_emergency_data_user_cost_grouper <- cohort_emergency_data_user_cost_grouper %>%
  mutate(
    across(
      starts_with("INV_"),
      ~ replace_na(.x, "")
    ),
    across(
      starts_with("TREAT_"),
      ~ replace_na(.x, "")
    )
  )


# Save outputs for grouper
write.csv(cohort_emergency_data_user_cost_grouper,
          "CPRD Raw Analysis/Results/SecondaryCareAnalysis/AE/GL/cohort_emergency_data_user_cost_grouper.csv",
          row.names = FALSE)


# --- 9) Prepare NHS cost collection ---------------------------------------------------

nhs_em_cost <- rio::import("CPRD Raw Analysis/SRC/SecondaryCare/APC/GL/NHS_Collection_Costs_2023_2024.xlsx", sheet = "EC") 

nhs_em_cost_weighted <- nhs_em_cost %>%
  rename(Currency_Group = `Currency Code`) %>%
  filter(!is.na (Currency_Group)) %>% 
  mutate(
    #Currency_Group = "Currency Code",
    
    # --- Clean numeric columns ---
    # Replace '*' with '3' BEFORE removing commas and converting to numeric
    n_attendances = str_replace_all(`Number of attendances`, "\\*", "3"),
    n_attendances = as.numeric(str_remove_all(n_attendances, "[,]")),
    
    cost = as.numeric(str_remove_all(Cost, "[£,]"))
    #service_description = str_to_lower(str_trim(`Service Description`))
  ) %>%
  group_by( Currency_Group) %>%
  summarise(
    total_cost = sum(cost, na.rm = TRUE),
    total_attendances = sum(n_attendances, na.rm = TRUE),
    weighted_avg_unit_cost = total_cost / total_attendances,
    .groups = "drop"
  )


# View result
print(nhs_em_cost_weighted)

# ============================================================
# 13. Append cost 
# ============================================================

hrg_output <- read.csv(
  "CPRD Raw Analysis/SRC/SecondaryCare/AE/GL/output_emergency_source_attend.csv",
  colClasses = "character"
)

# in the grouper we could not run the aekey but the output is in the same order, let's make some checks anyway
cohort_emergency_data_user_cost <- cohort_emergency_data_user_cost %>% 
  rename(AGE = startage)

#check n columns is the same
stopifnot(nrow(hrg_output) == nrow(cohort_emergency_data_user_cost))

# check that these variables are the same for each row
check_vars <- c( "INV_01", "INV_02", "INV_03", "TREAT_01", "TREAT_02", "TREAT_03", "AGE")
# Compare only shared variables
matching <- all(mapply(function(x, y) all(x == y, na.rm = TRUE),
                       cohort_emergency_data_user_cost[check_vars],
                       hrg_output[check_vars]))

if (!matching) {
  warning("⚠️ The order or content of rows does not match between the two datasets!")
} else {
  message("✅ Variables match perfectly by row order — safe to append HRG.")
}


# now append hrg
cohort_emergency_data_user_cost <- cohort_emergency_data_user_cost %>%
  mutate(HRG = hrg_output$EM_HRG)
#now let's create a 4 letter HRG

# delete spells with errors
cohort_emergency_data_user_cost <- cohort_emergency_data_user_cost %>%  
  filter(HRG != "UZ01Z") # u means error in spell grouping method

# cohort_emergency_data_user_cost <- cohort_emergency_data_user_cost %>%
#   mutate(HRG4 = substr(HRG, 1, 4))

# ============================================================
# 14. Calculate cost 
# ============================================================

# now let's attach costs based on hrg and tretspef
cohort_emergency_data_user_cost <- cohort_emergency_data_user_cost %>%
  left_join(
    nhs_em_cost_weighted %>%
      dplyr::select(Currency_Group, weighted_avg_unit_cost),
    by = c("HRG" = "Currency_Group")
  )

# check how many AE appointments have no cost
test <-cohort_emergency_data_user_cost %>% filter(is.na (weighted_avg_unit_cost))

# create cost per patient
cohort_emergency_user_cost <- cohort_emergency_data_user_cost %>%
  summarise(total_ae_cost = sum(weighted_avg_unit_cost), .by = patid)

cohort_emergency_all_cost <- cohort_emergency_all %>%
  left_join(cohort_emergency_user_cost, by = "patid", dplyr:: select (subject_id, weighted_avg_unit_cost)) %>%
  mutate(
    total_ae_cost = coalesce(total_ae_cost, 0L))


# =====================================================
# A) Emergency costs per patient
# =====================================================

# Helper summary function
summarise_costs <- function(df_costs, varname = "total_ae_cost") {
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
ae_costs_all <- cohort_emergency_all_cost %>%
  summarise_costs(varname = "total_ae_cost") %>%
  mutate(group = "All Patients") %>%
  relocate(group)

## Users only
ae_costs_users <- cohort_emergency_all_cost %>%
  filter(user_status_emergency == "User") %>%
  summarise_costs(varname = "total_ae_cost") %>%
  mutate(group = "Users") %>%
  relocate(group)

# Combine summaries
combined_summary_ae_costs <- bind_rows(
  ae_costs_all,
  ae_costs_users
) %>%
  arrange(group)

# Save outputs
write.csv(combined_summary_ae_costs,
          "CPRD Raw Analysis/SRC/SecondaryCare/AE/GL/combined_summary_ae_costs_coactionlogic.csv",
          row.names = FALSE)

# =====================================================
# B) Bootstrap summaries for emergency costs
# =====================================================

bootstrap_summary_cost <- function(data, varname = "total_ae_cost", R = 1000, seed = 626) {
  
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
boot_cost_all <- cohort_emergency_all_cost %>%
  bootstrap_summary_cost(varname = "total_ae_cost") %>%
  mutate(group = "All Patients") %>%
  relocate(group)

## Users only
boot_cost_users <- cohort_emergency_all_cost %>%
  filter(user_status_emergency == "User") %>%
  bootstrap_summary_cost(varname = "total_ae_cost") %>%
  mutate(group = "Users") %>%
  relocate(group)

# Combine bootstrap results
combined_bt_summary_ae_costs <- bind_rows(
  boot_cost_all,
  boot_cost_users
)
