# =====================================================
# Inpatient Episode Preparation Script for COSTS/Grouper
# =====================================================

library(dplyr)
library(boot)
library(purrr)
library(stringr)
library (lubridate)
library(tidyr)
library(ggplot2)

# --- 1) Episodes in study period -----------------------------------------
episodes <- my_dm$validation_hes_episodes %>%
  filter(
    epistart >= study_start_date,
    epiend <= study_end_date
  ) %>% collect()

# --- 2) Restrict to episodes belonging to included spells ----------------
valid_spell_ids <- cohort_hospital_data_user %>%
  filter(!is.na(spno)) %>%
  pull(spno) %>%
  unique()

cohort_hospital_episodes_data_user <- episodes %>%
  filter(spno %in% valid_spell_ids)

# --- 3) Admission / Discharge variables ----------------------------------
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>% 
  left_join(spells %>% select (spno, admisorc, admimeth, disdest, dismeth),
            by = "spno")

# --- 4) Diagnoses ---------------------------------------------------------
diagnoses <- my_dm$validation_hes_diagnosis_epi %>% collect()

diagnoses_wide <- diagnoses %>%
  # clean and ensure ordering
  mutate(
    icd = gsub("\\.", "", icd),        # drop dots
  ) %>%
  # label columns as DIAG_01, DIAG_02, ...
  mutate(diag_label = sprintf("DIAG_%02d", d_order)) %>%
  select(epikey, patid, spno, diag_label, icd) %>% # we keep patid e spno 
  pivot_wider(
    id_cols = c(patid,spno,epikey),   # one row per epikey, patid, spno because I noticed that the same epikey can be found for another patient or spno
    names_from = diag_label,          
    values_from = icd,
    values_fill = NA
  )

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  left_join(diagnoses_wide, by = c("patid", "spno", "epikey"))

# extract list o diag with 3 characters (4 are needed)
diagnoses_3char <- cohort_hospital_episodes_data_user %>%
  dplyr::select(matches("^DIAG_")) %>%
  pivot_longer(everything(), names_to = "diag_col", values_to = "code") %>%
  filter(!is.na(code), code != "", str_length(code) == 3) %>%
  distinct(code)

# add X to diagnoses with 3 characters) 
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(across(matches("^DIAG_"),
                ~ ifelse(!is.na(.x) & str_length(.x) == 3, str_c(.x, "X"), .x)))

# remove Diag that are not recognised by 2024-25 grouper

# There are no I84 in list of accepted ICD10 - we drop them 
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(across(
    matches("^DIAG_"),
    ~ ifelse(str_detect(.x, "^I84"), NA, .x)
  ))

# ICD: I48 needs a fourth character but this cannot be the X so we use 9 that stands for unspecified 
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(across(
    matches("^DIAG_"),
    ~ ifelse(str_detect(.x, "^I48"), "I489", .x)
  ))


# ICD: G90.3 is not accepted - so we use 9 that stands for unspecified 
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(across(
    matches("^DIAG_"),
    ~ ifelse(.x == "G903", "G909", .x)
  ))

# ICD: Z22.5 is not accepted - so we use 9 that stands for unspecified 
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(across(
    matches("^DIAG_"),
    ~ ifelse(.x == "Z225", "Z229", .x)
  ))

# ICD: A09X is not accepted - so we use 9 that stands for unspecified 
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(across(
    matches("^DIAG_"),
    ~ ifelse(.x == "A09X", "A099", .x)
  ))

# There are no U80|U81|U88|U89 in list of accepted ICD10 - we drop them
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(across(
    matches("^DIAG_"),
    ~ ifelse(str_detect(.x, "^(U80|U81|U88|U89)"), NA, .x)
  ))

#----------------
# Count episodes without a primary diagnosis 
noprimarydiag <- cohort_hospital_episodes_data_user %>%
  filter(is.na(DIAG_01)) %>% tally

# mode imputation for episodes without primary diag using the primary diag of other episodes in the same spell
mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  group_by(spno) %>%
  mutate(
    diag_mode = mode_value(DIAG_01),
    DIAG_01 = if_else(is.na(DIAG_01), diag_mode, DIAG_01)
  ) %>%
  ungroup() %>%
  filter(!is.na(DIAG_01)) %>%       # drop only those episodes still missing (i.e., spells with one epi that has no diag)
  dplyr::select(-diag_mode)

# clean invalid diagnoses codes

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  filter(!(DIAG_01 %in% c("R69X", "T140"))) # drop episode


# --- 5) Procedures --------------------------------------------------------

procedures <- my_dm$validation_hes_procedures_epi %>% collect()

procedures_wide <- procedures %>%
  # clean and ensure ordering
  mutate(
    opcs = gsub("\\.", "", opcs),        # drop dots
  ) %>%
  # label columns as OPER_01, OPER_02, ...
  mutate(oper_label = sprintf("OPER_%02d", p_order)) %>%
  select(epikey, patid, spno, oper_label, opcs) %>% # we keep patid e spno 
  pivot_wider(
    id_cols = c(patid,spno,epikey),   # one row per epikey, patid, spno because I noticed that the same epikey can be found for another patient or spno
    names_from = oper_label,          
    values_from = opcs,
    values_fill = NA
  )

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  left_join(procedures_wide,  by = c("patid", "spno", "epikey"))

# Remove operations with only OPER codes starting with Z or Y (subsidiary) - if Z/Y are paired with other codes leave
# Identify diagnosis columns
oper_cols <- grep("^OPER_", names(cohort_hospital_episodes_data_user), value = TRUE)

# Check rows where *all* OPER_ columns start with Z or Y (ignoring missing)
only_ZY <- apply(
  cohort_hospital_episodes_data_user[oper_cols], 
  1, 
  function(x) {
    x <- na.omit(x)
    length(x) > 0 && all(grepl("^[ZY]", x))
  }
)

# Count how many rows are affected
n_removed <- sum(only_ZY)

# Remove them
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user[!only_ZY, ]

# Define invalid or retired operation codes
invalid_oper_codes <- c(
  "G802", "Z466", "Z450", "Z514", "O831", "Z400", "Z480", "Z390", "Z516",
  "Z290", "Z513", "Z512", "Z467", "Z430", "Z006", "Z460", "Z465", "Z510",
  "Z005", "Z030", "Z026", "&"
)

#  count
n_invalid <- sum(sapply(
  cohort_hospital_episodes_data_user[oper_cols],
  function(x) sum(toupper(trimws(x)) %in% invalid_oper_codes, na.rm = TRUE)
))
n_invalid

# Replace invalid codes with NA
cohort_hospital_episodes_data_user[oper_cols] <- lapply(
  cohort_hospital_episodes_data_user[oper_cols],
  function(x) {
    x <- toupper(trimws(x))
    x[x %in% invalid_oper_codes] <- NA
    x
  }
)

# OPCS: Z010 is not accepted - so we use 9 that stands for unspecified 
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(across(
    matches("^OPER_"),
    ~ ifelse(str_detect(.x, "Z010"), "Z019", .x)
  ))

# --- 6) Mainspef/Tretspef---------------------------------------------------

#  "&" means "not known" and is replaced with mainspef code
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>% 
  mutate(
    mainspef = if_else(mainspef %in% c("&", "-"), NA_character_, mainspef),
    tretspef = if_else(tretspef %in% c("&", "-"), NA_character_, tretspef),
  )

# 1) If only MAINSPEF missing, copy from TRETSPEF (and vice versa) ----

invalid_mainspef <- c( # derived from dictionary
  102, 103, 104, 105, 106, 108, 109, 111, 113, 115,
  144, 161, 172, 173, 174, 211, 212, 213, 214, 215,
  216, 217, 218, 219, 220, 221, 222, 223, 230, 240,
  241, 242, 250, 251, 252, 253, 254, 255, 256, 257,
  258, 259, 260, 261, 262, 263, 264, 270, 280, 290,
  291, 306, 307, 308, 309, 316, 318, 319, 322, 323,
  721, 343, 324, 660, 840, 675, 327, 656, 344, 331,
  727, 920, 812, 654, 659, 720, 505, 677, 503, 335,
  811, 722, 346, 726, 725, 661, 422, 730, 651, 461,
  662, 431, 655, 658, 724, 650, 663, 653, 348, 657,
  723, 342, 333, 341, 347, 345, 652, 328, 329, 670,
  673, 424
)
invalid_tretspef <- c(
  146, 147, 148, 149, 312, 500, 600, 601,
  810, 820, 821, 823, 824, 830, 831, 833,
  900, 901, 902, 903, 904, 950, 960
)

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
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

# 2) If mainspef/tretspef or both are still missing we replace with the mode across all appointments with same tretspef/mainspef or global mode if that does not exist

get_mode <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# global mode
global_mode_mainspef <- get_mode(cohort_hospital_episodes_data_user$mainspef)
global_mode_tretspef <- get_mode(cohort_hospital_episodes_data_user$tretspef)

# conditional modes
# TRETSPEF mode by MAINSPEF
tretspef_mode_by_mainspef <- cohort_hospital_episodes_data_user %>%
  group_by(mainspef) %>%
  summarise(
    mode_tretspef = get_mode(tretspef),
    .groups = "drop"
  )

# MAINSPEF mode by TRETSPEF
mainspef_mode_by_tretspef <- cohort_hospital_episodes_data_user %>%
  group_by(tretspef) %>%
  summarise(
    mode_mainspef = get_mode(mainspef),
    .groups = "drop"
  )

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
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



# --- 7) Add age and sex ---------------------------------------------------
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  left_join(
    all_patients %>%
      dplyr::select(patid, yob, gender),
    by = "patid",
    copy = TRUE
  ) %>%
  mutate(
    startage = year(epistart) - yob,
    sex = case_when(
           gender == "M" ~ 1,# Female
           gender == "F" ~ 2,
           TRUE          ~ NA_real_

    ))

# --- 8) Classpat
# we check and correct for spells that have episodes with different classpats, if found we use the mode (same formula as above)

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  group_by(spno) %>%
  mutate(
    n_classpat = n_distinct(classpat, na.rm = TRUE),
    classpat_mode = mode_value(classpat),
    classpat = if_else(n_classpat > 1 & !is.na(classpat_mode), classpat_mode, classpat)
  ) %>%
  ungroup() %>%
  select(-n_classpat, -classpat_mode)

#---------------------------------------------
# lets clean and prepare for grouper

cohort_hospital_episodes_data_user_grouper <- cohort_hospital_episodes_data_user %>%
  rename(
    provspno = spno,
    epiorder = eorder
  ) %>% 
  mutate (
    neocare = NA,
    criticalcaredays = NA,
    rehabilitationdays = NA,
    spcdays = NA,
    procodet = "ABC"
  )

cohort_hospital_episodes_data_user_grouper <- cohort_hospital_episodes_data_user_grouper %>%
  dplyr::select(
    procodet,
    provspno,
    epiorder,
    startage,
    sex,
    classpat,
    admisorc,
    admimeth,
    disdest,
    dismeth,
    epidur,
    mainspef,
    neocare,
    tretspef,
    paste0("DIAG_", sprintf("%02d", 1:14)), # grouper takes up to 14 diag and 12 oper
    paste0("OPER_", sprintf("%02d", 1:12)),
    criticalcaredays,
    rehabilitationdays,
    spcdays
  )

cohort_hospital_episodes_data_user_grouper <- cohort_hospital_episodes_data_user_grouper %>% 
  mutate(across(everything(), ~replace(., is.na(.), ""))) # replace NA with empty

## Sex, amimeth, amisorc must be the same for all episodes within spells

cohort_hospital_episodes_data_user_grouper <- cohort_hospital_episodes_data_user_grouper %>%
  group_by(provspno) %>%
  mutate(
    admimeth = if (n_distinct(admimeth, na.rm = TRUE) > 1)
      admimeth[which.min(epiorder)]
    else
      admimeth,
    admisorc = if (n_distinct(admisorc, na.rm = TRUE) > 1)
      admisorc[which.min(epiorder)]
    else
      admisorc
  ) %>%
  ungroup()

# Save outputs for grouper
write.csv(cohort_hospital_episodes_data_user_grouper,
          "CPRD Raw Analysis/SRC/Final/secondary_care/APC/cohort_hospital_episodes_data_user_grouper.csv",
          row.names = FALSE)

# --- 9) Prepare NHS cost collection ---------------------------------------------------

nhs_im_cost <- rio::import("NHS_Collection_Costs_2023_2024.xlsx", sheet = "APC") 

nhs_im_cost_weighted <- nhs_im_cost %>%
  mutate(
    # since * means less than five we replace with 3
    n_fce = str_replace_all(`Number of finished consultant episodes`, "\\*", "3"),
    n_fce = as.numeric(str_remove_all(n_fce, "[,]")),
    hrg = `Currency Code`,
    cost = as.numeric(str_remove_all(Cost, "[£,]")),
  )%>%
  group_by (hrg) %>%
  summarise(
    total_cost = sum(cost, na.rm = TRUE),
    total_fce = sum(n_fce, na.rm = TRUE),
    weighted_avg_unit_cost = total_cost / total_fce,
    .groups = "drop"
  ) %>%   filter(!is.na(hrg)) 

# --- 10) Append HRG and Costs  ---------------------------------------------------

# Import HRG output ---
hrg_output_inpatient <- read.csv("CPRD Raw Analysis/SRC/Final/secondary_care/APC/Output_inpatient_grouper_source_spell_rel.csv") 

# delete spells with errors
hrg_output_inpatient <- hrg_output_inpatient %>%  
  filter(SpellGroupingMethodFlag != "U") # u means error in spell grouping method

# Append HRGs to cohort SPELL data ---
cohort_hospital_data_user_cost <- cohort_hospital_data_user %>%
  left_join(
    hrg_output_inpatient %>%
      dplyr::select(PROCODET, PROVSPNO, SpellHRG, SpellDominantProcedure, SpellPDiag, SpellGroupingMethodFlag),
    by = c(
      "spno" = "PROVSPNO"
    )
  )

# Append costs ---
cohort_hospital_data_user_cost <- cohort_hospital_data_user_cost %>%
  left_join(
    nhs_im_cost_weighted %>% dplyr::select(hrg, weighted_avg_unit_cost),
    by = c("SpellHRG" = "hrg")
  )

# check HRGs without cost
test <-cohort_hospital_data_user_cost %>%
  filter(is.na(weighted_avg_unit_cost)|weighted_avg_unit_cost == 0) %>%
  distinct(SpellHRG)

# HRG: LA97A - sourced from NHS Devon literature
cohort_hospital_data_user_cost <- cohort_hospital_data_user_cost %>%
  mutate(weighted_avg_unit_cost = case_when(
    SpellHRG == "LA97A" ~ 144,
    TRUE ~ weighted_avg_unit_cost
  ))

# HRG SC97Z - we give the same cost as SB97Z
sb_vals <- nhs_im_cost_weighted %>%
  filter(hrg == "SB97Z") %>%
  pull(weighted_avg_unit_cost) %>%
  unique()

if (length(sb_vals) == 0) {
  stop("SB97Z not found in nhs_im_cost_weighted — check hrgs or spelling")
} else {
  SB97Z_cost <- sb_vals[1]  # use the unique value; if multiple, take first (or handle explicitly)
  if (length(sb_vals) > 1) {
    warning("Multiple SB97Z weighted_avg_unit_cost values found; using the first.")
  }
}

rm (sb_vals)

cohort_hospital_data_user_cost <- cohort_hospital_data_user_cost %>%
  mutate(weighted_avg_unit_cost = case_when(
    SpellHRG == "SC97Z" ~ SB97Z_cost,
    TRUE ~ weighted_avg_unit_cost
  ))


# HRG WF02A - remove spell as it is not actually a hospitalisation (just one case anyway)
cohort_hospital_data_user_cost <- cohort_hospital_data_user_cost %>%
  filter(is.na(SpellHRG) | SpellHRG != "WF02A")

# create cost per patient — sum only the costed/groupable spells;
# track how many of each patient's spells couldn't be costed
cohort_inpatient_user_cost <- cohort_hospital_data_user_cost %>%
  summarise(
    total_in_cost     = sum(weighted_avg_unit_cost, na.rm = TRUE),
    n_uncosted_spells = sum(is.na(weighted_avg_unit_cost)),
    .by = patid
  )

cohort_hospital_all_costs <- cohort_hospital_all %>%
  left_join(cohort_inpatient_user_cost, by = "patid") %>%
  mutate(
    n_uncosted_spells = coalesce(n_uncosted_spells, 0L),
    total_in_cost = case_when(
      total_spells == 0                 ~ 0,
      n_uncosted_spells == total_spells ~ NA_real_,
      TRUE                               ~ total_in_cost
    )
  )

# check number of uncosted spells
cohort_hospital_all_costs %>%
  summarise(
    n_users_with_any_uncosted   = sum(n_uncosted_spells > 0),
    n_users_fully_uncosted      = sum(total_spells > 0 & n_uncosted_spells == total_spells),
    total_uncosted_spells       = sum(n_uncosted_spells),
    total_spells                = sum(total_spells),
    pct_spells_uncosted         = round(100 * total_uncosted_spells / total_spells, 2)
  )


# =====================================================
# A) Inpatient costs per patient
# =====================================================

# Helper summary function
summarise_costs <- function(df_costs, varname = "total_in_cost") {
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
      ci_lower = mean - qt(0.975, df = n_costed - 1) * se,
      ci_upper = mean + qt(0.975, df = n_costed - 1) * se,
      .groups  = "drop"
    ) %>%
    mutate(across(where(is.numeric) & !any_of("n_pts"), ~ round(.x, 2)))
}

## All patients (users + non-users, zeros included)
in_costs_all <- cohort_hospital_all_costs %>%
  summarise_costs(varname = "total_in_cost") %>%
  mutate(group = "All Patients") 

## Users only
in_costs_users <- cohort_hospital_all_costs %>%
  filter(user_status_inpatient == "User") %>%
  summarise_costs(varname = "total_in_cost") %>%
  mutate(group = "Users") 

# Combine summaries
combined_summary_inpatient_costs <- bind_rows(
  in_costs_all,
  in_costs_users
) 

# =====================================================
# B) Bootstrap summaries for inpatient costs
# =====================================================

bootstrap_summary_cost <- function(data, varname = "total_in_cost", R = 1000, seed = 626) {
  
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
boot_cost_all <- bootstrap_summary_cost(cohort_hospital_all_costs, varname = "total_in_cost") %>%
  mutate(group = "All Patients")


## Users only
boot_cost_users <- cohort_hospital_all_costs %>%
  filter(user_status_inpatient == "User") %>%
  bootstrap_summary_cost(varname = "total_in_cost") %>%
  mutate(group = "Users")


# Combine bootstrap results
combined_bt_summary_inpatient_costs <- bind_rows(
  boot_cost_all,
  boot_cost_users
) %>%
  dplyr::select(group, everything())

## Save outputs
write.csv(combined_summary_inpatient_costs, "CPRD Raw Analysis/Results/Final/Secondary care/APC/combined_summary_inpatient_costs.csv", row.names = FALSE)
write.csv(combined_bt_summary_inpatient_costs,"CPRD Raw Analysis/Results/Final/Secondary care/APC/combined_bt_summary_inpatient_costs.csv", row.names = FALSE)


## count of HRGs

# --- Count of categories ---
overall_hrg_count <- cohort_hospital_data_user_cost %>% 
  group_by(SpellHRG) %>% 
  summarise(
    count_hrg = n(),
    .groups = "drop"
  ) %>% 
  mutate(
    percent_hrg = round(count_hrg / sum(count_hrg) * 100, 2)
  )

write.csv(
  overall_hrg_count,
  "CPRD Raw Analysis/Results/Final/Secondary care/APC/overall_hrg_count.csv",
  row.names = FALSE
)
