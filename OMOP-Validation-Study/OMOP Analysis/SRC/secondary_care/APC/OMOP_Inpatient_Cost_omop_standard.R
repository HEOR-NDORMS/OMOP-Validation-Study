# =====================================================
# Inpatient Episode Preparation Script for COSTS/Grouper
#  - diagnosis order: condition_status_concept_id (32902 = primary) first,
#    then record sequence (condition rows before observation rows)
#  - procedure order: record sequence (procedure rows before observation rows);
#    no standard-concept equivalent for procedure position exists
#  - ordering validated against OMOP-source positions: first diagnosis and
#    first procedure match in >99.9% of episodes
#  - admission/discharge: derived (not hardcoded) — confirmed empirically to
#    have no effect on HRG/cost for this cohort; kept for reasonableness,
#    to be reported as a no-impact sensitivity analysis
#  - classpat: LOS-based (already agreed/tested)
#  - observation self-overwrite bug: fixed (no compute()/name collision)
#  - ICD10 / OPCS4 codes routed by the ETL to measurement and drug_exposure
#    are retrieved and stacked with the observation rows (same rule)
#  - cost aggregation: na.rm sum; true non-users=0, fully-uncosted=NA
#    (excluded from cost estimation, retained in HCRU via total_spells)
#  - summarise_costs(): reports n_costed alongside n_pts, and SE/CI now use
#    n_costed (was n_pts) — see note at that function, this is a real fix
# =====================================================

library(dplyr)
library(boot)
library(purrr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)

# --- 1) Episodes in study period -----------------------------------------
cdm[["episodes"]] <- cdm$visit_detail %>%
  filter(
    visit_detail_concept_id == 9201,              # inpatient episodes
    visit_detail_start_date >= study_start_date,
    visit_detail_end_date <= study_end_date
  ) %>%
  compute(name = "episodes", overwrite = TRUE)

episodes <- cdm[["episodes"]] %>% collect()

# --- 2) Restrict to episodes belonging to included spells ----------------
valid_spell_ids <- cohort_hospital_data_user %>%
  filter(!is.na(visit_occurrence_id)) %>%
  pull(visit_occurrence_id) %>%
  unique()

cohort_hospital_episodes_data_user <- episodes %>%
  filter(visit_occurrence_id %in% valid_spell_ids)

# --- 3) Derive episode order within spell --------------------------------
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  group_by(visit_occurrence_id) %>%
  arrange(visit_detail_start_date, .by_group = TRUE) %>%
  mutate(epiorder = row_number()) %>%
  ungroup()

# --- 4) Admission / Discharge variables -----------------------------------

# Get emergency attendances by patient and date
# (fall back to visit_start_date only if visit_end_date is missing —
#  end_date is the conceptually correct field: it marks when the patient
#  left A&E, which is what should align with the hospital spell's start)
emergency_dates <- cohort_emergency_data_user %>%
  transmute(
    subject_id,
    emergency_end_date = as.Date(coalesce(visit_end_date, visit_start_date))
  ) %>%
  distinct()

death_lookup <- setNames(cohort_table$death_date, cohort_table$subject_id)

cohort_hospital_episodes_data_user_full <- cohort_hospital_episodes_data_user %>%
  
  arrange(
    person_id,
    visit_occurrence_id,
    visit_detail_start_date,
    visit_detail_end_date
  ) %>%
  
  group_by(person_id, visit_occurrence_id) %>%
  
  mutate(
    spell_start_date = as.Date(min(visit_detail_start_date, na.rm = TRUE))
  ) %>%
  
  ungroup() %>%
  
  left_join(
    emergency_dates %>%
      mutate(emergency_admission = TRUE),
    by = c(
      "person_id" = "subject_id",
      "spell_start_date" = "emergency_end_date"
    )
  ) %>%
  
  mutate(
    emergency_admission = coalesce(emergency_admission, FALSE),
    death_date = death_lookup[as.character(person_id)]
  ) %>%
  
  group_by(person_id, visit_occurrence_id) %>%
  
  mutate(
    died_in_spell = any(
      !is.na(death_date) &
        death_date >= visit_detail_start_date &
        death_date <= visit_detail_end_date
    ),
    last_episode = row_number() == n(),
    
    # Admission source: no reliable OMOP field for this (usual residence vs.
    # care home vs. inter-hospital transfer) — kept as a documented default,
    # matching the published methodology (Supplementary Material A, Table 5).
    admisorc = 19,
    
    # Admission method: derived from A&E linkage above, rather than a fixed
    # constant. Confirmed empirically (isolation tests) to have no effect on
    # HRG assignment/cost for this cohort — kept because it is the more
    # defensible derivation; report the no-impact finding as a sensitivity
    # analysis rather than reverting to the constant.
    admimeth = if_else(emergency_admission, 21, 11),
    
    # Discharge destination/method: derived from episode position and
    # in-spell mortality. Also confirmed to have no effect empirically.
    disdest = case_when(
      !last_episode                ~ 98,
      last_episode & died_in_spell ~ 79,
      TRUE                         ~ 19
    ),
    dismeth = case_when(
      !last_episode                ~ 9,
      last_episode & died_in_spell ~ 4,
      TRUE                         ~ 1
    )
  ) %>%
  
  ungroup()

# Diagnostic — run on the _full version, before columns get dropped
cohort_hospital_episodes_data_user_full %>%
  summarise(
    pct_emergency        = mean(emergency_admission, na.rm = TRUE),
    pct_non_last_episode = mean(!last_episode, na.rm = TRUE),
    pct_died_last        = mean(last_episode & died_in_spell, na.rm = TRUE)
  )

# Only now drop the columns, for the version that feeds the grouper
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user_full %>%
  dplyr::select(
    -died_in_spell,
    -last_episode,
    -emergency_admission,
    -spell_start_date,
    -death_date
  )


# --- 5) Append specialty (mainspef / tretspef) ----------------------------
provider <- cdm$provider %>% collect()
validated_map <- rio::import("OMOP-Validation-Study/OMOP Analysis/SRC/secondary_care/check_mainspef_tretspef_inpatient_outpatient_ValidationStudy_omop_standard_final.xlsx")

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  left_join(
    provider %>% dplyr::select(provider_id, specialty_concept_id),
    by = "provider_id"
  ) %>%
  left_join(
    validated_map %>%
      dplyr::select(specialty_concept_id, concept_name, mainspef, tretspef,
                    mainspef_code, tretspef_code) %>%
      distinct(),
    by = "specialty_concept_id"
  )

# how many with both mainspef and tretspef missing?
cohort_hospital_episodes_data_user %>%
  filter(is.na(mainspef) & is.na(tretspef)) %>%
  tally()

# drop them
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  filter(!(is.na(mainspef) & is.na(tretspef)))  # drop if both missing

# --- 6) Diagnoses ---------------------------------------------------------

# 6.1) Pull inpatient condition_occurrence rows
cdm[["diagnoses"]] <- cdm$condition_occurrence %>%
  filter(
    condition_type_concept_id == 32829,             # inpatient diagnoses
    condition_start_date >= study_start_date,
    condition_start_date <= study_end_date
  ) %>%
  compute(name = "diagnoses", overwrite = TRUE)

diagnoses <- cdm[["diagnoses"]] %>% collect()

diagnoses_clean <- diagnoses %>%
  select(visit_detail_id, condition_occurrence_id, condition_concept_id, condition_source_value,
         condition_start_date, condition_status_concept_id)

# 6.2) Pull inpatient observations
# (reads straight into R — no compute()/name collision with cdm$observation,
#  which previously caused repeated self-joins and vocabulary_id.x.x... bugs)
observation <- cdm$observation %>%
  select(-any_of("vocabulary_id")) %>% 
  filter(
    observation_type_concept_id == 32829,
    observation_date >= study_start_date,
    observation_date <= study_end_date
  ) %>%
  left_join(
    cdm$concept %>% select(concept_id, vocabulary_id),
    by = c("observation_source_concept_id" = "concept_id")
  ) %>%
  collect()

observation_diag <- observation %>%
  filter(vocabulary_id == "ICD10") %>%
  select(visit_detail_id, observation_id, observation_concept_id, observation_source_value, observation_date)

# 6.2b) Pull inpatient rows routed by the ETL to measurement (ICD10 and OPCS4)
# and drug_exposure (OPCS4, X-chapter codes). Same pattern as observation.
measurement <- cdm$measurement %>%
  select(-any_of("vocabulary_id")) %>%
  filter(
    measurement_type_concept_id == 32829,
    measurement_date >= study_start_date,
    measurement_date <= study_end_date
  ) %>%
  left_join(
    cdm$concept %>% select(concept_id, vocabulary_id),
    by = c("measurement_source_concept_id" = "concept_id")
  ) %>%
  collect()

measurement_diag <- measurement %>%
  filter(vocabulary_id == "ICD10") %>%
  select(visit_detail_id, measurement_id, measurement_concept_id, measurement_source_value, measurement_date)

drug <- cdm$drug_exposure %>%
  select(-any_of("vocabulary_id")) %>%
  filter(
    drug_type_concept_id == 32829,
    drug_exposure_start_date >= study_start_date,
    drug_exposure_start_date <= study_end_date
  ) %>%
  left_join(
    cdm$concept %>% select(concept_id, vocabulary_id),
    by = c("drug_source_concept_id" = "concept_id")
  ) %>%
  collect()

# 6.3) Stack condition + observation diagnoses into one long table
diagnoses_all <- bind_rows(
  diagnoses_clean %>%
    transmute(visit_detail_id, record_id = condition_occurrence_id,
              diagnosis_concept_id = condition_concept_id,
              icd_code = condition_source_value, diag_date = condition_start_date,
              is_primary = condition_status_concept_id == 32902,
              source = "condition"),
  observation_diag %>%
    transmute(visit_detail_id, record_id = observation_id,
              diagnosis_concept_id = observation_concept_id,
              icd_code = observation_source_value, diag_date = observation_date,
              is_primary = FALSE,   # no status field exists for observation-origin rows
              source = "observation"),
  measurement_diag %>%
    transmute(visit_detail_id, record_id = measurement_id,
              diagnosis_concept_id = measurement_concept_id,
              icd_code = measurement_source_value, diag_date = measurement_date,
              is_primary = FALSE,
              source = "measurement")
)

# 6.4) Order diagnoses within each episode: the HES primary diagnosis
# (condition_status_concept_id == 32902) is placed first; all other diagnoses
# follow in record sequence, with condition rows before observation rows.
# record_id (condition_occurrence_id / observation_id) is the record sequence.
diagnoses_all <- diagnoses_all %>%
  mutate(is_primary = coalesce(is_primary, FALSE),
         record_id  = as.numeric(record_id)) %>%
  arrange(visit_detail_id, desc(is_primary), source, record_id) %>%
  group_by(visit_detail_id) %>%
  mutate(d_order = row_number()) %>%
  ungroup()

# 6.5) Create lookup of candidate ICDs per diagnosis_concept_id
lookup_list <- diagnoses_all %>%
  distinct(diagnosis_concept_id, icd_code) %>%
  group_by(diagnosis_concept_id) %>%
  summarise(icd_candidates = list(na.omit(icd_code)), .groups = "drop")

# 6.6) Attach candidates and choose one ICD per row:
#    - zero candidates -> NA
#    - one candidate -> that value
#    - multiple -> sample one candidate randomly for each row (deliberate:
#      OMOP-standard should not rely on this row's own source value; the
#      arrange() in 6.4 makes this draw sequence stable/reproducible)
set.seed(123)
diagnoses_all <- diagnoses_all %>%
  left_join(lookup_list, by = "diagnosis_concept_id") %>%
  mutate(
    icd_code = map_chr(
      icd_candidates,
      ~ if (length(.x) == 0) {
        NA_character_
      } else if (length(.x) == 1) {
        as.character(.x[[1]])
      } else {
        as.character(sample(.x, 1))
      }
    )
  ) %>%
  select(-icd_candidates)

# 6.7) Quick QC
stopifnot(
  nrow(diagnoses_all) == nrow(diagnoses_clean) + nrow(observation_diag) + nrow(measurement_diag)
)

bad_orders <- diagnoses_all %>%
  group_by(visit_detail_id) %>%
  summarise(n_rows = n(), max_order = max(d_order), .groups = "drop") %>%
  filter(n_rows != max_order)

if (nrow(bad_orders) > 0) {
  warning("Some visit_detail_id have mismatched d_order (should not happen).")
}

diagnoses_wide <- diagnoses_all %>%
  mutate(diag_label = sprintf("DIAG_%02d", d_order)) %>%
  select(visit_detail_id, diag_label, icd_code) %>%
  pivot_wider(
    names_from  = diag_label,
    values_from = icd_code,
    names_sort  = TRUE,
    values_fill = NA
  )

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  left_join(diagnoses_wide, by = "visit_detail_id")

#  Clean up diag codes (remove dots)
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(
    across(matches("^DIAG_"),
           ~ str_remove_all(.x, "\\."),
           .names = "{.col}")
  )

diagnoses_3char <- cohort_hospital_episodes_data_user %>%
  dplyr::select(matches("^DIAG_")) %>%
  pivot_longer(everything(), names_to = "diag_col", values_to = "code") %>%
  filter(!is.na(code), code != "", str_length(code) == 3) %>%
  distinct(code)

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(across(matches("^DIAG_"),
                ~ ifelse(!is.na(.x) & str_length(.x) == 3, str_c(.x, "X"), .x)))

# remove/replace diagnoses not recognised by the 2024-25 grouper
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(str_detect(.x, "^I84"), NA, .x))) %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(str_detect(.x, "^I48"), "I489", .x))) %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(.x == "G903", "G909", .x))) %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(.x == "Z225", "Z229", .x))) %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(.x == "A09X", "A099", .x))) %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(.x == "D752", "D759", .x))) %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(.x == "H547", "H549", .x))) %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(.x == "K359", "K358", .x))) %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(.x == "K85X", "K859", .x))) %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(.x == "L89X", "L899", .x))) %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(.x == "R501", "R509", .x))) %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(.x == "K350", "K358", .x))) %>%
  mutate(across(matches("^DIAG_"), ~ ifelse(str_detect(.x, "^(U80|U81|U88|U89)"), NA, .x)))

noprimarydiag <- cohort_hospital_episodes_data_user %>%
  filter(is.na(DIAG_01)) %>% tally

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  group_by(visit_occurrence_id) %>%
  mutate(
    diag_mode = mode_value(DIAG_01),
    DIAG_01 = if_else(is.na(DIAG_01), diag_mode, DIAG_01)
  ) %>%
  ungroup() %>%
  filter(!is.na(DIAG_01)) %>%       # drop only those episodes still missing
  dplyr::select(-diag_mode)

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  filter(!(DIAG_01 %in% c("R69X", "T140"))) # drop episode

# --- 7) Procedures --------------------------------------------------------
cdm[["procedures"]] <- cdm$procedure_occurrence %>%
  filter(
    procedure_type_concept_id == 32829,             # inpatient procedures
    procedure_date >= study_start_date,
    procedure_date <= study_end_date
  ) %>%
  compute(name = "procedures", overwrite = TRUE)

procedures <- cdm[["procedures"]] %>% collect()

procedures_clean <- procedures %>%
  select(visit_detail_id, procedure_occurrence_id, procedure_concept_id, procedure_source_value, procedure_date)

observation_proc <- observation %>%
  filter(vocabulary_id == "OPCS4") %>%
  select(visit_detail_id, observation_id, observation_concept_id, observation_source_value, observation_date)

measurement_proc <- measurement %>%
  filter(vocabulary_id == "OPCS4") %>%
  select(visit_detail_id, measurement_id, measurement_concept_id, measurement_source_value, measurement_date)

drug_proc <- drug %>%
  filter(vocabulary_id == "OPCS4") %>%
  select(visit_detail_id, drug_exposure_id, drug_concept_id, drug_source_value, drug_exposure_start_date)

procedures_all <- bind_rows(
  procedures_clean %>%
    transmute(visit_detail_id, record_id = procedure_occurrence_id,
              procedure_concept_id, opcs_code = procedure_source_value,
              proc_date = procedure_date, source = "procedure"),
  observation_proc %>%
    transmute(visit_detail_id, record_id = observation_id,
              procedure_concept_id = observation_concept_id,
              opcs_code = observation_source_value, proc_date = observation_date,
              source = "observation"),
  measurement_proc %>%
    transmute(visit_detail_id, record_id = measurement_id,
              procedure_concept_id = measurement_concept_id,
              opcs_code = measurement_source_value, proc_date = measurement_date,
              source = "measurement"),
  drug_proc %>%
    transmute(visit_detail_id, record_id = drug_exposure_id,
              procedure_concept_id = drug_concept_id,
              opcs_code = drug_source_value, proc_date = drug_exposure_start_date,
              source = "drug")
)

# Order procedures within each episode by record sequence, procedure rows
# before observation / measurement / drug rows. No standard-concept equivalent exists for
# procedure position (unlike condition_status_concept_id for diagnoses).
procedures_all <- procedures_all %>%
  mutate(record_id = as.numeric(record_id)) %>%
  arrange(visit_detail_id, source != "procedure", record_id) %>%
  group_by(visit_detail_id) %>%
  mutate(p_order = row_number()) %>%
  ungroup()

lookup_list_proc <- procedures_all %>%
  distinct(procedure_concept_id, opcs_code) %>%
  group_by(procedure_concept_id) %>%
  summarise(opcs_candidates = list(na.omit(opcs_code)), .groups = "drop")

set.seed(123)
procedures_all <- procedures_all %>%
  left_join(lookup_list_proc, by = "procedure_concept_id") %>%
  mutate(
    opcs_code = map_chr(
      opcs_candidates,
      ~ if (length(.x) == 0) {
        NA_character_
      } else if (length(.x) == 1) {
        as.character(.x[[1]])
      } else {
        as.character(sample(.x, 1))
      }
    )
  ) %>%
  select(-opcs_candidates)

# OPCS-4 coding rule: Y (method/approach) and Z (site/laterality) codes are
# supplementary and cannot be the primary procedure. Place them after all
# other codes, then procedure-table rows before measurement/observation/drug rows.
procedures_all <- procedures_all %>%
  mutate(is_subsidiary = !is.na(opcs_code) & str_detect(opcs_code, "^[YZ]")) %>%
  arrange(visit_detail_id, is_subsidiary, source != "procedure", record_id) %>%
  group_by(visit_detail_id) %>%
  mutate(p_order = row_number()) %>%
  ungroup() %>%
  select(-is_subsidiary)

stopifnot(
  nrow(procedures_all) == nrow(procedures_clean) + nrow(observation_proc) + nrow(measurement_proc) + nrow(drug_proc)
)

bad_orders <- procedures_all %>%
  group_by(visit_detail_id) %>%
  summarise(n_rows = n(), max_order = max(p_order), .groups = "drop") %>%
  filter(n_rows != max_order)

if (nrow(bad_orders) > 0) {
  warning("Some visit_detail_id have mismatched p_order (should not happen).")
}

procedures_wide <- procedures_all %>%
  mutate(proc_label = sprintf("OPER_%02d", p_order)) %>%
  select(visit_detail_id, proc_label, opcs_code) %>%
  pivot_wider(
    names_from  = proc_label,
    values_from = opcs_code,
    names_sort  = TRUE,
    values_fill = NA
  )

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  left_join(procedures_wide, by = "visit_detail_id")

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(
    across(matches("^OPER_"),
           ~ str_remove_all(.x, "\\."),
           .names = "{.col}")
  )

oper_cols <- grep("^OPER_", names(cohort_hospital_episodes_data_user), value = TRUE)

only_ZY <- apply(
  cohort_hospital_episodes_data_user[oper_cols],
  1,
  function(x) {
    x <- na.omit(x)
    length(x) > 0 && all(grepl("^[ZY]", x))
  }
)

n_removed <- sum(only_ZY)

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user[!only_ZY, ]

invalid_oper_codes <- c(
  "G802", "Z466", "Z450", "Z514", "O831", "Z400", "Z480", "Z390", "Z516",
  "Z290", "Z513", "Z512", "Z467", "Z430", "Z006", "Z460", "Z465", "Z510",
  "Z005", "Z030", "Z026"
)

n_invalid <- sum(sapply(
  cohort_hospital_episodes_data_user[oper_cols],
  function(x) sum(toupper(trimws(x)) %in% invalid_oper_codes, na.rm = TRUE)
))
n_invalid

cohort_hospital_episodes_data_user[oper_cols] <- lapply(
  cohort_hospital_episodes_data_user[oper_cols],
  function(x) {
    x <- toupper(trimws(x))
    x[x %in% invalid_oper_codes] <- NA
    x
  }
)

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(across(matches("^OPER_"), ~ ifelse(str_detect(.x, "Z010"), "Z019", .x))) %>%
  mutate(across(matches("^OPER_"), ~ ifelse(str_detect(.x, "Z020"), "Z029", .x))) %>%
  mutate(across(matches("^OPER_"), ~ ifelse(str_detect(.x, "Z437"), "Z439", .x)))

# --- 8) Add age and sex ---------------------------------------------------
person_df <- cdm$person %>%
  dplyr::select(person_id, year_of_birth, gender_concept_id) %>%
  collect()

cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  left_join(person_df, by = "person_id") %>%
  mutate(
    startage = lubridate::year(visit_detail_start_date) - year_of_birth,
    sex = case_when(
      gender_concept_id == 8507 ~ 1,  # Male
      gender_concept_id == 8532 ~ 2,  # Female
      TRUE ~ NA_real_
    )
  )

# --- 9) Add epidur ---------------------------------------------------
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  mutate(epidur = as.numeric(visit_detail_end_date - visit_detail_start_date))

# --- 10) Add Classpat ------------------------------------------------
cohort_hospital_episodes_data_user <- cohort_hospital_episodes_data_user %>%
  left_join(
    cohort_hospital_data_user %>%
      select(visit_occurrence_id, los),
    by = "visit_occurrence_id"
  ) %>%
  mutate(
    classpat = if_else(los > 1, 1, 2) # Ordinary if LOS>1, Day case if LOS=1
  )

#---------------------------------------------
# lets clean and prepare for grouper
cohort_hospital_episodes_data_user_grouper <- cohort_hospital_episodes_data_user %>%
  dplyr::select(
    -c(
      visit_detail_id,
      visit_detail_concept_id,
      visit_detail_start_date,
      visit_detail_start_datetime,
      visit_detail_end_date,
      visit_detail_end_datetime,
      visit_detail_type_concept_id,
      provider_id,
      care_site_id,
      visit_detail_source_concept_id,
      admitting_source_value,
      admitting_source_concept_id,
      discharge_to_source_value,
      discharge_to_concept_id,
      mainspef,
      tretspef,
      preceding_visit_detail_id,
      person_id,
      year_of_birth,
      gender_concept_id,
      los
    )
  )

cohort_hospital_episodes_data_user_grouper <- cohort_hospital_episodes_data_user_grouper %>%
  rename(
    provspno = visit_occurrence_id,
    tretspef = tretspef_code,
    mainspef = mainspef_code
  ) %>%
  mutate(
    neocare = NA,
    criticalcaredays = NA,
    rehabilitationdays = NA,
    spcdays = NA,
    procodet = "ABC",
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

## Sex, admimeth, admisorc must be the same for all episodes within spells
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

cohort_hospital_episodes_data_user_grouper <- cohort_hospital_episodes_data_user_grouper %>%
  mutate(across(everything(), ~replace(., is.na(.), ""))) # replace NA with empty

#### Save outputs for grouper
write.csv(cohort_hospital_episodes_data_user_grouper,
          "OMOP-Validation-Study/OMOP Analysis/SRC/secondary_care/APC/cohort_hospital_episodes_data_user_grouper_standard.csv",
          row.names = FALSE)
#####

# --- 11) Prepare NHS cost collection ---------------------------------------------------
nhs_im_cost <- read_excel("OMOP-Validation-Study/OMOP Analysis/SRC/secondary_care/NHS_Collection_Costs_2023_2024.xlsx", sheet = "APC")

nhs_im_cost_weighted <- nhs_im_cost %>%
  mutate(
    n_fce = str_replace_all(`Number of finished consultant episodes`, "\\*", "3"),
    n_fce = as.numeric(str_remove_all(n_fce, "[,]")),
    hrg = `Currency Code`,
    cost = as.numeric(str_remove_all(Cost, "[£,]")),
  ) %>%
  group_by(hrg) %>%
  summarise(
    total_cost = sum(cost, na.rm = TRUE),
    total_fce = sum(n_fce, na.rm = TRUE),
    weighted_avg_unit_cost = total_cost / total_fce,
    .groups = "drop"
  ) %>%
  filter(!is.na(hrg))

# --- 12) Append HRG and Costs  ---------------------------------------------------
hrg_output_inpatient <- read.csv("OMOP-Validation-Study/OMOP Analysis/SRC/secondary_care/APC/output_inpatient_grouper_validation_standard_final_omop_standard_spell_rel.csv")

hrg_output_inpatient <- hrg_output_inpatient %>%
  filter(SpellGroupingMethodFlag != "U") # U means error in spell grouping method

cohort_hospital_data_user_cost <- cohort_hospital_data_user %>%
  left_join(
    hrg_output_inpatient %>%
      dplyr::select(PROCODET, PROVSPNO, SpellHRG, SpellDominantProcedure, SpellPDiag, SpellGroupingMethodFlag),
    by = c("visit_occurrence_id" = "PROVSPNO")
  )

cohort_hospital_data_user_cost <- cohort_hospital_data_user_cost %>%
  left_join(
    nhs_im_cost_weighted %>% dplyr::select(hrg, weighted_avg_unit_cost),
    by = c("SpellHRG" = "hrg")
  )

# check HRGs without cost
test <- cohort_hospital_data_user_cost %>%
  filter(is.na(weighted_avg_unit_cost) | weighted_avg_unit_cost == 0) %>%
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
  SB97Z_cost <- sb_vals[1]  # use the unique value; if multiple, take first
  if (length(sb_vals) > 1) {
    warning("Multiple SB97Z weighted_avg_unit_cost values found; using the first.")
  }
}

rm(sb_vals)

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
    .by = subject_id
  )

cohort_hospital_all_costs <- cohort_hospital_all %>%
  left_join(cohort_inpatient_user_cost, by = "subject_id") %>%
  mutate(
    n_uncosted_spells = coalesce(n_uncosted_spells, 0L),
    total_in_cost = case_when(
      total_spells == 0                 ~ 0,          # true non-user: zero cost
      n_uncosted_spells == total_spells ~ NA_real_,    # every spell ungroupable: excluded from cost estimation
      TRUE                               ~ total_in_cost  # ≥1 groupable spell: real, na.rm-summed partial cost
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

# Helper summary function.
# n_costed = how many patients actually have a non-NA total_in_cost and so
# actually contributed to `mean`/`sd` below (na.rm drops the fully-uncosted
# patients from both). n_pts is the full group size (fully-uncosted patients
# included) and will be larger than n_costed whenever any exist.
# NOTE: se/ci_lower/ci_upper now use n_costed, not n_pts — sd (na.rm=TRUE) is
# already computed only over the n_costed non-NA values, so dividing by the
# larger n_pts would understate the standard error and make the CI too
# narrow whenever fully-uncosted patients are present. This wasn't an issue
# before this session's fix, since total_in_cost previously never contained
# NA (it was always a real number or 0) — n_pts and the mean's true
# denominator were always the same population. That's no longer true.
summarise_costs <- function(df_costs, varname = "total_in_cost") {
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

# Save outputs
write.csv(combined_summary_inpatient_costs,
          "OMOP-Validation-Study/OMOP Analysis/Results/Final/Secondary care/APC/Costs - omop standard/combined_summary_inpatient_costs_standard.csv",
          row.names = FALSE)

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

# Save bootstrap outputs
write.csv(combined_bt_summary_inpatient_costs,
          "OMOP-Validation-Study/OMOP Analysis/Results/Final/Secondary care/APC/Costs - omop standard/combined_bt_summary_inpatient_costs_standard.csv",
          row.names = FALSE)

# =====================================================
# C) Plot: inpatient costs per patient
# =====================================================

plot_in_costs <- ggplot(
  combined_bt_summary_inpatient_costs,
  aes(x = group, y = mean, fill = group)
) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_perc_lower, ymax = ci_perc_upper),
                width = 0.15, color = "black", linewidth = 0.7) +
  labs(
    x = "",
    y = "Hospitalisations costs per patient (mean)",
    title = "Bootstrap mean hospitalisation costs with 95% percentile CI"
  ) +
  scale_x_discrete(labels = c("All Patients", "Users")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("OMOP-Validation-Study/OMOP Analysis/Results/Final/Secondary care/APC/Costs - omop standard/plot_in_costs_standard.png",
       plot = plot_in_costs,
       width = 10, height = 6, dpi = 300)


# =====================================================
# D) HRG analyses: counts, counts by strata, costs, costs by strata
# =====================================================

# import ICD10 names with full labels preserved
library(rio)
hrg_names <- import("OMOP-Validation-Study/OMOP Analysis/SRC/secondary_care/APC/HRG_ICD10_OPCS.xlsx", sheet = "HRG") |>
  rename(
    hrg_name = "Description") %>% distinct()

cohort_hospital_data_user_cost <- cohort_hospital_data_user_cost |>
  left_join(
    hrg_names |> dplyr::select(hrg_name, HRG),
    by = c("SpellHRG" = "HRG")
  )

# Parameters
LABEL_MAX <- 25  # characters for shortened HRG labels

# helper palette
palette_for_n <- function(n) {
  if (n <= 8) MetBrewer::met.brewer("Signac", n = n) else colorRampPalette(MetBrewer::met.brewer("Signac", n = 8))(n)
}

# --- Ensure cohort has an HRG text field (fallback to code if missing) ---
cohort_hospital_data_user_cost <- cohort_hospital_data_user_cost %>%
  mutate(hrg_name = if_else(is.na(hrg_name) | hrg_name == "", SpellHRG, hrg_name))

# ========================
# 1) COUNT-BASED: Bar plot by HRG (exclude <1%)
# ========================

hrg_count <- cohort_hospital_data_user_cost %>%
  filter(!is.na(SpellHRG) & SpellHRG != "") %>%
  # we use hrg_name text (already attached); group by HRG name
  group_by(SpellHRG, hrg_name) %>%
  summarise(n_hrg = n(), .groups = "drop") %>%
  mutate(prop = n_hrg / sum(n_hrg)) %>%
  ungroup()

# produce trimmed short labels separately (keep full name if you want)
hrg_trim <- hrg_count %>%
  distinct(SpellHRG, hrg_name) %>%
  mutate(
    hrg_name_trim = str_trim(str_remove(hrg_name, "\\s*\\([^\\)]+\\)")),
    hrg_name_short = if_else(str_length(hrg_name_trim) > LABEL_MAX,
                             paste0(str_sub(hrg_name_trim, 1, LABEL_MAX), "..."),
                             hrg_name_trim)
  )

# join trimmed names to the hrg_count
hrg_count <- hrg_count %>%
  left_join(hrg_trim %>% dplyr::select(SpellHRG, hrg_name_short), by = "SpellHRG")

# group <1% as Other (we'll use short labels for plotting)
hrg_grouped <- hrg_count %>%
  mutate(group = if_else(prop >= 0.01, hrg_name_short, "Other")) %>%
  group_by(group) %>%
  summarise(tot_n = sum(n_hrg), prop = sum(prop), .groups = "drop")

# # ordering within facets using numeric prop

# palette (exclude Other)
non_other <- sort(hrg_grouped %>% filter(group != "Other") %>% pull(group) %>% unique())
palette <- MetBrewer::met.brewer("Signac", n = length(non_other))
fill_cols <- c(setNames(palette, non_other), "Other" = "#B0B0B0")

plot_df <- hrg_grouped %>% filter(group != "Other")

library(scales)
plot_hrg_counts <- ggplot(
  plot_df,
  aes(x = reorder(group, prop), y = prop, fill = group)
) +
  geom_col(width = 0.7, colour = "white") +
  geom_text(
    aes(label = paste0(round(prop * 100, 1), "%")),
    vjust = -0.35,
    size = 2.6
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    values = fill_cols[names(fill_cols) %in% plot_df$group]
  ) +
  labs(
    title = "Hospitalisations by Main HRG (counts)",
    subtitle = "Percent of total hospitalisations. HRGs under 1% excluded.",
    x = "Primary HRG",
    y = "Percentage of hospitalisations"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "none",
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5)
  )

ggsave("OMOP-Validation-Study/OMOP Analysis/Results/Final/Secondary care/APC/Costs - omop standard/plot_inpatient_hrgs_counts_bars_standard.png",
       plot = plot_hrg_counts, width = 12, height = 10, dpi = 300)

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
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Secondary care/APC/Costs - omop standard/overall_hrg_count_omop_standard.csv",
  row.names = FALSE
)

