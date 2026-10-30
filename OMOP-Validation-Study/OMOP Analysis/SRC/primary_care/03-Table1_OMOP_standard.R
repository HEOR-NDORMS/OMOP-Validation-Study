# ****************************************************************************** #
#	Project name: Validation Study			    	
# Script: Table 1: description of the patients
# ****************************************************************************** #

# Preparation for analyses ----

# 1. Define follow-up reasons
cohort_table <- cohort_table %>% 
  mutate(fracture_site = recode(fracture_site, NHNS = "NHNV")) %>% 
  mutate(follow_up_reason = case_when(
    follow_up_reason %in% c("observation_period_end_date, cancer_date, death_date",
                            "observation_period_end_date, death_date",
                            "observation_period_end_date, death_date, study_end",
                            "two_yrs_after_index, observation_period_end_date, death_date") ~ "Death",
    follow_up_reason == "cancer_date" ~ "Cancer",
    follow_up_reason == "cancer_date, fracture_after_index" ~ "Cancer, Fracture ensuing incident (index) fracture",
    follow_up_reason == "fracture_after_index" ~ "Fracture ensuing incident (index) fracture",
    follow_up_reason == "fracture_after_index, study_end" ~ "Fracture ensuing incident (index) fracture, End of Study",
    follow_up_reason == "mbd_date" ~ "Paget’s disease of the bone, or other metabolic bone diseases",
    follow_up_reason == "observation_period_end_date" ~ "End of data collection period",
    follow_up_reason == "observation_period_end_date, fracture_after_index" ~ "End of data collection period, Fracture ensuing incident (index) fracture",
    follow_up_reason == "study_end" ~ "End of Study",
    follow_up_reason == "two_yrs_after_index" ~ "Max of 730 days from index date",
    follow_up_reason == "two_yrs_after_index, fracture_after_index" ~ "Max of 730 days from index date, Fracture ensuing incident (index) fracture",
    follow_up_reason == "two_yrs_after_index, study_end" ~ "Max of 730 days from index date, End of Study"
  )) %>%
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  dplyr::mutate(age_fracture = lubridate::year(index_date) - year_of_birth) %>% 
  select(subject_id, age_fracture, fracture_site, index_date, follow_up_end, follow_up_reason, follow_up_time, death_date)


# 2. Import unit costs
provider_cost_inputs <- rio::import(
  file.path(
    "OMOP-Validation-Study", "OMOP Analysis", "SRC", "primary_care",
    "provider_specialty_cprd_omop_final_2026.xlsx"
  ),
  sheet = "omop_standard"
)

provider_cost_inputs <- provider_cost_inputs %>% select(specialty_concept_id, concept_name,unit_cost_encounter, category)

provider_cost_inputs <- provider_cost_inputs %>%
  mutate(
    specialty_concept_id = if_else(is.na(specialty_concept_id), -1L, as.integer(specialty_concept_id))
  ) # when specialty is missing I assign a -1 concept id just to allow the joining with the cost

# 3. Prepare visit table
cdm[["visit_data"]] <- cdm$visit_detail %>%
  dplyr::filter(
    visit_detail_concept_id == 581477,  # Primary care
    visit_detail_start_date >= study_start_date, # only visits in the study period
    visit_detail_start_date <= study_end_date
  ) %>%
  dplyr::select(
    visit_detail_id, person_id, provider_id, 
    visit_detail_start_date, visit_detail_concept_id
  ) %>%
  # Join visit detail and provider table to get specialty_concept_id
  dplyr::inner_join( # only visits with provider are retained
    cdm$provider %>% dplyr::select(provider_id, specialty_concept_id),
    by = "provider_id"
  ) %>%
  mutate(
    specialty_concept_id = if_else(is.na(specialty_concept_id), -1L, as.integer(specialty_concept_id))
  ) %>%
  # Join with provider_cost_input to get unit cost 
  dplyr::left_join(
    provider_cost_inputs, by = "specialty_concept_id", copy = TRUE
  ) %>% 
  dplyr::select(
    person_id, visit_detail_id, visit_detail_start_date, visit_detail_concept_id,
    provider_id, specialty_concept_id, concept_name,
    unit_cost_encounter, category
  ) %>%
  dplyr::rename(
    specialty = concept_name,
    specialty_reduced = category,
    unit_cost = unit_cost_encounter
  ) %>%
  computeQuery()


# 4. Merge cohort with visit data
cohort_visit_data_user <- cohort_table %>%
  dplyr::left_join(
    cdm[["visit_data"]] %>% collect(),
    by = c("subject_id" = "person_id")
  ) %>%
  dplyr::filter(# this will keep only patients with visits
      visit_detail_start_date >= index_date &
         visit_detail_start_date <= follow_up_end)%>% 
  select(subject_id, index_date, follow_up_end, follow_up_reason, follow_up_time, visit_detail_id, 
         visit_detail_start_date, specialty,
         specialty_reduced, unit_cost)

# 5. Deduplicate based on visit date and provider
cohort_visit_data_user <- cohort_visit_data_user %>% 
  #filter(!is.na(specialty)) %>% # keep only records with specialties (there were some with provider but not specialty) 
  # UPDATE: I keep them in now and cost specialty_concept_id = NA (i.e., not found in any ATHENA vocab, not just standard) same way as those =0 (i.e., no standard concept)
  distinct(subject_id, visit_detail_start_date, specialty , .keep_all = TRUE) 

# 6. Create a dataframe where all patients are included (also those without visits)
cohort_visit_data_all <- left_join(
  cohort_table, 
  cohort_visit_data_user %>%
    select(-c( index_date, follow_up_end, follow_up_reason, follow_up_time)),
  by = "subject_id"
) %>% 
  group_by(subject_id) %>%
  mutate(user_status = case_when( 
    # define user status (user= at least 1 non admin, non user= no clinical or no visits)
    any(specialty_reduced != "Admin", na.rm = TRUE) ~ "User",
    TRUE ~ "Non-User"
  )) %>%
  ungroup()

# sense check

# Summary - Table 1 -----

table_one_omop_visit_data <- cohort_visit_data_all %>% 
  distinct(subject_id, user_status, .keep_all = TRUE)

# Reorder factor by frequency
table_one_omop_visit_data <- table_one_omop_visit_data %>%
  mutate(follow_up_reason = forcats::fct_infreq(follow_up_reason)) %>% 
  mutate(follow_up_time = as.numeric(follow_up_time))

# Using gtsummary::tbl_summary, we can summarise the table as follows
(gt_summary_table <- table_one_omop_visit_data %>% 
    gtsummary::tbl_summary(
      # select the variables of interest
      include = c(age_fracture,fracture_site, follow_up_reason, 
                  follow_up_time, user_status),
      # rename the variable names
      label = list(
        age_fracture = "Age at Fracture Occurrence",
        fracture_site = "Fracture Site", 
        follow_up_reason = "Censoring Event",
        follow_up_time = "Follow up Time",
        user_status = "Heathcare Service User Status"
      ),
      # indicate whether we want the missing values to be seen in the summary table
      missing = "no"
    ) %>%
    gtsummary::modify_header(label = "**Variable**"))


# Check summary statistics

# Follow-up time
mean_follow_up_time <- mean(table_one_omop_visit_data$follow_up_time, na.rm = TRUE)
sd_follow_up_time <- sd(table_one_omop_visit_data$follow_up_time, na.rm = TRUE)
median_followup_time <- median(table_one_omop_visit_data$follow_up_time, na.rm = TRUE)
iqr_followup_time <- IQR(table_one_omop_visit_data$follow_up_time, na.rm = TRUE)

# Age
mean_age<-mean(table_one_omop_visit_data$age_fracture, na.rm = TRUE)
sd_age <- sd(table_one_omop_visit_data$age_fracture, na.rm = TRUE)

# Number of patients
number_of_patients <- n_distinct(table_one_omop_visit_data$subject_id)

# Site of index fracture summary
fracture_site_count <- table_one_omop_visit_data %>% 
    group_by(fracture_site) %>%
    summarise(count_of_pts = n_distinct(subject_id)) %>%
    mutate(percent = round(count_of_pts / sum(count_of_pts) * 100, 2),
           result = paste0(count_of_pts, "(", percent, "%", ")")) %>% 
    ungroup()

# Censoring events summary
censoring_events_count <- table_one_omop_visit_data %>% 
  group_by(follow_up_reason) %>%
  summarise(count_of_pts = n_distinct(subject_id), .groups = "drop") %>%
  mutate(
    percent = round(count_of_pts / sum(count_of_pts) * 100, 1),
    result = paste0(count_of_pts, " (", percent, "%)")
  )

# User vs Non-user count
u_n_summary_t1 <- table_one_omop_visit_data %>%
  group_by(user_status) %>%
  summarise(count_of_pts = n_distinct(subject_id), .groups = "drop")

# Clean objects
rm(mean_follow_up_time, sd_follow_up_time, median_followup_time, iqr_followup_time, 
   number_of_patients, median_age_pt, iqr_age_pt, fracture_site_count, censoring_events_count, u_n_summary_t1)

# ---- Export summary table ----

# As data frame for Excel
gt_summary_table_df <- as_tibble(gt_summary_table) %>% 
  rename("Variable" = "**Variable**",
         "N = 22,900" = "**N = 22,900**") %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

# Export to Excel
rio::export(gt_summary_table_df, here::here("OMOP-Validation-Study", "OMOP Analysis", "Results", "Final", "Cohort", "Table1_OMOP_standard.xlsx"))

# Export to HTML
gtsummary::as_gt(gt_summary_table) %>%
  gt::gtsave(here::here("OMOP-Validation-Study", "OMOP Analysis", "Results", "Final", "Cohort", "gt_summary_table_OMOP_standard.html"))
 
# # Clean environment
# rm(list = setdiff(ls(), c("cohort_table", "cdm", "all_patients", "AttritionReportCoh", 
#                           "AttritionReportFrac" ,"cdm", "db", "cohort_visit_data_all", 
#                           "provider_cost_inputs", "table_one_raw_visit_data", "study_start_date", "study_end_date", 
#                           "gt_summary_table", "gt_summary_table_df")))
