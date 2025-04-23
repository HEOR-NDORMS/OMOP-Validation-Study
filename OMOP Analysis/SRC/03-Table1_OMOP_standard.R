# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Table 1: description of the patients
# Date: 05.05.2025
# Last date edited: 05.02.2025
# ****************************************************************************** #

# Number of patients
# Age (median, IQR)
# Follow-up time in years, median (IQR)
# Site of index fracture
# hip (n%)
# vertebra (n%)
# NHNS (n%)
# Censoring events:
# fracture ensuing incident (index) fracture (n%)
# cancer (except non-melanoma skin cancer) (n%)
# Paget’s disease of the bone, or other metabolic bone diseases (n%)
# death (n%)
# end of data collection period (n%)
# maximum of 730 days from index date (n%)

# For the first 3 items, it’s def patients. Then the index 
# I think that we should also have one per patient but not sure 
# if by the end of the cleaning we still have cases of more than one site (hip,vertebra or nhns) possibly associated with the same index fx.
# Let’s check. 
# If yes, then let’s count the number of fractures instead. 
# Same for censoring event, check if there is anyone with more than one censoring reason and if so report the n of events.
# I think in most cases there should be just one event per person as it’s unlikely that they have for example cancer and fracture on the same day.
# Definitely impossible that they die and have fracture on the same day. Or at least if we have these cases then it’s not good.

# Table 1 ----

# Starting with cohort_table from CleaningCohort_OMOP.R
# let's clean some of the variables
table_one_omop <- cohort_table %>% 
  mutate(fracture_site = recode(fracture_site, NHNS = "NHNV")) %>% 
  mutate(follow_up_reason = case_when(
    follow_up_reason %in% c("after_index, observation_period_end_date, death_date",
                            "death_date",
                            "observation_period_end_date, cancer_date, death_date",
                            "observation_period_end_date, death_date") ~ "Death",
    follow_up_reason == "after_index" ~ "Max of 730 days from index date",
    follow_up_reason == "after_index, cancer_date" ~ "Max of 730 days from index date, Cancer",
    follow_up_reason == "after_index, fracture_after_index" ~ "Max of 730 days from index date, Fracture ensuing incident (index) fracture",
    follow_up_reason == "after_index, mbd_date" ~ "Max of 730 days from index date, Paget’s disease of the bone, or other metabolic bone diseases",
    follow_up_reason == "after_index, observation_period_end_date" ~ "Max of 730 days from index date, End of data collection period",
    follow_up_reason == "cancer_date" ~ "Cancer",
    follow_up_reason == "cancer_date, fracture_after_index" ~ "Cancer, Fracture ensuing incident (index) fracture",
    follow_up_reason == "cancer_date, mbd_date" ~ "Cancer, Paget’s disease of the bone, or other metabolic bone diseases",
    follow_up_reason == "fracture_after_index" ~ "Fracture ensuing incident (index) fracture",
    follow_up_reason == "mbd_date" ~ "Paget’s disease of the bone, or other metabolic bone diseases",
    follow_up_reason == "mbd_date, fracture_after_index" ~ "Paget’s disease of the bone, or other metabolic bone diseases, Fracture ensuing incident (index) fracture",
    follow_up_reason == "observation_period_end_date" ~ "End of data collection period",
    follow_up_reason == "observation_period_end_date, cancer_date" ~ "End of data collection period, Cancer",
    follow_up_reason == "observation_period_end_date, fracture_after_index" ~ "End of data collection period, Fracture ensuing incident (index) fracture")) %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  dplyr::mutate(age_fracture = lubridate::year(condition_start_date) - year_of_birth)


# Remove the visits that are not clinically relevance
# the costs are to be updated
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.4.xlsx") 

# Join visit_detail and provider tables
joined_visit_provider_tables <- cdm$visit_detail %>% 
  dplyr::inner_join(cdm$provider) 

## Sub-setting the visit data
# cdm[["visit_data"]] <- NULL
cdm[["visit_data"]] <- joined_visit_provider_tables %>%
  dplyr::left_join(provider_cost_inputs %>% dplyr::filter(Include_standard == "1"),
                   by = "specialty_concept_id", copy=TRUE) %>% # Filter for only meaningful specialties
  dplyr::select(person_id, provider_id, specialty_concept_id, concept_name, specialty_source_value_summarised, 
                visit_detail_start_date, visit_detail_id, unit_cost, visit_detail_concept_id) %>%
  dplyr::filter(visit_detail_start_date >= study_start_date & # include only visits in the study period
                  visit_detail_start_date <= study_end_date) %>%
  # Filter for only primary care visits
  dplyr::filter(visit_detail_concept_id == 581477) %>% 
  dplyr::rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  dplyr::distinct(visit_detail_start_date, subject_id, specialty_concept_id, .keep_all=TRUE) %>% # Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
  dplyr::rename(specialty = concept_name,
                specialty_reduced =  specialty_source_value_summarised) %>%  # renaming for consistency with other countries as this variable is used for the HCRU and costs too
  computeQuery()

visit_data_df <- cdm[["visit_data"]] %>% dplyr::collect()

# merge the cohort data to this visit_data_df
table_one_omop_visit_data <- left_join(table_one_omop, visit_data_df, 
                        by = "subject_id") %>% 
  # in order to count all the visits from index date until follow-up end date
  filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end)

## Clinically Relevant Consultations
specialty_source_value_ids <- unique(provider_cost_inputs$concept_name)

### Filter the data with these clinically relevant
# This would make it easier to identify users/non-users as well. Bc technically, if a patient had only visits before index but not during the exposure time, that is a non-user. 
# So it is simpler if you prepare the visit data first, with visits in the exposure time and then join back to the list of patients to identify users as those who have at last one visit during that time (and with a relevant staff) and non-users are all the others (i.e., zero visits or only non-relevant visits).
table_one_omop_visit_data <- table_one_omop %>% 
  left_join(table_one_omop_visit_data, by = c("subject_id", "cohort_start_date", 
                               "cohort_end_date", "condition_concept_id",
                               "condition_start_date", "fracture_site", 
                               "class", "index_date", "after_index", 
                               "observation_period_end_date", "cancer_date", 
                               "mbd_date", "fracture_after_index", "death_date",
                               "follow_up_end", "follow_up_reason", 
                               "follow_up_time", "gender_concept_id", 
                               "year_of_birth", "month_of_birth", "day_of_birth",
                               "birth_datetime", "race_concept_id",
                               "ethnicity_concept_id", "location_id", 
                               "care_site_id", "person_source_value", 
                               "gender_source_value", "gender_source_concept_id",
                               "race_source_value", "race_source_concept_id", 
                               "ethnicity_source_value", "ethnicity_source_concept_id", 
                               "age_fracture", "exposed_yrs", "exposed_months", "excluded_flag"
  ), copy = T) %>% 
  mutate(
    specialty = case_when(
      specialty %in% specialty_source_value_ids ~ specialty,
      TRUE ~ "Not Clinically Relevant"
    ),
    is_clinically_relevant = if_else(specialty == "Not Clinically Relevant", 0, 1),
    in_exposure_time = if_else(
      !is.na(visit_detail_start_date), 1, 0
    )
  ) %>% 
  mutate(specialty_reduced = ifelse(
    is.na(specialty_reduced), 
    "Not Clinically Relevant",
    specialty_reduced)) %>%
  group_by(subject_id) %>%
  # User is defined as patients who had >=1 visit(s) to a clinically relevant specialty
  # Non-User is defined as (1) Not Clinically relevant visit 
  # (2) No visits(i.e., missing visit date) (3) unknown visit (missing specialty)
  mutate(user_status = if_else(
    any(is_clinically_relevant == 1 & in_exposure_time == 1, na.rm = TRUE),
    "User", "Non-User"
  )) %>%
  mutate(not_users_status = case_when(
    user_status == "Non-User" & in_exposure_time == 0 ~ "Visits with Non-clinically Relevant Staff",
    user_status == "Non-User" ~ "Visits with Non-clinically Relevant Staff"
  )) %>% 
  ungroup() %>% 
  distinct(subject_id, user_status, .keep_all = TRUE)


# Summary table ----

# Reorder factor by frequency
table_one_omop_visit_data <- table_one_omop_visit_data %>%
  mutate(follow_up_reason = forcats::fct_infreq(follow_up_reason)) %>% 
  mutate(follow_up_time = as.numeric(follow_up_time))

# Using gtsummary::tbl_summary, we can summarise the table as follows
(gt_summary_table <- table_one_omop_visit_data %>% 
   gtsummary::tbl_summary(
     # select the variables of interest
     include = c(age_fracture, fracture_site, follow_up_reason, follow_up_time,
                 user_status, not_users_status),
     # rename the variable names
     label = list(
       age_fracture = "Age at Fracture Occurrence",
       fracture_site = "Fracture Site", 
       follow_up_reason = "Censoring Event",
       follow_up_time = "Follow up time",
       user_status = "Heathcare Service User Status",
       not_users_status = "Heathcare Service Non-user Visit Status"
     ),
     # indicate whether we want the missing values to be seen in the summary table
     missing = "no"
   ) %>%
   gtsummary::modify_header(label = "**Variable**"))

# Sense check ----
## Number of patients ----
(number_of_patients <- n_distinct(table_one_omop_visit_data$subject_id))

## Age (median, IQR) ----
(median_age <- median(table_one_omop_visit_data$age_fracture))
(iqr_age <- IQR(table_one_omop_visit_data$age_fracture))
(results_age <- paste0(median_age, "(", iqr_age, ")"))

## Site of index fracture ----
# hip (n%)
# vertebra (n%)
# NHNS (n%)

(site_count <- table_one_omop_visit_data %>% 
   group_by(fracture_site) %>%
   summarise(count_of_pts = n_distinct(subject_id)) %>%
   mutate(percent = round(count_of_pts / sum(count_of_pts) * 100, 2),
          result = paste0(count_of_pts, "(", percent, "%", ")")) %>% 
   ungroup())

## Censoring events ----
# fracture ensuing incident (index) fracture (n%)
# cancer (except non-melanoma skin cancer) (n%)
# Paget’s disease of the bone, or other metabolic bone diseases (n%)
# death (n%)
# end of data collection period (n%)
# maximum of 730 days from index date (n%)

(censoring_events_count <- table_one_omop_visit_data %>% 
   group_by(follow_up_reason) %>%
   summarise(count_of_pts = n_distinct(subject_id), .groups = "drop") %>% 
   mutate(percent = round(count_of_pts / sum(count_of_pts) * 100, 2),
          result = paste0(count_of_pts, "(", percent, "%", ")")))

## Follow-up time in days ----
(median_followup_time <- median(table_one_omop_visit_data$follow_up_time))
(iqr_followup_time <- IQR(table_one_omop_visit_data$follow_up_time))

# Export ----
gt_summary_table_df <- as_tibble(gt_summary_table) %>% 
  rename("Variable" = "**Variable**",
         "N = 23,957" = "**N = 23,957**") %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

rio::export(gt_summary_table_df, here::here("OMOP Analysis", "Results", "Table1_OMOP.xlsx"))

# or HTML
# Saving the summary table as an HTML file
# gtsummary::as_gt(gt_summary_table) %>%
#   gt::gtsave(here::here("OMOP Analysis", "Results", "gt_summary_table_OMOP.html"))
