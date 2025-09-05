# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki, Gianluca Fabiano
# Do file name: Table 1: description of the patients
# Date: 05.05.2025
# Last date edited: 10.07.2025
# ****************************************************************************** #
# Libraries
library(dplyr)
library(gtsummary)
library(gt)
library(forcats)
library(rio)
library(here)

# Preparation for analyses ----

# 1. Define follow-up reasons
cohort_table <- cohort_table %>% 
  mutate(site = recode(site, hip = "Hip", vert = "Vertebra", nhnv = "NHNV")) %>% 
  mutate(follow_up_reason = case_when(
    follow_up_reason %in% c(
      "death_date", "observation_period_end_date, cancer_date, death_date", 
      "observation_period_end_date, death_date", "two_yrs_after_index, observation_period_end_date, death_date",
      "observation_period_end_date, death_date, study_end") ~ "Death",
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
  )) %>% select(patid, index_date, site, follow_up_end, follow_up_reason, follow_up_time)

# 2. Import the old vaidation table, to bring in the age variable
all_patients_old <- my_dm$`_validation_old` %>%  collect() %>%  
  select(patid, yob) 
cohort_table <- left_join(cohort_table, all_patients_old, by = "patid") %>% 
  mutate(age_fracture = lubridate::year(index_date) - yob) %>% 
  select(!yob)

# 2. Import unit costs
provider_cost_inputs <- rio::import("provider_specialty_cprd_omop_final_after_lindsey.xlsx", sheet = "source_cprd_aurum")
provider_cost_inputs <- provider_cost_inputs %>% select(description, unit_cost_encounter, category)

# 3. Prepare visit table
visit_data <- my_dm$`validation_consultation` %>%  
  dplyr::filter(
    consdate >= study_start_date,
    consdate <= study_end_date) %>% 
  dplyr::inner_join(
    my_dm$`validation_staff`, by = "staffid"
  ) %>% 
  # Join with provider_cost_input to get unit cost 
  dplyr::left_join(
    provider_cost_inputs, by = "description", copy = TRUE
  )%>%
  dplyr::rename(
    specialty = description,
    specialty_reduced = category,
    unit_cost = unit_cost_encounter
  ) %>%
  computeQuery()
  
# 4. Merge cohort with visit data 
cohort_visit_data_user <- cohort_table %>%
  dplyr::left_join(
    visit_data %>% collect(),
    by = "patid"
  ) %>%
  dplyr::filter( # this will keep only patients with visits
    consdate >= index_date &
      consdate <= follow_up_end) %>% 
  select(patid, index_date, site, follow_up_end, follow_up_reason, follow_up_time, consid, consdate, specialty,
         specialty_reduced, unit_cost)

# 5. Deduplicate based on visit date and provider
cohort_visit_data_user <- cohort_visit_data_user %>% 
  # keep only records with specialties (there were some with provider but not specialty)
  filter(!is.na(specialty)) %>% 
  distinct(patid, consdate, specialty , .keep_all = TRUE) 

# 6. Create a dataframe where all patients are included (also those without visits)
visit_data_all <- left_join(
  cohort_table,
  cohort_visit_data_user %>%
    select(-c(index_date, site, follow_up_end, follow_up_reason, follow_up_time)),
  by = "patid"
) %>%
  group_by(patid) %>%
  mutate(user_status = case_when( 
    # define user status (user= at least 1 non admin, non user= no clinical or no visits)
    any(specialty_reduced != "Admin", na.rm = TRUE) ~ "User",
    TRUE ~ "Non-User"
  )) %>%
  ungroup()


# sense check
#visit_data_all %>% distinct(patid) %>% tally()
#visit_data_user %>% distinct(patid) %>% tally()


# Summary - Table 1 -----

# Prepare data: keep one row per patient
table_one_raw_visit_data <- visit_data_all %>% 
  distinct(patid, user_status, .keep_all = TRUE)

# Reorder factor levels and ensure numeric follow-up time
table_one_raw_visit_data <- table_one_raw_visit_data %>%
  mutate(
    follow_up_reason = forcats::fct_infreq(follow_up_reason),
    follow_up_time = as.numeric(follow_up_time)
  )

# ---- Create Table 1 ----
(gt_summary_table <- table_one_raw_visit_data %>% 
   gtsummary::tbl_summary(
     include = c(
       age_fracture, site, follow_up_reason,
       follow_up_time, user_status),
     label = list(
       age_fracture = "Age at Fracture Occurrence",
       site = "Fracture Site", 
       follow_up_reason = "Censoring Event",
       follow_up_time = "Follow up Time",
       user_status = "Heathcare Service User Status"
     ),
     missing = "no"
   ) %>%
   gtsummary::modify_header(label = "**Variable**"))

# Check summary statistics

# Follow-up time
mean_follow_up_time <- mean(table_one_raw_visit_data$follow_up_time, na.rm = TRUE)
sd_follow_up_time <- sd(table_one_raw_visit_data$follow_up_time, na.rm = TRUE)
median_followup_time <- median(table_one_raw_visit_data$follow_up_time, na.rm = TRUE)
iqr_followup_time <- IQR(table_one_raw_visit_data$follow_up_time, na.rm = TRUE)

# Number of patients
number_of_patients <- n_distinct(table_one_raw_visit_data$patid)

# Age (median, IQR)
median_age_pt <- median(table_one_raw_visit_data$age_fracture, na.rm = TRUE)
iqr_age_pt <- IQR(table_one_raw_visit_data$age_fracture, na.rm = TRUE)

# Site of index fracture summary
site_count <- table_one_raw_visit_data %>% 
  group_by(site) %>%
  summarise(count_of_pts = n_distinct(patid), .groups = "drop") %>%
  mutate(
    percent = round(count_of_pts / sum(count_of_pts) * 100, 1),
    result = paste0(count_of_pts, " (", percent, "%)")
  )

# Censoring events summary
censoring_events_count <- table_one_raw_visit_data %>% 
  group_by(follow_up_reason) %>%
  summarise(count_of_pts = n_distinct(patid), .groups = "drop") %>%
  mutate(
    percent = round(count_of_pts / sum(count_of_pts) * 100, 1),
    result = paste0(count_of_pts, " (", percent, "%)")
  )

# User vs Non-user count
u_n_summary_t1 <- table_one_raw_visit_data %>%
  group_by(user_status) %>%
  summarise(count_of_pts = n_distinct(patid), .groups = "drop")

# Clean objects
rm(mean_follow_up_time, sd_follow_up_time, median_followup_time, iqr_followup_time, 
   number_of_patients, median_age_pt, iqr_age_pt, site_count, censoring_events_count, u_n_summary_t1)

# ---- Export summary table ----

# As data frame for Excel
gt_summary_table_df <- as_tibble(gt_summary_table) %>% 
  rename(
    "Variable" = "**Variable**",
    "N = 23,106" = "**N = 23,106**"
  ) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

# Export to Excel
rio::export(gt_summary_table_df, here::here("CPRD Raw Analysis", "Results", "Cohort", "Table1_Raw.xlsx"))

# Export to HTML
gtsummary::as_gt(gt_summary_table) %>%
  gt::gtsave(here::here("CPRD Raw Analysis", "Results", "Cohort", "gt_summary_table_Raw.html"))

# Clean environment
rm(list = setdiff(ls(), c("cohort_table", "all_patients", "AttritionReportCoh", 
                          "AttritionReportFrac_Raw" ,"db", "my_dm", "visit_data_all", 
                          "provider_cost_inputs", "study_start_date", "study_end_date", 
                          "gt_summary_table", "gt_summary_table_df")))

