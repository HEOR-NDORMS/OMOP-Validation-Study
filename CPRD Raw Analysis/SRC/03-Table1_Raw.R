# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Table 1: description of the patients
# Date: 31.01.2025
# Last date edited: 04.02.2025
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

# Starting with cohort_table from CleaningCohort_Raw.R
# let's clean some of the variables
table_one_raw <- cohort_table %>% 
  mutate(site = recode(site, hip = "Hip", vert = "Vertebra", nhnv = "NHNV")) %>% 
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
    follow_up_reason == "fracture_after_index" ~ "Fracture ensuing incident (index) fracture",
    follow_up_reason == "mbd_date" ~ "Paget’s disease of the bone, or other metabolic bone diseases",
    follow_up_reason == "observation_period_end_date" ~ "End of data collection period",
    follow_up_reason == "observation_period_end_date, cancer_date" ~ "End of data collection period, Cancer",
    follow_up_reason == "observation_period_end_date, fracture_after_index" ~ "End of data collection period, Fracture ensuing incident (index) fracture"))


## Import the staff and consultation tables
# this is to be used for the specialty
validation_staff <- my_dm$`validation_staff` %>% collect()
validation_consultation <- my_dm$`validation_consultation` %>% collect()

## Merge with validation_consultation ----
# Filter from index date to follow up end date
table_one_raw_visit_data <- inner_join(table_one_raw, validation_consultation,
                         by = "patid"
) %>%
  # in order to count all the visits from index date until follow-up end date.
  filter(consdate >= index_date & consdate <= follow_up_end)
# left_join it back to the cohort data - so this 13 data will be non-users
# NOTE: 13 patients are being dropped when we filter to have consultation date be
# after or on index date and before or on follow-up end date.

# Merge cohort data
# merge back to cohort data
table_one_raw_visit_data <- left_join(
  cohort_table_raw, table_one_raw_visit_data,
  by = c(
    "patid", "gender", "yob", "death", "death_date",
    "aurum_min_accept_date", "aurum_max_accept_date", "site",
    "fx_date", "days_prior_obs", "days_after_obs", "age_fracture",
    "type", "cancer_fa_index_dt", "paget_fa_index_dt", "index_date",
    "after_index", "fracture_after_index", "follow_up_end",
    "follow_up_reason", "follow_up_time"
  )
)

## Merge with validation_staff
table_one_raw_visit_data <- left_join(table_one_raw_visit_data, validation_staff, by = "staffid") %>%
  ## Keep only distinct rows
  distinct(consdate, patid, description, .keep_all = TRUE)

# Provider unit costs
# the costs are to be updated
# file changed to include the specialty source values that have been grouped 
# into 3: GP, Nurse, Nurse
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.3_summarised.xlsx") %>%
  janitor::clean_names() %>%
  # include those with 1 which are clinically relevant
  filter(include == 1) %>%
  select(!notes)

# Remove the visits that are not clinically relevant
specialty_source_value_ids <- unique(provider_cost_inputs$specialty_source_value)

# Create the user_status variable
table_one_raw_visit_data <- table_one_raw_visit_data %>%
  rename("specialty_source_value" = "description") %>%
  mutate(specialty_source_value = case_when(
    specialty_source_value %in% specialty_source_value_ids ~ specialty_source_value,
    TRUE ~ "Not Clinically Relevant"
  )) %>%
  left_join(provider_cost_inputs, by = "specialty_source_value") %>%
  mutate(is_clinically_relevant = if_else(
    specialty_source_value == "Not Clinically Relevant", 0, 1)) %>%
  mutate(specialty_source_value_summarised = ifelse(
    is.na(specialty_source_value_summarised), 
    "Not Clinically Relevant",
    specialty_source_value_summarised)) %>%
  group_by(patid) %>%
  mutate(user_status = if_else(
    any(is_clinically_relevant == 1, na.rm = TRUE), 
    "User", "Non-User")) %>%
  mutate(not_users_status = case_when(
    user_status == "Non-User" & is.na(consdate) ~ "No Visits",
    user_status == "Non-User" ~ "Visits with Non-clinically Relevant Staff"
  )) %>% 
  ungroup() %>% 
  distinct(patid, user_status, .keep_all = TRUE)

# Summary table ----
# Using gtsummary::tbl_summary, we can summarise the table as follows
(gt_summary_table <- table_one_raw_visit_data %>% 
   gtsummary::tbl_summary(
     # select the variables of interest
     include = c(age_fracture, site, follow_up_reason, 
                 follow_up_time, user_status, not_users_status),
     # rename the variable names
     label = list(
       age_fracture = "Age at Fracture Occurrence",
       site = "Fracture Site", 
       follow_up_reason = "Censoring Event",
       follow_up_time = "Follow up Time",
       user_status = "User Status",
       not_users_status = "Non-user Visit Status"
     ),
     # indicate whether we want the missing values to be seen in the summary table
     missing = "no"
   ) %>%
   gtsummary::modify_header(label = "**Variable**"))
   

# Reorder factor by frequency
table_one_raw_visit_data <- table_one_raw_visit_data %>%
  mutate(follow_up_reason = forcats::fct_infreq(follow_up_reason))  

(gt_summary_table <- table_one_raw_visit_data %>% 
  gtsummary::tbl_summary(
    include = c(age_fracture, site, follow_up_reason, 
                follow_up_time, user_status, not_users_status),
    label = list(
      age_fracture = "Age at Fracture Occurrence",
      site = "Fracture Site", 
      follow_up_reason = "Censoring Event",
      follow_up_time = "Follow up Time",
      user_status = "User Status",
      not_users_status = "Non-user Visit Status"
    ),
    missing = "no"
  ) %>%
  gtsummary::modify_header(label = "**Variable**"))


   # gtsummary::modify_header(label = "**Variable**") %>% 
   # gtsummary::remove_footnote_header(stat_0) %>% 
   
 # %>% 
 #   gtsummary::modify_header(stat_0 = "Median (Q1, Q3); n(%)") %>%  
 #   gtsummary::modify
 #   gtsummary::remove_footnote_header(stat_0)



# Sense check ----
## Number of patients ----
(number_of_patients <- n_distinct(table_one_raw_visit_data$patid))

## Age (median, IQR) ----
(median_age <- median(table_one_raw_visit_data$age_fracture))
(iqr_age <- IQR(table_one_raw_visit_data$age_fracture))
(results_age <- paste0(median_age, "(", iqr_age, ")"))

table_one_raw_pt <- table_one_raw_visit_data %>% 
  distinct(patid, .keep_all = TRUE)

(median_age_pt <- median(table_one_raw_visit_data$age_fracture))
(iqr_age_pt <- IQR(table_one_raw_visit_data$age_fracture))

## Site of index fracture ----
# hip (n%)
# vertebra (n%)
# NHNS (n%)

(site_count <- table_one_raw_visit_data %>% 
   group_by(site) %>%
   summarise(count_of_pts = n_distinct(patid)) %>%
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

(censoring_events_count <- table_one_raw_visit_data %>% 
   group_by(follow_up_reason) %>%
   summarise(count_of_pts = n_distinct(patid), .groups = "drop") %>% 
   mutate(percent = round(count_of_pts / sum(count_of_pts) * 100, 2),
          result = paste0(count_of_pts, "(", percent, "%", ")")))

## Follow-up time in days ----
(median_followup_time <- median(table_one_raw_visit_data$follow_up_time))
(iqr_followup_time <- IQR(table_one_raw_visit_data$follow_up_time))

# Export ----
gt_summary_table_df <- as_tibble(gt_summary_table) %>% 
  rename("Variable" = "**Variable**",
         "N = 25,138" = "**N = 25,138**") %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

rio::export(gt_summary_table_df, here::here("CPRD Raw Analysis", "Results", "Table1_Raw.xlsx"))

# or HTML
# Saving the summary table as an HTML file
gtsummary::as_gt(gt_summary_table) %>%
  gt::gtsave(here::here("CPRD Raw Analysis", "Results", "gt_summary_table_Raw.html"))
