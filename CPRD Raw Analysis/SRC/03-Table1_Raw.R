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

# Summary table ----
# Using gtsummary::tbl_summary, we can summarise the table as follows
(gt_summary_table <- table_one_raw %>% 
   gtsummary::tbl_summary(
     # select the variables of interest
     include = c(age_fracture, site, follow_up_reason, follow_up_time),
     # rename the variable names
     label = list(
       age_fracture = "Age at fracture occurrence",
       site = "Fracture site", 
       follow_up_reason = "Censoring event",
       follow_up_time = "Follow up time"
     ),
     # indicate whether we want the missing values to be seen in the summary table
     missing = "no"
   ) %>%
   gtsummary::modify_header(label = "**Variable**"))
   
   # gtsummary::modify_header(label = "**Variable**") %>% 
   # gtsummary::remove_footnote_header(stat_0) %>% 
   
 # %>% 
 #   gtsummary::modify_header(stat_0 = "Median (Q1, Q3); n(%)") %>%  
 #   gtsummary::modify
 #   gtsummary::remove_footnote_header(stat_0)
)


# Sense check ----
## Number of patients ----
(number_of_patients <- n_distinct(table_one_raw$patid))

## Age (median, IQR) ----
(median_age <- median(table_one_raw$age_fracture))
(iqr_age <- IQR(table_one_raw$age_fracture))
(results_age <- paste0(median_age, "(", iqr_age, ")"))

table_one_raw_pt <- table_one_raw %>% 
  distinct(patid, .keep_all = TRUE)

(median_age_pt <- median(table_one_raw_pt$age_fracture))
(iqr_age_pt <- IQR(table_one_raw_pt$age_fracture))

## Site of index fracture ----
# hip (n%)
# vertebra (n%)
# NHNS (n%)

(site_count <- table_one_raw %>% 
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

(censoring_events_count <- table_one_raw %>% 
   group_by(follow_up_reason) %>%
   summarise(count_of_pts = n_distinct(patid), .groups = "drop") %>% 
   mutate(percent = round(count_of_pts / sum(count_of_pts) * 100, 2),
          result = paste0(count_of_pts, "(", percent, "%", ")")))

## Follow-up time in days ----
(median_followup_time <- median(table_one_raw$follow_up_time))
(iqr_followup_time <- IQR(table_one_raw$follow_up_time))

# Export ----
gt_summary_table_df <- as_tibble(gt_summary_table)

rio::export(gt_summary_table_df, here::here("CPRD Raw Analysis", "Results", "Table1_Raw.xlsx"))

# or HTML
# Saving the summary table as an HTML file
gtsummary::as_gt(gt_summary_table) %>%
  gt::gtsave(here::here("CPRD Raw Analysis", "Results", "gt_summary_table_Raw.html"))
