# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: OP Table 1: description of the patients
# Date: 16.07.2025
# Last date edited: 16.07.2025
# ****************************************************************************** #

# Number of patients
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
# attended – Attended or Did Not Attend (n%)
# atentype – Attendance Type (n%)
# firstatt – First Attendance (n%)

# Some info about the variables

# attended – Attended or Did Not Attend
# This variable indicates whether the patient attended the appointment.
# The values are:

# 2 = appointment cancelled by or on behalf of the patient
# 3 = didn't attended - no advance warning given
# 4 = Appointment cancelled or postponed by the health care provider
# 5 = seen, having attended on time or, if late, before the relevant care professional care was ready to see the patient
# 6 = arrived late after the relevant care professional was ready to see the patient, but was seen
# 7 = didn't attended - patient arrived late
# 9 = not known

# atentype – Attendance Type
# This describes the nature of the appointment:

# 1 = Attended first appointment
# 2 = Attended subsequent appointment
# 3 = Attended but first/subsequent/tele unknown
# 4 = Did not attend first appointment
# 5 = Did not attend subsequent appointment
# 6 = Did not attend but first/subsequent/tele unknown
# 7 = Patient cancelled first appointment
# 8 = Patient cancelled subsequent appointment
# 9 = Patient cancelled but first/subsequent/tele unknown
# 10 = Hospital postponed/cancelled first appointment
# 11 = Hospital postponed/cancelled subsequent appointment
# 12 = Hospital postponed/cancelled but first/subsequent unknown
# 13 = Not known
# 21 = Attended first tele consultation
# 22 = Attended subsequent tele consultation
# 24 = Did not attend first tele consultation
# 25 = Did not attend subsequent tele consultation
# 27 = Patient cancelled first tele consultation
# 28 = Patient cancelled subsequent tele consultation
# 30 = Hospital postponed/cancelled first tele consultation
# 31 = Hospital postponed/cancelled subsequent tele consultation

# firstatt – First Attendance
# This is a character field that further categorizes the appointment:

# 1 = First attendance face to face
# 2 = Follow-up attendance face to face
# 3 = First telephone or telemedicine consultation
# 4 = Follow-up telephone or telemedicine consultation
# X = Not known (from 2008/09)
# 9 = Not known (prior to 2008/09)

# Starting with cohort_table from CleaningCohort_Raw_new.R ----
table_one_op_raw <- cohort_table

# Importing the appointments data ----
validation_appointment <- my_dm$validation_hesop_appointment %>% collect() %>% 
  # Filter such that we have the visits between the study start and study end dates
  filter(apptdate >= study_start_date & apptdate <= study_end_date)

# Merge with cohort data ----
table_one_op_raw_appt <- left_join(table_one_op_raw, validation_appointment, by = "patid") %>% 
  # in order to count all the visits  from index date until follow-up end date.
  filter(apptdate >= index_date & apptdate <= follow_up_end)

# Merge cohort data
# merge back to cohort data, inorder to have patients have been filtered out 
# in the previous step
table_one_op_raw_appt <- left_join(
  cohort_table, table_one_op_raw_appt,
  by = c(
    "patid", "death", "death_date",
    "aurum_min_accept_date", "aurum_max_accept_date", "site",
    "index_date","cancer_fa_index_dt", "paget_fa_index_dt", 
    "two_yrs_after_index","fracture_after_index", "study_end",
    "follow_up_end", "follow_up_reason", "follow_up_time"
  )
)

# Remove unattended appointments ----
names(table_one_op_raw_appt)
table(table_one_op_raw_appt$attended)

# 2 = appointment cancelled by or on behalf of the patient
# 3 = didn't attended - no advance warning given
# 4 = Appointment cancelled or postponed by the health care provider
# 5 = seen, having attended on time or, if late, before the relevant care professional care was ready to see the patient
# 6 = arrived late after the relevant care professional was ready to see the patient, but was seen
# 7 = didn't attended - patient arrived late
# 9 = not known

## Check if there are unattended appts ----
any(
  table_one_op_raw_appt %>% 
    filter(!(attended %in% c(5, 6))) %>% 
    summarise(n = n()) %>% 
    pull(n) > 0
)

table_one_op_raw_appt.1 <- table_one_op_raw_appt %>% 
  filter(attended %in% c(5, 6))

# Recode variables ----
table_one_op_raw_appt_recoded <- table_one_op_raw_appt %>% 
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
  )) %>% 
  mutate_at(c("attended", "atentype", "firstatt"), as.factor) %>%
  mutate(
    attended_recoded = fct_recode(
      attended,
      "Appointment cancelled by or on behalf of the patient" = "2",
      "Did not attend – no advance warning given" = "3",
      "Appointment cancelled or postponed by the health care provider" = "4",
      "Seen – attended on time or, if late, before the relevant care professional was ready to see the patient" = "5",
      "Arrived late after the relevant care professional was ready to see the patient, but was seen" = "6",
      "Did not attend – patient arrived late" = "7",
      "Attendance status not known" = "9"
      
      ),
    atentype_recoded = fct_recode(
      atentype,
      "Attended first appointment" = "1",
      "Attended subsequent appointment" = "2",
      "Attended but first/subsequent/tele unknown" = "3",
      "Did not attend first appointment" = "4",
      "Did not attend subsequent appointment" = "5",
      "Did not attend but first/subsequent/tele unknown" = "6",
      "Patient cancelled first appointment" = "7",
      "Patient cancelled subsequent appointment" = "8",
      "Patient cancelled but first/subsequent/tele unknown" = "9",
      "Hospital postponed/cancelled first appointment" = "10",
      "Hospital postponed/cancelled subsequent appointment" = "11",
      "Hospital postponed/cancelled but first/subsequent unknown" = "12",
      "Not known" = "13",
      "Attended first tele consultation" = "21",
      "Attended subsequent tele consultation" = "22",
      "Did not attend first tele consultation" = "24",
      "Did not attend subsequent tele consultation" = "25",
      "Patient cancelled first tele consultation" = "27",
      "Patient cancelled subsequent tele consultation" = "28",
      "Hospital postponed/cancelled first tele consultation" = "30",
      "Hospital postponed/cancelled subsequent tele consultation" = "31"
    ),
    firstatt_recoded = fct_recode(
      firstatt,
      "First attendance face to face" = "1",
      "Follow-up attendance face to face" = "2",
      "First telephone or telemedicine consultation" = "3",
      "Follow-up telephone or telemedicine consultation" = "4",
      "Not known (from 2008/09)" = "X",
      "Not known (prior to 2008/09)" = "9"
  )
   ) %>% 
  mutate(appt_status = if_else(is.na(apptdate), "No Appointment", "Had Appointment"))

# count of patients with appointments/or not
(c_pts_appts <- table_one_op_raw_appt_recoded %>% 
  group_by(appt_status) %>% 
  summarise(n_pts = n_distinct(patid),
            .groups = "drop")
)

## Keep only distinct rows ----
# table_one_op_raw_appt_recoded <- table_one_op_raw_appt_recoded %>% 
#   distinct(apptdate, patid, attendkey, .keep_all = TRUE)


# Reorder factor by frequency
table_one_op_raw_appt_recoded <- table_one_op_raw_appt_recoded %>%
  mutate(follow_up_reason = forcats::fct_infreq(follow_up_reason),
         attended_recoded = forcats::fct_infreq(attended_recoded),
         atentype_recoded = forcats::fct_infreq(atentype_recoded),
         firstatt_recoded = forcats::fct_infreq(firstatt_recoded)) %>% 
  mutate(follow_up_time = as.numeric(follow_up_time))


# (op_gt_summary_table <- table_one_op_raw_appt_recoded %>% 
#     gtsummary::tbl_summary(
#       include = c(site, follow_up_reason, 
#                   follow_up_time, attended_recoded,
#                   atentype_recoded, firstatt_recoded),
#       label = list(
#         site = "Fracture Site", 
#         follow_up_reason = "Censoring Event",
#         follow_up_time = "Follow Up Time",
#         attended_recoded = "Attended or Did Not Attend",
#         atentype_recoded = "Attendance Type",
#         firstatt_recoded = "First Attendance"
#       ),
#       missing = "no"
#     ) %>%
#     gtsummary::modify_header(label = "**Variable**"))

(op_gt_summary_table <- table_one_op_raw_appt_recoded %>% 
    gtsummary::tbl_summary(
      include = c(attended_recoded,
                  atentype_recoded, firstatt_recoded, appt_status),
      label = list(
        attended_recoded = "Attended or Did Not Attend",
        atentype_recoded = "Attendance Type",
        firstatt_recoded = "First Attendance",
        appt_status = "OP Appointment Status"
      ),
      missing = "no"
    ) %>%
    gtsummary::modify_header(label = "**Variable**"))

# Export ----
op_gt_summary_table_df <- as_tibble(op_gt_summary_table) %>% 
  rename("Variable" = "**Variable**",
         "N (Appointments) = 206,398" = "**N = 206,398**") %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

rio::export(op_gt_summary_table_df, here::here("CPRD Raw Analysis", "Results",
                                               "SecondaryCareAnalysis", "Table1_OP_Raw.xlsx"))
