# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: AE Table 2: description of the patients' AE attendances
# Date: 16.07.2025
# Last date edited: 08.09.2025
# ****************************************************************************** #

# From Antonella:

# 2.	The following tables have been added
# validation._hes_ccare
# validation._hes_diagnosis_epi
# validation._hes_episodes
# validation._hes_hospital
# validation._hes_hrg
# validation._hes_procedures_epi
# validation._hesae_attendance
# validation._hesae_hrg
# validation._hesae_investigation
# validation._hesae_treatment
# validation._hesop_appointment
# validation._hesop_cliinical

# Starting with cohort_table from CleaningCohort_Raw_new.R ----
table_two_ae_raw <- cohort_table %>% 
  # Select the variables of interest
  select(patid, index_date, follow_up_end)


# Importing the AE attendance data ----
validation_ae_attendance <- my_dm$validation_hesae_attendance %>% collect() %>% 
  # Filter such that we have the visits between the study start and study end dates
  filter(arrivaldate >= study_start_date & arrivaldate <= study_end_date)

# Merge with cohort data ----
table_two_ae_raw <- left_join(table_two_ae_raw, validation_ae_attendance, 
                                by = "patid") %>% 
  # in order to count all the visits  from index date until follow-up end date.
  filter(arrivaldate >= index_date & arrivaldate <= follow_up_end)

# Import other AE datasets and merge with attandacnce data ----
## Treatment data ----
validation_ae_treatment <- my_dm$validation_hesae_treatment %>% collect()
table_two_ae_raw <- left_join(table_two_ae_raw,
                                validation_ae_treatment, 
                                by = c("patid", "aekey"))
## Investigation data ----
validation_ae_investigation <- my_dm$validation_hesae_investigation %>% collect()
table_two_ae_raw <- left_join(table_two_ae_raw,
                                validation_ae_investigation, 
                                by = c("patid", "aekey"))
## HRG data ----
validation_ae_hrg <- my_dm$validation_hesae_hrg %>% collect()
table_two_ae_raw <- left_join(table_two_ae_raw,
                                validation_ae_hrg, 
                                by = c("patid", "aekey"))
# Merge back to cohort data, so that we can hve the number of the cohortpts
# This will allow us to have users and non-users
table_two_ae_raw <- left_join(cohort_table %>% select(patid, index_date, follow_up_end), table_two_ae_raw,
                                by = c("patid", "index_date", "follow_up_end"))

# Create the users status
table_two_ae_raw <- table_two_ae_raw %>% 
  mutate(
    user_status = if_else(is.na(arrivaldate), "Non-User", "User")
  )

# Recode variables
# table_two_ae_raw_recoded <- table_two_ae_raw %>% 
#   mutate(
#     
#   )