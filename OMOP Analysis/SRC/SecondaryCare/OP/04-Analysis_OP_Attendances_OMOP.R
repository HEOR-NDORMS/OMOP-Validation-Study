# CPRD primary care data: 
# https://www.cprd.com/sites/default/files/2024-11/HES_OP_Documentation_v2.1.pdf

# SOME NOTES FROM THE DATA DICTIONARY

# HES OP data are a collection of individual records of outpatient appointments 
# occurring in England only. 

# It includes information on the type of outpatient consultation appointment 
# dates, the main specialty and treatment specialty under which the patient was 
# treated, referral source, waiting times, clinical diagnosis and procedures 
# performed

# Each record represents a single outpatient attendance at a consultant or
# allied health clinic at a single hospital provider.

# Load R Packages ----
library(tidyverse)

# Loading appointments data ----
appt_data <- cohort_appt_data_min

# Minimise data ----
names(appt_data)
appt_data_min <- appt_data %>% 
  select(patid, site, index_date, follow_up_end, follow_up_reason,
         follow_up_time, attendkey, apptdate, attended, atentype,
         firstatt, attended_recoded, atentype_recoded, firstatt_recoded,
         diag_01_description, collapsed_diag_01, opertn_01_description,
         collapsed_opertn_01, user_status) %>% 
  arrange(patid)

# HRCU ----
## Users ----
users_appts_data <- appt_data_min %>%
  filter(user_status == "User") 

users_appts <- users_appts_data %>% 
  group_by(patid) %>%                     # group data by patid
  select(patid, apptdate, attendkey) %>%  # select only variables of interest: patid, apptdate, attendkey
  arrange(patid, apptdate) %>%            # sort data according to appointment date
  distinct() %>%                          # deduplicate       
  mutate(appt_no  = 1:n()) %>%            # generate an appointment counter
  ungroup()

users_appts_data_final <- merge(users_appts_data, users_appts , 
                                all.x=T, all.y=T,
                                by=c("patid", "apptdate", "attendkey")) %>% 
  arrange(patid)
rm(users_appts)

users_appts_per_patient <- users_appts_data_final %>%
  group_by(patid) %>%                     # group data per patient
  summarise(n_appts = n()) %>%   # derive total number of appointments by picking max value of apptorder
  ungroup()  %>%                          # now, ungroup the data and save only one row summary per patient
  select(patid, n_appts)  %>%             # save only patid and nr of appts
  distinct()


### Summary table ----
users_overall_summary_appts <-  users_appts_per_patient %>%
  summarise(
    n        = n(),
    mean     = mean(n_appts),
    min      = min(n_appts),
    max      = max(n_appts),
    sd       = sd(n_appts),
    median   = quantile(n_appts, 0.5),
    lower_q  = quantile(n_appts, 0.25),
    upper_q  = quantile(n_appts, 0.75),
    se               = sd / sqrt(n),
    ci_lower         = mean - qt(0.975, df = n - 1) * se,
    ci_upper         = mean + qt(0.975, df = n - 1) * se
  ) %>% 
  mutate(across(everything(), round, 2))

## All ----
# users_appts_data <- appt_data_min 
all_appts <- appt_data_min %>% 
  group_by(patid) %>%                     # group data by patid
  select(patid, apptdate, attendkey) %>%  # select only variables of interest: patid, apptdate, attendkey
  arrange(patid, apptdate) %>%            # sort data according to appointment date
  distinct() %>%                          # deduplicate       
  mutate(appt_no  = 1:n()) %>%            # generate an appointment counter
  ungroup() %>% 
  mutate(appt_no = if_else(is.na(apptdate), 0, appt_no))

all_appts_data_final <- merge(all_appts, appt_data_min , 
                              all.x=T, all.y=T,
                              by=c("patid", "apptdate", "attendkey")) %>% 
  arrange(patid)
rm(all_appts)

all_appts_per_patient <- all_appts_data_final %>%
  group_by(patid) %>%                     # group data per patient
  summarise(n_appts = max(appt_no)) %>%   # derive total number of appointments by picking max value of apptorder
  ungroup()  %>%                          # now, ungroup the data and save only one row summary per patient
  select(patid, n_appts)  %>%             # save only patid and nr of appts
  distinct()


