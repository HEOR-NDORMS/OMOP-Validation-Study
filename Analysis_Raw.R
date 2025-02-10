# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Primary care and Secondary care analysis	
# Date: 10.02.2025
# Last date edited: 10.02.2025
# ****************************************************************************** #

# Service users are defined as patients with records of healthcare encounters 
# with the NHS and their resource use and associated costs will be estimated 
# separately for primary care (hence excluding patients with no record of primary care visits with any relevant healthcare staff),

# Table 5 “validation_staff” contains the requested fields from aurum.staff

# Starting with table_one_raw

visit_data <- table_one_raw

# HRCU Analysis ----
## Import the staff and consultation tables ----
# this is to be used for the specialty
validation_staff <- my_dm$`validation_staff` %>%  collect()
validation_consultation <- my_dm$`validation_consultation` %>%  collect()

## Merge with validation_consultation ----
# Filter with study dates
visit_data.1  <- inner_join(visit_data, validation_consultation ,  
                               by = "patid") %>%
  # in order to count all the visits from index date until follow-up end date.
  filter(consdate >= index_date & consdate <= follow_up_end)

## Merge with validation_staff ----
visit_data_final <- left_join(visit_data.1, validation_staff,
                           by = "staffid")

## Keep only distinct rows ----
# - eliminate all visits in the same day with the same specialist by the same patient

(visits_specialist_day <- visit_data_final %>%
   distinct(consdate, patid, description) %>% tally())

visit_data_final <- visit_data_final %>% 
  distinct(consdate, patid, description, .keep_all = TRUE)

## Select columns needed ----
visit_data_final_min <- visit_data_final %>% 
  select(!c(gender, yob, death, death_date,
            fx_date, cancer_fa_index_dt, paget_fa_index_dt)) %>% 
  rename("fracture_site" = "site")

## Count of visits ----
### Filtering visits based on the cohort_combined
filtered_visits <- visit_data_final_min %>%
  group_by(patid, index_date, description) %>% # we group by index date to ensure each visit is associated with an entry
  summarise(visit_count = n()) %>%
  ungroup() 
