# I'd suggest to also go back to the consultation table as it was delivered by
# antonella so before doing any cleaning on our side and look for these two patients
# one at the time, include the visits between index and follow up for each specific 
# patient and distinct to exclude visits with same provider and day
# (can use the provider id so there is no need to join) and then
# check if you get the same counts. 
# Just to make sure we did not introduce any extra visit by joining or
# doing any of the other preparation on the data as a whole. 

# into 3: GP, Nurse, Nurse
provider_cost_inputs <- rio::import("provider_specialty_cprd_V.0.3.xlsx") %>%
  janitor::clean_names() %>%
  # include those with 1 which are clinically relevant
  filter(include == 1) %>%
  select(!notes)

# Remove the visits that are not clinically relevant
specialty_source_value_ids <- unique(provider_cost_inputs$specialty_source_value)

# pt_case1_con <- validation_consultation %>% 
#   filter(patid == bit64::as.integer64(6222535220539)) %>% 
#   left_join(validation_staff, by = "staffid") %>% 
#   inner_join(cohort_table_raw, validation_consultation,
#              by = "patid"
#   ) %>% 
#   filter(consdate >= index_date & consdate <= follow_up_end) %>% 
#   distinct(consdate, patid, description, .keep_all = TRUE) %>% 
#   mutate(description = case_when(
#     description %in% specialty_source_value_ids ~ description,
#     TRUE ~ "Not Clinically Relevant"
#   )) %>%
#   rename("specialty_source_value" = "description") %>% 
#   left_join(provider_cost_inputs, by = "specialty_source_value") 
#  

pt_case1_con <- validation_consultation %>% 
  filter(patid == bit64::as.integer64(6222535220539)) %>% 
  left_join(validation_staff, by = "staffid") %>% 
  filter(consdate >= as.Date("2013-04-03") & consdate <= as.Date("2015-01-06")) %>% 
  distinct(consdate, patid, description, .keep_all = TRUE) %>% 
  mutate(description = case_when(
    description %in% specialty_source_value_ids ~ description,
    TRUE ~ "Not Clinically Relevant"
  )) 

# Create summary visits
summary_visits_pt_case1 <- pt_case1_con %>%
  group_by(patid, description) %>%
  summarise(visit_count = n(), .groups = "drop")


# pt_case2_con <- validation_consultation %>% 
#   filter(patid == bit64::as.integer64(2860458720395)) %>% 
#   left_join(validation_staff, by = "staffid") %>% 
#   inner_join(cohort_table_raw, validation_consultation,
#              by = "patid"
#   ) %>% 
#   filter(consdate >= index_date & consdate <= follow_up_end) %>% 
#   mutate(description = case_when(
#     description %in% specialty_source_value_ids ~ description,
#     TRUE ~ "Not Clinically Relevant"
#   ))

pt_case2_con <- validation_consultation %>% 
  filter(patid == bit64::as.integer64(2860458720395)) %>% 
  left_join(validation_staff, by = "staffid") %>% 
  filter(consdate >= as.Date("2013-11-06") & consdate <= as.Date("2014-03-28")) %>% 
  distinct(consdate, patid, description, .keep_all = TRUE) %>% 
  mutate(description = case_when(
    description %in% specialty_source_value_ids ~ description,
    TRUE ~ "Not Clinically Relevant"
  ))

# Create summary visits
summary_visits_pt_case2 <- pt_case2_con %>%
  group_by(patid, description) %>%
  summarise(visit_count = n(), .groups = "drop")

