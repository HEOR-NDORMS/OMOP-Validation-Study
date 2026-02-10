library(dplyr)

pid <- 1901750820497

coaction <- cohort_outpatient_data_user_cost_coaction %>%
  #filter(subject_id == pid) %>%
  rename(cost_coact = weighted_avg_unit_cost)

standard <- cohort_outpatient_data_user_cost_standard %>%
  #filter(subject_id == pid) %>%
  rename(cost_stand = weighted_avg_unit_cost)

stopifnot(nrow(coaction) == nrow(standard))
intersect(names(coaction), names(standard))
visit_key <- c("subject_id", "visit_occurrence_id")

visit_compare <- coaction %>%
  select(all_of(visit_key), cost_coact) %>%
  inner_join(
    standard %>%
      select(all_of(visit_key), cost_stand),
    by = visit_key
  ) %>%
  mutate(diff = cost_coact - cost_stand)

visit_compare_specialties <- visit_compare %>%
  left_join(
    cohort_outpatient_data_user_cost_coaction %>%
      select(visit_occurrence_id, specialty_source_value),
    by = "visit_occurrence_id"
  ) %>% filter(diff != 0) %>% distinct(specialty_source_value, diff)
