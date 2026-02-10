cohort_hospital_all_costs_standard # ok

cohort_hospital_all_costs_coaction # ok

cohort_outpatient_all_cost_standard # ok

cohort_outpatient_all_cost_coaction # ok

cohort_emergency_all # ok


library(dplyr)

df_costs_standard <- cohort_emergency_all %>%
  select(subject_id, total_ae_cost, user_status_emergency) %>%
  full_join(
    cohort_outpatient_all_cost_standard %>% select(subject_id, total_op_cost, user_status_outpatient),
    by = "subject_id"
  ) %>%
  full_join(
    cohort_hospital_all_costs_standard %>% select(subject_id, total_in_cost, user_status_inpatient),
    by = "subject_id"
  ) %>%
  mutate(across(-subject_id, ~replace_na(.x, 0)))

df_costs_standard <- df_costs_standard %>%
  mutate(
    total_cost = total_ae_cost + total_op_cost + total_in_cost
  )


library(boot)
library(dplyr)
library(tibble)

bootstrap_total_cost_standard <- bootstrap_summary_cost(
  data = df_costs_standard,
  varname = "total_cost",
  R = 1000,
  seed = 626
)


##


df_costs_source <- cohort_emergency_all %>%
  select(subject_id, total_ae_cost, user_status_emergency) %>%
  full_join(
    cohort_outpatient_all_cost_coaction %>% select(subject_id, total_op_cost, user_status_outpatient),
    by = "subject_id"
  ) %>%
  full_join(
    cohort_hospital_all_costs_coaction %>% select(subject_id, total_in_cost, user_status_inpatient),
    by = "subject_id"
  ) %>%
  mutate(across(-subject_id, ~replace_na(.x, 0)))

df_costs_source <- df_costs_source %>%
  mutate(
    total_cost = total_ae_cost + total_op_cost + total_in_cost
  )


library(boot)
library(dplyr)
library(tibble)

bootstrap_total_cost_source <- bootstrap_summary_cost(
  data = df_costs_source,
  varname = "total_cost",
  R = 1000,
  seed = 626
)
