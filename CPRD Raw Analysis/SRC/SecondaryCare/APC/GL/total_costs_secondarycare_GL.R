cohort_hospital_all_costs

cohort_outpatient_all_cost 

cohort_emergency_all_cost


library(dplyr)

df_costs <- cohort_emergency_all_cost %>%
  select(patid, total_ae_cost, user_status_emergency) %>%
  full_join(
    cohort_outpatient_all_cost %>% select(patid, total_op_cost, user_status_outpatient),
    by = "patid"
  ) %>%
  full_join(
    cohort_hospital_all_costs %>% select(patid, total_in_cost, user_status_inpatient),
    by = "patid"
  ) %>%
  mutate(across(-patid, ~replace_na(.x, 0)))

df_costs <- df_costs %>%
  mutate(
    total_cost = total_ae_cost + total_op_cost + total_in_cost
  )


library(boot)
library(dplyr)
library(tibble)

bootstrap_total_cost <- bootstrap_summary_cost(
  data = df_costs,
  varname = "total_cost",
  R = 1000,
  seed = 626
)
