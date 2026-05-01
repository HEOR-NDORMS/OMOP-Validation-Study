
## HCRU

## standard
all_patients_all_encounters_omop_standard <- all_patients_all_encounters_counts ##OK

cohort_hospital_all_omop_standard <- cohort_hospital_all %>% ##OK
  select(subject_id, total_spells)

cohort_outpatient_all_omop_standard <- cohort_outpatient_all %>% ##OK
  select(subject_id, total_op)

cohort_emergency_all_omop_standard <- cohort_emergency_all %>% ##OK
  select(subject_id, total_ae)

## source
all_patients_all_encounters_omop_source <- all_patients_all_encounters_counts ##OK

cohort_hospital_all_omop_source  <- cohort_hospital_all %>% ##OK
  select(subject_id, total_spells)

cohort_outpatient_all_omop_source <- cohort_outpatient_all %>% ##OK
  select(subject_id, total_op)

cohort_emergency_all_omop_source <- cohort_emergency_all %>% ##OK
  select(subject_id, total_ae)

# Save files standard
write.csv(
  all_patients_all_encounters_omop_standard,
  "OMOP Analysis/Results/Final/Comparison/all_patients_all_encounters_omop_standard.csv",
  row.names = FALSE
)

write.csv(
  cohort_hospital_all_omop_standard,
  "OMOP Analysis/Results/Final/Comparison/cohort_hospital_all_omop_standard.csv",
  row.names = FALSE
)

write.csv(
  cohort_outpatient_all_omop_standard,
  "OMOP Analysis/Results/Final/Comparison/cohort_outpatient_all_omop_standard.csv",
  row.names = FALSE
)

write.csv(
  cohort_emergency_all_omop_standard,
  "OMOP Analysis/Results/Final/Comparison/cohort_emergency_all_omop_standard.csv",
  row.names = FALSE)
  
# Save files source
write.csv(
  all_patients_all_encounters_omop_source,
  "OMOP Analysis/Results/Final/Comparison/all_patients_all_encounters_omop_source.csv",
  row.names = FALSE
)

write.csv(
  cohort_hospital_all_omop_source,
  "OMOP Analysis/Results/Final/Comparison/cohort_hospital_all_omop_source.csv",
  row.names = FALSE
)

write.csv(
  cohort_outpatient_all_omop_source,
  "OMOP Analysis/Results/Final/Comparison/cohort_outpatient_all_omop_source.csv",
  row.names = FALSE
)

write.csv(
  cohort_emergency_all_omop_source,
  "OMOP Analysis/Results/Final/Comparison/cohort_emergency_all_omop_source.csv",
  row.names = FALSE
)




## COSTS

# standard
all_patients_all_encounters_costs_omop_standard <- all_patients_all_encounters_costs ##OK

cohort_hospital_all_costs_omop_standard <- cohort_hospital_all_costs %>% ##OK
  select(subject_id, total_in_cost)

cohort_outpatient_all_cost_omop_standard <- cohort_outpatient_all_cost %>% ##OK
  select(subject_id, total_op_cost)

cohort_emergency_all_cost_omop_standard <- cohort_emergency_all %>% ##OK
  select(subject_id, total_ae_cost)

# source
all_patients_all_encounters_costs_omop_source <- all_patients_all_encounters_costs ##OK

cohort_hospital_all_costs_omop_source <- cohort_hospital_all_costs %>% ##OK
  select(subject_id, total_in_cost)

cohort_outpatient_all_cost_omop_source <- cohort_outpatient_all_cost %>% ##OK
  select(subject_id, total_op_cost)

cohort_emergency_all_cost_omop_source <- cohort_emergency_all %>% ##OK
  select(subject_id, total_ae_cost)

# Save files standard
write.csv(
  all_patients_all_encounters_costs_omop_standard,
  "OMOP Analysis/Results/Final/Comparison/all_patients_all_encounters_costs_omop_standard.csv",
  row.names = FALSE
)

write.csv(
  cohort_hospital_all_costs_omop_standard,
  "OMOP Analysis/Results/Final/Comparison/cohort_hospital_all_costs_omop_standard.csv",
  row.names = FALSE
)

write.csv(
  cohort_outpatient_all_cost_omop_standard,
  "OMOP Analysis/Results/Final/Comparison/cohort_outpatient_all_cost_omop_standard.csv",
  row.names = FALSE
)

write.csv(
  cohort_emergency_all_cost_omop_standard,
  "OMOP Analysis/Results/Final/Comparison/cohort_emergency_all_cost_omop_standard.csv",
  row.names = FALSE
)

# Save files source
write.csv(
  all_patients_all_encounters_costs_omop_source,
  "OMOP Analysis/Results/Final/Comparison/all_patients_all_encounters_costs_omop_source.csv",
  row.names = FALSE
)

write.csv(
  cohort_hospital_all_costs_omop_source,
  "OMOP Analysis/Results/Final/Comparison/cohort_hospital_all_costs_omop_source.csv",
  row.names = FALSE
)

write.csv(
  cohort_outpatient_all_cost_omop_source,
  "OMOP Analysis/Results/Final/Comparison/cohort_outpatient_all_cost_omop_source.csv",
  row.names = FALSE
)

write.csv(
  cohort_emergency_all_cost_omop_source,
  "OMOP Analysis/Results/Final/Comparison/cohort_emergency_all_cost_omop_source.csv",
  row.names = FALSE
)
