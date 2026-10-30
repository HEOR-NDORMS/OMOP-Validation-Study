# Save_results_for_comparison.R
#
# Writes the patient-level HCRU and cost files that the OMOP analysis hands over
# to the CPRD Aurum-HES (source) analysis. The comparison scripts on the source
# side read these files to produce the final tables, the plot, the matched
# subgroup analysis and the Bland-Altman analysis.
#
# Run on the OMOP server, at the end of each strategy's analysis, and run only
# the matching half: the OMOP-standard block after the OMOP-standard scripts,
# the OMOP-source block after the OMOP-source scripts. Both blocks read the same
# object names (all_patients_all_encounters_counts, cohort_hospital_all, ...),
# so running the whole file in one session would write both sets of files from
# whichever strategy is in memory.
#
# The 16 files written here are then copied to the source server, into
# CPRD Raw Analysis/Results/Final/Comparison/.


## HCRU

## OMOP standard
all_patients_all_encounters_omop_standard <- all_patients_all_encounters_counts

cohort_hospital_all_omop_standard <- cohort_hospital_all %>%
  select(subject_id, total_spells)

cohort_outpatient_all_omop_standard <- cohort_outpatient_all %>%
  select(subject_id, total_op)

cohort_emergency_all_omop_standard <- cohort_emergency_all %>%
  select(subject_id, total_ae)

## OMOP source
all_patients_all_encounters_omop_source <- all_patients_all_encounters_counts

cohort_hospital_all_omop_source  <- cohort_hospital_all %>%
  select(subject_id, total_spells)

cohort_outpatient_all_omop_source <- cohort_outpatient_all %>%
  select(subject_id, total_op)

cohort_emergency_all_omop_source <- cohort_emergency_all %>%
  select(subject_id, total_ae)

# Save files standard
write.csv(
  all_patients_all_encounters_omop_standard,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/all_patients_all_encounters_omop_standard.csv",
  row.names = FALSE
)

write.csv(
  cohort_hospital_all_omop_standard,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/cohort_hospital_all_omop_standard.csv",
  row.names = FALSE
)

write.csv(
  cohort_outpatient_all_omop_standard,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/cohort_outpatient_all_omop_standard.csv",
  row.names = FALSE
)

write.csv(
  cohort_emergency_all_omop_standard,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/cohort_emergency_all_omop_standard.csv",
  row.names = FALSE)
  
# Save files source
write.csv(
  all_patients_all_encounters_omop_source,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/all_patients_all_encounters_omop_source.csv",
  row.names = FALSE
)

write.csv(
  cohort_hospital_all_omop_source,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/cohort_hospital_all_omop_source.csv",
  row.names = FALSE
)

write.csv(
  cohort_outpatient_all_omop_source,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/cohort_outpatient_all_omop_source.csv",
  row.names = FALSE
)

write.csv(
  cohort_emergency_all_omop_source,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/cohort_emergency_all_omop_source.csv",
  row.names = FALSE
)




## COSTS

# OMOP standard
all_patients_all_encounters_costs_omop_standard <- all_patients_all_encounters_costs

cohort_hospital_all_costs_omop_standard <- cohort_hospital_all_costs %>%
  select(subject_id, total_in_cost,total_spells, n_uncosted_spells)

cohort_outpatient_all_cost_omop_standard <- cohort_outpatient_all_cost %>%
  select(subject_id, total_op_cost)

cohort_emergency_all_cost_omop_standard <- cohort_emergency_all %>%
  select(subject_id, total_ae_cost)

# OMOP source
all_patients_all_encounters_costs_omop_source <- all_patients_all_encounters_costs

cohort_hospital_all_costs_omop_source <- cohort_hospital_all_costs %>%
  select(subject_id, total_in_cost, total_spells, n_uncosted_spells)

cohort_outpatient_all_cost_omop_source <- cohort_outpatient_all_cost %>%
  select(subject_id, total_op_cost)

cohort_emergency_all_cost_omop_source <- cohort_emergency_all %>%
  select(subject_id, total_ae_cost)

# Save files standard
write.csv(
  all_patients_all_encounters_costs_omop_standard,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/all_patients_all_encounters_costs_omop_standard.csv",
  row.names = FALSE
)

write.csv(
  cohort_hospital_all_costs_omop_standard,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/cohort_hospital_all_costs_omop_standard.csv",
  row.names = FALSE
)

write.csv(
  cohort_outpatient_all_cost_omop_standard,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/cohort_outpatient_all_cost_omop_standard.csv",
  row.names = FALSE
)

write.csv(
  cohort_emergency_all_cost_omop_standard,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/cohort_emergency_all_cost_omop_standard.csv",
  row.names = FALSE
)

# Save files source
write.csv(
  all_patients_all_encounters_costs_omop_source,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/all_patients_all_encounters_costs_omop_source.csv",
  row.names = FALSE
)

write.csv(
  cohort_hospital_all_costs_omop_source,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/cohort_hospital_all_costs_omop_source.csv",
  row.names = FALSE
)

write.csv(
  cohort_outpatient_all_cost_omop_source,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/cohort_outpatient_all_cost_omop_source.csv",
  row.names = FALSE
)

write.csv(
  cohort_emergency_all_cost_omop_source,
  "OMOP-Validation-Study/OMOP Analysis/Results/Final/Comparison/cohort_emergency_all_cost_omop_source.csv",
  row.names = FALSE
)
