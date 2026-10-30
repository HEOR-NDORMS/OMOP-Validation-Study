# RUN_SOURCE_ANALYSIS.R
#
# Driver for the source CPRD Aurum-HES side of the analysis, and for the
# comparison between the source and OMOP estimates. Run it from the project
# root, that is the folder that contains "CPRD Raw Analysis/".
#
# Two things to know before starting.
#
# 1. The NHS HRG4+ Payment Grouper runs outside R, in the middle of the inpatient,
#    outpatient and A&E costing scripts: each writes the grouper input and reads
#    the grouper output further down, so they cannot be sourced in one go. Run the
#    part above the write, run the grouper, then run the rest. Line numbers are
#    given at each step.
#
# 2. Steps 13 onwards compare the source estimates with the OMOP estimates and
#    need the 16 CSVs written by Save_results_for_comparison.R on the OMOP server.
#    Copy them into "CPRD Raw Analysis/Results/Final/Comparison/" before running
#    them.

root <- file.path("CPRD Raw Analysis", "SRC", "Final")

# ---- Cohort ----------------------------------------------------------------

# 1. Packages, database connection and study parameters
source(file.path(root, "00-CodeToConnect_Raw.R"))

# 2. Identify fractures and build the fracture table
source(file.path(root, "01-CleaningFractures_Raw.R"))

# 3. Apply the cohort inclusion and exclusion criteria
#    (this script sources CPRD Raw Analysis/SRC/Functions_Raw.R)
source(file.path(root, "02-CleaningCohort_Raw.R"))

# 4. Table 1
source(file.path(root, "03-Table1_Raw.R"))

# ---- Resource use and costs by setting -------------------------------------

# 5. Primary care
source(file.path(root, "primary_care", "04-Analysis-PrimaryCare_Raw.R"))

# 6. Inpatient resource use
source(file.path(root, "secondary_care", "APC", "04-Analysis_APC_Raw.R"))

# 7. Inpatient costs. GROUPER BREAK: writes the grouper input at line 397,
#    reads the grouper output at line 424.
stop("Run 05-Analysis_APC_Raw_Costs.R with the grouper step, then continue.")

# 8. Outpatient resource use
source(file.path(root, "secondary_care", "OP", "Source_Outpatient_HE.R"))

# 9. Outpatient costs. GROUPER BREAK: writes at line 136, reads at line 171.
stop("Run Source_Outpatient_Costs_HE.R with the grouper step, then continue.")

# 10. A&E resource use
source(file.path(root, "secondary_care", "AE", "Source_Emergency_HE.R"))

# 11. A&E costs. GROUPER BREAK: writes at line 165, reads at line 204.
stop("Run Source_Emergency_Costs_HE.R with the grouper step, then continue.")

# 12. A&E sensitivity analysis: re-cost the source attendances with the same
#     weighted mean unit cost used in the OMOP analyses.
source(file.path(root, "secondary_care", "AE", "Source_Emergency_Costs_HE_Sensitivity.R"))

# ---- Comparison with the OMOP estimates ------------------------------------
# These steps read the 16 CSVs copied from the OMOP server. Stop here and copy
# them in if you have not already.

# 13. Resource use comparison table
source(file.path(root, "Final_table_all_results_hcru.R"))

# 14. Cost comparison table
source(file.path(root, "Final_table_all_results_costs.R"))

# 15. Resource use and cost figure
source(file.path(root, "Final_plot_all_results.R"))

# 16. Matched subgroup: paired bootstrap, Bland-Altman agreement, and the
#     decomposition of the inpatient cost difference by costing completeness
source(file.path(root, "Subgroup_analysis_same_patients_comparison.R"))

