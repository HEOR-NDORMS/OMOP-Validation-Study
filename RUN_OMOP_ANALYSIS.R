# RUN_OMOP_ANALYSIS.R
#
# Driver for the OMOP-CDM side of the analysis. Run it from the project root,
# that is the folder that contains "OMOP-Validation-Study/".
#
# Two things to know before starting.
#
# 1. The NHS HRG4+ Payment Grouper runs outside R, in the middle of the inpatient
#    and outpatient costing scripts: each script writes the grouper input, and
#    reads the grouper output further down. Those scripts therefore cannot be
#    sourced in one go. Run the part above the export, run the grouper, then run
#    the rest. The line numbers are given at each step below.
#
# 2. Steps 1 to 5 are shared. Steps 6 to 11 are run twice, once for OMOP-standard
#    and once for OMOP-source, and Save_results_for_comparison.R is run at the end
#    of each of those two passes, running only the matching half of the file.
#
# The 16 CSVs written by Save_results_for_comparison.R are then copied to the
# source server, into "CPRD Raw Analysis/Results/Final/Comparison/", where
# RUN_SOURCE_ANALYSIS.R picks them up.

root <- file.path("OMOP-Validation-Study", "OMOP Analysis", "SRC")

# ---- Shared steps ----------------------------------------------------------

# 1. Packages, database connection and study parameters
source(file.path(root, "00-CodeToConnect.R"))

# 2. Identify fractures and build the fracture table
source(file.path(root, "01-CleaningFractures_OMOP.R"))

# 3. Apply the cohort inclusion and exclusion criteria
#    (this script sources Functions_OMOP.R)
source(file.path(root, "02-CleaningCohort_OMOP.R"))

# 4. Table 1, one script per strategy
source(file.path(root, "primary_care", "03-Table1_OMOP_standard.R"))
source(file.path(root, "primary_care", "03-Table1_OMOP_source.R"))

# 5. Primary care resource use and costs
source(file.path(root, "primary_care", "04-Analysis-PrimaryCare_OMOP.R"))

# ---- Hospital care: run once per strategy ----------------------------------

# 6. Inpatient resource use
source(file.path(root, "secondary_care", "APC", "OMOP_Inpatient_HCRU.R"))

# 7. Inpatient costs. GROUPER BREAK.
#    OMOP_Inpatient_Cost_omop_standard.R writes the grouper input at line 683
#    and reads the grouper output at line 708.
#    OMOP_Inpatient_Cost_omop_source.R   writes at line 767, reads at line 795.
#    Run to the write, run HRG4+ on the file it produced, then run the rest.
stop("Run the inpatient costing script for this strategy, with the grouper step, then continue.")

# 8. Outpatient resource use
source(file.path(root, "secondary_care", "OP", "OMOP_Outpatient_HCRU.R"))

# 9. Outpatient costs. GROUPER BREAK.
#    OMOP_Outpatient_Costs_omop_standard.R exports the grouper input at line 137
#    and reads the grouper output at line 177.
#    OMOP_Outpatient_Costs_omop_source.R   exports at line 137, reads at line 179.
stop("Run the outpatient costing script for this strategy, with the grouper step, then continue.")

# 10. A&E resource use and costs. No grouper step: the fields HRG4+ requires are
#     not available in the OMOP-mapped A&E data, so a weighted mean unit cost
#     from the National Schedule of NHS Costs is applied to every attendance.
source(file.path(root, "secondary_care", "OMOP_Emergency_HE.R"))

# 11. Write the files for the comparison on the source server.
#     Run ONLY the half of this script matching the strategy just analysed.
#     See the header of the script itself.
source(file.path(root, "Save_results_for_comparison.R"))
