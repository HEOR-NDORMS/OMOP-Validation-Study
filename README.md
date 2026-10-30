# Validation of resource use and cost estimates in OMOP-mapped versus source administrative data

R code for a validation study comparing estimates of primary and hospital care resource use and
costs derived from CPRD Aurum linked to Hospital Episode Statistics (HES), against estimates derived
from the same data mapped to the OMOP Common Data Model.

Three analyses are run in parallel on the same cohort definition:

- **Source CPRD Aurum-HES**, the reference analysis
- **OMOP-standard**, using only standardised OMOP concepts
- **OMOP-source**, additionally using the source values retained in the mapped data

## Repository layout

The code was developed on two separate servers, one holding the source CPRD Aurum-HES data and one
holding the OMOP-mapped data. Paths in the scripts are relative to a project root that contains the
folder for each side, so the two trees must be kept as they are:

```
<project root>
├── RUN_OMOP_ANALYSIS.R              driver for the OMOP side
├── RUN_SOURCE_ANALYSIS.R            driver for the source side and the comparison
├── OMOP-Validation-Study/
│   └── OMOP Analysis/
│       ├── SRC/                     all OMOP scripts and reference inputs
│       └── Results/                 outputs, including Results/Final/Comparison/
└── CPRD Raw Analysis/
    ├── SRC/
    │   ├── Functions_Raw.R
    │   └── Final/                   all source scripts and reference inputs
    └── Results/                     outputs, including Results/Final/Comparison/
```

Scripts are not written to be run from an arbitrary working directory. Open the project root in R
and run the drivers from there.

## How to run

Start with `RUN_OMOP_ANALYSIS.R`, then `RUN_SOURCE_ANALYSIS.R`. Each lists the scripts in order with
a comment for every step. Two features of the pipeline mean neither driver runs unattended.

**The HRG grouper runs outside R.** Hospital costs are derived by generating Healthcare Resource
Groups with the NHS HRG4+ Payment Grouper, which is a separate Windows application. Seven costing
scripts write the grouper input, and read the grouper output further down the same script, so they
have to be run in two parts with the grouper in between:

| Script | writes input | reads output |
|---|---|---|
| `OMOP_Inpatient_Cost_omop_standard.R` | line 683 | line 708 |
| `OMOP_Inpatient_Cost_omop_source.R` | line 767 | line 795 |
| `OMOP_Outpatient_Costs_omop_standard.R` | line 137 | line 177 |
| `OMOP_Outpatient_Costs_omop_source.R` | line 137 | line 179 |
| `05-Analysis_APC_Raw_Costs.R` | line 397 | line 424 |
| `Source_Outpatient_Costs_HE.R` | line 136 | line 171 |
| `Source_Emergency_Costs_HE.R` | line 165 | line 204 |

The OMOP A&E analysis has no grouper step: the investigation and treatment fields that HRG4+ requires
are not available in the OMOP-mapped A&E data, so a weighted mean unit cost is applied to every
attendance. `Source_Emergency_Costs_HE_Sensitivity.R` applies that same unit cost to the source
attendances, which isolates the effect of the costing method from the effect of the mapping.

**The two sides exchange patient-level files.** On the OMOP server,
`Save_results_for_comparison.R` writes 16 CSVs of patient-level resource use and costs, eight for
OMOP-standard and eight for OMOP-source. Run only the half of that script matching the strategy just
analysed: both halves read the same object names, so running the whole file in one session would
write both sets of files from whichever strategy is in memory.

Copy those 16 files into `CPRD Raw Analysis/Results/Final/Comparison/` on the source server. They
are read by `Final_table_all_results_hcru.R`, `Final_table_all_results_costs.R`,
`Final_plot_all_results.R` and `Subgroup_analysis_same_patients_comparison.R`, which produce the
comparison tables, the figure, the paired bootstrap in the matched subgroup, the Bland-Altman
agreement analysis and the decomposition of the inpatient cost difference by costing completeness.

## External inputs

Not included here, obtained separately:

- **NHS HRG4+ Payment Grouper** (2024/25 reference), NHS England
- **National Schedule of NHS Costs 2023-24**, for hospital unit costs
- **PSSRU Unit Costs of Health and Social Care 2024**, for primary care unit costs

Reference inputs are included, none of which contain patient-level data.

Cohort definition, in `OMOP Analysis/SRC/`:

| File | Used by |
|---|---|
| `fracture_sites_conditions_codes.xlsx` | `01-CleaningFractures_OMOP.R` |
| `trauma_codes.xlsx` | `01-CleaningFractures_OMOP.R` |
| `cancer_codes.xlsx` | `02-CleaningCohort_OMOP.R` |
| `mbd_codes.xlsx` | `02-CleaningCohort_OMOP.R` |

These code lists are read on the OMOP side only. The source CPRD Aurum-HES data were supplied as a
prepared cohort extract: five read-only tables holding the requested variables for the patients
already selected at the point of data access. The fracture, cancer and metabolic bone disease code
lists were applied in building that extract, which delivers the relevant dates as derived variables
(for example `hip_fx_fa_index_dt`, `cancer_fa_index_dt`, `paget_lpon_index_dt`). The source scripts
therefore start from those variables and apply only the remaining cohort criteria, whereas the OMOP
scripts have to apply the same definitions to the mapped tables themselves.

Costing and specialty mapping:

| File | Location | Used for |
|---|---|---|
| `provider_specialty_cprd_omop_final_2026.xlsx` | `OMOP Analysis/SRC/primary_care/` and `CPRD Raw Analysis/SRC/Final/primary_care/` | primary care specialty categories and mean encounter durations; one sheet per analysis |
| `NHS_Collection_Costs_2023_2024.xlsx` | `OMOP Analysis/SRC/secondary_care/` and the project root | hospital unit costs; the copy at the root is for the three source costing scripts, which read it as a bare filename |
| `HRG_ICD10_OPCS.xlsx` | `OMOP Analysis/SRC/secondary_care/APC/` | valid ICD-10 and OPCS-4 codes, used to clean grouper inputs |
| `check_mainspef_tretspef_..._omop_source_final.xlsx` and `..._omop_standard_final.xlsx` | `OMOP Analysis/SRC/secondary_care/` | reconstructed mainspef and tretspef pairs, one file per OMOP strategy |
| `outpatient_specialty_categories_hes_omop_final.xlsx` | both `secondary_care/OP/` folders | outpatient specialty categories |
| `mainspef_tretspef_dictionary.xlsx` | `CPRD Raw Analysis/SRC/Final/secondary_care/OP/` | HES mainspef to tretspef lookup |

The fracture and trauma code lists, the primary care specialties and the outpatient specialty
categories are also published as supplementary material with the paper.

The specialty mapping figure script is not included.

## Data

No patient data is included. CPRD Aurum and the linked HES datasets are available from the Clinical
Practice Research Datalink under its Research Data Governance process; this study was approved under
protocol 22_002498. The OMOP-mapped data are a local mapping of the same extract and are not
publicly available.
