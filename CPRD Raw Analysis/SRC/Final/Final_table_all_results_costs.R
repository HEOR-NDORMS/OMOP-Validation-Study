library(boot)
library(dplyr)
library(purrr)
library(readr)
library(scales)

# -------- import -----------

all_patients_all_encounters_cost_source <- all_patients_all_encounters_costs
cohort_hospital_all_cost_source <-cohort_hospital_all_costs %>% select(patid, total_in_cost)
cohort_outpatient_all_cost_source <-cohort_outpatient_all_cost %>%  select(patid, total_op_cost)
cohort_emergency_all_cost_source <-cohort_emergency_all_cost %>% select(patid, total_ae_cost)


all_patients_all_encounters_cost_omop_standard <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/all_patients_all_encounters_costs_omop_standard.csv") 
cohort_hospital_all_cost_omop_standard <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_hospital_all_costs_omop_standard.csv") 
cohort_outpatient_all_cost_omop_standard <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_outpatient_all_cost_omop_standard.csv") 
cohort_emergency_all_cost_omop_standard <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_emergency_all_cost_omop_standard.csv") 


all_patients_all_encounters_cost_omop_source <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/all_patients_all_encounters_costs_omop_source.csv") 
cohort_hospital_all_cost_omop_source <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_hospital_all_costs_omop_source.csv") 
cohort_outpatient_all_cost_omop_source <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_outpatient_all_cost_omop_source.csv") 
cohort_emergency_all_cost_omop_source <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_emergency_all_cost_omop_source.csv") 

# ---------- helper to build per-patient total cost ----------
build_total_cost_df <- function(primary, inpatient, outpatient, emergency, id_col) {
  
  primary2 <- primary %>%
    transmute(id = .data[[id_col]], primary_cost = total_patient_cost) %>%
    distinct(id, .keep_all = TRUE)
  
  inpatient2 <- inpatient %>%
    transmute(id = .data[[id_col]], inpatient_cost = total_in_cost) %>%
    distinct(id, .keep_all = TRUE)
  
  outpatient2 <- outpatient %>%
    transmute(id = .data[[id_col]], outpatient_cost = total_op_cost) %>%
    distinct(id, .keep_all = TRUE)
  
  emergency2 <- emergency %>%
    transmute(id = .data[[id_col]], emergency_cost = total_ae_cost) %>%
    distinct(id, .keep_all = TRUE)
  
  primary2 %>%
    left_join(inpatient2, by = "id") %>%
    left_join(outpatient2, by = "id") %>%
    left_join(emergency2, by = "id") %>%
    mutate(
      across(c(primary_cost, inpatient_cost, outpatient_cost, emergency_cost), ~ coalesce(.x, 0)),
      total_cost = primary_cost + inpatient_cost + outpatient_cost + emergency_cost
    ) %>%
    select(id, total_cost)
}


# ---------- build total cost objects ----------
total_cost_source <- build_total_cost_df(
  primary    = all_patients_all_encounters_cost_source,
  inpatient  = cohort_hospital_all_cost_source,
  outpatient = cohort_outpatient_all_cost_source,
  emergency   = cohort_emergency_all_cost_source,
  id_col     = "patid"
)

total_cost_omop_standard <- build_total_cost_df(
  primary    = all_patients_all_encounters_cost_omop_standard,
  inpatient  = cohort_hospital_all_cost_omop_standard,
  outpatient = cohort_outpatient_all_cost_omop_standard,
  emergency   = cohort_emergency_all_cost_omop_standard,
  id_col     = "subject_id"
)

total_cost_omop_source <- build_total_cost_df(
  primary    = all_patients_all_encounters_cost_omop_source,
  inpatient  = cohort_hospital_all_cost_omop_source,
  outpatient = cohort_outpatient_all_cost_omop_source,
  emergency   = cohort_emergency_all_cost_omop_source,
  id_col     = "subject_id"
)


# ---------- CONFIG ----------
R <- 1000
seed <- 626
out_dir <- "CPRD Raw Analysis/Results/Final/Comparison/final_table_costs"
dir.create(out_dir, showWarnings = FALSE)

# ---------- DEFINE DOMAIN x COHORT DATAFRAMES ----------
# Replace these names with the exact objects you have in your environment.
# Use the pattern you suggested: all_patients_all_encounters_costs_source, ..._omop_standard, ..._omop_source, etc.

# Domain -> named list of cohort dataframes
domain_cohort_map <- list(
  primary_care = list(
    Source =   all_patients_all_encounters_cost_source,
    OMOP_standard = all_patients_all_encounters_cost_omop_standard,
    OMOP_source   = all_patients_all_encounters_cost_omop_source,
    var = "total_patient_cost",
    label = "Primary care",
    is_currency = TRUE
  ),
  inpatient = list(
    Source =   cohort_hospital_all_cost_source,
    OMOP_standard = cohort_hospital_all_cost_omop_standard,
    OMOP_source   = cohort_hospital_all_cost_omop_source,
    var = "total_in_cost",
    label = "Inpatient",
    is_currency = TRUE
  ),
  outpatient = list(
    Source =   cohort_outpatient_all_cost_source,
    OMOP_standard = cohort_outpatient_all_cost_omop_standard,
    OMOP_source   = cohort_outpatient_all_cost_omop_source,
    var = "total_op_cost",
    label = "Outpatient",
    is_currency = TRUE
  ),
  ae = list(
    Source =   cohort_emergency_all_cost_source,
    OMOP_standard = cohort_emergency_all_cost_omop_standard,
    OMOP_source   = cohort_emergency_all_cost_omop_source,
    var = "total_ae_cost",
    label = "A&E",
    is_currency = TRUE
  )
)

domain_cohort_map$total_cost <- list(
  Source = total_cost_source,
  OMOP_standard = total_cost_omop_standard,
  OMOP_source = total_cost_omop_source,
  var = "total_cost",
  label = "Total costs",
  is_currency = TRUE
)

# comparisons (paired differences)
comparisons <- list(c("Source","OMOP_standard"), c("Source","OMOP_source"))

# ---------- helper: simple bootstrap wrapper ----------
run_boot <- function(df, varname, R = 1000, seed = 626) {
  if (!varname %in% names(df)) stop("variable not in df: ", varname)
  stat_fn <- function(d, i) mean(d[i, ][[varname]], na.rm = TRUE)
  set.seed(seed)
  b <- boot::boot(data = df, statistic = stat_fn, R = R)
  # return both the boot object and t vector
  list(boot = b, t = as.numeric(b$t))
}

format_currency <- function(x) scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 0.01)(x)

# ---------- 1) Run separate bootstraps for every domain and cohort ----------
boot_store <- list()  # boot_store[[domain]][[cohort]] -> boot obj
t_store <- list()     # t_store[[domain]][[cohort]] -> numeric vector length R

for (domain in names(domain_cohort_map)) {
  entry <- domain_cohort_map[[domain]]
  varname <- entry$var
  boot_store[[domain]] <- list()
  t_store[[domain]] <- list()
  for (cohort_name in c("Source","OMOP_standard","OMOP_source")) {
    message("Bootstrap: domain=", domain, " cohort=", cohort_name)
    df <- entry[[cohort_name]]
    # Optional: check unique-subject rows, id consistency etc here if needed
    out <- run_boot(df, varname = varname, R = R, seed = seed)
    boot_store[[domain]][[cohort_name]] <- out$boot
    t_store[[domain]][[cohort_name]] <- out$t
    # save artifacts
  }
}

# ---------- 2) Compute per-domain cohort summaries and paired differences ----------
results <- list()

for (domain in names(domain_cohort_map)) {
  entry <- domain_cohort_map[[domain]]
  label <- entry$label
  is_currency <- entry$is_currency
  
  # cohort summaries (mean and percentile 95% CI from boot.ci(..., type="perc"))
  cohort_summary <- map_dfr(c("Source","OMOP_standard","OMOP_source"), function(cn) {
    boot_obj <- boot_store[[domain]][[cn]]          # the boot object returned by run_boot()
    vec <- t_store[[domain]][[cn]]                  # bootstrap replicate vector (for fallback)
    
    # try to get percentile CI via boot.ci; if not available, fallback to quantile(vec,...)
    boot_ci <- tryCatch(
      boot::boot.ci(boot_obj, type = "perc"),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    if (!is.null(boot_ci) && !is.null(boot_ci$perc)) {
      # boot.ci$perc format: vector with (conf level, ?, lower, upper, ?)
      ci_lower <- boot_ci$perc[4]
      ci_upper <- boot_ci$perc[5]
    } else {
      ci <- quantile(vec, c(0.025, 0.975), na.rm = TRUE)
      ci_lower <- ci[1]; ci_upper <- ci[2]
    }
    
    # match the second script: use mean(boot_result$t) as "bootstrap mean"
    mean_est <- mean(vec, na.rm = TRUE)
    
    tibble(cohort = cn, mean = mean_est, lower = ci_lower, upper = ci_upper)
  })
  
  # paired diffs draw-by-draw (leave as before)
  diffs <- list()
  for (cmp in comparisons) {
    L <- cmp[1]; Rgt <- cmp[2]
    lv <- t_store[[domain]][[L]]; rv <- t_store[[domain]][[Rgt]]
    if (length(lv) != length(rv)) stop("R mismatch for domain ", domain)
    diffs[[ paste0(L,"-",Rgt) ]] <- lv - rv
  }
  
  # format cohort strings and diff summaries
  cohort_vals <- cohort_summary %>%
    mutate(formatted = if (is_currency) {
      sprintf("%s\n(%s, %s)",
              format_currency(mean), format_currency(lower), format_currency(upper))
    } else {
      sprintf("%0.2f\n(%0.2f, %0.2f)", mean, lower, upper)
    })
  
  diff_summaries <- map(names(diffs), function(nm) {
    v <- diffs[[nm]]
    mn <- mean(v); ci <- quantile(v, c(0.025, 0.975))
    formatted <- if (is_currency) {
      sprintf("%s\n(%s, %s)", format_currency(mn), format_currency(ci[1]), format_currency(ci[2]))
    } else {
      sprintf("%0.2f\n(%0.2f, %0.2f)", mn, ci[1], ci[2])
    }
    list(name = nm, mean = mn, lower = ci[1], upper = ci[2], formatted = formatted, raw = v)
  }) %>% set_names(names(diffs))
  
  results[[domain]] <- list(label = label, cohort_table = cohort_vals, diffs = diff_summaries)
}

# ---------- 3) Build final summary table (rows per domain) ----------
rows <- map(results, function(r) {
  vals <- r$cohort_table
  tibble(
    measure = r$label,
    Source = vals$formatted[vals$cohort == "Source"],
    OMOP_Standard = vals$formatted[vals$cohort == "OMOP_standard"],
    OMOP_Source = vals$formatted[vals$cohort == "OMOP_source"],
    Diff_a_minus_b = r$diffs[["Source-OMOP_standard"]]$formatted,
    Diff_a_minus_c = r$diffs[["Source-OMOP_source"]]$formatted
  )
}) %>% bind_rows() %>%
  mutate(type = "Costs") %>%
  select(type, measure, Source, OMOP_Standard, OMOP_Source, Diff_a_minus_b, Diff_a_minus_c)

print(rows)
write_csv(rows, file.path(out_dir, "summary_table_per_domain_per_cohort.csv"))
message("Done. Artifacts in: ", out_dir)

results_costs <- results

# at the end of the costs build script:
domain_cohort_map_costs <- domain_cohort_map
