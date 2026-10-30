# bootstrap_per_domain_counts_per_cohort.R
library(boot)
library(dplyr)
library(purrr)
library(readr)

# -------- import -----------

all_patients_all_encounters_counts_source <- all_patients_all_encounters_counts
cohort_hospital_all_counts_source <- cohort_hospital_all %>% select(patid, total_spells)
cohort_outpatient_all_counts_source <- cohort_outpatient_all %>% select(patid, total_op)
cohort_emergency_all_counts_source <- cohort_emergency_all %>% select(patid, total_ae)

all_patients_all_encounters_counts_omop_standard <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/all_patients_all_encounters_omop_standard.csv")
cohort_hospital_all_omop_standard <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_hospital_all_omop_standard.csv")
cohort_outpatient_all_omop_standard <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_outpatient_all_omop_standard.csv")
cohort_emergency_all_omop_standard <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_emergency_all_omop_standard.csv")

all_patients_all_encounters_counts_omop_source <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/all_patients_all_encounters_omop_source.csv")
cohort_hospital_all_omop_source <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_hospital_all_omop_source.csv")
cohort_outpatient_all_omop_source <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_outpatient_all_omop_source.csv")
cohort_emergency_all_omop_source <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_emergency_all_omop_source.csv")

# ---------- helper to build per-patient total HCRU ----------
# total HCRU = primary care + inpatient + outpatient + emergency
build_total_hcru_df <- function(primary, inpatient, outpatient, emergency, id_col) {
  
  primary2 <- primary %>%
    transmute(id = .data[[id_col]], primary_count = total_patient_encounters) %>%
    distinct(id, .keep_all = TRUE)
  
  inpatient2 <- inpatient %>%
    transmute(id = .data[[id_col]], inpatient_count = total_spells) %>%
    distinct(id, .keep_all = TRUE)
  
  outpatient2 <- outpatient %>%
    transmute(id = .data[[id_col]], outpatient_count = total_op) %>%
    distinct(id, .keep_all = TRUE)
  
  emergency2 <- emergency %>%
    transmute(id = .data[[id_col]], emergency_count = total_ae) %>%
    distinct(id, .keep_all = TRUE)
  
  primary2 %>%
    full_join(inpatient2, by = "id") %>%
    full_join(outpatient2, by = "id") %>%
    full_join(emergency2, by = "id") %>%
    mutate(
      across(c(primary_count, inpatient_count, outpatient_count, emergency_count), ~ coalesce(.x, 0)),
      total_hcru = primary_count + inpatient_count + outpatient_count + emergency_count
    ) %>%
    select(id, total_hcru)
}

# ---------- build total HCRU objects ----------
total_hcru_source <- build_total_hcru_df(
  primary    = all_patients_all_encounters_counts_source,
  inpatient  = cohort_hospital_all_counts_source,
  outpatient = cohort_outpatient_all_counts_source,
  emergency  = cohort_emergency_all_counts_source,
  id_col     = "patid"
)

total_hcru_omop_standard <- build_total_hcru_df(
  primary    = all_patients_all_encounters_counts_omop_standard,
  inpatient  = cohort_hospital_all_omop_standard,
  outpatient = cohort_outpatient_all_omop_standard,
  emergency  = cohort_emergency_all_omop_standard,
  id_col     = "subject_id"
)

total_hcru_omop_source <- build_total_hcru_df(
  primary    = all_patients_all_encounters_counts_omop_source,
  inpatient  = cohort_hospital_all_omop_source,
  outpatient = cohort_outpatient_all_omop_source,
  emergency  = cohort_emergency_all_omop_source,
  id_col     = "subject_id"
)

# ---------- CONFIG ----------
R <- 1000
seed <- 626
out_dir <- "CPRD Raw Analysis/Results/Final/Comparison/final_table_hcru"
dir.create(out_dir, showWarnings = FALSE)

# ---------- DOMAIN x COHORT MAPPING ----------
domain_cohort_map <- list(
  primary_care = list(
    Source = all_patients_all_encounters_counts_source,
    OMOP_standard = all_patients_all_encounters_counts_omop_standard,
    OMOP_source = all_patients_all_encounters_counts_omop_source,
    var = "total_patient_encounters",
    label = "Primary care",
    is_count = TRUE
  ),
  inpatient = list(
    Source = cohort_hospital_all_counts_source,
    OMOP_standard = cohort_hospital_all_omop_standard,
    OMOP_source = cohort_hospital_all_omop_source,
    var = "total_spells",
    label = "Inpatient",
    is_count = TRUE
  ),
  outpatient = list(
    Source = cohort_outpatient_all_counts_source,
    OMOP_standard = cohort_outpatient_all_omop_standard,
    OMOP_source = cohort_outpatient_all_omop_source,
    var = "total_op",
    label = "Outpatient",
    is_count = TRUE
  ),
  ae = list(
    Source = cohort_emergency_all_counts_source,
    OMOP_standard = cohort_emergency_all_omop_standard,
    OMOP_source = cohort_emergency_all_omop_source,
    var = "total_ae",
    label = "A&E",
    is_count = TRUE
  ),
  total_hcru = list(
    Source = total_hcru_source,
    OMOP_standard = total_hcru_omop_standard,
    OMOP_source = total_hcru_omop_source,
    var = "total_hcru",
    label = "Total HCRU",
    is_count = TRUE
  )
)

# comparisons (unpaired differences)
comparisons <- list(c("Source", "OMOP_standard"), c("Source", "OMOP_source"))

# ---------- helper: bootstrap wrapper ----------
run_boot <- function(df, varname, R = 1000, seed = 626) {
  if (!varname %in% names(df)) stop("variable not in df: ", varname)
  stat_fn <- function(d, i) mean(d[i, ][[varname]], na.rm = TRUE)
  set.seed(seed)
  b <- boot::boot(data = df, statistic = stat_fn, R = R)
  list(boot = b, t = as.numeric(b$t))
}

fmt_count <- function(mean, lo, hi) sprintf("%0.2f\n(%0.2f, %0.2f)", mean, lo, hi)

# ---------- 1) Run separate bootstraps for each domain & cohort ----------
boot_store <- list()
t_store <- list()

for (domain in names(domain_cohort_map)) {
  entry <- domain_cohort_map[[domain]]
  varname <- entry$var
  boot_store[[domain]] <- list()
  t_store[[domain]] <- list()
  
  for (cohort_name in c("Source", "OMOP_standard", "OMOP_source")) {
    message("Bootstrap (counts): domain=", domain, " cohort=", cohort_name, " var=", varname)
    df <- entry[[cohort_name]]
    out <- run_boot(df, varname = varname, R = R, seed = seed)
    boot_store[[domain]][[cohort_name]] <- out$boot
    t_store[[domain]][[cohort_name]] <- out$t
  }
}

# ---------- 2) Compute cohort summaries and paired differences ----------
results <- list()

for (domain in names(domain_cohort_map)) {
  entry <- domain_cohort_map[[domain]]
  label <- entry$label
  
  cohort_summary <- map_dfr(c("Source", "OMOP_standard", "OMOP_source"), function(cn) {
    vec <- t_store[[domain]][[cn]]
    ci <- quantile(vec, c(0.025, 0.975), na.rm = TRUE)
    tibble(cohort = cn, mean = mean(vec, na.rm = TRUE), lower = ci[1], upper = ci[2])
  })
  
  diffs <- list()
  for (cmp in comparisons) {
    L <- cmp[1]
    Rgt <- cmp[2]
    lv <- t_store[[domain]][[L]]
    rv <- t_store[[domain]][[Rgt]]
    if (length(lv) != length(rv)) stop("R mismatch for domain ", domain)
    diffs[[paste0(L, "-", Rgt)]] <- lv - rv
  }
  
  cohort_vals <- cohort_summary %>%
    mutate(formatted = fmt_count(mean, lower, upper)) %>%
    arrange(factor(cohort, levels = c("Source", "OMOP_standard", "OMOP_source")))
  
  diff_summaries <- map(names(diffs), function(nm) {
    v <- diffs[[nm]]
    mn <- mean(v, na.rm = TRUE)
    ci <- quantile(v, c(0.025, 0.975), na.rm = TRUE)
    list(
      name = nm,
      mean = mn,
      lower = ci[1],
      upper = ci[2],
      formatted = sprintf("%0.2f\n(%0.2f, %0.2f)", mn, ci[1], ci[2]),
      raw = v
    )
  }) %>% set_names(names(diffs))
  
  results[[domain]] <- list(
    label = label,
    cohort_table = cohort_vals,
    diffs = diff_summaries
  )
}

# ---------- 3) Build final summary table ----------
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
}) %>%
  bind_rows() %>%
  mutate(type = "HCRU") %>%
  select(type, measure, Source, OMOP_Standard, OMOP_Source, Diff_a_minus_b, Diff_a_minus_c)

print(rows)
write_csv(rows, file.path(out_dir, "summary_table_counts_per_domain_per_cohort.csv"))
message("Done. Artifacts in: ", out_dir)

results_hcru <- results

# at the end of the HCRU build script:
domain_cohort_map_hcru <- domain_cohort_map