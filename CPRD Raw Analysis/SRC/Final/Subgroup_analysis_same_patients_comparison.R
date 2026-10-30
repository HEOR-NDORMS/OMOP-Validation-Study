library(dplyr)
library(purrr)
library(readr)
library(scales)
library(ggplot2)
library(boot)

# ============================================================
##  1) Regenerate results for only the subgroupd of patients with ID in common between source and omop
##  we use a genuinely paired bootstrap now, not the independent-resampling version

analysis_maps <- list(
  Costs = domain_cohort_map_costs,
  HCRU  = domain_cohort_map_hcru
)

out_dir <- "CPRD Raw Analysis/Results/Final/Comparison/final_table_matched_subgroup"
dir.create(out_dir, showWarnings = FALSE)

format_currency <- function(x) scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 0.01)(x)
fmt_val <- function(x, is_currency) if (is_currency) format_currency(x) else sprintf("%0.2f", x)
fmt_ci  <- function(mean, lower, upper, is_currency) sprintf("%s\n(%s, %s)", fmt_val(mean, is_currency), fmt_val(lower, is_currency), fmt_val(upper, is_currency))

# ---------- helper: build the 3-way matched (Source + OMOP-standard + OMOP-source) dataset ----------
build_three_way_matched_df <- function(entry, varname) {
  source_id_col <- if ("id" %in% names(entry$Source))        "id" else "patid"
  std_id_col    <- if ("id" %in% names(entry$OMOP_standard)) "id" else "subject_id"
  src_id_col    <- if ("id" %in% names(entry$OMOP_source))   "id" else "subject_id"
  
  s_df <- entry$Source        %>% rename(patid = all_of(source_id_col), source_value = all_of(varname)) %>% select(patid, source_value)
  b_df <- entry$OMOP_standard %>% rename(patid = all_of(std_id_col),    std_value    = all_of(varname)) %>% select(patid, std_value)
  c_df <- entry$OMOP_source   %>% rename(patid = all_of(src_id_col),    src_value    = all_of(varname)) %>% select(patid, src_value)
  
  s_df %>%
    inner_join(b_df, by = "patid") %>%
    inner_join(c_df, by = "patid")
  # no NA filter -- keep every patient present in all three ID systems,
  # even if their cost/count is NA in one or more cohorts. na.rm handles
  # that inside the mean, same as the original (non-subgroup) table.
}

paired_bootstrap_stat <- function(d, i) {
  d_sample <- d[i, ]
  mean_source <- mean(d_sample$source_value, na.rm = TRUE)
  mean_std    <- mean(d_sample$std_value,    na.rm = TRUE)
  mean_src    <- mean(d_sample$src_value,    na.rm = TRUE)
  c(
    mean_source    = mean_source,
    mean_std       = mean_std,
    mean_src       = mean_src,
    diff_a_minus_b = mean_source - mean_std,
    diff_a_minus_c = mean_source - mean_src
  )
}# ---------- run for every domain, both analysis types ----------
matched_results <- list()

for (analysis_type in names(analysis_maps)) {
  domain_map <- analysis_maps[[analysis_type]]
  matched_results[[analysis_type]] <- list()
  
  for (domain in names(domain_map)) {
    entry   <- domain_map[[domain]]
    varname <- entry$var
    message("Matched-subgroup bootstrap: ", analysis_type, " - ", domain)
    matched_results[[analysis_type]][[domain]] <- run_matched_subgroup(entry, varname, R = 1000, seed = 626)
  }
}

# ---------- build the final table, same shape as the original, N embedded in the measure label ----------
build_rows <- function(analysis_type) {
  domain_map <- analysis_maps[[analysis_type]]
  
  map_dfr(names(matched_results[[analysis_type]]), function(domain) {
    entry       <- domain_map[[domain]]
    is_currency <- isTRUE(entry$is_currency)
    r <- matched_results[[analysis_type]][[domain]]
    s <- r$stats
    get_stat <- function(nm) s %>% filter(statistic == nm)
    
    ms  <- get_stat("mean_source")
    mb  <- get_stat("mean_std")
    mc  <- get_stat("mean_src")
    dab <- get_stat("diff_a_minus_b")
    dac <- get_stat("diff_a_minus_c")
    
    tibble(
      measure = sprintf("%s (N=%s in common)", entry$label, format(r$n, big.mark = ",")),
      Source = fmt_ci(ms$mean, ms$lower, ms$upper, is_currency),
      OMOP_Standard = fmt_ci(mb$mean, mb$lower, mb$upper, is_currency),
      OMOP_Source = fmt_ci(mc$mean, mc$lower, mc$upper, is_currency),
      Diff_a_minus_b = fmt_ci(dab$mean, dab$lower, dab$upper, is_currency),
      Diff_a_minus_c = fmt_ci(dac$mean, dac$lower, dac$upper, is_currency)
    )
  })
}

rows_matched <- bind_rows(build_rows("HCRU"), build_rows("Costs"))

print(rows_matched, n = Inf)
write_csv(rows_matched, file.path(out_dir, "matched_subgroup_summary.csv"))
message("Done. Artifacts in: ", out_dir)

results_matched_subgroup <- matched_results

# ============================================================
# 2) Decomposing patients for inpatient cost
library(dplyr)
library(purrr)
library(readr)
library(scales)
library(boot)

format_currency <- function(x) scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 0.01)(x)
fmt_ci <- function(mean, lower, upper) sprintf("%s\n(%s, %s)", format_currency(mean), format_currency(lower), format_currency(upper))

out_dir <- "CPRD Raw Analysis/Results/Final/Comparison/final_table_decomposition"
dir.create(out_dir, showWarnings = FALSE)

# ============================================================
# Full 3-group inpatient decomposition, on the common (matched)
# cohort, always using Source CPRD-HES cost as the reference:
#   1. Not costed at all   -- OMOP captured £0 of known activity
#   2. Partially costed    -- some hospitalisations costed, some not
#   3. Fully costed        -- everything costed; residual gap here
#                              is purely a lower-tariff/HRG effect
# Run twice: grouping defined by OMOP-standard, then by OMOP-source.
# NOTE: "Not costed at all" shows OMOP's cost as £0 -- a deliberate,
# illustrative convention for THIS table only, distinct from the NA
# used in the primary cost-estimation tables (which exclude rather
# than zero-impute, to avoid the zero-imputation bias fixed earlier
# this session).
# ============================================================

# ---------- 1) build the richer matched dataset: Source + both OMOP variants,
# keeping spell counts so we can classify costing completeness ----------
build_inpatient_richer_matched_df <- function() {
  source_df <- cohort_hospital_all_costs %>%
    transmute(patid, source_cost = total_in_cost, source_spells = total_spells, source_uncosted = n_uncosted_spells)
  
  std_df <- cohort_hospital_all_cost_omop_standard %>%
    transmute(patid = subject_id, std_cost = total_in_cost, std_spells = total_spells, std_uncosted = n_uncosted_spells)
  
  src_df <- cohort_hospital_all_cost_omop_source %>%
    transmute(patid = subject_id, src_cost = total_in_cost, src_spells = total_spells, src_uncosted = n_uncosted_spells)
  
  source_df %>%
    inner_join(std_df, by = "patid") %>%
    inner_join(src_df, by = "patid")
}

matched_rich <- build_inpatient_richer_matched_df() %>%
  mutate(
    std_costed_spells = std_spells - coalesce(std_uncosted, 0L),
    src_costed_spells = src_spells - coalesce(src_uncosted, 0L),
    std_status = case_when(
      std_spells == 0                 ~ "Non-user",
      std_costed_spells == 0          ~ "Not costed at all",
      std_costed_spells == std_spells ~ "Fully costed",
      TRUE                             ~ "Partially costed"
    ),
    src_status = case_when(
      src_spells == 0                 ~ "Non-user",
      src_costed_spells == 0          ~ "Not costed at all",
      src_costed_spells == src_spells ~ "Fully costed",
      TRUE                             ~ "Partially costed"
    )
  )

# ---------- 2) bootstrap helper: Source vs OMOP gap within one subgroup ----------
subgroup_gap_stat <- function(d, i) {
  d_sample <- d[i, ]
  mean_source <- mean(d_sample$source_cost, na.rm = TRUE)
  mean_omop   <- mean(d_sample$omop_cost,   na.rm = TRUE)
  c(mean_source = mean_source, mean_omop = mean_omop, gap = mean_source - mean_omop)
}

run_subgroup_gap <- function(data, status_col, omop_cost_col, subgroup_value, R = 1000, seed = 626) {
  d <- data %>%
    filter(.data[[status_col]] == subgroup_value) %>%
    transmute(
      source_cost = source_cost,
      omop_cost = if (subgroup_value == "Not costed at all") {
        coalesce(.data[[omop_cost_col]], 0)   # Option A: illustrative £0, this table only
      } else {
        .data[[omop_cost_col]]
      }
    )
  
  n <- nrow(d)
  set.seed(seed)
  boot_result <- boot::boot(data = d, statistic = subgroup_gap_stat, R = R)
  
  stat_names <- names(boot_result$t0)
  stats <- map_dfr(seq_along(stat_names), function(idx) {
    vec <- boot_result$t[, idx]
    ci  <- quantile(vec, c(0.025, 0.975))
    tibble(statistic = stat_names[idx], mean = mean(vec), lower = ci[1], upper = ci[2])
  })
  
  list(n = n, stats = stats)
}

# ---------- 3) run for both OMOP variants x all three subgroups ----------
variants <- list(
  OMOP_standard = list(status_col = "std_status", omop_cost_col = "std_cost"),
  OMOP_source   = list(status_col = "src_status", omop_cost_col = "src_cost")
)
subgroups <- c("Not costed at all", "Partially costed", "Fully costed")

subgroup_results <- list()
for (variant_name in names(variants)) {
  v <- variants[[variant_name]]
  subgroup_results[[variant_name]] <- list()
  for (sg in subgroups) {
    message("Subgroup gap: ", variant_name, " - ", sg)
    subgroup_results[[variant_name]][[sg]] <- run_subgroup_gap(
      matched_rich, v$status_col, v$omop_cost_col, sg, R = 1000, seed = 626
    )
  }
}

# ---------- 4) build the summary table ----------
subgroup_rows <- map_dfr(names(subgroup_results), function(variant_name) {
  map_dfr(subgroups, function(sg) {
    r <- subgroup_results[[variant_name]][[sg]]
    s <- r$stats
    get_stat <- function(nm) s %>% filter(statistic == nm)
    
    ms <- get_stat("mean_source")
    mo <- get_stat("mean_omop")
    gp <- get_stat("gap")
    
    tibble(
      comparison  = paste0("Source vs ", variant_name),
      subgroup    = sg,
      n           = r$n,
      mean_source = fmt_ci(ms$mean, ms$lower, ms$upper),
      mean_omop   = fmt_ci(mo$mean, mo$lower, mo$upper),
      gap         = fmt_ci(gp$mean, gp$lower, gp$upper),
      pct_gap     = sprintf("%.1f%%", 100 * gp$mean / ms$mean)
    )
  }) %>% mutate(subgroup = factor(subgroup, levels = subgroups)) %>% arrange(subgroup)
})

print(subgroup_rows, n = Inf, width = Inf)
write_csv(subgroup_rows, file.path(out_dir, "inpatient_3group_decomposition.csv"))
message("Done. Artifacts in: ", out_dir)

results_3group_decomposition <- subgroup_results

# ============================================================
# 3) Bland-Altman: Source vs OMOP-standard / OMOP-source
# Runs for BOTH Costs and HCRU domain_cohort_maps in one script.
# Only valid on patients present in BOTH pipelines (patient-level
# pairing on patid == subject_id).
#
# NOTE: normality of the paired differences was checked (skewness/
# kurtosis + Q-Q plots) and clearly violated for these domains. The
# EMPIRICAL (nonparametric) Limits of Agreement -- the observed
# 2.5th/97.5th percentile of the differences, no distributional
# assumption -- are therefore the PRIMARY reported result. The
# parametric LoA (bias +/- 1.96*SD, which assumes normal differences)
# is kept only as a secondary reference column, since it is not
# reliable under the confirmed non-normality. Bias itself is
# unaffected by this -- it's a plain mean, protected by the CLT at
# this sample size regardless of the differences' shape.
# ============================================================

analysis_maps <- list(
  Costs = domain_cohort_map_costs,
  HCRU  = domain_cohort_map_hcru
)

# ---------- CONFIG ----------
out_dir <- "CPRD Raw Analysis/Results/Final/Comparison/final_table_bland_altman"
dir.create(out_dir, showWarnings = FALSE)

format_currency <- function(x) scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 0.01)(x)
fmt_val <- function(x, is_currency) if (is_currency) format_currency(x) else sprintf("%0.2f", x)

# ---------- helper: build the matched (paired) dataset for one domain x comparison ----------
build_matched_df <- function(source_df, omop_df, varname, source_id_col, omop_id_col) {
  source_df %>%
    rename(patid = all_of(source_id_col), source_value = all_of(varname)) %>%
    select(patid, source_value) %>%
    inner_join(
      omop_df %>% rename(subject_id = all_of(omop_id_col), omop_value = all_of(varname)) %>%
        select(subject_id, omop_value),
      by = c("patid" = "subject_id")
    ) %>% 
    filter(!is.na(source_value), !is.na(omop_value))
}

# ---------- helper: Bland-Altman stats on one paired vector (bootstrap) ----------
# Reports empirical (nonparametric) LoA as primary, parametric as secondary reference.
bland_altman_stats <- function(source_value, omop_value, R = 1000, seed = 626) {
  df <- tibble(source_value = source_value, omop_value = omop_value)
  n  <- nrow(df)
  
  stat_fn <- function(d, i) {
    diff    <- d$omop_value[i] - d$source_value[i]
    bias    <- mean(diff)
    sd_diff <- sd(diff)
    emp     <- quantile(diff, c(0.025, 0.975))
    c(
      bias = bias,
      loa_lower_emp = emp[[1]],              # PRIMARY: empirical LoA, no normality assumption
      loa_upper_emp = emp[[2]],
      loa_lower_param = bias - 1.96 * sd_diff,  # secondary reference only
      loa_upper_param = bias + 1.96 * sd_diff
    )
  }
  
  set.seed(seed)
  boot_result <- boot::boot(data = df, statistic = stat_fn, R = R)
  
  bias_est            <- mean(boot_result$t[, 1])
  loa_lower_emp_est   <- mean(boot_result$t[, 2])
  loa_upper_emp_est   <- mean(boot_result$t[, 3])
  loa_lower_param_est <- mean(boot_result$t[, 4])
  loa_upper_param_est <- mean(boot_result$t[, 5])
  
  ci_bias            <- quantile(boot_result$t[, 1], c(0.025, 0.975))
  ci_loa_lower_emp   <- quantile(boot_result$t[, 2], c(0.025, 0.975))
  ci_loa_upper_emp   <- quantile(boot_result$t[, 3], c(0.025, 0.975))
  ci_loa_lower_param <- quantile(boot_result$t[, 4], c(0.025, 0.975))
  ci_loa_upper_param <- quantile(boot_result$t[, 5], c(0.025, 0.975))
  
  tibble(
    n = n,
    bias = bias_est, bias_lower = ci_bias[1], bias_upper = ci_bias[2],
    loa_lower_emp = loa_lower_emp_est, loa_lower_emp_ci_low = ci_loa_lower_emp[1], loa_lower_emp_ci_high = ci_loa_lower_emp[2],
    loa_upper_emp = loa_upper_emp_est, loa_upper_emp_ci_low = ci_loa_upper_emp[1], loa_upper_emp_ci_high = ci_loa_upper_emp[2],
    loa_lower_param = loa_lower_param_est, loa_lower_param_ci_low = ci_loa_lower_param[1], loa_lower_param_ci_high = ci_loa_lower_param[2],
    loa_upper_param = loa_upper_param_est, loa_upper_param_ci_low = ci_loa_upper_param[1], loa_upper_param_ci_high = ci_loa_upper_param[2]
  )
}

# ---------- helper: Bland-Altman plot -- empirical LoA is the primary (solid, black) line;

plot_bland_altman <- function(df, s, title) {
  df %>%
    mutate(diff = omop_value - source_value, avg = (omop_value + source_value) / 2) %>%
    ggplot(aes(x = avg, y = diff)) +
    geom_point(alpha = 0.3, size = 0.8) +
    geom_hline(yintercept = s$bias, colour = "black") +
    geom_hline(yintercept = s$loa_lower_emp, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = s$loa_upper_emp, colour = "red", linetype = "dashed") +
    labs(title = title, x = "Mean of Source and OMOP", y = "Difference (OMOP - Source)") +
    theme_minimal(base_size = 11)
}
# ---------- run Bland-Altman for every analysis type x domain x comparison ----------
comparisons <- c("OMOP_standard", "OMOP_source")
ba_results <- list()

for (analysis_type in names(analysis_maps)) {
  domain_map <- analysis_maps[[analysis_type]]
  ba_results[[analysis_type]] <- list()
  
  for (domain in names(domain_map)) {
    entry       <- domain_map[[domain]]
    varname     <- entry$var
    is_currency <- isTRUE(entry$is_currency)
    source_df   <- entry$Source
    
    source_id_col <- if ("id" %in% names(entry$Source))        "id" else "patid"
    omop_id_col   <- if ("id" %in% names(entry$OMOP_standard))  "id" else "subject_id"
    
    ba_results[[analysis_type]][[domain]] <- list()
    
    for (cmp_name in comparisons) {
      omop_df <- entry[[cmp_name]]
      matched <- build_matched_df(source_df, omop_df, varname, source_id_col, omop_id_col)
      s <- bland_altman_stats(matched$source_value, matched$omop_value, R = 1000, seed = 626)
      ba_results[[analysis_type]][[domain]][[cmp_name]] <- list(stats = s, data = matched)
      
      p <- plot_bland_altman(matched, s, title = paste0(analysis_type, " - ", entry$label, ": Source vs ", cmp_name))
      ggsave(file.path(out_dir, paste0("bland_altman_", analysis_type, "_", domain, "_", cmp_name, ".png")),
             plot = p, width = 7, height = 5, dpi = 300)
    }
  }
}

# ---------- build combined summary table -- empirical LoA first (primary), parametric last (reference) ----------
rows <- map(names(ba_results), function(analysis_type) {
  domain_map <- analysis_maps[[analysis_type]]
  
  map(names(ba_results[[analysis_type]]), function(domain) {
    entry       <- domain_map[[domain]]
    is_currency <- isTRUE(entry$is_currency)
    
    map_dfr(comparisons, function(cmp_name) {
      s <- ba_results[[analysis_type]][[domain]][[cmp_name]]$stats
      tibble(
        type = analysis_type,
        measure = entry$label,
        comparison = paste0("Source vs ", cmp_name),
        n_matched = s$n,
        bias = sprintf("%s\n(%s, %s)",
                       fmt_val(s$bias, is_currency), fmt_val(s$bias_lower, is_currency), fmt_val(s$bias_upper, is_currency)),
        limits_of_agreement = sprintf("%s to %s",
                                      fmt_val(s$loa_lower_emp, is_currency), fmt_val(s$loa_upper_emp, is_currency)),
        limits_of_agreement_parametric_reference = sprintf("%s to %s",
                                                           fmt_val(s$loa_lower_param, is_currency), fmt_val(s$loa_upper_param, is_currency))
      )
    })
  }) %>% bind_rows()
}) %>% bind_rows() %>%
  select(type, measure, comparison, n_matched, bias, limits_of_agreement, limits_of_agreement_parametric_reference)

print(rows, n = Inf)
write_csv(rows, file.path(out_dir, "bland_altman_summary_costs_and_hcru.csv"))
message("Done. Artifacts in: ", out_dir)

results_bland_altman <- ba_results


library(dplyr)
library(purrr)
library(ggplot2)

# ---------- build one long dataframe per analysis type, ready for faceting ----------
domain_order_costs <- c("Primary care", "Inpatient", "Outpatient", "A&E", "Total costs")
domain_order_hcru  <- c("Primary care", "Inpatient", "Outpatient", "A&E", "Total HCRU")

# ---------- build one long dataframe per analysis type, ready for faceting ----------
# Visual-only trim: points beyond the 1st/99th percentile of avg or diff WITHIN
# each panel are dropped from view, so one extreme patient doesn't stretch the
# axis for everyone. The bias/LoA reported in the table use the full data and are
# untouched by this.
#
# Degenerate case: when almost all differences in a panel are identical (e.g.
# primary care, Source vs OMOP-source, where >99% of differences are exactly 0),
# the 1st and 99th percentiles are equal and the filter would keep only those
# identical points, hiding exactly the few patients that drive the bias. When
# that happens we keep the full range for that variable instead of trimming.
#
# n_trimmed / pct_trimmed record how many points are hidden per panel, so the
# figure caption can quote it.

build_combined_plot_df <- function(analysis_type, trim_pct = 0.01) {
  domain_map <- analysis_maps[[analysis_type]]
  
  map_dfr(names(ba_results[[analysis_type]]), function(domain) {
    map_dfr(comparisons, function(cmp_name) {
      
      s <- ba_results[[analysis_type]][[domain]][[cmp_name]]$stats
      d <- ba_results[[analysis_type]][[domain]][[cmp_name]]$data %>%
        mutate(diff = omop_value - source_value,
               avg  = (omop_value + source_value) / 2)
      
      avg_range  <- quantile(d$avg,  c(trim_pct, 1 - trim_pct), na.rm = TRUE)
      diff_range <- quantile(d$diff, c(trim_pct, 1 - trim_pct), na.rm = TRUE)
      
      full_avg  <- range(d$avg,  na.rm = TRUE)
      full_diff <- range(d$diff, na.rm = TRUE)
      
      # if the trim would keep only a sliver of the real spread, it is hiding the
      # very points that drive the bias, so keep everything in that panel
      if (diff(diff_range) < 0.01 * diff(full_diff)) diff_range <- full_diff
      if (diff(avg_range)  < 0.01 * diff(full_avg))  avg_range  <- full_avg      
      d_trim <- d %>%
        filter(avg  >= avg_range[1],  avg  <= avg_range[2],
               diff >= diff_range[1], diff <= diff_range[2])
      
      d_trim %>%
        mutate(
          domain      = domain_map[[domain]]$label,
          comparison  = if_else(cmp_name == "OMOP_standard",
                                "Source vs OMOP-standard", "Source vs OMOP-source"),
          bias        = s$bias,
          loa_lower   = s$loa_lower_emp,
          loa_upper   = s$loa_upper_emp,
          n_total     = nrow(d),
          n_trimmed   = nrow(d) - nrow(d_trim),
          pct_trimmed = 100 * (nrow(d) - nrow(d_trim)) / nrow(d)
        )
    })
  })
}

plot_grid_ba <- function(analysis_type, domain_order, y_lab, expand_y = c(-1, 1)) {
  plot_data <- build_combined_plot_df(analysis_type) %>%
    mutate(
      domain = factor(domain, levels = domain_order),
      panel  = factor(paste0(domain, " — ", comparison),
                      levels = as.vector(t(outer(domain_order,
                                                 c("Source vs OMOP-source", "Source vs OMOP-standard"),
                                                 paste, sep = " — "))))
    )
  
  p <- ggplot(plot_data, aes(x = avg, y = diff)) +
    geom_point(alpha = 0.15, size = 0.4) +
    geom_hline(aes(yintercept = bias), colour = "black") +
    geom_hline(aes(yintercept = loa_lower), colour = "red", linetype = "dashed") +
    geom_hline(aes(yintercept = loa_upper), colour = "red", linetype = "dashed") +
    facet_wrap(~ panel, ncol = 2, scales = "free") +
    labs(x = "Mean of Source and OMOP", y = y_lab,
         caption = "Outermost 1% of points per domain (by mean or difference) trimmed from view for readability;\nfull data used in reported bias/LoA.") +
    theme_minimal(base_size = 11) +
    theme(strip.background = element_rect(fill = "grey95", colour = NA),
          plot.caption = element_text(size = 8, colour = "grey40"))
  
  # widens only the panels whose own range is narrower than this;
  # panels with a wider range are unaffected
  if (!is.null(expand_y)) p <- p + expand_limits(y = expand_y)
  p
}

p_costs_grid <- plot_grid_ba("Costs", domain_order_costs, "Difference (OMOP - Source), £", expand_y = NULL)
p_hcru_grid  <- plot_grid_ba("HCRU",  domain_order_hcru,  "Difference (OMOP - Source)",   expand_y = NULL)

ggsave(file.path(out_dir, "bland_altman_grid_costs.png"), p_costs_grid, width = 11, height = 14, dpi = 300)
ggsave(file.path(out_dir, "bland_altman_grid_hcru.png"),  p_hcru_grid,  width = 11, height = 14, dpi = 300)
