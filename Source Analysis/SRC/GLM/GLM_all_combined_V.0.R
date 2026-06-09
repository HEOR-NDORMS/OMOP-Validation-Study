# ============================================================
# Compare CPRD raw vs OMOP-source vs OMOP-standard
# using only the exported Excel result files.
#
# Outputs:
# 1) Comparison tables for ORs, cost ratios, and AMEs
# 2) Forest plots with the three strategies shown in different colours
# 3) Optional concordance scatter plots for point estimates
#
# Assumptions:
# - Each workbook contains the publication-ready sheets:
#   paper_or_table, paper_cr_table, paper_ame_table
# - The first two tables contain columns:
#   setting, predictor, estimate_ci, p_value
# - The AME table contains columns:
#   predictor, estimate_ci, p_value
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(patchwork)
library(readr)
library(writexl)

# -----------------------------
# File paths
# -----------------------------
# Update these paths if needed.
path_cprd_raw      <- "CPRD Raw Analysis/Results/Final/GLM/glm_cost_results_tables_cprd_source.xlsx"
path_omop_source   <- "CPRD Raw Analysis/Results/Final/GLM/glm_cost_results_tables_omop_source.xlsx"
path_omop_standard <- "CPRD Raw Analysis/Results/Final/GLM/glm_cost_results_tables_omop_standard.xlsx"

# Output directory
out_dir <- "glm_comparison_outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# -----------------------------
# Helpers
# -----------------------------

# Parse values like: "0.37 (0.33, 0.42)" or "-£1,234 ( -£2,000, +£10 )"
parse_point_estimate <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "£", "")
  x <- str_replace_all(x, ",", "")
  x <- str_replace_all(x, "<", "")
  x <- str_trim(x)
  # Extract the first numeric value, allowing sign and decimals
  as.numeric(str_extract(x, "[-+]?[0-9]*\\.?[0-9]+"))
}

standardise_name <- function(x) {
  x %>%
    str_replace_all("–", "-") %>%
    str_replace_all("[[:space:]]+", " ") %>%
    str_trim()
}

add_strategy <- function(df, strategy) {
  df %>% mutate(strategy = strategy)
}

# Order settings exactly as requested
setting_levels_or_cr <- c("Total costs", "Primary care", "Inpatient", "Outpatient", "Emergency")
setting_levels_or   <- c("Inpatient", "Outpatient", "Emergency")

strategy_levels <- c(
  "Source CPRD-HES",
  "OMOP-source",
  "OMOP-standard"
)

strategy_cols <- c(
  "Source CPRD-HES" = "#D62728",     # red
  "OMOP-source" = "#1F77B4", # blue
  "OMOP-standard" = "#2CA02C"         # green
)

# Small vertical offsets so the three strategies do not sit on top of each other.
strategy_shifts <- c(
  "CPRD raw" = -0.22,
  "OMOP-source" = 0.00,
  "OMOP-standard" = 0.22
)

# Convert the exported tables into a comparison-ready format
# by parsing the point estimate out of estimate_ci.
prepare_or_cr <- function(df) {
  df %>%
    mutate(
      setting = standardise_name(setting),
      predictor = standardise_name(predictor),
      estimate = parse_point_estimate(estimate_ci),
      p_value = as.character(p_value)
    )
}

prepare_ame <- function(df) {
  df %>%
    mutate(
      predictor = standardise_name(predictor),
      estimate = parse_point_estimate(estimate_ci),
      p_value = as.character(p_value)
    )
}

# Create a comparison table with one row per setting/predictor and one column per strategy
make_comparison_table <- function(cprd, source, standard, key_cols) {
  bind_rows(
    add_strategy(cprd, "CPRD raw"),
    add_strategy(source, "OMOP-source"),
    add_strategy(standard, "OMOP-standard")
  ) %>%
    mutate(across(all_of(key_cols), as.character)) %>%
    dplyr::select(all_of(key_cols), strategy, estimate_ci, p_value, estimate) %>%
    arrange(across(all_of(key_cols))) %>%
    pivot_wider(
      names_from = strategy,
      values_from = c(estimate_ci, p_value, estimate)
    )
}

# Generic long-to-forest plot helper
make_forest_plot <- function(df_long, title, subtitle, xlab, facet_var, facet_levels, x_limits = NULL) {
  df_long <- df_long %>%
    mutate(
      strategy = factor(strategy, levels = strategy_levels),
      setting = factor(setting, levels = facet_levels)
    )
  
  if (!is.null(x_limits)) {
    df_long <- df_long %>% filter(is.finite(estimate))
  }
  
  p <- ggplot(df_long, aes(x = estimate, y = predictor, colour = strategy)) +
    geom_vline(xintercept = 1, linewidth = 0.4, colour = "grey55") +
    geom_point(position = position_dodge(width = 0.65), size = 2.2) +
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high),
      colour = "black",
      height = 0.10,
      linewidth = 0.35
    ) +
    facet_wrap(stats::as.formula(paste("~", facet_var)), scales = "free_y") +
    scale_colour_manual(values = strategy_cols) +
    labs(title = title, subtitle = subtitle, x = xlab, y = NULL, colour = NULL) +
    theme_classic(base_size = 11) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey95", colour = NA),
      strip.text = element_text(face = "bold")
    )
  
  if (!is.null(x_limits)) {
    p <- p + coord_cartesian(xlim = x_limits)
  }
  
  p
}

# ------------------------------------------------------------
# Read the three workbooks
# ------------------------------------------------------------

cprd_raw_or <- readxl::read_excel(path_cprd_raw, sheet = "paper_or_table") %>% prepare_or_cr()
cprd_raw_cr <- readxl::read_excel(path_cprd_raw, sheet = "paper_cr_table") %>% prepare_or_cr()
cprd_raw_ame <- readxl::read_excel(path_cprd_raw, sheet = "paper_ame_table") %>% prepare_ame()

omop_source_or <- readxl::read_excel(path_omop_source, sheet = "paper_or_table") %>% prepare_or_cr()
omop_source_cr <- readxl::read_excel(path_omop_source, sheet = "paper_cr_table") %>% prepare_or_cr()
omop_source_ame <- readxl::read_excel(path_omop_source, sheet = "paper_ame_table") %>% prepare_ame()

omop_standard_or <- readxl::read_excel(path_omop_standard, sheet = "paper_or_table") %>% prepare_or_cr()
omop_standard_cr <- readxl::read_excel(path_omop_standard, sheet = "paper_cr_table") %>% prepare_or_cr()
omop_standard_ame <- readxl::read_excel(path_omop_standard, sheet = "paper_ame_table") %>% prepare_ame()

# ------------------------------------------------------------
# Build comparison tables
# ------------------------------------------------------------

# Table A: ORs, ordered by setting
or_comp <- bind_rows(
  add_strategy(cprd_raw_or, "CPRD raw"),
  add_strategy(omop_source_or, "OMOP-source"),
  add_strategy(omop_standard_or, "OMOP-standard")
) %>%
  mutate(
    setting = factor(setting, levels = setting_levels_or),
    predictor = factor(predictor, levels = unique(predictor))
  ) %>%
  arrange(setting, predictor, strategy) %>%
  dplyr::select(setting, predictor, strategy, estimate_ci, p_value) %>%
  pivot_wider(
    names_from = strategy,
    values_from = c(estimate_ci, p_value)
  )

# Table B: cost ratios, ordered by setting
cr_comp <- bind_rows(
  add_strategy(cprd_raw_cr, "CPRD raw"),
  add_strategy(omop_source_cr, "OMOP-source"),
  add_strategy(omop_standard_cr, "OMOP-standard")
) %>%
  mutate(
    setting = factor(setting, levels = setting_levels_or_cr),
    predictor = factor(predictor, levels = unique(predictor))
  ) %>%
  arrange(setting, predictor, strategy) %>%
  dplyr::select(setting, predictor, strategy, estimate_ci, p_value) %>%
  pivot_wider(
    names_from = strategy,
    values_from = c(estimate_ci, p_value)
  )

# Table C: AMEs, sorted by predictor (same order as in your figure)
ame_order <- c(
  "Age at fracture (per 10 years)",
  "Site: Vertebra vs Hip",
  "Site: NHNV vs Hip",
  "Race: Black vs White",
  "Race: Asian vs White",
  "Race: Missing vs White",
  "Cardiovascular disease (2y)",
  "Heart failure (2y)",
  "Rheumatoid arthritis (2y)",
  "Osteoporosis (2y)"
)

ame_comp <- bind_rows(
  add_strategy(cprd_raw_ame, "CPRD raw"),
  add_strategy(omop_source_ame, "OMOP-source"),
  add_strategy(omop_standard_ame, "OMOP-standard")
) %>%
  mutate(
    predictor = factor(predictor, levels = ame_order)
  ) %>%
  arrange(predictor, strategy) %>%
  dplyr::select(predictor, strategy, estimate_ci, p_value) %>%
  pivot_wider(
    names_from = strategy,
    values_from = c(estimate_ci, p_value)
  )

# Optional: add simple deltas versus CPRD raw for point estimates
add_deltas <- function(tbl, key_cols) {
  tbl %>%
    # The pivoted estimate columns created by pivot_wider will be named like estimate_CPRD raw
    mutate(
      delta_source = .data[["estimate_OMOP-source"]] - .data[["estimate_CPRD raw"]],
      delta_standard = .data[["estimate_OMOP-standard"]] - .data[["estimate_CPRD raw"]]
    )
}

# Note: deltas will only work if the estimate columns are numeric.
# Because the tables currently only contain estimate_ci and p_value after pivot_wider,
# we also create a numeric version for convenience.

make_numeric_comp <- function(df, key_cols) {
  bind_rows(
    add_strategy(df[[1]], "CPRD raw")
  )
}

# Build numeric versions for delta calculations
or_num <- bind_rows(
  add_strategy(cprd_raw_or, "CPRD raw"),
  add_strategy(omop_source_or, "OMOP-source"),
  add_strategy(omop_standard_or, "OMOP-standard")
) %>%
  mutate(setting = factor(setting, levels = setting_levels_or)) %>%
  dplyr::select(setting, predictor, strategy, estimate)

cr_num <- bind_rows(
  add_strategy(cprd_raw_cr, "CPRD raw"),
  add_strategy(omop_source_cr, "OMOP-source"),
  add_strategy(omop_standard_cr, "OMOP-standard")
) %>%
  mutate(setting = factor(setting, levels = setting_levels_or_cr)) %>%
  dplyr::select(setting, predictor, strategy, estimate)

ame_num <- bind_rows(
  add_strategy(cprd_raw_ame, "CPRD raw"),
  add_strategy(omop_source_ame, "OMOP-source"),
  add_strategy(omop_standard_ame, "OMOP-standard")
) %>%
  dplyr::select(predictor, strategy, estimate)

or_delta <- or_num %>%
  pivot_wider(names_from = strategy, values_from = estimate) %>%
  mutate(
    delta_source = `OMOP-source` - `CPRD raw`,
    delta_standard = `OMOP-standard` - `CPRD raw`
  )

cr_delta <- cr_num %>%
  pivot_wider(names_from = strategy, values_from = estimate) %>%
  mutate(
    delta_source = `OMOP-source` - `CPRD raw`,
    delta_standard = `OMOP-standard` - `CPRD raw`
  )

ame_delta <- ame_num %>%
  pivot_wider(names_from = strategy, values_from = estimate) %>%
  mutate(
    delta_source = `OMOP-source` - `CPRD raw`,
    delta_standard = `OMOP-standard` - `CPRD raw`
  )

# Save tables
write_xlsx(
  list(
    table_A_or_comparison = or_comp,
    table_A_or_deltas = or_delta,
    table_B_cr_comparison = cr_comp,
    table_B_cr_deltas = cr_delta,
    table_C_ame_comparison = ame_comp,
    table_C_ame_deltas = ame_delta
  ),
  path = file.path(out_dir, "glm_comparison_tables.xlsx")
)

# ------------------------------------------------------------
# Create plotting-ready data with confidence intervals
# ------------------------------------------------------------

extract_numeric_values <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[£,]", "")
  purrr::map(
    stringr::str_extract_all(x, "[-+]?[0-9]*\\.?[0-9]+"),
    ~ as.numeric(.x)
  )
}

parse_point_estimate <- function(x) {
  vals <- extract_numeric_values(x)
  purrr::map_dbl(vals, ~ if (length(.x) >= 1) .x[1] else NA_real_)
}

parse_ci_low <- function(x) {
  vals <- extract_numeric_values(x)
  purrr::map_dbl(vals, ~ if (length(.x) >= 2) .x[2] else NA_real_)
}

parse_ci_high <- function(x) {
  vals <- extract_numeric_values(x)
  purrr::map_dbl(vals, ~ if (length(.x) >= 3) .x[3] else NA_real_)
}

strategy_levels <- c("CPRD raw", "OMOP-source", "OMOP-standard")
strategy_cols <- c(
  "CPRD raw" = "#111111",
  "OMOP-source" = "#1f77b4",
  "OMOP-standard" = "#ff7f0e"
)

strategy_shifts <- c(
  "CPRD raw" = -0.22,
  "OMOP-source" = 0.00,
  "OMOP-standard" = 0.22
)

# Shared display labels for Figures 1 and 2
orcr_predictor_order <- c(
  "Age at fracture",
  "Site: Vertebra vs Hip",
  "Site: NHNV vs Hip",
  "Ethnicity: Black vs White",
  "Ethnicity: Asian vs White",
  "Ethnicity: Missing vs White",
  "Cardiovascular disease (2y)",
  "Heart failure (2y)",
  "Rheumatoid arthritis (2y)",
  "Osteoporosis (2y)"
)

# Display labels for Figure 3
ame_predictor_order <- c(
  "Age at fracture (per 10 years)",
  "Site: Vertebra vs Hip",
  "Site: NHNV vs Hip",
  "Race: Black vs White",
  "Race: Asian vs White",
  "Race: Missing vs White",
  "Cardiovascular disease (2y)",
  "Heart failure (2y)",
  "Rheumatoid arthritis (2y)",
  "Osteoporosis (2y)"
)

pretty_orcr_predictor <- function(x) {
  dplyr::recode(
    x,
    "Site: Vertebra" = "Site: Vertebra vs Hip",
    "Site: NHNV" = "Site: NHNV vs Hip",
    .default = x
  )
}

pretty_ame_predictor <- function(x) {
  dplyr::recode(
    x,
    "Site: Vertebra" = "Site: Vertebra vs Hip",
    "Site: NHNV" = "Site: NHNV vs Hip",
    .default = x
  )
}

# OR data
or_plot <- bind_rows(
  add_strategy(cprd_raw_or, "CPRD raw"),
  add_strategy(omop_source_or, "OMOP-source"),
  add_strategy(omop_standard_or, "OMOP-standard")
) %>%
  mutate(
    setting = factor(setting, levels = setting_levels_or),
    predictor_display = pretty_orcr_predictor(predictor),
    predictor_display = factor(predictor_display, levels = orcr_predictor_order),
    strategy = factor(strategy, levels = strategy_levels),
    estimate = parse_point_estimate(estimate_ci),
    conf.low = parse_ci_low(estimate_ci),
    conf.high = parse_ci_high(estimate_ci),
    y_base = as.integer(predictor_display),
    y_pos = y_base + strategy_shifts[as.character(strategy)]
  ) %>%
  filter(
    !is.na(predictor_display),
    !is.na(estimate),
    !is.na(conf.low),
    !is.na(conf.high),
    !is.na(y_pos)
  )

# CR data
cr_plot <- bind_rows(
  add_strategy(cprd_raw_cr, "CPRD raw"),
  add_strategy(omop_source_cr, "OMOP-source"),
  add_strategy(omop_standard_cr, "OMOP-standard")
) %>%
  mutate(
    setting = factor(setting, levels = setting_levels_or_cr),
    predictor_display = pretty_orcr_predictor(predictor),
    predictor_display = factor(predictor_display, levels = orcr_predictor_order),
    strategy = factor(strategy, levels = strategy_levels),
    estimate = parse_point_estimate(estimate_ci),
    conf.low = parse_ci_low(estimate_ci),
    conf.high = parse_ci_high(estimate_ci),
    y_base = as.integer(predictor_display),
    y_pos = y_base + strategy_shifts[as.character(strategy)]
  ) %>%
  filter(
    !is.na(predictor_display),
    !is.na(estimate),
    !is.na(conf.low),
    !is.na(conf.high),
    !is.na(y_pos)
  )

# -----------------------------
# AME plot data
# -----------------------------

strategy_shifts_ame <- c(
  "CPRD raw" = -0.22,
  "OMOP-source" = 0.00,
  "OMOP-standard" = 0.22
)

ame_plot <- bind_rows(
  add_strategy(cprd_raw_ame, "CPRD raw"),
  add_strategy(omop_source_ame, "OMOP-source"),
  add_strategy(omop_standard_ame, "OMOP-standard")
) %>%
  mutate(
    predictor_display = pretty_ame_predictor(predictor),
    predictor_display = factor(predictor_display, levels = ame_predictor_order),
    strategy = factor(strategy, levels = strategy_levels),
    estimate = parse_point_estimate(estimate_ci),
    conf.low = parse_ci_low(estimate_ci),
    conf.high = parse_ci_high(estimate_ci),
    y_base = as.integer(predictor_display),
    y_pos = y_base + strategy_shifts_ame[as.character(strategy)]
  ) %>%
  filter(
    !is.na(predictor_display),
    !is.na(estimate),
    !is.na(conf.low),
    !is.na(conf.high),
    !is.na(y_pos)
  )

# ------------------------------------------------------------
# Figure 3: AME plot (bars only, no dots)
# ------------------------------------------------------------

strategy_shifts_ame <- c(
  "CPRD raw" = -0.22,
  "OMOP-source" = 0.00,
  "OMOP-standard" = 0.22
)

pretty_ame_predictor <- function(x) {
  dplyr::recode(
    x,
    "Site: Vertebra" = "Site: Vertebra vs Hip",
    "Site: NHNV" = "Site: NHNV vs Hip",
    .default = x
  )
}

ame_predictor_order <- c(
  "Age at fracture (per 10 years)",
  "Site: Vertebra vs Hip",
  "Site: NHNV vs Hip",
  "Race: Black vs White",
  "Race: Asian vs White",
  "Race: Missing vs White",
  "Cardiovascular disease (2y)",
  "Heart failure (2y)",
  "Rheumatoid arthritis (2y)",
  "Osteoporosis (2y)"
)

ame_plot <- bind_rows(
  add_strategy(cprd_raw_ame, "CPRD raw"),
  add_strategy(omop_source_ame, "OMOP-source"),
  add_strategy(omop_standard_ame, "OMOP-standard")
) %>%
  mutate(
    predictor_display = pretty_ame_predictor(predictor),
    predictor_display = factor(predictor_display, levels = ame_predictor_order),
    strategy = factor(strategy, levels = strategy_levels),
    estimate = parse_point_estimate(estimate_ci),
    conf.low = parse_ci_low(estimate_ci),
    conf.high = parse_ci_high(estimate_ci),
    y_base = as.integer(predictor_display),
    y_pos = y_base + strategy_shifts_ame[as.character(strategy)]
  ) %>%
  filter(
    !is.na(predictor_display),
    !is.na(estimate),
    !is.na(conf.low),
    !is.na(conf.high),
    !is.na(y_pos)
  )

make_ame_plot <- function(df, title, subtitle, xlab) {
  n_items <- length(levels(df$predictor_display))
  
  ggplot(df) +
    geom_hline(
      yintercept = seq(1.5, n_items - 0.5, by = 1),
      colour = "grey92",
      linewidth = 0.30
    ) +
    geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.40) +
    geom_rect(
      aes(
        xmin = pmin(0, estimate),
        xmax = pmax(0, estimate),
        ymin = y_pos - 0.14,
        ymax = y_pos + 0.14,
        fill = strategy
      ),
      colour = NA,
      alpha = 0.95
    ) +
    geom_errorbarh(
      aes(
        y = y_pos,
        xmin = conf.low,
        xmax = conf.high
      ),
      colour = "black",
      height = 0.08,
      linewidth = 0.35
    ) +
    scale_fill_manual(values = strategy_cols) +
    scale_colour_manual(values = strategy_cols, guide = "none") +
    scale_y_continuous(
      breaks = seq_len(n_items),
      labels = levels(df$predictor_display),
      limits = c(0.5, n_items + 0.5),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_x_continuous(
      labels = scales::label_number(prefix = "£", big.mark = ","),
      breaks = scales::pretty_breaks(n = 5),
      expand = expansion(mult = c(0.03, 0.06))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = NULL,
      fill = NULL
    ) +
    theme_classic(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      axis.text.y = element_text(size = 8)
    )
}

fig3_compare <- make_ame_plot(
  ame_plot,
  title = "Figure 3. Average marginal effects on total annual health care costs",
  subtitle = "CPRD raw vs OMOP-source vs OMOP-standard",
  xlab = "Average marginal effect (£ per patient-year)"
)

ggsave(
  file.path(out_dir, "figure3_ame_comparison.png"),
  fig3_compare,
  width = 12,
  height = 6.5,
  dpi = 300
)
# ------------------------------------------------------------
# Plot helpers
# ------------------------------------------------------------

make_ratio_plot <- function(df, title, subtitle, xlab, x_breaks, x_labels, x_limits = NULL, nrow = 1) {
  n_items <- length(levels(df$predictor_display))
  
  p <- ggplot(df, aes(x = estimate, y = y_pos, colour = strategy)) +
    geom_vline(xintercept = 1, linewidth = 0.40, colour = "grey55") +
    geom_hline(
      yintercept = seq(1.5, n_items - 0.5, by = 1),
      colour = "grey92",
      linewidth = 0.30
    ) +
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high),
      height = 0.10,
      linewidth = 0.35
    ) +
    geom_point(size = 2.5) +
    facet_wrap(~setting, scales = "free_y", nrow = nrow) +
    scale_x_log10(
      breaks = x_breaks,
      labels = x_labels,
      expand = expansion(mult = c(0.03, 0.05))
    ) +
    scale_y_continuous(
      breaks = seq_along(levels(df$predictor_display)),
      labels = levels(df$predictor_display),
      limits = c(0.5, n_items + 0.5),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_colour_manual(values = strategy_cols) +
    labs(title = title, subtitle = subtitle, x = xlab, y = NULL, colour = NULL) +
    theme_classic(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey95", colour = "grey40"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      axis.text.y = element_text(size = 8)
    )
  
  if (!is.null(x_limits)) {
    p <- p + coord_cartesian(xlim = x_limits)
  }
  
  p
}


# ------------------------------------------------------------
# Create comparison figures
# ------------------------------------------------------------

fig1_compare <- make_ratio_plot(
  or_plot,
  title = "Figure 1. Odds ratios for any use of health service",
  subtitle = "CPRD raw vs OMOP-source vs OMOP-standard",
  xlab = "Odds ratio (log scale)",
  x_breaks = c(0.20, 0.40, 0.80, 1.60, 3.20),
  x_labels = c("0.20", "0.40", "0.80", "1.60", "3.20"),
  x_limits = c(0.08, 4.50),
  nrow = 1
)

fig2_compare <- make_ratio_plot(
  cr_plot,
  title = "Figure 2. Cost ratios for total and setting-specific annual costs",
  subtitle = "CPRD raw vs OMOP-source vs OMOP-standard",
  xlab = "Cost ratio (log scale)",
  x_breaks = c(0.20, 0.40, 0.80, 1.60, 3.20),
  x_labels = c("0.20", "0.40", "0.80", "1.60", "3.20"),
  x_limits = c(0.20, 4.80),
  nrow = 2
)

# fig3_compare <- make_ame_plot(
#   ame_plot,
#   title = "Figure 3. Average marginal effects on total annual health care costs",
#   subtitle = "CPRD raw vs OMOP-source vs OMOP-standard",
#   xlab = "Average marginal effect (£ per patient-year)"
# )

# ------------------------------------------------------------
# Save figures
# ------------------------------------------------------------

ggsave(file.path(out_dir, "figure1_or_comparison.png"), fig1_compare, width = 16, height = 6.2, dpi = 300)
ggsave(file.path(out_dir, "figure2_cr_comparison.png"), fig2_compare, width = 16, height = 8.0, dpi = 300)
#ggsave(file.path(out_dir, "figure3_ame_comparison.png"), fig3_compare, width = 12, height = 6.5, dpi = 300)