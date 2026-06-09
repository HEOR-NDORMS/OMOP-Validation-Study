library(gt)
library(flextable)
library(officer)
library(ggplot2)
library(scales)
library(patchwork)
library(writexl)
library(MASS)
library(rio)

# ============================================================
# 0) Small helpers
# ============================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================
# 1) Build analysis dataset
# ============================================================

all_patients_all_encounters_cost_omop_source <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/all_patients_all_encounters_costs_omop_source.csv")  %>%
  dplyr::mutate(subject_id = as.character(subject_id))
cohort_hospital_all_cost_omop_source <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_hospital_all_costs_omop_source.csv")  %>%
  dplyr::mutate(subject_id = as.character(subject_id))
cohort_outpatient_all_cost_omop_source <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_outpatient_all_cost_omop_source.csv") %>%
  dplyr::mutate(subject_id = as.character(subject_id))
cohort_emergency_all_cost_omop_source <- rio::import("CPRD Raw Analysis/Results/Final/Comparison/cohort_emergency_all_cost_omop_source.csv") %>%
  dplyr::mutate(subject_id = as.character(subject_id))
cohort_table <- rio::import("CPRD Raw Analysis/SRC/Final/GLM/omop_cohort_glm.xlsx") %>% 
  dplyr::rename(site = fracture_site) %>%
  # dplyr::mutate(
  #   site = dplyr::recode(site,
  #                        vert = "Vertebra",
  #                        nhnv = "NHNV",
  #                        .default = site
  #   ),
  dplyr::mutate(subject_id = as.character(subject_id)
  ) 

total_cost_source_glm <- cohort_table %>%
  dplyr::select(subject_id, age_fracture, site, follow_up_time) %>%
  left_join(
    all_patients_all_encounters_cost_omop_source %>%
      dplyr::select(subject_id, primary_cost = total_patient_cost),
    by = "subject_id"
  ) %>%
  left_join(
    cohort_hospital_all_cost_omop_source %>%
      dplyr::select(subject_id, inpatient_cost = total_in_cost),
    by = "subject_id"
  ) %>%
  left_join(
    cohort_outpatient_all_cost_omop_source %>%
      dplyr::select(subject_id, outpatient_cost = total_op_cost),
    by = "subject_id"
  ) %>%
  left_join(
    cohort_emergency_all_cost_omop_source %>%
      dplyr::select(subject_id, emergency_cost = total_ae_cost),
    by = "subject_id"
  ) %>%
  mutate(
    across(c(primary_cost, inpatient_cost, outpatient_cost, emergency_cost), ~ coalesce(.x, 0)),
    follow_up_years = as.numeric(follow_up_time, units = "days") / 365.25,
    total_cost = primary_cost + inpatient_cost + outpatient_cost + emergency_cost,
    primary_cost_py    = primary_cost / follow_up_years,
    inpatient_cost_py  = inpatient_cost / follow_up_years,
    outpatient_cost_py = outpatient_cost / follow_up_years,
    emergency_cost_py  = emergency_cost / follow_up_years,
    total_cost_py      = total_cost / follow_up_years
  ) %>%
  dplyr::select(-c(primary_cost, inpatient_cost, outpatient_cost, emergency_cost, total_cost)) %>%
  left_join(
    cohort_table %>%
      dplyr::select(
        subject_id,
        cardiovascular_2y,
        osteoporosis_2y,
        ra_2y,
        heart_2y,
        race_source_value
      ),
    by = "subject_id"
  ) %>%
  mutate(
    race_group = case_when(
      is.na(race_source_value) ~ "Missing",
      race_source_value == "White" ~ "White",
      grepl("^Black", race_source_value) ~ "Black",
      race_source_value %in% c("Indian", "Pakistani", "Bangladeshi", "Chinese", "Other_Asian") ~ "Asian",
      TRUE ~ "Other"
    )
  )

analysis_df <- total_cost_source_glm %>%
  mutate(
    site = factor(site),
    race_group = factor(race_group, levels = c("White", "Black", "Asian", "Missing", "Other")),
    cardiovascular_2y = factor(cardiovascular_2y),
    heart_2y = factor(heart_2y),
    ra_2y = factor(ra_2y),
    osteoporosis_2y = factor(osteoporosis_2y)
  ) %>%
  mutate(
    inpatient_use  = as.integer(inpatient_cost_py > 0),
    outpatient_use = as.integer(outpatient_cost_py > 0),
    emergency_use  = as.integer(emergency_cost_py > 0)
  )


# ============================================================
# 2) Descriptive tables
# ============================================================

outcomes <- c(
  "total_cost_py",
  "primary_cost_py",
  "inpatient_cost_py",
  "outpatient_cost_py",
  "emergency_cost_py"
)

util_outcomes <- c("inpatient_use", "outpatient_use", "emergency_use")

summary_table <- map_dfr(outcomes, function(v) {
  n_total <- sum(!is.na(analysis_df[[v]]))
  n_zero <- sum(analysis_df[[v]] == 0, na.rm = TRUE)
  n_positive <- sum(analysis_df[[v]] > 0, na.rm = TRUE)
  zero_prop <- n_zero / n_total
  
  tibble(
    outcome = v,
    n_total = n_total,
    n_zero = n_zero,
    n_positive = n_positive,
    zero_prop = zero_prop,
    zero_percent = round(100 * zero_prop, 2),
    suggested_model = ifelse(zero_prop < 0.05, "Gamma GLM", "Two-part model")
  )
})

vif_model <- lm(
  total_cost_py ~ site + race_group + age_fracture + cardiovascular_2y + heart_2y + ra_2y + osteoporosis_2y,
  data = analysis_df
)

missingness_table <- analysis_df %>%
  summarise(
    across(
      c(
        total_cost_py,
        primary_cost_py,
        inpatient_cost_py,
        outpatient_cost_py,
        emergency_cost_py,
        site,
        age_fracture,
        race_group,
        cardiovascular_2y,
        heart_2y,
        ra_2y,
        osteoporosis_2y
      ),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) %>%
  mutate(percent_missing = round(100 * n_missing / nrow(analysis_df), 2))

zero_summary_table <- summary_table %>%
  dplyr::select(outcome, n_total, n_zero, zero_percent, suggested_model)

race_distribution_table <- analysis_df %>%
  count(race_group) %>%
  mutate(
    proportion = n / sum(n),
    percent = round(100 * proportion, 2)
  ) %>%
  arrange(desc(n))

predictor_prevalence_table <- tibble(
  predictor = c(
    "Cardiovascular disease (2y)",
    "Heart failure (2y)",
    "Rheumatoid arthritis (2y)",
    "Osteoporosis (2y)"
  ),
  n_positive = c(
    sum(analysis_df$cardiovascular_2y == 1, na.rm = TRUE),
    sum(analysis_df$heart_2y == 1, na.rm = TRUE),
    sum(analysis_df$ra_2y == 1, na.rm = TRUE),
    sum(analysis_df$osteoporosis_2y == 1, na.rm = TRUE)
  )
) %>%
  mutate(percent = round(100 * n_positive / nrow(analysis_df), 2))

vif_table <- tibble(
  predictor = rownames(vif(vif_model)),
  gvif = vif(vif_model)[, 1],
  df = vif(vif_model)[, 2],
  adjusted_gvif = vif(vif_model)[, 3]
) %>%
  mutate(
    gvif = round(gvif, 3),
    adjusted_gvif = round(adjusted_gvif, 3)
  )

outcome_distribution_table <- analysis_df %>%
  summarise(
    across(
      c(
        total_cost_py,
        primary_cost_py,
        inpatient_cost_py,
        outpatient_cost_py,
        emergency_cost_py
      ),
      list(
        mean = mean,
        median = median,
        sd = sd,
        min = min,
        max = max
      ),
      na.rm = TRUE
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("outcome", "statistic"),
    names_pattern = "(.*)_([^_]+)$",
    values_to = "value"
  ) %>%
  mutate(
    outcome = dplyr::recode(
      outcome,
      "total_cost_py" = "Total",
      "primary_cost_py" = "Primary",
      "inpatient_cost_py" = "Inpatient",
      "outpatient_cost_py" = "Outpatient",
      "emergency_cost_py" = "Emergency"
    ),
    value = round(value, 2)
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  )

print(missingness_table)
print(zero_summary_table)
print(race_distribution_table)
print(predictor_prevalence_table)
print(vif_table)
print(outcome_distribution_table)

missingness_gt <- missingness_table %>% gt() %>% tab_header(title = "Missingness assessment")
zero_summary_gt <- zero_summary_table %>% gt() %>% tab_header(title = "Zero-cost proportions and model choice")
race_distribution_gt <- race_distribution_table %>% gt() %>% tab_header(title = "Race group distribution")
predictor_prevalence_gt <- predictor_prevalence_table %>% gt() %>% tab_header(title = "Predictor prevalence")
vif_gt <- vif_table %>% gt() %>% tab_header(title = "Variance inflation factors")
outcome_distribution_gt <- outcome_distribution_table %>% gt() %>% tab_header(title = "Outcome distribution summaries")

# ============================================================
# 3) Helpers
# ============================================================

robust_tidy <- function(model, exponentiate = FALSE, part = NA_character_, outcome = NA_character_) {
  ct <- coeftest(model, vcov. = vcovHC(model, type = "HC0"))
  est <- ct[, 1]
  se <- ct[, 2]
  stat <- ct[, 3]
  pval <- ct[, 4]
  
  out <- tibble(
    term = names(est),
    estimate = as.numeric(est),
    robust_se = as.numeric(se),
    statistic = as.numeric(stat),
    p_value = as.numeric(pval),
    conf.low = estimate - 1.96 * robust_se,
    conf.high = estimate + 1.96 * robust_se,
    outcome = outcome,
    part = part
  )
  
  if (exponentiate) {
    out <- out %>%
      mutate(
        estimate_exp = exp(estimate),
        conf.low_exp = exp(conf.low),
        conf.high_exp = exp(conf.high)
      )
  }
  
  out
}

pretty_site_value <- function(x) {
  case_when(
    x %in% c("vert", "vertebra", "VERTEBRA") ~ "Vertebra",
    x %in% c("nhnv", "NHNV") ~ "NHNV",
    TRUE ~ x
  )
}

pretty_term <- function(x) {
  case_when(
    x == "(Intercept)" ~ "Intercept",
    x == "sitevert" ~ "Site: Vertebra vs NHNV",
    x == "sitenhnv" ~ "Site: NHNV vs Vertebra",
    grepl("^site", x) ~ paste0("Site: ", sub("^site", "", x)),
    x == "race_groupBlack" ~ "Ethnicity: Black vs White",
    x == "race_groupAsian" ~ "Ethnicity: Asian vs White",
    x == "race_groupMissing" ~ "Ethnicity: Missing vs White",
    x == "race_groupOther" ~ "Ethnicity: Other vs White",
    x == "cardiovascular_2y1" ~ "Cardiovascular disease (2y)",
    x == "heart_2y1" ~ "Heart failure (2y)",
    x == "ra_2y1" ~ "Rheumatoid arthritis (2y)",
    x == "osteoporosis_2y1" ~ "Osteoporosis (2y)",
    x == "age_fracture" ~ "Age at fracture",
    TRUE ~ x
  )
}

sig_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

theme_hce <- function(base_size = 10) {
  theme_classic(base_size = base_size) +
    theme(
      text = element_text(family = "sans", colour = "#1a1a2e"),
      axis.line = element_line(colour = "#d4cfc7", linewidth = 0.4),
      axis.ticks = element_line(colour = "#d4cfc7", linewidth = 0.4),
      axis.text = element_text(size = rel(0.85), colour = "#444"),
      axis.title = element_text(size = rel(0.9), colour = "#333", face = "italic"),
      plot.title = element_text(size = rel(1.1), face = "bold", colour = "#1a1a2e", margin = margin(b = 3)),
      plot.subtitle = element_text(size = rel(0.82), colour = "#666", margin = margin(b = 8)),
      plot.caption = element_text(size = rel(0.72), colour = "#888", hjust = 0, margin = margin(t = 8)),
      panel.grid.major.x = element_line(colour = "#ece9e2", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#f2efe8", colour = NA),
      strip.text = element_text(size = rel(0.82), face = "bold", colour = "#333"),
      legend.position = "bottom",
      legend.key.size = unit(0.45, "cm"),
      legend.text = element_text(size = rel(0.82)),
      legend.title = element_text(size = rel(0.82), face = "bold"),
      plot.margin = margin(10, 14, 8, 8)
    )
}

save_fig <- function(plot, filename, width_in = 7, height_in = 5,
                     dpi = 300, formats = "png") {
  for (fmt in formats) {
    ggsave(
      filename = paste0(filename, ".", fmt),
      plot = plot,
      width = width_in,
      height = height_in,
      dpi = dpi,
      device = fmt
    )
  }
}

make_ordered_terms <- function(actual_terms, preferred_terms) {
  c(intersect(preferred_terms, actual_terms), setdiff(actual_terms, preferred_terms))
}

get_model_levels <- function(model, var) {
  mf <- model.frame(model)
  if (!var %in% names(mf)) return(NULL)
  x <- mf[[var]]
  if (!is.factor(x)) return(NULL)
  levels(x)
}

# ----------------------------
# Gamma AME helpers for Figure 3
# ----------------------------

predict_gamma_draws <- function(model, newdata, B = 1000, seed = 1) {
  set.seed(seed)
  
  b_all <- coef(model)
  keep <- !is.na(b_all)
  
  b <- b_all[keep]
  V <- sandwich::vcovHC(model, type = "HC0")[keep, keep, drop = FALSE]
  
  draws <- MASS::mvrnorm(B, mu = b, Sigma = V)
  
  X <- stats::model.matrix(delete.response(terms(model)), newdata)
  
  missing_cols <- setdiff(names(b), colnames(X))
  if (length(missing_cols) > 0) {
    X <- cbind(
      X,
      matrix(
        0,
        nrow = nrow(X),
        ncol = length(missing_cols),
        dimnames = list(NULL, missing_cols)
      )
    )
  }
  
  extra_cols <- setdiff(colnames(X), names(b))
  if (length(extra_cols) > 0) {
    X <- X[, setdiff(colnames(X), extra_cols), drop = FALSE]
  }
  
  X <- X[, names(b), drop = FALSE]
  eta <- X %*% t(draws)
  
  exp(eta)
}

calc_ame_gamma <- function(model, data, var, x0, x1, label, B = 1000, seed = 1, model_levels = NULL) {
  d0 <- data
  d1 <- data
  
  if (is.numeric(data[[var]])) {
    d0[[var]] <- x0
    d1[[var]] <- x1
  } else {
    lvls <- model_levels %||% levels(data[[var]])
    d0[[var]] <- factor(x0, levels = lvls)
    d1[[var]] <- factor(x1, levels = lvls)
  }
  
  pred0 <- predict(model, newdata = d0, type = "response")
  pred1 <- predict(model, newdata = d1, type = "response")
  point_ame <- mean(pred1 - pred0, na.rm = TRUE)
  
  y0 <- predict_gamma_draws(model, d0, B = B, seed = seed)
  y1 <- predict_gamma_draws(model, d1, B = B, seed = seed + 1)
  diff_draws <- colMeans(y1 - y0, na.rm = TRUE)
  
  tibble(
    predictor = label,
    estimate = point_ame,
    conf.low = unname(quantile(diff_draws, 0.025)),
    conf.high = unname(quantile(diff_draws, 0.975)),
    p.value = 2 * min(mean(diff_draws <= 0), mean(diff_draws >= 0))
  )
}

# ============================================================
# 4) Fit logistic models for utilisation
# ============================================================

util_models <- list()
util_results <- list()

for (v in util_outcomes) {
  fml <- as.formula(
    paste0(
      v, " ~ site + race_group + age_fracture + cardiovascular_2y + heart_2y + ra_2y + osteoporosis_2y"
    )
  )
  
  m <- glm(fml, data = analysis_df, family = binomial())
  
  util_models[[v]] <- m
  util_results[[v]] <- robust_tidy(
    m,
    exponentiate = TRUE,
    part = "Logistic model",
    outcome = v
  )
}

or_panel_table <- bind_rows(util_results) %>%
  mutate(
    term = pretty_term(term),
    setting = dplyr::recode(
      outcome,
      "inpatient_use" = "Inpatient",
      "outpatient_use" = "Outpatient",
      "emergency_use" = "Emergency"
    ),
    setting = factor(setting, levels = c("Inpatient", "Outpatient", "Emergency"))
  ) %>%
  filter(term != "Intercept") %>%
  mutate(
    order_code = case_when(
      term == "Age at fracture" ~ 1,
      term == "Site: Vertebra vs NHNV" ~ 2,
      term == "Site: NHNV vs Vertebra" ~ 3,
      term == "Ethnicity: Black vs White" ~ 4,
      term == "Ethnicity: Asian vs White" ~ 5,
      term == "Ethnicity: Other vs White" ~ 6,
      term == "Ethnicity: Missing vs White" ~ 7,
      term == "Cardiovascular disease (2y)" ~ 8,
      term == "Heart failure (2y)" ~ 9,
      term == "Rheumatoid arthritis (2y)" ~ 10,
      term == "Osteoporosis (2y)" ~ 11,
      TRUE ~ 99
    )
  ) %>%
  arrange(order_code, term) %>%
  group_by(setting) %>%
  mutate(
    predictor = factor(term, levels = make_ordered_terms(term, c(
      "Age at fracture",
      "Site: Vertebra vs NHNV",
      "Site: NHNV vs Vertebra",
      "Ethnicity: Black vs White",
      "Ethnicity: Asian vs White",
      "Ethnicity: Other vs White",
      "Ethnicity: Missing vs White",
      "Cardiovascular disease (2y)",
      "Heart failure (2y)",
      "Rheumatoid arthritis (2y)",
      "Osteoporosis (2y)"
    )))
  ) %>%
  ungroup() %>%
  mutate(
    label = paste0(
      sprintf("%.2f", estimate_exp), " (",
      sprintf("%.2f", conf.low_exp), "–",
      sprintf("%.2f", conf.high_exp), ")",
      if_else(p_value < 0.001, " ***", if_else(p_value < 0.01, " **", if_else(p_value < 0.05, " *", "")))
    ),
    significant = conf.low_exp > 1 | conf.high_exp < 1
  )

# ============================================================
# 5) Fit Gamma models for costs
# ============================================================

cost_models <- list()
cost_results <- list()

for (v in outcomes) {
  pos_df <- analysis_df %>% filter(.data[[v]] > 0)
  
  fml <- as.formula(
    paste0(
      v, " ~ site + race_group + age_fracture + cardiovascular_2y + heart_2y + ra_2y + osteoporosis_2y"
    )
  )
  
  m <- glm(fml, data = pos_df, family = Gamma(link = "log"))
  
  cost_models[[v]] <- m
  cost_results[[v]] <- robust_tidy(
    m,
    exponentiate = TRUE,
    part = "Gamma model",
    outcome = v
  )
}

cr_panel_table <- bind_rows(cost_results) %>%
  mutate(
    term = pretty_term(term),
    setting = dplyr::recode(
      outcome,
      "total_cost_py" = "Total costs",
      "primary_cost_py" = "Primary care",
      "inpatient_cost_py" = "Inpatient",
      "outpatient_cost_py" = "Outpatient",
      "emergency_cost_py" = "Emergency"
    ),
    setting = factor(setting, levels = c("Total costs", "Primary care", "Inpatient", "Outpatient", "Emergency"))
  ) %>%
  filter(term != "Intercept") %>%
  mutate(
    order_code = case_when(
      term == "Age at fracture" ~ 1,
      term == "Site: Vertebra vs NHNV" ~ 2,
      term == "Site: NHNV vs Vertebra" ~ 3,
      term == "Ethnicity: Black vs White" ~ 4,
      term == "Ethnicity: Asian vs White" ~ 5,
      term == "Ethnicity: Other vs White" ~ 6,
      term == "Ethnicity: Missing vs White" ~ 7,
      term == "Cardiovascular disease (2y)" ~ 8,
      term == "Heart failure (2y)" ~ 9,
      term == "Rheumatoid arthritis (2y)" ~ 10,
      term == "Osteoporosis (2y)" ~ 11,
      TRUE ~ 99
    )
  ) %>%
  arrange(order_code, term) %>%
  group_by(setting) %>%
  mutate(
    predictor = factor(term, levels = make_ordered_terms(term, c(
      "Age at fracture",
      "Site: Vertebra vs NHNV",
      "Site: NHNV vs Vertebra",
      "Ethnicity: Black vs White",
      "Ethnicity: Asian vs White",
      "Ethnicity: Other vs White",
      "Ethnicity: Missing vs White",
      "Cardiovascular disease (2y)",
      "Heart failure (2y)",
      "Rheumatoid arthritis (2y)",
      "Osteoporosis (2y)"
    )))
  ) %>%
  ungroup() %>%
  mutate(
    label = paste0(
      sprintf("%.2f", estimate_exp), " (",
      sprintf("%.2f", conf.low_exp), "–",
      sprintf("%.2f", conf.high_exp), ")",
      if_else(p_value < 0.001, " ***", if_else(p_value < 0.01, " **", if_else(p_value < 0.05, " *", "")))
    ),
    significant = conf.low_exp > 1 | conf.high_exp < 1
  )

# ============================================================
# 6) Plot helpers
# ============================================================

ratio_breaks <- c(0.20, 0.40, 0.80, 1.60, 3.20)
ratio_labels <- c("0.20", "0.40", "0.80", "1.60", "3.20")

make_ratio_panel <- function(df, setting_name, show_y = FALSE,
                             x_breaks, x_labels, panel_colour,
                             xlab, x_min, x_right, title = NULL) {
  
  d <- df %>%
    filter(setting == setting_name) %>%
    mutate(label_x = x_right * 2.00)
  
  ggplot(d, aes(x = estimate_exp, y = predictor)) +
    geom_vline(xintercept = 1, colour = "grey60", linewidth = 0.5) +
    geom_linerange(
      aes(xmin = conf.low_exp, xmax = conf.high_exp, colour = significant),
      linewidth = 0.8
    ) +
    geom_point(aes(colour = significant), size = 2.6) +
    geom_text(
      aes(x = label_x, label = label),
      hjust = 1,
      size = 2.4,
      family = "mono"
    ) +
    scale_x_log10(
      breaks = x_breaks,
      labels = x_labels,
      expand = expansion(mult = c(0.03, 0.02))
    ) +
    scale_colour_manual(
      values = c("TRUE" = panel_colour, "FALSE" = "#9ca3af"),
      guide = "none"
    ) +
    coord_cartesian(
      xlim = c(x_min, x_right * 2.15),
      clip = "off"
    ) +
    labs(
      title = title %||% setting_name,
      x = xlab,
      y = NULL
    ) +
    theme_hce() +
    theme(
      plot.title = element_text(size = rel(0.95), face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 7),
      axis.title.x = element_text(size = 8),
      axis.text.y = if (show_y) element_text(size = 8) else element_blank(),
      axis.ticks.y = if (show_y) element_line() else element_blank(),
      plot.margin = margin(10, 18, 8, 8)
    )
}
make_ame_plot <- function(df) {
  d <- df %>% mutate(label_x = max(conf.high, na.rm = TRUE) * 1.35)
  
  ggplot(d, aes(x = estimate, y = predictor)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.5) +
    geom_col(aes(fill = estimate >= 0), width = 0.55) +
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high),
      height = 0.25, linewidth = 0.5, colour = "#333"
    ) +
    geom_text(
      aes(x = label_x, label = label),
      hjust = 1,
      size = 2.6,
      family = "mono"
    ) +
    scale_x_continuous(
      labels = label_number(prefix = "£", big.mark = ","),
      breaks = pretty_breaks(n = 5),
      expand = expansion(mult = c(0.05, 0.02))
    ) +
    scale_fill_manual(values = c("TRUE" = "#0d7377", "FALSE" = "#94a3b8"), guide = "none") +
    coord_cartesian(
      xlim = c(min(d$conf.low, na.rm = TRUE) * 1.08, max(d$conf.high, na.rm = TRUE) * 1.55),
      clip = "off"
    ) +
    labs(
      title = "Figure 3. Average Marginal Effects on Total Annual Health Care Costs",
      subtitle = "From the Gamma model for total costs; effects shown in £ per patient-year",
      x = "Average marginal effect (£ per year)",
      y = NULL,
      caption = "AMEs compare predicted total annual cost under the observed value versus a counterfactual value for each predictor."
    ) +
    theme_hce() +
    theme(
      axis.text.x = element_text(size = 8),
      plot.margin = margin(10, 18, 8, 8)
    )
}
# ============================================================
# 7) Figure 1
# ============================================================

fig1 <- wrap_plots(
  make_ratio_panel(
    or_panel_table, "Inpatient", TRUE,
    x_breaks = ratio_breaks,
    x_labels = ratio_labels,
    panel_colour = "#2563a8",
    xlab = "Odds Ratio (log scale)",
    x_min = 0.20,
    x_right = 4.60,
    title = "Inpatient"
  ),
  make_ratio_panel(
    or_panel_table, "Outpatient", FALSE,
    x_breaks = ratio_breaks,
    x_labels = ratio_labels,
    panel_colour = "#2563a8",
    xlab = "Odds Ratio (log scale)",
    x_min = 0.40,
    x_right = 5.20,
    title = "Outpatient"
  ),
  make_ratio_panel(
    or_panel_table, "Emergency", FALSE,
    x_breaks = ratio_breaks,
    x_labels = ratio_labels,
    panel_colour = "#2563a8",
    xlab = "Odds Ratio (log scale)",
    x_min = 0.20,
    x_right = 4.60,
    title = "Emergency"
  ),
  nrow = 1,
  widths = c(1, 1, 1)
) +
  plot_annotation(
    title = "Figure 1. Odds Ratios for Any Use of Health Service",
    subtitle = "Adjusted ORs (95% CI) from logistic regression",
    caption = "Panels shown for inpatient, outpatient and emergency only. Reference line at OR = 1.0. * p<0.05, ** p<0.01, *** p<0.001"
  )

# ============================================================
# 8) Figure 2
#   Three equal-width panels on top, three equal-width slots on bottom,
#   with the last slot blank so the panel widths match.
# ============================================================

fig2_top <- wrap_plots(
  make_ratio_panel(
    cr_panel_table, "Total costs", TRUE,
    x_breaks = ratio_breaks,
    x_labels = ratio_labels,
    panel_colour = "#b45309",
    xlab = "Cost Ratio (log scale)",
    x_min = 0.20,
    x_right = 3.20,
    title = "Total costs"
  ),
  make_ratio_panel(
    cr_panel_table, "Primary care", FALSE,
    x_breaks = ratio_breaks,
    x_labels = ratio_labels,
    panel_colour = "#b45309",
    xlab = "Cost Ratio (log scale)",
    x_min = 0.40,
    x_right = 4.60,
    title = "Primary care"
  ),
  make_ratio_panel(
    cr_panel_table, "Inpatient", FALSE,
    x_breaks = ratio_breaks,
    x_labels = ratio_labels,
    panel_colour = "#b45309",
    xlab = "Cost Ratio (log scale)",
    x_min = 0.20,
    x_right = 3.20,
    title = "Inpatient"
  ),
  nrow = 1,
  widths = c(1, 1, 1)
)

fig2_bottom <- wrap_plots(
  make_ratio_panel(
    cr_panel_table, "Outpatient", FALSE,
    x_breaks = ratio_breaks,
    x_labels = ratio_labels,
    panel_colour = "#b45309",
    xlab = "Cost Ratio (log scale)",
    x_min = 0.40,
    x_right = 4.60,
    title = "Outpatient"
  ),
  make_ratio_panel(
    cr_panel_table, "Emergency", FALSE,
    x_breaks = ratio_breaks,
    x_labels = ratio_labels,
    panel_colour = "#b45309",
    xlab = "Cost Ratio (log scale)",
    x_min = 0.40,
    x_right = 3.20,
    title = "Emergency"
  ),
  plot_spacer(),
  nrow = 1,
  widths = c(1, 1, 1)
)

fig2 <- fig2_top / fig2_bottom +
  plot_annotation(
    title = "Figure 2. Cost Ratios for Total and Setting-Specific Annual Costs",
    subtitle = "Adjusted cost ratios (95% CI) from Gamma GLM with log link",
    caption = "Reference line at cost ratio = 1.0. * p<0.05, ** p<0.01, *** p<0.001"
  )

# ============================================================
# 9) AME for total costs only
# ============================================================

total_gamma_model <- cost_models[["total_cost_py"]]

ame_model_check <- tibble(
  quantity = c("Observed mean total cost", "Fitted mean total cost", "Difference"),
  value = c(
    mean(analysis_df$total_cost_py, na.rm = TRUE),
    mean(predict(total_gamma_model, newdata = analysis_df, type = "response"), na.rm = TRUE),
    mean(predict(total_gamma_model, newdata = analysis_df, type = "response"), na.rm = TRUE) -
      mean(analysis_df$total_cost_py, na.rm = TRUE)
  )
)
print(ame_model_check)

site_ref <- levels(model.frame(total_gamma_model)$site)[1]
site_lvs <- setdiff(
  levels(model.frame(total_gamma_model)$site),
  site_ref
)

race_levels_model <- get_model_levels(total_gamma_model, "race_group")
if (is.null(race_levels_model)) race_levels_model <- levels(analysis_df$race_group)
race_ref <- "White"
race_lvs <- intersect(setdiff(race_levels_model, race_ref), c("Black", "Asian", "Other", "Missing"))

ame_list <- list(
  calc_ame_gamma(
    model = total_gamma_model,
    data = analysis_df,
    var = "age_fracture",
    x0 = analysis_df$age_fracture,
    x1 = analysis_df$age_fracture + 10,
    label = "Age at fracture (per 10 years)",
    B = 1000,
    seed = 101
  )
)

if (length(site_lvs) > 0) {
  ame_list <- c(
    ame_list,
    map(site_lvs, function(lvl) {
      calc_ame_gamma(
        model = total_gamma_model,
        data = analysis_df,
        var = "site",
        x0 = site_ref,
        x1 = lvl,
        label = paste0("Site: ", pretty_site_value(lvl), " vs ", pretty_site_value(site_ref)),
        B = 1000,
        seed = 200 + match(lvl, site_lvs),
        model_levels = levels(model.frame(total_gamma_model)$site)
      )
    })
  )
}

if (length(race_lvs) > 0) {
  ame_list <- c(
    ame_list,
    map(race_lvs, function(lvl) {
      calc_ame_gamma(
        model = total_gamma_model,
        data = analysis_df,
        var = "race_group",
        x0 = race_ref,
        x1 = lvl,
        label = paste0("Race: ", lvl, " vs ", race_ref),
        B = 1000,
        seed = 300 + match(lvl, race_lvs),
        model_levels = levels(model.frame(total_gamma_model)$race_group)
      )
    })
  )
}

ame_list <- c(
  ame_list,
  list(
    calc_ame_gamma(
      model = total_gamma_model,
      data = analysis_df,
      var = "cardiovascular_2y",
      x0 = "0",
      x1 = "1",
      label = "Cardiovascular disease (2y)",
      B = 1000,
      seed = 401
    ),
    calc_ame_gamma(
      model = total_gamma_model,
      data = analysis_df,
      var = "heart_2y",
      x0 = "0",
      x1 = "1",
      label = "Heart failure (2y)",
      B = 1000,
      seed = 402
    ),
    calc_ame_gamma(
      model = total_gamma_model,
      data = analysis_df,
      var = "ra_2y",
      x0 = "0",
      x1 = "1",
      label = "Rheumatoid arthritis (2y)",
      B = 1000,
      seed = 403
    ),
    calc_ame_gamma(
      model = total_gamma_model,
      data = analysis_df,
      var = "osteoporosis_2y",
      x0 = "0",
      x1 = "1",
      label = "Osteoporosis (2y)",
      B = 1000,
      seed = 404
    )
  )
)

ame_total <- bind_rows(ame_list) %>%
  mutate(
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    p.value = signif(p.value, 3),
    predictor = factor(predictor, levels = unique(predictor)),
    label = paste0(
      ifelse(estimate >= 0, "+£", "-£"),
      comma(abs(round(estimate))),
      if_else(p.value < 0.001, "***", if_else(p.value < 0.01, "**", if_else(p.value < 0.05, "*", "")))
    )
  )

fig3 <- make_ame_plot(ame_total)

# ============================================================
# 10) Save figures
# ============================================================

save_fig(fig1, "CPRD Raw Analysis/Results/Final/GLM/fig1_utilization_OR_panel_omop_source", width_in = 15, height_in = 5.4)
save_fig(fig2, "CPRD Raw Analysis/Results/Final/GLM/fig2_cost_ratios_panel_omop_source", width_in = 15, height_in = 7.4)
save_fig(fig3, "CPRD Raw Analysis/Results/Final/GLM/fig3_ame_total_costs_omop_source", width_in = 11, height_in = 6.0)

# ============================================================
# 11) Export paper tables
# ============================================================

paper_or_table <- or_panel_table %>%
  mutate(
    estimate_ci = paste0(
      sprintf("%.2f", estimate_exp), " (",
      sprintf("%.2f", conf.low_exp), ", ",
      sprintf("%.2f", conf.high_exp), ")"
    ),
    p_value = ifelse(p_value < 0.001, "<0.001", as.character(round(p_value, 3)))
  ) %>%
  dplyr::select(setting, predictor, estimate_ci, p_value)

paper_cr_table <- cr_panel_table %>%
  mutate(
    estimate_ci = paste0(
      sprintf("%.2f", estimate_exp), " (",
      sprintf("%.2f", conf.low_exp), ", ",
      sprintf("%.2f", conf.high_exp), ")"
    ),
    p_value = ifelse(p_value < 0.001, "<0.001", as.character(round(p_value, 3)))
  ) %>%
  dplyr::select(setting, predictor, estimate_ci, p_value)

paper_ame_table <- ame_total %>%
  mutate(
    estimate_ci = paste0(
      ifelse(estimate >= 0, "+£", "-£"),
      comma(abs(round(estimate))),
      " (",
      ifelse(conf.low >= 0, "+£", "-£"),
      comma(abs(round(conf.low))),
      ", ",
      ifelse(conf.high >= 0, "+£", "-£"),
      comma(abs(round(conf.high))),
      ")"
    ),
    p_value = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))
  ) %>%
  dplyr::select(predictor, estimate_ci, p_value)

writexl::write_xlsx(
  list(
    missingness_table = missingness_table,
    zero_summary_table = zero_summary_table,
    race_distribution_table = race_distribution_table,
    predictor_prevalence_table = predictor_prevalence_table,
    vif_table = vif_table,
    outcome_distribution_table = outcome_distribution_table,
    model_check_table = ame_model_check,
    paper_or_table = paper_or_table,
    paper_cr_table = paper_cr_table,
    paper_ame_table = paper_ame_table
  ),
  path = "CPRD Raw Analysis/Results/Final/GLM/glm_cost_results_tables_omop_source.xlsx"
)