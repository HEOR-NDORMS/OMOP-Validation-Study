library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)

out_dir <- "CPRD Raw Analysis/Results/Final/Comparison/"

make_plot_df <- function(results_obj, type_label) {
  map_dfr(results_obj, function(r) {
    r$cohort_table %>%
      transmute(
        type = type_label,
        measure = r$label,
        cohort = cohort,
        mean = mean,
        lower = lower,
        upper = upper
      )
  })
}

plot_df <- bind_rows(
  make_plot_df(results_hcru, "HCRU"),
  make_plot_df(results_costs, "Costs")
) %>%
  filter(measure %in% c("Primary care", "Inpatient", "Outpatient", "A&E")) %>%
  mutate(
    cohort = factor(cohort, levels = c("Source", "OMOP_standard", "OMOP_source")),
    measure = factor(measure, levels = c("Primary care", "Inpatient", "Outpatient", "A&E")),
    type = factor(type, levels = c("HCRU", "Costs"))
  )

fill_scale <- scale_fill_manual(
  values = c(
    "Source" = "#F8766D",
    "OMOP_standard" = "#00BA38",
    "OMOP_source" = "#619CFF"
  ),
  labels = c(
    "Source" = "Source CPRD-HES",
    "OMOP_standard" = "OMOP-standard",
    "OMOP_source" = "OMOP-source"
  )
)

base_theme <- theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    strip.text.y = element_text(angle = 0)
    
  )

p_hcru <- plot_df %>%
  filter(type == "HCRU") %>%
  ggplot(aes(x = measure, y = mean, fill = cohort)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  fill_scale +
  scale_x_discrete(drop = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = NULL,
    x = NULL,
    y = "Mean count",
    fill = NULL
  ) +
  base_theme

p_costs <- plot_df %>%
  filter(type == "Costs") %>%
  ggplot(aes(x = measure, y = mean, fill = cohort)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  fill_scale +
  scale_x_discrete(drop = TRUE) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  labs(
    title = NULL,
    x = NULL,
    y = "Mean cost",
    fill = NULL
  ) +
  base_theme

p <- p_hcru / p_costs + plot_layout(guides = "collect") &
  theme(legend.position = "top")


ggsave(
  filename = file.path(out_dir, "combined_hcru_costs_plot.png"),
  plot = p,
  width = 11,
  height = 10,
  dpi = 300
)

pct_table <- plot_df %>%
  group_by(type, measure) %>%
  mutate(
    source_mean = mean[cohort == "Source"][1],
    pct_diff = (mean - source_mean) / source_mean * 100
  ) %>%
  ungroup() %>%
  select(type, measure, cohort, mean, pct_diff) %>%
  pivot_wider(
    names_from = cohort,
    values_from = c(mean, pct_diff)
  )

write_csv(pct_table, file.path(out_dir, "combined_pct_differences.csv"))
##### for poster

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)

out_dir <- "CPRD Raw Analysis/Results/Final/Comparison/"

make_plot_df <- function(results_obj, type_label) {
  map_dfr(results_obj, function(r) {
    r$cohort_table %>%
      transmute(
        type = type_label,
        measure = r$label,
        cohort = cohort,
        mean = mean,
        lower = lower,
        upper = upper
      )
  })
}

plot_df <- bind_rows(
  make_plot_df(results_hcru, "HCRU"),
  make_plot_df(results_costs, "Costs")
) %>%
  filter(measure != "Primary care") %>%
  mutate(
    cohort = factor(cohort, levels = c("Source", "OMOP_standard", "OMOP_source")),
    measure = factor(measure, levels = c("Inpatient", "Outpatient", "A&E")),
    type = factor(type, levels = c("HCRU", "Costs"))
  )

fill_scale <- scale_fill_manual(
  values = c(
    "Source" = "#F8766D",
    "OMOP_standard" = "#00BA38",
    "OMOP_source" = "#619CFF"
  ),
  labels = c(
    "Source" = "Source CPRD-HES",
    "OMOP_standard" = "OMOP-standard",
    "OMOP_source" = "OMOP-source"
  )
)

base_theme <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(size = 14)  # increase here
  )

p_hcru <- plot_df %>%
  filter(type == "HCRU") %>%
  ggplot(aes(x = measure, y = mean, fill = cohort)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  fill_scale +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = NULL,
    x = NULL,
    y = "Mean count",
    fill = NULL
  ) +
  base_theme

p_costs <- plot_df %>%
  filter(type == "Costs") %>%
  ggplot(aes(x = measure, y = mean, fill = cohort)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  fill_scale +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  labs(
    title = NULL,
    x = NULL,
    y = "Mean cost",
    fill = NULL
  ) +
  base_theme

p <- p_hcru / p_costs + plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggsave(
  filename = file.path(out_dir, "combined_hcru_costs_plot_no_primary_care.png"),
  plot = p,
  width = 11,
  height = 10,
  dpi = 300
)

