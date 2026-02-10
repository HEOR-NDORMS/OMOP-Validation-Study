library(tidyverse)

costs_df <- tibble(
  source = c("CPRD-HES", "OMOP-standard", "OMOP-source"),
  
  inpatient_cost = c(3720.684, 2464.119, 3541.202),
  inpatient_lwr  = c(3639.411, 2403.777, 3465.318),
  inpatient_upr  = c(3796.503, 2524.008, 3616.368),
  
  outpatient_cost = c(1195.823, 1143.680, 1154.048),
  outpatient_lwr  = c(1173.638, 1122.912, 1131.383),
  outpatient_upr  = c(1217.907, 1163.446, 1175.949),
  
  emergency_cost = c(434.071, 342.872, 342.872),
  emergency_lwr  = c(426.971, 337.199, 337.199),
  emergency_upr  = c(441.808, 348.446, 348.446)
)


costs_long <- costs_df %>%
  pivot_longer(
    cols = -source,
    names_to = c("service", ".value"),
    names_pattern = "(inpatient|outpatient|emergency)_(.*)"
  ) %>%
  mutate(
    service = recode(
      service,
      inpatient  = "Inpatient",
      outpatient = "Outpatient",
      emergency  = "Emergency"
    ),
    source = factor(source, levels = c("CPRD-HES", "OMOP-source", "OMOP-standard"))
  )


p <- ggplot(costs_long, aes(x = source, y = cost, fill = source)) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(ymin = lwr, ymax = upr),
    width = 0.2,
    linewidth = 0.6
  ) +
  geom_text(
    aes(label = scales::label_comma(prefix = "£")(round(cost, 0))),
    vjust = 2.3,        # push text down inside the bar
    color = "white",   # readable on dark bars
    size = 3.5
  ) +
  facet_wrap(~ service, scales = "free_y") +
  labs(
    #title = "Costs comparison by care setting (2023–24)",
    x = NULL,
    y = "Cost (£)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )



ggsave(
  filename = "costs_comparison_2324.png",
  plot = p,
  width = 10,
  height = 5,
  dpi = 300
)

