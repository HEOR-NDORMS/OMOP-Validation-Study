
# DENDOBAR CIRCLE plot 1 ------

# --- Libraries ---
library(tidyverse)
library(readxl)
library(ggraph)
library(igraph)
library(colorspace)

# --- Caricamento dati ---
df <- read_excel("plot_map_specialties.xlsx") %>%
  filter(!is.na(source_specialties), !is.na(omop_specialties)) %>%
  distinct(source_specialties, omop_specialties,
           unit_cost_source, clinical_relevance_source,
           unit_cost_omop, clinical_relevance_omop) %>%
  mutate(
    omop_abbr = if_else(
      str_length(omop_specialties) > 30,
      str_c(str_sub(omop_specialties, 1, 28), ".."),
      omop_specialties
    )  )

# --- Normalizzazione costi ---
max_cost <- max(c(df$unit_cost_source, df$unit_cost_omop), na.rm = TRUE)
df <- df %>%
  mutate(
    size_source = 0.5 + 1.2 * unit_cost_source / max_cost,
    size_omop = 0.5 + 1.2 * unit_cost_omop / max_cost
  )

# --- Gruppi OMOP ---
omop_groups <- df %>%
  distinct(omop_abbr) %>%
  arrange(omop_abbr) %>%
  mutate(group_id = omop_abbr)

# --- Palette base per gruppi OMOP ---
n_groups <- nrow(omop_groups)
group_base_colors <- qualitative_hcl(n = n_groups, palette = "Pastel 1")
omop_groups <- omop_groups %>%
  mutate(base_color = group_base_colors)

# --- Colori gradienti per source specialties ---
df_gradient <- df %>%
  left_join(omop_groups, by = "omop_abbr") %>%
  group_by(group_id) %>%
  arrange(desc(unit_cost_source), .by_group = TRUE) %>%
  mutate(
    rank_within_group = row_number(),
    n = n(),
    gradient_color = lighten(base_color, amount = 0.4 * (1 - (rank_within_group - 1) / (n - 1))),
    gradient_color = if_else(clinical_relevance_source == 1,
                             darken(gradient_color, 0.25),
                             gradient_color)
  ) %>%
  ungroup()

# --- Edges: OMOP→SOURCE + root invisibile ---
edges <- bind_rows(
  tibble(from = "Specialties", to = unique(df$omop_abbr), type = "root", group = NA, color = NA),
  df_gradient %>%
    transmute(from = omop_abbr, to = source_specialties, type = "normal", group = group_id, color = gradient_color)
)

# --- Grafo ---
g <- graph_from_data_frame(edges)
E(g)$type <- edges$type
E(g)$group <- edges$group
E(g)$color <- edges$color

# --- Attributi dei nodi ---
node_data <- tibble(name = V(g)$name) %>%
  left_join(
    df_gradient %>%
      select(source_specialties, unit_cost_source, clinical_relevance_source, group_id, gradient_color) %>%
      rename(
        name = source_specialties,
        unit_cost = unit_cost_source,
        relevance = clinical_relevance_source,
        bar_color = gradient_color
      ),
    by = "name"
  ) %>%
  left_join(
    df %>% select(omop_abbr, unit_cost_omop, clinical_relevance_omop) %>%
      distinct() %>%
      rename(
        name = omop_abbr,
        unit_cost_omop = unit_cost_omop,
        relevance_omop = clinical_relevance_omop
      ),
    by = "name"
  ) %>%
  mutate(
    unit_cost = coalesce(unit_cost, unit_cost_omop),
    relevance = coalesce(relevance, relevance_omop),
    group_id = coalesce(group_id, name),
    relevance = factor(relevance, levels = c(0, 1)),
    bar_color = coalesce(bar_color, "#BBBBBB"),
    omop_node_color = case_when(
      name %in% df$omop_abbr & relevance == 1 ~ darken("#6BAED6", 0.4),  # blu scuro
      name %in% df$omop_abbr & relevance == 0 ~ lighten("#6BAED6", 0.4), # blu chiaro
      TRUE ~ NA_character_
    )
  )

V(g)$unit_cost <- node_data$unit_cost
V(g)$bar_color <- node_data$bar_color
V(g)$omop_node_color <- node_data$omop_node_color

# --- Layout circolare ---
layout <- create_layout(g, layout = "dendrogram", circular = TRUE)

# --- Dati per barre radiali ---
bars_data <- layout %>%
  filter(leaf) %>%
  left_join(
    node_data,
    by = "name"
  ) %>%
  mutate(
    unit_cost_bar = coalesce(unit_cost.x, unit_cost.y),
    bar_color = coalesce(bar_color.x, bar_color.y)
  ) %>%
  mutate(
    angle = atan2(y, x),
    angle_deg = angle * 180 / pi,
    angle = ifelse(angle < 0, angle + 2 * pi, angle),
    bar_length = 0.1 + 0.35 * unit_cost_bar / max(unit_cost_bar, na.rm = TRUE),
    xend = x + bar_length * cos(angle),
    yend = y + bar_length * sin(angle),
    angle_text = ifelse(angle_deg >= 90 & angle_deg <= 270, angle_deg + 180, angle_deg),
    hjust = ifelse(angle_deg >= 90 & angle_deg <= 270, 1, 0),
    label_x = xend + 0.02 * cos(angle),
    label_y = yend + 0.02 * sin(angle)
  )

# --- Plot finale ---
ggraph(layout) +
  geom_edge_diagonal(aes(color = I(color), filter = type == "normal"), width = 0.5, alpha = 0.8) +
  geom_edge_diagonal(aes(filter = type == "root"), color = "white", width = 0.3) +
  geom_segment(
    data = bars_data,
    aes(x = x, y = y, xend = xend, yend = yend, color = I(bar_color)),
    size = 2.0, lineend = "round", alpha = 0.95,
    inherit.aes = FALSE
  ) +
  geom_node_point(
    aes(filter = !leaf & name != "Specialties", color = I(omop_node_color)),
    size = 2.5, alpha = 0.9
  ) +
  geom_text(
    data = bars_data,
    aes(x = label_x, y = label_y, label = name, angle = angle_text, hjust = hjust),
    size = 4, family = "sans", color = "black"
  ) +
  geom_node_text(
    aes(label = name, filter = !leaf & name != "Specialties", angle = node_angle(x, y), hjust = 'inward'),
    size = 3, family = "sans", color = "black", nudge_x = -0.02, nudge_y = -0.02
  ) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(25, 25, 25, 25),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


ggsave(
  filename = here::here("CPRD Raw Analysis", "Results", "Results_GL", "Cohort", "specialties_graph.png"),
  width = 18,
  height = 15,
  dpi = 300,
  bg = "white"
)

## DENDOGRAM HORIZONTAL PLOT 2 -------

# === 0. Libraries and Data ===
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(here)

df <- read_excel("plot_map_specialties.xlsx")

# === 1. Setup ===
label_x <- 3.5
omop_x <- 1.5
max_cost <- max(c(df$unit_cost_source, df$unit_cost_omop), na.rm = TRUE)
cost_axis_breaks <- seq(0, ceiling(max_cost / 10) * 10, by = 10)

# Rescale costs onto plot space
cost_scale <- 4
x_zero_cost <- label_x + 2
x_breaks <- x_zero_cost + cost_axis_breaks / max_cost * cost_scale

df_sorted <- df %>%
  filter(!is.na(unit_cost_source), !is.na(unit_cost_omop)) %>%
  mutate(
    omop_specialties = ifelse(
      is.na(omop_specialties) | omop_specialties == "No matching concept",
      "(Unmapped)", omop_specialties
    )
  ) %>%
  arrange(omop_specialties, unit_cost_source) %>%
  mutate(
    source_specialties = factor(source_specialties, levels = unique(source_specialties)),
    y_pos = row_number() * 10,
    label_x = label_x
  )

# === 2. Mappings ===
df_mappings <- df_sorted %>%
  group_by(omop_specialties) %>%
  mutate(y_omop = mean(y_pos)) %>%
  ungroup() %>%
  mutate(
    x_omop = omop_x,
    x_mid = (x_omop + label_x) / 2,
    x_source = label_x
  )

# === 3. Cost profile data ===
df_profile <- df_sorted %>%
  select(source_specialties, y_pos, unit_cost_source, unit_cost_omop) %>%
  pivot_longer(cols = starts_with("unit_cost"),
               names_to = "cost_type", values_to = "unit_cost") %>%
  mutate(
    x_pos = x_zero_cost + unit_cost / max_cost * cost_scale,
    shape = ifelse(cost_type == "unit_cost_source", 1, 4),
    cost_group = ifelse(cost_type == "unit_cost_source", "Source cost", "OMOP cost"),
    color = factor(cost_group, levels = c("Source cost", "OMOP cost"))
  )

# === 4. Plot ===
p <- ggplot() +
  # Elbow connectors
  geom_segment(data = df_mappings,
               aes(x = x_omop, y = y_omop, xend = x_mid, yend = y_omop),
               color = "gray60", linewidth = 0.2) +
  geom_segment(data = df_mappings,
               aes(x = x_mid, y = y_omop, xend = x_mid, yend = y_pos),
               color = "gray60", linewidth = 0.2) +
  geom_segment(data = df_mappings,
               aes(x = x_mid, y = y_pos, xend = x_source, yend = y_pos),
               color = "gray60", linewidth = 0.2) +
  
  # OMOP labels
  geom_text(
    data = df_mappings %>% distinct(omop_specialties, y_omop, x_omop),
    aes(x = x_omop - 0.05, y = y_omop, label = omop_specialties),
    hjust = 1, size = 3.5
  ) +
  
  # Source labels
  geom_text(
    data = df_sorted,
    aes(x = label_x + 0.05, y = y_pos, label = source_specialties),
    hjust = 0, size = 3.2
  ) +
  
  # Cost points and lines
  geom_point(
    data = df_profile,
    aes(x = x_pos, y = y_pos, shape = factor(shape), color = color),
    size = 1.2
  ) +
  geom_line(
    data = df_profile,
    aes(x = x_pos, y = y_pos, group = source_specialties),
    color = "grey70", linewidth = 0.3
  ) +
  
  # Horizontal reference lines
  geom_segment(
    data = df_sorted,
    aes(x = x_zero_cost, xend = max(df_profile$x_pos, na.rm = TRUE) + 0.5,
        y = y_pos, yend = y_pos),
    color = "gray90", linewidth = 0.2
  ) +
  
  # Vertical axis at cost = 0
  geom_segment(
    aes(x = x_zero_cost, xend = x_zero_cost,
        y = min(df_sorted$y_pos) - 5, yend = max(df_sorted$y_pos) + 5),
    color = "gray60", linewidth = 0.3
  ) +
  
  # Vertical grid lines for cost axis
  geom_vline(xintercept = x_breaks,
             color = "gray85", linewidth = 0.2) +
  
  # Top cost axis
  scale_x_continuous(
    name = "Unit Cost (£)",
    breaks = x_breaks,
    labels = cost_axis_breaks,
    position = "top"
  ) +
  
  scale_shape_manual(values = c("1" = 1, "4" = 4), guide = "none") +
  
  scale_color_manual(
    name = "Cost Type",
    values = c("Source cost" = "steelblue", "OMOP cost" = "tomato")
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_cartesian(xlim = c(-1, max(df_profile$x_pos, na.rm = TRUE) + 1))

# === 5. Save ===
ggsave(
  filename = here::here("CPRD Raw Analysis", "Results", "Results_GL", "Cohort", "specialties_graph_2.png"),
  plot = p,
  width = 20,
  height = 18,
  dpi = 300,
  bg = "white"
)
