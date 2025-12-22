op_specialty_groups_df_omop <- import("outpatient_specialty_categories_hes_omop.V.0.xlsx",
                                 sheet = "OP_OMOP_specialty") |> 
  janitor::clean_names()

op_specialty_groups_df_raw <- import("outpatient_specialty_categories_hes_omop.V.0.xlsx",
                                      sheet = "OP_source_mainspef") |> 
  janitor::clean_names()

appt_data_omop <- left_join(appt_data_omop, op_specialty_groups_df_omop |> 
                              mutate(concept_name = str_to_lower(
                                str_trim(concept_name))),
                            by = c("concept_name", "specialty_concept_id"))
appt_data_raw <- left_join(appt_data_raw, op_specialty_groups_df_raw |> 
                             rename(mainspef = mainspef_code) |> 
                             mutate(mainspef = as.character(mainspef)), 
                           by = "mainspef")

specialty_data_omop <- appt_data_omop |> 
  filter(user_status == "User") |> 
  select(subject_id, category) |> 
  mutate(`Data Source` = "OMOP")
  
specialty_data_raw <- appt_data_raw |>
  filter(user_status == "User") |> 
  select(patid, category) |> 
  rename(subject_id = patid) |> 
  mutate(`Data Source` = "SOURCE")

specialty_data_combined <- rbind(specialty_data_omop, 
                                      specialty_data_raw)

# specialty_summary <- specialty_data_combined |> 
#   group_by(tretspef, `Data Source`) |> 
#   summarise(n_appts = n_distinct(subject_id)) |> 
#   mutate(n_percents = round(n_appts/ sum(n_appts) * 100, 2)) |> 
#   mutate(label = paste0(scales::comma(n_appts), "(", n_percents, "%", ")")) |> 
#   mutate(n_percents_label = paste0(n_percents, "%")) |> 
#   ungroup()
#   
specialty_summary <- specialty_data_combined |>
  group_by(category, `Data Source`) |>
  summarise(n_appts = n(), .groups = "drop") |>
  group_by(`Data Source`) |>
  mutate(
    n_percents = round(n_appts / sum(n_appts) * 100, 2),
    label = paste0(scales::comma(n_appts), "\n", "(", n_percents, "%", ")"),
    n_percents_label = paste0(n_percents, "%")
  ) |>
  ungroup()

# Find specialties appearing in the top 30 *rows* by n_appts (OMOP + SOURCE not summed)
# user_top30_specialties <- user_specialty_summary %>%
#   arrange(desc(n_appts)) %>%
#   slice(1:30) %>% 
#   pull(category) %>%
#   unique()

# Now filter the full dataset to include both sources for these specialties
(specialty_combined_plot <- specialty_summary |> 
    # filter(category %in% user_top30_specialties) |> 
    mutate(category = if_else(is.na(category), "missing specialty", category)) |> 
    ggplot(aes(x = reorder(category, n_appts), y = n_appts, fill = category)) +
    geom_col() +
    geom_text(aes(label = label), 
              hjust = -0.1, color = "black", size = 3.5) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    labs(
      # title = "Outpatient Healthcare Service Users",
      y = "Number of Outpatient Appointments",
      x = "Main Specialty"
    ) +
    coord_flip() +
    facet_wrap(~ `Data Source`, nrow = 1, scales = "free_x") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(face = "bold", size = 18),
          axis.text = element_text(size = 16),
          plot.title = element_text(face = "bold", size = 20)))


ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "Plots", 
  "combined_specialty_plot.PNG"),
  plot = specialty_combined_plot,
  width = 29,
  height = 13,
  bg = "white"
)




# all_specialty_data_omop <- appt_data_omop |> 
#   select(subject_id, category, user_status) |> 
#   mutate(`Data Source` = "OMOP")
# 
# all_specialty_data_raw <- appt_data_raw |>
#   select(patid, category, user_status) |> 
#   rename(subject_id = patid) |> 
#   mutate(`Data Source` = "SOURCE")
# 
# all_specialty_data_combined <- rbind(all_specialty_data_omop, 
#                                       all_specialty_data_raw)
# 
# 
# 
# all_specialty_summary <- all_specialty_data_combined |>
#   group_by(subject_id, category, `Data Source`, user_status) |>
#   summarise(n_appts_p = n(), .groups = "drop") |>
#   mutate(
#     n_appts_p = if_else(user_status == "Non-User", 0L, n_appts_p)
#   ) |>
#   group_by(category, `Data Source`) |>
#   summarise(
#     total_appts = sum(n_appts_p),
#     .groups = "drop"
#   ) |>
#   group_by(`Data Source`) |>
#   mutate(
#     n_percents = round(total_appts / sum(total_appts) * 100, 2),
#     label = paste0(
#       scales::comma(total_appts),
#       "\n(",
#       n_percents,
#       "%)"
#     ),
#     n_percents_label = paste0(n_percents, "%")
#   ) |>
#   ungroup()
# 
# 
# (all_combined_plot <- all_specialty_summary |> 
#    mutate(category = if_else(is.na(category), "missing specialty", category)) |> 
#     ggplot(aes(x = reorder(category, total_appts), y = total_appts, fill = category)) +
#     geom_col() +
#     geom_text(aes(label = label), 
#               hjust = -0.1, color = "black", size = 3.5) +
#     viridis::scale_fill_viridis(discrete = TRUE) +
#     labs(
#       title = "All Patients with Outpatient Visits",
#       y = "Number of Outpatient Appointments",
#       x = "Main Specialty"
#     ) +
#     coord_flip() +
#     facet_wrap(~ `Data Source`, nrow = 1, scales = "free_x") +
#     theme_bw() +
#     theme(legend.position = "none",
#           axis.title = element_text(face = "bold", size = 18),
#           axis.text = element_text(size = 16),
#           plot.title = element_text(face = "bold", size = 20)))
# 
# 
# ggsave(here::here(
#   "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "Plots", 
#   "user_combined_specialty_plot.PNG"),
#   plot = user_combined_plot,
#   width = 28,
#   height = 12,
#   bg = "white"
# )
# (omop_plot <- specialty_summary |> 
#     filter(`Data Source` == "OMOP") |> 
#     mutate(tretspef = if_else(tretspef == "", "Missing Specialty", tretspef)) |> 
#     ggplot(aes(x = reorder(tretspef, n_appts), y = n_appts, fill = tretspef)) +
#     geom_col() +
#     viridis::scale_fill_viridis(option = "A", discrete = TRUE) +
#     coord_flip() +
#     labs(y = "Number of Patients W/ Outpatient Appointments",
#          x = "Treatment Specialty") +
#   
#   # facet_wrap(~ `Data Source`, ncol = 1, scales = "free_x") +
#   theme_bw() +
#   theme(legend.position = "none"))
# 
# ggsave(here::here(
#   "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "Plots", 
#   "omop_specialty_plot.PNG"),
#   plot = omop_plot,
#   width = 12,
#   height = 10,
#   bg = "white"
# )
# 
# (source_plot <- specialty_summary |> 
#     filter(`Data Source` == "SOURCE") |> 
#     mutate(tretspef = if_else(tretspef == "", "Missing Specialty", tretspef)) |> 
#     ggplot(aes(x = reorder(tretspef, n_appts), y = n_appts, fill = tretspef)) +
#     geom_col() +
#     viridis::scale_fill_viridis(option = "D", discrete = TRUE) +
#     labs(y = "Number of Patients W/ Outpatient Appointments",
#          x = "Treatment Specialty") +
#     coord_flip() +
#     # facet_wrap(~ `Data Source`, ncol = 1, scales = "free_x") +
#     theme_bw() +
#     theme(legend.position = "none"))
# 
# ggsave(here::here(
#   "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "Plots", 
#   "source_specialty_plot.PNG"),
#   plot = source_plot,
#   width = 12,
#   height = 10,
#   bg = "white"
# )

# (combined_plot <- specialty_summary |> 
#     top_n(20, n_appts)  |>  
#     mutate(tretspef = if_else(tretspef == "", "Missing Specialty", tretspef)) |> 
#     ggplot(aes(x = reorder(tretspef, n_appts), y = n_appts, fill = tretspef)) +
#     geom_col() +
#     # viridis::scale_fill_viridis(option = "D", discrete = TRUE) +
#     labs(y = "Number of Outpatient Appointments",
#          x = "Treatment Specialty") +
#     coord_flip() +
#     facet_wrap(~ `Data Source`, nrow = 1, scales = "free_x") +
#     theme_bw() +
#     theme(legend.position = "none"))



# Now filter the full dataset to include both sources for these specialties
(top_30_combined_plot.1 <- specialty_summary |> 
    filter(tretspef %in% top30_specialties) |> 
    mutate(tretspef = if_else(tretspef == "", "missing specialty", tretspef)) |> 
    ggplot(aes(x = reorder(tretspef, n_appts), y = n_percents, fill = tretspef)) +
    geom_col() +
    scale_y_continuous(limits = c(0, 100))+
    geom_text(aes(label = n_percents_label), 
              hjust = -0.1, color = "black", size = 3.5) +
    labs(
      title = "Top 30 Visited Treatment Specialties",
      y = "% of Patients Who Visited Outpatient Appointments",
      x = "Treatment Specialty"
    ) +
    coord_flip() +
    facet_wrap(~ `Data Source`, nrow = 1, scales = "free_x") +
    theme_bw() +
    theme(legend.position = "none"))


ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "Plots", 
  "top_30_combined_specialty_plot_vb.PNG"),
  plot = top_30_combined_plot.1,
  width = 10,
  height = 8,
  bg = "white"
)
export(specialty_summary, here("OMOP Analysis", "Results", "SecondaryCareAnalysis", "OP", "specialty_data.xlsx"))


