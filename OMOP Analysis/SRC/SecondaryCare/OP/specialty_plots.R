specialty_data_omop <- appt_data_omop |> 
  filter(user_status == "User") |> 
  mutate(tretspef = case_when(
    specialty_source_value_modified == "ear nose and throat" ~ "ear nose and throat service", 
    specialty_source_value_modified == "respiratory medicine" ~ "respiratory medicine service", 
    specialty_source_value_modified == "allergy service" ~ "allergy service", 
    specialty_source_value_modified == "burns care" ~ "burns care service", 
    specialty_source_value_modified == "clinical neuro-physiology" ~ "clinical neurophysiology service",
    specialty_source_value_modified == "haemophilia service" ~ "haemophilia service", 
    specialty_source_value_modified == "maxillofacial surgery" ~ "maxillofacial surgery service", 
    specialty_source_value_modified == "mental illness" ~ "adult mental health service",
    specialty_source_value_modified == "obstetrics service" ~ "obstetrics service",
    specialty_source_value_modified == "radiology" ~ "interventional radiology service",
    specialty_source_value_modified == "specialist rehabilitation service" ~ "specialist rehabilitation service",
    specialty_source_value_modified == "speech and language therapy" ~ "Speech and Language Therapy Service",
    TRUE ~ tretspef)) |> 
  select(subject_id, tretspef) |> 
  mutate(`Data Source` = "OMOP")
  
specialty_data_raw <- appt_data_raw |>
  filter(user_status == "User") |> 
  select(patid, tretspef_recoded) |> 
  rename(tretspef = tretspef_recoded,
         subject_id = patid) |> 
  mutate(tretspef = tolower(tretspef)) |> 
  mutate(`Data Source` = "SOURCE")

specialty_data_combined <- rbind(specialty_data_omop, specialty_data_raw)

specialty_summary <- specialty_data_combined |> 
  group_by(tretspef, `Data Source`) |> 
  summarise(n_appts = n()) |> 
  ungroup()


(omop_plot <- specialty_summary |> 
    filter(`Data Source` == "OMOP") |> 
    mutate(tretspef = if_else(tretspef == "", "Missing Specialty", tretspef)) |> 
  ggplot(aes(x = reorder(tretspef, n_appts), y = n_appts, fill = tretspef)) +
  geom_col() +
    viridis::scale_fill_viridis(option = "A", discrete = TRUE) +
    labs(x = "Number of Outpatient Appointments",
         y = "Treatment Specialty") +
  coord_flip() +
  # facet_wrap(~ `Data Source`, ncol = 1, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "none"))

ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "Plots", 
  "omop_specialty_plot.PNG"),
  plot = omop_plot,
  width = 12,
  height = 10,
  bg = "white"
)

(source_plot <- specialty_summary |> 
    filter(`Data Source` == "SOURCE") |> 
    mutate(tretspef = if_else(tretspef == "", "Missing Specialty", tretspef)) |> 
    ggplot(aes(x = reorder(tretspef, n_appts), y = n_appts, fill = tretspef)) +
    geom_col() +
    viridis::scale_fill_viridis(option = "D", discrete = TRUE) +
    labs(x = "Number of Outpatient Appointments",
         y = "Treatment Specialty") +
    coord_flip() +
    # facet_wrap(~ `Data Source`, ncol = 1, scales = "free_x") +
    theme_bw() +
    theme(legend.position = "none"))

ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "Plots", 
  "source_specialty_plot.PNG"),
  plot = source_plot,
  width = 12,
  height = 10,
  bg = "white"
)

(combined_plot <- specialty_summary |> 
    top_n(20, n_appts)  |>  
    mutate(tretspef = if_else(tretspef == "", "Missing Specialty", tretspef)) |> 
    ggplot(aes(x = reorder(tretspef, n_appts), y = n_appts, fill = tretspef)) +
    geom_col() +
    # viridis::scale_fill_viridis(option = "D", discrete = TRUE) +
    labs(x = "Number of Outpatient Appointments",
         y = "Treatment Specialty") +
    coord_flip() +
    facet_wrap(~ `Data Source`, nrow = 1, scales = "free_x") +
    theme_bw() +
    theme(legend.position = "none"))

# Find specialties appearing in the top 30 *rows* by n_appts (OMOP + SOURCE not summed)
top30_specialties <- specialty_summary %>%
  arrange(desc(n_appts)) %>%
  slice(1:50) %>% 
  pull(tretspef) %>%
  unique()

# Now filter the full dataset to include both sources for these specialties
(top_50_combined_plot <- specialty_summary |> 
  filter(tretspef %in% top30_specialties) |> 
  mutate(tretspef = if_else(tretspef == "", "missing specialty", tretspef)) |> 
  ggplot(aes(x = reorder(tretspef, n_appts), y = n_appts, fill = tretspef)) +
  geom_col() +
    geom_text(aes(label = scales::comma(n_appts)), 
              hjust = -0.1, color = "black", size = 2) +
  labs(
    title = "Top 50 Visited Treatment Specialties",
    x = "Number of Outpatient Appointments",
    y = "Treatment Specialty"
  ) +
  coord_flip() +
  facet_wrap(~ `Data Source`, nrow = 1, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "none"))


ggsave(here::here(
  "OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "Plots", 
  "top_50_combined_specialty_plot.PNG"),
  plot = top_50_combined_plot,
  width = 15,
  height = 7,
  bg = "white"
)
export(specialty_summary, here("OMOP Analysis", "Results", "SecondaryCareAnalysis", "OP", "specialty_data.xlsx"))


