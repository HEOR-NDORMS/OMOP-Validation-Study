df_test <- visit_data %>% 
  filter(patid == "6222535220539") 
  
df_test1 <- df_test %>% 
  filter(user_status == "User",
         specialty != "Not Clinically Relevant") %>% 
  group_by(patid, specialty) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  )

user_only_summary_statistics_df <- visit_data %>%
  filter(user_status == "User",
         specialty != "Not Clinically Relevant") %>%
  group_by(patid, specialty) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  ) %>%
  group_by(patid) %>%
  summarise(
    tot_visits = sum(total_patient_visits, na.rm = TRUE),
    tot_exposed_yrs = first(total_exposed_yrs),
    tot_visits_per_yr = tot_visits / tot_exposed_yrs,
    .groups = "drop"
  )


# mean(user_only_summary_statistics_df$tot_visits_per_yr)

user_only_summary_statistics <- tibble(
  mean_visit_per_year = round(sum(user_only_summary_statistics_df$tot_visits) / sum(user_only_summary_statistics_df$tot_exposed_yrs), 2),
  min_visit_per_year = round(min(user_only_summary_statistics_df$tot_visits_per_yr), 2),
  max_visit_per_year = round(max(user_only_summary_statistics_df$tot_visits_per_yr), 2),
  sd_visit_per_year = round(sd(user_only_summary_statistics_df$tot_visits_per_yr), 2),
  median_visit_per_year = round(quantile(user_only_summary_statistics_df$tot_visits_per_yr, probs = (.5)), 2),
  lower_q_visits_per_year = round(quantile(user_only_summary_statistics_df$tot_visits_per_yr, probs = (.25)), 2),
  upper_q_visits_per_year = round(quantile(user_only_summary_statistics_df$tot_visits_per_yr, probs = (.75)), 2)
)

# box plot of top10 ----
(bx_plot1 <- ggplot(data = user_visits,
                    aes(x=reorder(specialty, visit_count), y=visit_count, fill=specialty)) +
   geom_boxplot(outlier.shape = NA ) + # outlier.shape = NA removes outliers
   coord_flip() +
   stat_summary(fun = "mean", geom = "point", shape = 2, size = 1.2, color = "red") +
   viridis::scale_fill_viridis(discrete = T, alpha = 0.7, option = "D") +
   labs(title = "Top10 specialties visited by Target Cohort in the T-C1 matching", 
        subtitle = "Only service users & absence of outliers",
        x = "Specialties", y = "Number of visits") +
   theme_minimal() +
   theme(
     legend.position="none",
     plot.title = element_text(size=14, hjust = 0.5),
     axis.text.x = element_text(angle = 45)
   ) 
)

user_visits_t <- user_visits %>% 
  group_by(patid, specialty) %>%
  summarise(
    total_patient_visits = first(visit_count),
    total_exposed_yrs = first(exposed_yrs),
    visits_per_year = total_patient_visits / total_exposed_yrs,
    .groups = "drop"
  )
  
ggplot(data = user_visits_t) +
  geom_histogram(aes(x = total_patient_visits), bins = 30)
