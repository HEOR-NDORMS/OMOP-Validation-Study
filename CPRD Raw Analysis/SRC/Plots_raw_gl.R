visit_data_reducedGL <-visit_data_reduced %>% select(patid,index_date, consdate, user_status, 
                                                     specialty_reduced, follow_up_end) 

### PLOT visits per month/specialty   -------

# Filter for relevant specialties and compute months since index
visit_data_reduced_user <- visit_data_reducedGL %>% 
  filter(specialty_reduced != "Not Clinically Relevant") %>% 
  mutate(month_after_index = interval(index_date, consdate) %/% months(1)) %>%  
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute the total number of visits per patient per month (sum across specialties)
total_visits_per_patient_month <- visit_data_reduced_user %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute the mean number of visits per month across all patients
mean_visits_per_month <- total_visits_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = sum(total_visits) / n_distinct(patid), 
            .groups = "drop")

# Step 3: Compute the total number of visits per specialty per month
visits_per_specialty_month <- visit_data_reduced_user %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_visits_specialty = sum(visit_count), .groups = "drop")

# Step 4: Merge the total mean visits per month
plots_users_mean_visits_specialty <- visits_per_specialty_month %>%
  left_join(mean_visits_per_month, by = "month_after_index") %>%
  group_by(month_after_index) %>% 
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE))

# Step 5: Plot the mean visits per month with specialty proportions
plot1<-ggplot(plots_users_mean_visits_specialty, 
       aes(x = month_after_index, y = mean_visit_count * proportion, fill = specialty_reduced)) +
  geom_bar(stat = "identity", position = "stack") +  
  scale_fill_manual(values = c("General Practitioner" = "#1f77b4", "Nurse" = "#377eb8", "Other" = "#253494")) + 
  labs(x = "Month After Index", y = "Mean Visits per User", fill = "Specialty") +
  theme_minimal()

plot1

### PLOT Tot Visits ------

visit_data_reduced_user2 <- visit_data_reducedGL %>% 
  filter(specialty_reduced != "Not Clinically Relevant") %>% 
  mutate(month_after_index = interval(index_date, consdate) %/% months(1)) %>%  
  group_by(month_after_index) %>%
  summarise(visit_count = n(), .groups = "drop")

plot2 <- ggplot(visit_data_reduced_user2, 
                aes(x = month_after_index, y = visit_count)) +
  geom_bar(stat = "identity", fill = "#1f77b4") +  # Blue color for the bars
  labs(x = "Month After Index", y = "Total Visits") +
  theme_minimal()

plot2


### PLOT exposure weighted ------

visit_data_reducedGL <-visit_data_reduced %>% select(patid,index_date, consdate, user_status, 
                                                     specialty_reduced, follow_up_end, exposed_yrs) 

# Step 1: Merge `exposed_yrs` into the user-specific data
visit_data_reduced_user <- visit_data_reducedGL %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = interval(index_date, consdate) %/% months(1)) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), 
            exposed_yrs = first(exposed_yrs), # Keep the exposed_yrs for each patient
            .groups = "drop")

# Step 2: Compute the total number of visits per patient per month
total_visits_per_patient_month <- visit_data_reduced_user %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(visit_count), 
            total_exposed_yrs = first(exposed_yrs),  # Get exposure years for each patient
            .groups = "drop")

# Step 3: Compute the exposure-weighted mean number of visits per month across all patients
mean_visits_per_month_weighted <- total_visits_per_patient_month %>%
  group_by(month_after_index) %>%
  summarise(weighted_visits = sum(total_visits) / sum(total_exposed_yrs), 
            .groups = "drop")

# Step 4: Compute the total number of visits per specialty per month
visits_per_specialty_month <- visit_data_reduced_user %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_visits_specialty = sum(visit_count), 
            total_exposed_yrs = first(exposed_yrs),  # Get exposure years for each patient
            .groups = "drop")

# Step 5: Merge the total exposure-weighted mean visits per month
plots_users_mean_visits_specialty_weighted <- visits_per_specialty_month %>%
  left_join(mean_visits_per_month_weighted, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE))

# Step 6: Plot the exposure-weighted mean visits per month with specialty proportions
plot3 <- ggplot(plots_users_mean_visits_specialty_weighted, 
                aes(x = month_after_index, y = weighted_visits * proportion, fill = specialty_reduced)) +
  geom_bar(stat = "identity", position = "stack") +  
  scale_fill_manual(values = c("General Practitioner" = "#1f77b4", 
                               "Nurse" = "#377eb8", 
                               "Other" = "#253494")) + 
  labs(x = "Month After Index", y = "Exposure-Weighted Visits per User", fill = "Specialty") +
  theme_minimal()

plot3
