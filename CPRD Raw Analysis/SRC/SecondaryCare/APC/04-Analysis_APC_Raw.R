# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: APC Analysis of the patients' spells
# Date: 05.09.2025
# Last date edited: 08.09.2025
# ****************************************************************************** #


# Starting with table_two_apc_raw_wide dataset ----
spells_apc_data <- table_two_apc_raw_wide %>% 
  mutate(
    user_status = if_else(is.na(admidate) & is.na(discharged), "Non-User", "User")
  )

# HRCU Analysis ----

## Number of admissions (spells) per patient ----
admission_no  <- spells_apc_data %>%
  group_by(patid) %>%                       # group data by patid
  select(patid, admidate, spno) %>%   # select only variables of interest: the patids, admission and discharge dates and provider code(spno)
  arrange(patid, admidate) %>%            # sort data according to admission date
  distinct() %>%                            # deduplicate       
  mutate(admission_no  = 1:n()) %>%         # generate an admission counter
  ungroup()  %>% 
  mutate(admission_no = if_else(is.na(admidate) & is.na(spno), 0, admission_no))

spells_apc_full_data <- merge(spells_apc_data, admission_no , all.x=T, all.y=T, 
                              by=c("patid", "admidate", "spno"))
rm(admission_no)

spells_per_patient <- spells_apc_full_data %>%
  group_by(patid) %>%                            # group data per patient
  summarise(n_spells = max(admission_no)) %>% # derive total number of spells by picking max value of spellorder
  ungroup()  %>%                                 # now, ungroup the data and save only one row summary per patient
  select(patid, n_spells)  %>%                  # save only HES ID and nr of spells
  distinct()

## Combine into one table ----

table2_spells_data <- spells_apc_full_data %>%
  left_join(spells_per_patient, by = "patid") %>%
  arrange(desc(n_spells)) %>% 
  select(patid, n_spells, user_status) %>% 
  distinct()
# Sense check for the patient with the largest no. of spells
View(validation_apc_hospital %>% filter(patid == 1423438520834) %>%
       filter(admidate >= study_start_date & discharged <= study_end_date) %>%
       filter(admidate >= as.Date("2016-02-13") & discharged <= as.Date("2018-02-12")))

## Users ----
summary_spells_users_df <- table2_spells_data %>% 
  filter(user_status == "User") %>%
  group_by(n_spells) %>% 
  summarise(
    number_of_patients = n(),
    .groups = "drop"
  )
# rio::export(summary_spells_users_df, here("CPRD Raw Analysis", "Results",
#                                           "SecondaryCareAnalysis", "APC", "Spells_Frequency_Table_Raw.xlsx"))

### Summary table ----
users_summary_stats_spells <-  table2_spells_data %>%
  filter(user_status == "User") %>%
  summarise(
    mean_spells = mean(n_spells),
    min_spells = min(n_spells),
    median_spells = median(n_spells),
    max_spells = max(n_spells),
    sd_spells = sd(n_spells),
    q1_spells = quantile(n_spells, probs = 0.25),
    q3_spells = quantile(n_spells, probs = 0.75),
    .groups = "drop"
  ) %>% 
  mutate(across(everything(), round, 2))


rio::export(users_summary_stats_spells,
            here("CPRD Raw Analysis", "Results","SecondaryCareAnalysis", "APC",
                 "Output-Spells_Summary_Statistics_Raw.xlsx"))

### Plot ----
# Users
(spells_plot_density_users <- ggplot(summary_spells_users_df,
                               aes(x = n_spells)) +
    geom_density(fill = "#3a5a40", 
                 colour = "#344e41") +
    labs(
      title = "",
      x = "Number of Inpatient Spells",
      y = "Density") +
   theme_minimal(base_size = 14, base_family = "Times") +
   theme(
     plot.title = element_text(face = "bold", size = 16),
     plot.subtitle = element_text(size = 13),
     axis.title = element_text(face = "bold"),
     axis.text = element_text(size = 12),
     plot.caption = element_text(size = 10, hjust = 0)
   )) 


ggsave(
  here::here("CPRD Raw Analysis", "Results","SecondaryCareAnalysis", "APC",
             "Plots","Density_plot_spells_users-Raw.PNG"),
  plot = spells_plot_density_users,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)

# All patients

summary_spells_all_df <- table2_spells_data %>% 
  group_by(n_spells) %>% 
  summarise(
    number_of_patients = n(),
    .groups = "drop"
  )

(spells_plot_density_all <- ggplot(summary_spells_all_df,
                               aes(x = n_spells)) +
    geom_density(fill = "#7f4f24", 
                 colour = "#936639") +
    labs(
      title = "",
      x = "Number of Inpatient Spells",
      y = "Density") +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10, hjust = 0)
    )) 


ggsave(
  here::here("CPRD Raw Analysis", "Results","SecondaryCareAnalysis", "APC",
             "Plots","Density_plot_spells_all-Raw.PNG"),
  plot = spells_plot_density_all,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Calculate the mean number of appointments
users_mean_spells <- mean(summary_spells_users_df$n_spells, na.rm = TRUE)

(users_spells_plot_hist <- ggplot(summary_spells_users_df, aes(x = n_spells)) +
    geom_histogram(binwidth = 2,
                   fill = "#1982C4", 
                   color = "#4267AC") +
    geom_vline(xintercept = users_mean_spells,
               linetype = "dashed", 
               color = "red",
               size = 0.7) +
    labs(title = "",
         subtitle = "",
         x = "Number of Spells (bin width = 2)", 
         y = "Count of Patients",
         caption = "Each bar represents a 2-spell range (e.g., 1–2, 3–4, ...).\nRed dashed line shows the average spells per user.") +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10, hjust = 0)
      )
)

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis", "APC", "Plots",
  "Mean_spells_hist_users-Raw.PNG"),
  plot = users_spells_plot_hist,
  width = 10,
  height = 7,
  bg = "white"
)

# All
# Calculate the mean number of appointments
all_mean_spells <- mean(summary_spells_all_df$n_spells, na.rm = TRUE)

(all_spells_plot_hist <- ggplot(summary_spells_all_df, aes(x = n_spells)) +
    geom_histogram(binwidth = 2,
                   fill = "#a4133c", 
                   color = "#800f2f") +
    geom_vline(xintercept = all_mean_spells,
               linetype = "dashed", 
               color = "#0077b6",
               size = 0.7) +
    labs(title = "",
         subtitle = "",
         x = "Number of Spells (bin width = 2)", 
         y = "Count of Patients",
         caption = "Each bar represents a 2-spell range (e.g., 1–2, 3–4, ...).\nBlue dashed line shows the average spells per user.") +
    theme_minimal(base_size = 14, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10, hjust = 0)
    )
)

# Save the plot
ggsave(here::here(
  "CPRD Raw Analysis", "Results", "SecondaryCareAnalysis", "APC", "Plots",
  "Mean_spells_hist_all-Raw.PNG"),
  plot = users_spells_plot_hist,
  width = 10,
  height = 7,
  bg = "white"
)

# Costs Analysis ----

## Prepare costs data ----
## Prepare the data ----
validation_old <- my_dm$`_validation_old` %>% collect()

# Merge with the above to get the sex and age which are required in the grouper
costs_data_hrg_apc_users <- left_join(table_two_apc_raw, validation_old %>% select(patid, gender, yob), 
                            by = "patid") %>% 
  filter(!is.na(admidate) & !is.na(discharged))

# Create the variables required by the grouper:
costs_data_hrg_apc_users_1 <- costs_data_hrg_apc_users %>% 
  mutate(neocare = NA,
         diag_02 = NA, diag_03 = NA, diag_04 = NA, diag_05 = NA,
         diag_06 = NA, diag_07 = NA, diag_08 = NA, diag_09 = NA,
         diag_10 = NA, diag_11 = NA, diag_12 = NA, diag_13 = NA,
         diag_14 = NA,
         oper_02 = NA,oper_03 = NA, oper_04 = NA, oper_05 = NA,
         oper_06 = NA, oper_07 = NA, oper_08 = NA, oper_09 = NA,
         oper_10 = NA, oper_11 = NA, oper_12 = NA,
         criticalcaredays = NA,
         rehabilitationdays = NA,
         spcdays = NA,
         procodet = NA) %>% 
  mutate(startage = as.numeric(format(admidate, "%Y")) - yob) %>% 
  mutate(gender = if_else(gender == "F", 2, 1)) %>% 
    rename(epiorder = eorder,
         diag_01 = icd,
         oper_01 = opcs,
         provspnops = spno,
         sex = gender) %>% 
  mutate(diag_01 = gsub("\\.", "", diag_01)) %>% 
  select(procodet, provspnops, epiorder, startage, sex, classpat,
         admisorc, admimeth, disdest, dismeth, epidur, mainspef,
         neocare, tretspef,  diag_01:diag_14, oper_01:oper_12,
         criticalcaredays, rehabilitationdays, spcdays)

rio::export(costs_data_hrg_apc_1,
            here("CPRD Raw Analysis", "Results","SecondaryCareAnalysis", "APC",
                 "apc_costs_data.csv"))
