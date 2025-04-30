#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Differences between OMOP and Raw using visits data
# Date: 29.04.2025
# Last date edited: 29.04.2025
# ****************************************************************************** #

# Load packages ----
library(here)
library(dplyr)
library(ggplot2)
library(VennDiagram)
library(patchwork)
library(eulerr)
library(tidylog)
library(rio)

# Import data ----
visit_data_raw <- rio::import("H:/Projects/Validation/Data/visit_data_min_raw.csv")

glimpse(visit_data_raw)

visit_data_omop <- rio::import("H:/Projects/Validation/Data/visit_data_min_cprd.csv")
glimpse(visit_data_omop)

# Identify overlapping and missing subjects ----
overlap_subjects <- intersect(visit_data_omop$subject_id, visit_data_raw$patid)
only_in_omop <- setdiff(visit_data_omop$subject_id, visit_data_raw$patid)
only_in_raw <- setdiff(visit_data_raw$patid, visit_data_omop$subject_id)	


cat("Subjects in both datasets:", length(overlap_subjects), "\n")
cat("Subjects only in OMOP:", length(only_in_omop), "\n")
cat("Subjects only in Source:", length(only_in_raw), "\n") 

## Venn Diagram ----
venn_plot <- draw.pairwise.venn(area1 = nrow(visit_data_omop), area2 = nrow(visit_data_raw), cross.area = length(overlap_subjects),
                                category = c("OMOP", "Source"), fill = c("#1E90FF", "#FF34B3"), alpha = 0.4)
grid.draw(venn_plot)


# 2*2 table of patients ----

# Convert IDs to character just to be safe
omop_ids <- visit_data_omop %>% select(subject_id) |> rename("patid" = "subject_id") %>% distinct()
raw_ids  <- visit_data_raw %>% select(patid) %>% distinct()

# Full join to get all unique patient IDs from both datasets
combined <- full_join(omop_ids %>% mutate(in_omop = TRUE),
                      raw_ids  %>% mutate(in_raw = TRUE),
                      by = "patid") %>%
  mutate(
    in_omop = ifelse(is.na(in_omop), FALSE, TRUE),
    in_raw  = ifelse(is.na(in_raw), FALSE, TRUE)
  )

# Create the 2x2 table
(presence_table <- table(OMOP = combined$in_omop, Source = combined$in_raw))
colnames(presence_table)
row.names(presence_table)


# Rename row and column names to "No" and "Yes"
colnames(presence_table) <- c("No", "Yes")
rownames(presence_table) <- c("No", "Yes")
print(presence_table)

# export(presence_table, here("Comparison Analysis", "Results", "contigency_table_for_pts.xlsx"))

# 2*2 for user/non-users ----

# Prepare the two datasets: keep only subject_id and user_status
omop_clean <- visit_data_omop %>%
  select(subject_id, user_status) %>%
  mutate(omop_status = user_status) |> 
  select(-user_status) |> 
  distinct(subject_id, omop_status, .keep_all = TRUE)

table(omop_clean$omop_status)
source_clean <- visit_data_raw %>%
  select(patid, user_status) %>%
  mutate(source_status = user_status) |> 
  select(-user_status) |> 
  rename("subject_id" = "patid") |> 
  distinct()
table(source_clean$source_status)

# Merge datasets
combined_df <- full_join(source_clean, omop_clean, by = "subject_id")

# Optional: Fill missing status with "Not in OMOP"/"Not in Source" or keep as NA
combined_df <- combined_df %>%
  mutate(
    source_status = ifelse(is.na(source_status), "Not in Source", source_status),
    omop_status   = ifelse(is.na(omop_status), "Not in OMOP", omop_status)
  )

# Create contingency table
(user_table <- table(Source = combined_df$source_status, OMOP = combined_df$omop_status))
colnames(user_table)
row.names(user_table)

# export(user_table, here("Comparison Analysis", "Results", "contigency_table_for_user_status.xlsx"))

# Monthly plot -----
# Raw
visit_data_reduced_MIN <- visit_data_reduced %>% select(
  subject_id, index_date, visit_detail_start_date, user_status,
  specialty_reduced, follow_up_end
)

### PLOT visits per month/specialty   -------

# Filter for relevant specialties and compute months since index
visit_data_reduced_user.1 <- visit_data_reduced_MIN %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>% 
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute the total number of visits per patient per month 
# (sum across specialties)
total_visits_per_patient_month.1 <- visit_data_reduced_user.1 %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute the mean number of visits per month across all patients
mean_visits_per_month.1 <- total_visits_per_patient_month.1 %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_count = sum(total_visits) / n_distinct(subject_id),
    .groups = "drop"
  )

# Step 3: Compute the total number of visits per specialty per month
visits_per_specialty_month.1 <- visit_data_reduced_user.1 %>%
  group_by(month_after_index, specialty_reduced) %>%
  summarise(total_visits_specialty = sum(visit_count), .groups = "drop")

# Step 4: Merge the total mean visits per month
plots_users_mean_visits_specialty.1 <- visits_per_specialty_month.1 %>%
  left_join(mean_visits_per_month.1, by = "month_after_index") %>%
  group_by(month_after_index) %>%
  mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE)) %>% 
  mutate(prop_mean = mean_visit_count * proportion)

# Step 5: Plot the mean visits per month with specialty proportions
(plot1 <- ggplot(
  plots_users_mean_visits_specialty.1,
  aes(
    x = factor(month_after_index),
    y = mean_visit_count * proportion,
    fill = specialty_reduced
  )
) +
    geom_bar(
      stat = "identity",
      position = "stack",
      width = 0.8
    ) +
    scale_fill_manual(values = c(
      "General Practitioner" = "#B4CDCD",
      "Nurse" = "#377eb8",
      "Other" = "#253494",
      "Uncategorised" = "#53868B"
    )) +
    labs(
      x = "Month After Index",
      y = "Mean Visits per Healthcare Service User",
      fill = "Specialty"
    ) +
    scale_y_continuous(limits = c(0, 4)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 14),
      legend.position = "top",
      panel.spacing = unit(0, "lines")
    )
)