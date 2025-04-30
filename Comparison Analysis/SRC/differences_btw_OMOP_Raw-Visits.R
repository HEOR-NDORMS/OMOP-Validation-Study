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

visit_data_omop <- rio::import("H:/Projects/Validation/Data/visit_data_min_omop.csv")
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

# Convert the table to a data frame with OMOP as a column
presence_df <- as.data.frame.matrix(presence_table)
presence_df <- tibble::rownames_to_column(presence_df, var = "OMOP")  # Now 'OMOP' is a column

# Rename the column names to reflect Raw status
colnames(presence_df)[2:3] <- c("Raw: No", "Raw: Yes")

export(presence_df, here("Comparison Analysis", "Results", "contigency_table_for_pts.xlsx"))

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

# Convert the table to a data frame with OMOP as a column
user_df <- as.data.frame.matrix(user_table)
user_df <- tibble::rownames_to_column(user_df, var = "Source")  # Now 'OMOP' is a column

export(user_df, here("Comparison Analysis", "Results", "contigency_table_for_user_status.xlsx"))

# Monthly  Visits plot -----
visit_data_reduced_min_raw <- rio::import("H:/Projects/Validation/Data/visit_data_reduced_min_raw.csv")

glimpse(visit_data_reduced_min_raw)

visit_data_reduced_min_omop <- rio::import("H:/Projects/Validation/Data/visit_data_reduced_min_omop.csv")
glimpse(visit_data_reduced_min_omop)

### PLOT visits per month/specialty   -------

# Filter for relevant specialties and compute months since index
visit_data_reduced_user_o <- visit_data_reduced_min_omop %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>% 
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute the total number of visits per patient per month 
# (sum across specialties)
total_visits_per_patient_month_o <- visit_data_reduced_user_o %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute the mean number of visits per month across all patients
mean_visits_per_month_o <- total_visits_per_patient_month_o %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_count = sum(total_visits) / n_distinct(subject_id),
    .groups = "drop"
  )


# Filter for relevant specialties
visit_data_reduced_user_r <- visit_data_reduced_min_raw %>%
  filter(specialty_reduced != "Not Clinically Relevant", 
         user_status == "User") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute the total number of visits per patient per month
total_visits_per_patient_month_r <- visit_data_reduced_user_r %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute the mean number of visits per month across fully contributing patients
mean_visits_per_month_r <- total_visits_per_patient_month_r %>%
  group_by(month_after_index) %>%
  summarise(
    mean_visit_count = sum(total_visits) / n_distinct(patid),  # Actual mean
    .groups = "drop"
  )

# Add a source label to each
mean_visits_raw  <- mean_visits_per_month_r  %>% mutate(source = "Source")
mean_visits_omop <- mean_visits_per_month_o %>% mutate(source = "OMOP")

# Combine into one long-format dataframe
combined_mean_visits <- bind_rows(mean_visits_raw, mean_visits_omop)

# Plot
(plot1 <- ggplot(combined_mean_visits, aes(x = month_after_index, y = mean_visit_count, color = source)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Mean Visits per Month by Data Source",
    x = "Month After Index",
    y = "Mean Visits per Healthcare Service User",
    color = "Data Source"
  ) +
  scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ))

ggsave(here::here(
  "Comparison Analysis", "Results", "Plots",
  "Mean_visits_per_specialty_unweighted.PNG"),
  plot = plot1,
  width = 6.5,
  height = 5,
  bg = "white"
)


### PLOT exposure weighted ------
# Prepare data (remove "Not Clinically Relevant" and compute monthly bins)
# Sense check: 23520018,243220018
visit_data_reduced_user_r.2 <- visit_data_reduced_min_raw %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
  mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44) + 1)) %>% 
  group_by(patid) %>% 
  filter(full_month_after_index >= month_after_index) %>%  # Fully contributing patients only
  # removed 29,293 rows
  ungroup() %>% 
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute total visits per patient per month
total_visits_per_patient_month_r.2 <- visit_data_reduced_user_r.2 %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute mean number of visits per month (no weighting)
mean_visits_per_month_r.2 <- total_visits_per_patient_month_r.2 %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = sum(total_visits) / n_distinct(patid), .groups = "drop")

# Prepare data (remove "Not Clinically Relevant" and compute monthly bins)
visit_data_reduced_user_o.2 <- visit_data_reduced_min_omop %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
  mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44) + 1)) %>% 
  group_by(subject_id) %>% 
  filter(full_month_after_index >= month_after_index) %>%  # Fully contributing patients only
  # removed 29,293 rows
  ungroup() %>% # Fully contributing patients only
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute total visits per patient per month
total_visits_per_patient_month_o.2 <- visit_data_reduced_user_o.2 %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute mean number of visits per month (no weighting)
mean_visits_per_month_o.2 <- total_visits_per_patient_month_o.2 %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = sum(total_visits) / n_distinct(subject_id), .groups = "drop")

# Add a source label to each
mean_visits_raw2  <- mean_visits_per_month_r.2  %>% mutate(source = "Source")
mean_visits_omop2 <- mean_visits_per_month_o.2 %>% mutate(source = "OMOP")

# Combine into one long-format dataframe
combined_mean_visits2 <- bind_rows(mean_visits_raw2, mean_visits_omop2)

# Plot
(plot2 <- ggplot(combined_mean_visits2, aes(x = month_after_index, y = mean_visit_count, color = source)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = "Mean Visits per Month by Data Source",
      x = "Month After Index",
      y = "Mean Visits per Healthcare Service User",
      caption = "Mean calculated among healthcare service users \ncontributing fully to each month.",
      color = "Data Source"
    ) +
    scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c")) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    ))

ggsave(here::here(
  "Comparison Analysis", "Results", "Plots",
  "Mean_visits_per_specialty_weighted.PNG"),
  plot = plot2,
  width = 6.5,
  height = 5,
  bg = "white"
)

# Monthly  Visits plot -----
visit_data_reduced_min_cost_raw <- rio::import("H:/Projects/Validation/Data/visit_data_reduced_min_cost_raw.csv")

glimpse(visit_data_reduced_min_cost_raw)

visit_data_reduced_min_cost_omop <- rio::import("H:/Projects/Validation/Data/visit_data_reduced_min_cost_omop.csv")
glimpse(visit_data_reduced_min_cost_omop)

### PLOT costs per month/specialty   -------
# Step 0: Filter and calculate month_end
visit_data_reduced_user_c_r <- visit_data_reduced_min_cost_raw %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
  ungroup() %>% 
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct(patid, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE)

# Step 1: Total cost per patient per month
total_costs_per_patient_month_r <- visit_data_reduced_user_c_r %>%
  group_by(patid, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# Step 2: Mean cost per month
mean_costs_per_month_r <- total_costs_per_patient_month_r %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs = mean(total_costs),
    .groups = "drop"
  )


# Step 0: Filter and calculate month_end
visit_data_reduced_user_c_o <- visit_data_reduced_min_cost_omop %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct(subject_id, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE)

# Step 1: Total cost per patient per month
total_costs_per_patient_month_o <- visit_data_reduced_user_c_o %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# Step 2: Mean cost per month across all users
mean_costs_per_month_o <- total_costs_per_patient_month_o %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs = mean(total_costs),
    .groups = "drop"
  )

raw_costs_by_month <- mean_costs_per_month_r |> 
  mutate(source = "Source")

omop_costs_by_month <- mean_costs_per_month_o |> 
  mutate(source = "OMOP")

combined_costs <- bind_rows(raw_costs_by_month, omop_costs_by_month)

(plot_costs1 <- ggplot(combined_costs, aes(x = month_after_index, y = mean_costs, color = source)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Mean Visits Cost per Month by Data Source",
    x = "Month After Index",
    y = "Mean Monthly Visits Cost Healthcare Service User",
    color = "Data Source"
  ) +
  scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ))

ggsave(here::here(
  "Comparison Analysis", "Results", "Plots",
  "Mean_costs_per_specialty_unweighted.PNG"),
  plot = plot_costs1,
  width = 6.5,
  height = 5,
  bg = "white"
)

### PLOT exposure weighted ------
# Step 0: Filter and calculate month_end
visit_data_reduced_user_c_r2 <- visit_data_reduced_min_cost_raw %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
  mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44))) %>% 
  group_by(patid) %>% 
  filter(full_month_after_index >= month_after_index) %>%
  ungroup() %>% 
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct(patid, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE)

# Step 1: Total cost per patient per month
total_costs_per_patient_month_r2 <- visit_data_reduced_user_c_r2 %>%
  group_by(patid, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# Step 2: Mean cost per month
mean_costs_per_month_r2 <- total_costs_per_patient_month_r2 %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs = mean(total_costs),
    .groups = "drop"
  )

visit_data_reduced_user_c_o2 <- visit_data_reduced_min_cost_omop %>%
  filter(specialty_reduced != "Not Clinically Relevant") %>%
  mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
  mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44))) %>% 
  group_by(subject_id) %>% 
  filter(full_month_after_index >= month_after_index) %>%  # Fully contributing patients only
  # removed 29,293 rows
  ungroup() %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    visit_costs = visit_count * unit_cost,
    .groups = "drop"
  ) %>% 
  distinct(subject_id, month_after_index, specialty_reduced, visit_costs, 
           .keep_all = TRUE)

# Step 1: Total cost per patient per month
total_costs_per_patient_month_o2 <- visit_data_reduced_user_c_o2 %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# Step 2: Mean cost per month
mean_costs_per_month_o2 <- total_costs_per_patient_month_o2 %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs = mean(total_costs),
    .groups = "drop"
  )

raw_costs_by_month2 <- mean_costs_per_month_r2 |> 
  mutate(source = "Source")

omop_costs_by_month2 <- mean_costs_per_month_o2 |> 
  mutate(source = "OMOP")

combined_costs2 <- bind_rows(raw_costs_by_month2, omop_costs_by_month2)

(plot_costs2 <- ggplot(combined_costs2, aes(x = month_after_index, y = mean_costs, color = source)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = "Mean Visits Cost per Month by Data Source",
      x = "Month After Index",
      y = "Mean Monthly Visits Cost Healthcare Service User",
      caption = "Mean calculated among healthcare service users \ncontributing fully to each month.",
      color = "Data Source"
    ) +
    scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c")) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    ))

ggsave(here::here(
  "Comparison Analysis", "Results", "Plots",
  "Mean_costs_per_specialty_weighted.PNG"),
  plot = plot_costs2,
  width = 6.5,
  height = 5,
  bg = "white"
)
