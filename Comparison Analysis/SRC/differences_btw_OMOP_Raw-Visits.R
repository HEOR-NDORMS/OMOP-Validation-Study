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
overlap_subjects <- intersect(visits_data_omop$subject_id, visits_data_raw$patid)
only_in_omop <- setdiff(visits_data_omop$subject_id, visits_data_raw$patid)
only_in_raw <- setdiff(visits_data_raw$patid, visits_data_omop$subject_id)	


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
raw_ids  <- visits_data_raw %>% select(patid) %>% distinct()

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

rio::export(presence_df, here::here("Comparison Analysis", "Results", "contigency_table_for_pts.xlsx"))

# 2*2 for user/non-users ----

# Prepare the two datasets: keep only subject_id and user_status
omop_clean <- visit_data_omop %>%
  select(subject_id, user_status) %>%
  mutate(omop_status = user_status) |> 
  select(-user_status) |> 
  distinct(subject_id, omop_status, .keep_all = TRUE)

table(omop_clean$omop_status)
source_clean <- visits_data_raw %>%
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

# export(user_df, here("Comparison Analysis", "Results", "contigency_table_for_user_status.xlsx"))

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
# # Step 3: Compute the total number of visits per specialty per month
# visits_per_specialty_month_o <- visit_data_reduced_user_o %>%
#   group_by(month_after_index, specialty_reduced) %>%
#   summarise(total_visits_specialty = sum(visit_count), .groups = "drop")
# 
# # Step 4: Merge mean visits and compute proportional contribution per specialty
# plots_users_mean_visits_specialty_o <- visits_per_specialty_month_o %>%
#   left_join(mean_visits_per_month_o, by = "month_after_index") %>%
#   group_by(month_after_index) %>%
#   mutate(proportion = total_visits_specialty / sum(total_visits_specialty, na.rm = TRUE)) %>%
#   mutate(prop_mean = mean_visit_count * proportion)


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


mean_visits_diff <- mean_visits_raw %>%
  select(month_after_index, mean_visit_count_raw = mean_visit_count) %>%
  inner_join(mean_visits_omop %>%
               select(month_after_index, mean_visit_count_omop = mean_visit_count),
             by = "month_after_index") %>%
  mutate(
    mean_visit_count = mean_visit_count_omop - mean_visit_count_raw ,
    source = "Difference"
  ) %>%
  select(month_after_index, mean_visit_count, source)

# Combine into one long-format dataframe
combined_mean_visits <- bind_rows(mean_visits_raw, mean_visits_omop, mean_visits_diff)

# Plot
(plot1 <- ggplot(combined_mean_visits, 
                 aes(x = month_after_index, 
                     y = mean_visit_count, color = source)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Mean Visits per Month by Data Source",
    x = "Month After Index",
    y = "Mean Visits per Healthcare Service User",
    color = "Data Source"
  ) +
  scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c",
                                "Difference" = "#595959")) +
    scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4)) +
    theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ))

# ggsave(here::here(
#   "Comparison Analysis", "Results", "Plots",
#   "Mean_visits_per_specialty_unweighted.PNG"),
#   plot = plot1,
#   width = 6.5,
#   height = 5,
#   bg = "white"
# )


### PLOT exposure weighted ------
# Prepare data (remove "Not Clinically Relevant" and compute monthly bins)
# Sense check: 23520018,243220018
adjusted_visits_data_r <- visit_data_reduced_min_raw %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(
    # Calculate month after index using 30.44 days/month approximation
    month_after_index = trunc((as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1),
    
    # Define month start and end using as.difftime to handle decimal days
    month_start = index_date + as.difftime((month_after_index - 1) * 30.44, units = "days"),
    month_end = index_date + as.difftime(month_after_index * 30.44, units = "days"),
    
    # Calculate exposure details
    exposure_end = as.Date(follow_up_end),
    days_exposed_in_month = as.numeric(pmin(exposure_end, month_end) - pmax(index_date, month_start) + 1),
    
    # Flags for contribution and death
    fully_contributed = days_exposed_in_month >= 30.44,
    half_contributed = days_exposed_in_month >= 15.22,
    died_in_month = !is.na(death_date) & death_date >= month_start & death_date <= month_end
  ) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    days_exposed_in_month = first(days_exposed_in_month),
    fully_contributed = first(fully_contributed),
    half_contributed = first(half_contributed),
    died_in_month = first(died_in_month),
    .groups = "drop"
  ) %>%
  filter(half_contributed == TRUE) %>%
  mutate(
    adjusted_visits = case_when(
      fully_contributed ~ visit_count,
      died_in_month ~ visit_count,  # No adjustment if patient died that month
      TRUE ~ (visit_count / days_exposed_in_month) / 30.44
    )
  )


# visit_data_reduced_user_r.2 <- visit_data_reduced_min_raw %>%
#   filter(specialty_reduced != "Not Clinically Relevant",
#          user_status == "User") %>%
#   mutate(month_after_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
#   mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44) + 1)) %>% 
#   group_by(patid) %>% 
#   filter(full_month_after_index >= month_after_index) %>%  # Fully contributing patients only
#   # removed 29,293 rows
#   ungroup() %>% 
#   group_by(patid, month_after_index, specialty_reduced) %>%
#   summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute total visits per patient per month
total_visits_per_patient_month_r.2 <- adjusted_visits_data_r %>%
  group_by(patid, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute mean number of visits per month (no weighting)
mean_visits_per_month_r.2 <- total_visits_per_patient_month_r.2 %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = sum(total_visits) / n_distinct(patid), .groups = "drop")

# Prepare data (remove "Not Clinically Relevant" and compute monthly bins)
adjusted_visits_data_o <- visit_data_reduced_min_omop %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(
    month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1),
    
    # Convert index_date to Date to avoid IDate + difftime issues
    month_start = as.Date(index_date) + as.difftime((month_after_index - 1) * 30.44, units = "days"),
    month_end = as.Date(index_date) + as.difftime(month_after_index * 30.44, units = "days"),
    
    exposure_end = as.Date(follow_up_end),
    days_exposed_in_month = as.numeric(pmin(exposure_end, month_end) - pmax(as.Date(index_date), month_start) + 1),
    
    fully_contributed = days_exposed_in_month >= 30.44,
    half_contributed = days_exposed_in_month >= 15.22,
    died_in_month = !is.na(death_date) & death_date >= month_start & death_date <= month_end
  ) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    days_exposed_in_month = first(days_exposed_in_month),
    fully_contributed = first(fully_contributed),
    half_contributed = first(half_contributed),
    died_in_month = first(died_in_month),
    .groups = "drop"
  ) %>%
  filter(half_contributed == TRUE) %>%
  mutate(
    adjusted_visits = case_when(
      fully_contributed ~ visit_count,
      died_in_month ~ visit_count,
      TRUE ~ (visit_count / days_exposed_in_month) / 30.44
    )
  )

# visit_data_reduced_user_o.2 <- visit_data_reduced_min_omop %>%
#   filter(specialty_reduced != "Not Clinically Relevant",
#          user_status == "User") %>%
#   mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
#   mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44) + 1)) %>% 
#   group_by(subject_id) %>% 
#   filter(full_month_after_index >= month_after_index) %>%  # Fully contributing patients only
#   # removed 29,293 rows
#   ungroup() %>% # Fully contributing patients only
#   group_by(subject_id, month_after_index, specialty_reduced) %>%
#   summarise(visit_count = n(), .groups = "drop")

# Step 1: Compute total visits per patient per month
total_visits_per_patient_month_o.2 <- adjusted_visits_data_o %>%
  group_by(subject_id, month_after_index) %>%
  summarise(total_visits = sum(visit_count), .groups = "drop")

# Step 2: Compute mean number of visits per month (no weighting)
mean_visits_per_month_o.2 <- total_visits_per_patient_month_o.2 %>%
  group_by(month_after_index) %>%
  summarise(mean_visit_count = sum(total_visits) / n_distinct(subject_id), .groups = "drop")

# Add a source label to each
mean_visits_raw2  <- mean_visits_per_month_r.2  %>% mutate(source = "Source")
mean_visits_omop2 <- mean_visits_per_month_o.2 %>% mutate(source = "OMOP")

mean_visits_diff2 <- mean_visits_raw2 %>%
  select(month_after_index, mean_visit_count_raw = mean_visit_count) %>%
  inner_join(mean_visits_omop2 %>%
               select(month_after_index, mean_visit_count_omop = mean_visit_count),
             by = "month_after_index") %>%
  mutate(
    mean_visit_count = mean_visit_count_omop - mean_visit_count_raw,
    source = "Difference"
  ) %>%
  select(month_after_index, mean_visit_count, source)

# Combine into one long-format dataframe
combined_mean_visits2 <- bind_rows(mean_visits_raw2, mean_visits_omop2, mean_visits_diff)

# Plot
(plot2 <- ggplot(combined_mean_visits2, 
                 aes(x = month_after_index, 
                     y = mean_visit_count, color = source)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = "Mean Visits per Month by Data Source",
      x = "Month After Index",
      y = "Mean Visits per Healthcare Service User",
      caption = "Visit counts were adjusted for partial month exposure when patients contributed ≥15.22 days. \nCounts were scaled by exposure days, except for full-month contributions or if the patient died during the month. \nRecords with <15.22 days were excluded.",
      color = "Data Source"
    ) +
    scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c",
                                  "Difference" = "#595959")) +
    scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    ))

# ggsave(here::here(
#   "Comparison Analysis", "Results", "Plots",
#   "Mean_visits_per_specialty_weighted.PNG"),
#   plot = plot2,
#   width = 7.5,
#   height = 6,
#   bg = "white"
# )

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

mean_costs_diff <- raw_costs_by_month %>%
  select(month_after_index, mean_cost_count_raw = mean_costs) %>%
  inner_join(omop_costs_by_month %>%
               select(month_after_index, mean_cost_count_omop = mean_costs),
             by = "month_after_index") %>%
  mutate(
    mean_costs = mean_cost_count_omop - mean_cost_count_raw,
    source = "Difference"
  ) %>%
  select(month_after_index, mean_costs, source)

# Combine into one long-format dataframe
combined_costs <- bind_rows(raw_costs_by_month, omop_costs_by_month, mean_costs_diff)

(plot_costs1 <- ggplot(combined_costs, aes(x = month_after_index, y = mean_costs, color = source)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
    title = "Mean Visits Cost per Month by Data Source",
    x = "Month After Index",
    y = "Mean Monthly Visits Cost Healthcare Service User",
    color = "Data Source"
  ) +
    scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c",
                                  "Difference" = "#595959")) +
    scale_y_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 140)) +
    theme_minimal() +
    theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ))

# ggsave(here::here(
#   "Comparison Analysis", "Results", "Plots",
#   "Mean_costs_per_specialty_unweighted.PNG"),
#   plot = plot_costs1,
#   width = 6.5,
#   height = 5,
#   bg = "white"
# )

### PLOT exposure weighted ------
# Step 0: Filter and calculate month_end
# Sense check: 23520018,243220018
adjusted_costs_data_r <- visit_data_reduced_min_cost_raw %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(
    # Calculate month index
    month_after_index = trunc((as.numeric(difftime(consdate, index_date, units = "days")) / 30.44) + 1),
    
    # Fix class conflict with as.Date()
    month_start = as.Date(index_date) + as.difftime((month_after_index - 1) * 30.44, units = "days"),
    month_end   = as.Date(index_date) + as.difftime(month_after_index * 30.44, units = "days"),
    
    # Exposure calculations
    exposure_end = as.Date(follow_up_end),
    days_exposed_in_month = as.numeric(pmin(exposure_end, month_end) - pmax(as.Date(index_date), month_start) + 1),
    
    # Contribution flags
    fully_contributed = days_exposed_in_month >= 30.44,
    half_contributed  = days_exposed_in_month >= 15.22,
    died_in_month     = !is.na(death_date) & death_date >= month_start & death_date <= month_end
  ) %>%
  group_by(patid, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    days_exposed_in_month = first(days_exposed_in_month),
    fully_contributed = first(fully_contributed),
    half_contributed  = first(half_contributed),
    died_in_month     = first(died_in_month),
    unit_cost         = first(unit_cost),
    .groups = "drop"
  ) %>%
  filter(half_contributed == TRUE) %>%
  mutate(
    adjusted_visits = case_when(
      fully_contributed ~ visit_count,
      died_in_month     ~ visit_count,
      TRUE              ~ (visit_count / days_exposed_in_month) / 30.44
    ),
    visit_costs = adjusted_visits * unit_cost
  )

# visit_data_reduced_user_c_r2 <- visit_data_reduced_min_cost_raw %>%
#   filter(specialty_reduced != "Not Clinically Relevant") %>%
#   mutate(month_after_index = trunc((as.numeric(difftime(consdate, index_date,units = "days"))/30.44) + 1)) %>%
#   mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44))) %>% 
#   group_by(patid) %>% 
#   filter(full_month_after_index >= month_after_index) %>%
#   ungroup() %>% 
#   group_by(patid, month_after_index, specialty_reduced) %>%
#   summarise(
#     visit_count = n(),
#     visit_costs = visit_count * unit_cost,
#     .groups = "drop"
#   ) %>% 
#   distinct(patid, month_after_index, specialty_reduced, visit_costs, 
#            .keep_all = TRUE)

# Step 1: Total cost per patient per month
total_costs_per_patient_month_r2 <- adjusted_costs_data_r %>%
  group_by(patid, month_after_index) %>%
  summarise(total_costs = sum(visit_costs), .groups = "drop")

# Step 2: Mean cost per month
mean_costs_per_month_r2 <- total_costs_per_patient_month_r2 %>%
  group_by(month_after_index) %>%
  summarise(
    mean_costs = mean(total_costs),
    .groups = "drop"
  )

# visit_data_reduced_user_c_r2 <- adjusted_costs_data_r %>%
#   filter(specialty_reduced != "Not Clinically Relevant") %>%
#   mutate(month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date,units = "days"))/30.44) + 1)) %>%
#   mutate(full_month_after_index = trunc((as.numeric(difftime(follow_up_end, index_date,units = "days"))/30.44))) %>% 
#   group_by(subject_id) %>% 
#   filter(full_month_after_index >= month_after_index) %>%  # Fully contributing patients only
#   # removed 29,293 rows
#   ungroup() %>%
#   group_by(subject_id, month_after_index, specialty_reduced) %>%
#   summarise(
#     visit_count = n(),
#     visit_costs = visit_count * unit_cost,
#     .groups = "drop"
#   ) %>% 
#   distinct(subject_id, month_after_index, specialty_reduced, visit_costs, 
#            .keep_all = TRUE)

# Omop
adjusted_costs_data_o <- visit_data_reduced_min_cost_omop %>%
  filter(specialty_reduced != "Not Clinically Relevant",
         user_status == "User") %>%
  mutate(
    # Calculate month index
    month_after_index = trunc((as.numeric(difftime(visit_detail_start_date, index_date, units = "days")) / 30.44) + 1),
    
    # Fix class conflict with as.Date()
    month_start = as.Date(index_date) + as.difftime((month_after_index - 1) * 30.44, units = "days"),
    month_end   = as.Date(index_date) + as.difftime(month_after_index * 30.44, units = "days"),
    
    # Exposure calculations
    exposure_end = as.Date(follow_up_end),
    days_exposed_in_month = as.numeric(pmin(exposure_end, month_end) - pmax(as.Date(index_date), month_start) + 1),
    
    # Contribution flags
    fully_contributed = days_exposed_in_month >= 30.44,
    half_contributed  = days_exposed_in_month >= 15.22,
    died_in_month     = !is.na(death_date) & death_date >= month_start & death_date <= month_end
  ) %>%
  group_by(subject_id, month_after_index, specialty_reduced) %>%
  summarise(
    visit_count = n(),
    days_exposed_in_month = first(days_exposed_in_month),
    fully_contributed = first(fully_contributed),
    half_contributed  = first(half_contributed),
    died_in_month     = first(died_in_month),
    unit_cost         = first(unit_cost),
    .groups = "drop"
  ) %>%
  filter(half_contributed == TRUE) %>%
  mutate(
    adjusted_visits = case_when(
      fully_contributed ~ visit_count,
      died_in_month     ~ visit_count,
      TRUE              ~ (visit_count / days_exposed_in_month) / 30.44
    ),
    visit_costs = adjusted_visits * unit_cost
  )

# Step 1: Total cost per patient per month
total_costs_per_patient_month_o2 <- adjusted_costs_data_o %>%
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

mean_costs_diff2 <- raw_costs_by_month2 %>%
  select(month_after_index, mean_cost_count_raw = mean_costs) %>%
  inner_join(omop_costs_by_month2 %>%
               select(month_after_index, mean_cost_count_omop = mean_costs),
             by = "month_after_index") %>%
  mutate(
    mean_costs = mean_cost_count_omop - mean_cost_count_raw,
    source = "Difference"
  ) %>%
  select(month_after_index, mean_costs, source)

# Combine into one long-format dataframe
combined_costs2 <- bind_rows(raw_costs_by_month2, omop_costs_by_month2, mean_costs_diff2)

(plot_costs2 <- ggplot(combined_costs2, aes(x = month_after_index, y = mean_costs, color = source)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = "Mean Visits Cost per Month by Data Source",
      x = "Month After Index",
      y = "Mean Monthly Visits Cost Healthcare Service User",
      caption = "Visit counts were adjusted for partial month exposure when patients contributed ≥15.22 days. \nCounts were scaled by exposure days, except for full-month contributions or if the patient died during the month. \nRecords with <15.22 days were excluded.",
      color = "Data Source"
    ) +
    scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c",
                                  "Difference" = "#595959")) +
    scale_y_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 140)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    ))

# ggsave(here::here(
#   "Comparison Analysis", "Results", "Plots",
#   "Mean_costs_per_specialty_weighted.PNG"),
#   plot = plot_costs2,
#   width = 7.5,
#   height = 6,
#   bg = "white"
# )


# Specialty count ----
specialty_count_r <- visits_data_raw |> 
  group_by(specialty) |> 
  summarise(n_pts = n_distinct(patid),
            n_rows = n(),
            .groups = "drop") |>
  arrange(desc(n_pts))

# export
rio::export(specialty_count_r, here("Comparison Analysis", "Results", "specialty_counts_source.xlsx"))

specialty_count_o <- visit_data_omop |> 
  group_by(specialty) |> 
  summarise(n_pts = n_distinct(subject_id),
            n_rows = n(),
            .groups = "drop") |>
  arrange(desc(n_pts))  
rio::export(specialty_count_o, here("Comparison Analysis", "Results", "specialty_counts_omop.xlsx"))


# Plots
top_20_r <- specialty_count_r |> 
  slice_head(n = 20)

library(viridis)
library(scales)
library(patchwork)

(top_20_plot_r <- ggplot(top_20_r, aes(x = reorder(specialty, n_pts,
                                                   decreasing = TRUE), y = n_pts, fill = specialty)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = comma(n_pts)), 
            hjust = -0.1, size = 3.2) +
  coord_flip() +
    labs(
    x = "Specialty",
    y = "Number of patients",
    title = "Source"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)),
                     limits = c(0, 25000)) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ))



# Plots
top_20_o <- specialty_count_o |> 
  slice_head(n = 20)

(top_20_plot_o <- ggplot(top_20_o, 
                         aes(x = reorder(specialty, n_pts), 
                             y = n_pts, 
                             fill = specialty)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = comma(n_pts)), 
              hjust = -0.1, size = 3.2) +
    coord_flip() +
    labs(
      x = "Specialty",
      y = "Number of patients",
      title = "OMOP"
    ) +
    scale_y_continuous(labels = comma, 
                       expand = expansion(mult = c(0, 0.15)),
                       limits = c(0, 25000)) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black")
    ))

(top_20_plot <- top_20_plot_r + top_20_plot_o + 
    plot_annotation('Top 20 Specialty',
                    caption = 'Healthcare Specialties',
                    theme=theme(plot.title=element_text(hjust=0.5))))
# ggsave(here::here(
#   "Comparison Analysis", "Results", "Plots",
#   "top_20_plot.PNG"),
#   plot = top_20_plot,
#   width = 12.5,
#   height = 7,
#   bg = "white"
# )

# Unadjusted plots ----
## Visits ----
# Add a source label to each
mean_visits_raw  <- mean_visits_combined_r  %>% mutate(source = "Source")
mean_visits_omop <- mean_visits_combined_o %>% mutate(source = "OMOP")

# Combine into one long-format dataframe
combined_mean_visits <- bind_rows(mean_visits_raw, mean_visits_omop)

library(ggplot2)

# Plot
(plot1 <- ggplot(combined_mean_visits, 
                 aes(x = month_after_index, 
                     y = mean_visits_boot, color = source)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.0) +
    geom_errorbar(aes(ymin=lower_ci,
                    ymax=upper_ci), width = 0.4)+
    labs(
      title = "Mean Primary Care Visits by Data Source",
      x = "Month After Index Fracture",
      y = "Mean Primary Care Visits",
      color = "Data Source"
    ) +
    scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c")) +
    scale_y_continuous(breaks = seq(0, 3, by = 1), limits = c(0, 3)) +
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
  "Mean_visits_per_data_source_unadjusted_no_washout.PNG"),
  plot = plot1,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)

## Costs ----
raw_costs_by_month <- mean_costs_combined_r |> 
  mutate(source = "Source")

omop_costs_by_month <- mean_costs_combined_o |> 
  mutate(source = "OMOP")

# Combine into one long-format dataframe
combined_costs <- bind_rows(raw_costs_by_month, omop_costs_by_month)

(plot_costs1 <- ggplot(combined_costs, 
                       aes(x = month_after_index, 
                           y = mean_costs, color = source)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.0) +
    geom_errorbar(aes(ymin=lower_ci,
                      ymax=upper_ci), width = 0.4)+
    labs(
      title = "Mean Costs of Primary Care Visit by Data Source",
      x = "Month After Index Fracture",
      y = "Mean Costs of Primary Care Visits (£)",
      color = "Data Source"
    ) +
    scale_color_manual(values = c("Source" = "#377eb8", "OMOP" = "#e41a1c")) +
    scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
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
  "Mean_costs_per_data_source_unadjusted_no_washout.PNG"),
  plot = plot_costs1,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)

## Distribution of visits ----
visits_raw <- visits_data_raw |> 
  group_by(patid, specialty) |> 
  summarise(visits = n()) |>
  ungroup() |> 
  mutate(visits = if_else(specialty == "Not Clinically Relevant", 0L, visits)) |> 
  group_by(patid) |> 
  summarise(tot_visits = sum(visits)) |> 
  ungroup() |> 
  mutate(data_source = "Source") |> 
  select(data_source, tot_visits)

visits_omop <- visit_data_omop |> 
  group_by(subject_id, specialty) |> 
  summarise(visits = n()) |>
  ungroup() |> 
  mutate(visits = if_else(specialty == "Not Clinically Relevant", 0L, visits)) |> 
  group_by(subject_id) |> 
  summarise(tot_visits = sum(visits)) |> 
  ungroup() |> 
  mutate(data_source = "OMOP")|> 
  select(data_source, tot_visits) 

visits_df <- bind_rows(visits_raw, visits_omop)

(hist_visits_plot <- ggplot(visits_df, 
                            aes(x = tot_visits, 
                                fill = data_source,
                                colour = data_source)) +
    geom_histogram(binwidth = 2, alpha = 0.7) +
    guides(colour = "none") +
    theme_minimal() +
    labs(
      title = "Distribution of Total Primary Care Visits by Data Source",
      x = "Total Primary Care Visits (bin width = 2)",
      y = "Count of Patients",
      fill = "Data Source",
      caption = "Each bar represents a 2-visit range (e.g., 0–1, 2–3, ...) \n up to the range of the data source."
    ) +
    theme(
      axis.title = element_text(hjust = 0.5, size = 12),
      plot.title = element_text(face = "bold", size = 14)
    ))

ggsave(here::here(
  "Comparison Analysis", "Results", "Plots",
  "Tot_visits_per_data_source_unadjusted_no_washout-hist.PNG"),
  plot = hist_visits_plot,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)

visits_raw.1 <- visits_data_raw |> 
  group_by(patid, specialty) |> 
  summarise(visits = n()) |>
  ungroup() |> 
  mutate(visits = if_else(specialty == "Not Clinically Relevant", 0L, visits)) |> 
  group_by(patid) |> 
  summarise(tot_visits = sum(visits)) |> 
  ungroup() |> 
  group_by(tot_visits) |> 
  summarise(n_pts = n()) |> 
  mutate(data_source = "Source") |> 
  select(data_source,n_pts, tot_visits)

visits_omop.1 <- visit_data_omop |> 
  group_by(subject_id, specialty) |> 
  summarise(visits = n()) |>
  ungroup() |> 
  mutate(visits = if_else(specialty == "Not Clinically Relevant", 0L, visits)) |> 
  group_by(subject_id) |> 
  summarise(tot_visits = sum(visits)) |> 
  ungroup() |>
  group_by(tot_visits) |> 
  summarise(n_pts = n()) |>
  mutate(data_source = "OMOP")|> 
  select(data_source, n_pts, tot_visits) 

visits_df.1 <- bind_rows(visits_raw.1, visits_omop.1)

(line_visits_plot <- ggplot(visits_df.1, 
                           aes(x = tot_visits, y = n_pts, colour = data_source)) +
  geom_line(aes(linetype = data_source), size = 0.8) +
  # geom_point(aes(shape = data_source), size = 1.0) +
  theme_minimal() +
  labs(
    title = "Distribution of Total Primary Care Visits by Data Source",
    x = "Total Primary Care Visits",
    y = "Count of Patients"
  ) +
  scale_colour_discrete(name = "Data Source") +
  scale_linetype_discrete(name = "Data Source") +
  scale_shape_discrete(name = "Data Source") +
  theme(
    axis.title = element_text(hjust = 0.5, size = 12),
    plot.title = element_text(face = "bold", size = 14)
  ))

ggsave(here::here(
  "Comparison Analysis", "Results", "Plots",
  "Tot_visits_per_data_source_unadjusted_no_washout-line.PNG"),
  plot = line_visits_plot,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)

(line_visits_plot_f <- ggplot(visits_df.1, 
                            aes(x = tot_visits, y = n_pts)) +
    geom_line(aes(linetype = data_source), 
              size = 0.8,
              colour = "#1874CD") +
    # geom_point(aes(shape = data_source), 
    #            size = 1.0,
    #            colour = "#1874CD") +
    theme_minimal() +
    labs(
      title = "Distribution of Total Primary Care Visits by Data Source",
      x = "Total Primary Care Visits",
      y = "Count of Patients",
      fill = "Data Source"
    ) +
    theme(
      axis.title = element_text(hjust = 0.5, size = 12),
      plot.title = element_text(face = "bold", size = 14)
    ) +
    facet_wrap(~data_source))

ggsave(here::here(
  "Comparison Analysis", "Results", "Plots",
  "Tot_visits_per_data_source_unadjusted_no_washout-line_faceted.PNG"),
  plot = line_visits_plot_f,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)

(density_plot_v <- ggplot(data = visits_df, 
                          aes(
                            x = tot_visits,
                            fill = data_source,
                            colour = data_source)) +
    geom_density() +
    guides(colour = "none") +
    theme_minimal() +
    labs(
      title = "Distribution of Total Primary Care Visitsby Data Source",
      x = "Total Primary Care Visits",
      y = "Count of Patients",
      fill = "Data Source"
    ) +
    theme(
      axis.title = element_text(hjust = 0.5, size = 12),
      plot.title = element_text(face = "bold", size = 14)
    ))

ggsave(here::here(
  "Comparison Analysis", "Results", "Plots",
  "Tot_visits_per_data_source_unadjusted_no_washout-dens.PNG"),
  plot = density_plot_v,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)

# (combined_plot <- ggplot(visits_df, 
#                         aes(x = tot_visits, fill = data_source, colour = data_source)) +
#   geom_histogram(binwidth = 2, alpha = 0.4, position = "identity") +  # Slightly transparent
#   geom_density(alpha = 0.8, linewidth = 1.1) +                         # Add smoothed lines
#   guides(colour = "none") +
#   theme_minimal() +
#   labs(
#     title = "Distribution of Total Visits by Data Source",
#     x = "Total Visits",
#     y = "Count of Patients",
#     fill = "Data Source",
#     caption = "Each bar represents a 2-visit range (e.g., 0–2, 2–4, ...) up to the range of the data source."
#   ) +
#   theme(
#     axis.title = element_text(hjust = 0.5, size = 12),
#     plot.title = element_text(face = "bold", size = 14)
#   ))

## Distribution of visit costs ----
costs_raw <- visits_data_raw |> 
  group_by(patid, specialty) |> 
  summarise(visits = n(), 
            visits_costs = visits * unit_cost) |>
  ungroup() |> 
  mutate(visits_costs = if_else(specialty == "Not Clinically Relevant", 0L, visits_costs)) |>
  distinct() |> 
  group_by(patid) |> 
  summarise(tot_costs = sum(visits_costs)) |> 
  ungroup() |> 
  mutate(data_source = "Source") |> 
  select(data_source, tot_costs)

costs_omop <- visit_data_omop |> 
  group_by(subject_id, specialty) |> 
  summarise(visits = n(), 
            visits_costs = visits * unit_cost) |>
  ungroup() |> 
  mutate(visits_costs = if_else(specialty == "Not Clinically Relevant", 0L, visits_costs)) |> 
  distinct() |>
  group_by(subject_id) |> 
  summarise(tot_costs = sum(visits_costs)) |> 
  ungroup() |> 
  mutate(data_source = "OMOP")|> 
  select(data_source, tot_costs) 

costs_df <- bind_rows(costs_raw, costs_omop)
options(scipen = 999)
(hist_costs_plot <- ggplot(costs_df, 
                           aes(x = tot_costs, 
                               fill = data_source,
                               colour = data_source)) +
    geom_histogram(binwidth = 100, alpha = 0.7) +
    guides(colour = "none") +
    theme_minimal() +
    labs(
      title = "Distribution of Total Visit Costs by Data Source",
      x = "Total Costs of Primary Care Visits (£) (bin width = 100)",
      y = "Count of Patients",
      fill = "Data Source",
      caption = "Each bar represents a 100-visit costs range (e.g., 0–100, 101-200, ...) \n up to the range of the data source."
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma) +
    theme(
      axis.title = element_text(hjust = 0.5, size = 12),
      plot.title = element_text(face = "bold", size = 14)
    ))
ggsave(here::here(
  "Comparison Analysis", "Results", "Plots",
  "Tot_costs_per_data_source_unadjusted_no_washout-hist.PNG"),
  plot = hist_costs_plot,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)
(density_plot_c <- ggplot(costs_df, 
                          aes(x = tot_costs, 
                              fill = data_source,
                              colour = data_source)) +
    geom_density() +
    guides(colour = "none") +
    theme_minimal() +
    labs(
      title = "Distribution of Total Visit Costs by Data Source",
      x = "Total Costs of Primary Care Visits (£)",
      y = "Density",
      fill = "Data Source"
    ) +
    # scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma) +
    theme(
      axis.title = element_text(hjust = 0.5, size = 12),
      plot.title = element_text(face = "bold", size = 14)
    ))
ggsave(here::here(
  "Comparison Analysis", "Results", "Plots",
  "Tot_costs_per_data_source_unadjusted_no_washout-dens.PNG"),
  plot = density_plot_c,
  width = 6.5,
  height = 5,
  dpi = 300,
  bg = "white"
)

# (combined_plot <- ggplot(visits_df, 
#                          aes(x = tot_visits, fill = data_source, colour = data_source)) +
#     geom_histogram(binwidth = 2, alpha = 0.4, position = "identity") +  # Slightly transparent
#     geom_density(alpha = 0.8, linewidth = 1.1) +                         # Add smoothed lines
#     guides(colour = "none") +
#     theme_minimal() +
#     labs(
#       title = "Distribution of Total Visits by Data Source",
#       x = "Total Visits",
#       y = "Count of Patients",
#       fill = "Data Source",
#       caption = "Each bar represents a 2-visit range (e.g., 0–2, 2–4, ...) up to the range of the data source."
#     ) +
#     theme(
#       axis.title = element_text(hjust = 0.5, size = 12),
#       plot.title = element_text(face = "bold", size = 14)
#     ))