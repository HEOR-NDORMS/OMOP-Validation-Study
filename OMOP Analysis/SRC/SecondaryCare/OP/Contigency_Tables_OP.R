#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Differences between OMOP and Raw using visits data
# Date: 26.11.2025
# Last date edited: 26.11.2025 
# ****************************************************************************** #

# Load packages ----
library(here)
library(dplyr)
library(tidylog)
library(rio)

# Import data ----
# Convert IDs to character just to be safe
omop_ids <- appt_data_omop %>% select(subject_id) |> rename("patid" = "subject_id") %>% distinct()
raw_ids  <- appt_data_raw %>% select(patid) %>% distinct()

# Full join to get all unique patient IDs from both datasets
combined <- full_join(omop_ids %>% mutate(in_omop = TRUE),
                      raw_ids  %>% mutate(in_raw = TRUE),
                      by = "patid") %>%
  mutate(
    in_omop = ifelse(is.na(in_omop), FALSE, TRUE),
    in_raw  = ifelse(is.na(in_raw), FALSE, TRUE)
  )

# Create the 2x2 table
(presence_table <- table(Source = combined$in_raw, OMOP = combined$in_omop))
colnames(presence_table)
row.names(presence_table)


# Rename row and column names to "No" and "Yes"
colnames(presence_table) <- c("No", "Yes")
rownames(presence_table) <- c("No", "Yes")
print(presence_table)

# Convert the table to a data frame with OMOP as a column
presence_df <- as.data.frame.matrix(presence_table)
presence_df <- tibble::rownames_to_column(presence_df, var = "Source")  # Now 'OMOP' is a column

# Rename the column names to reflect Raw status
colnames(presence_df)[2:3] <- c("OMOP: No", "OMOP: Yes")

rio::export(presence_df, here::here("OMOP Analysis", "Results", "SecondaryCareAnalysis", "Comparison", "OP", "OP_contigency_table_for_pts.xlsx"))

# 2*2 for user/non-users ----

# Prepare the two datasets: keep only subject_id and user_status
omop_clean <- appt_data_omop %>%
  select(subject_id, user_status) %>%
  mutate(omop_status = user_status) |> 
  select(-user_status) |> 
  distinct(subject_id, omop_status, .keep_all = TRUE)
table(omop_clean$omop_status)

source_clean <- appt_data_raw %>%
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

rio::export(user_df, here("OMOP Analysis", "Results", "SecondaryCareAnalysis", "contigency_table_for_user_status.xlsx"))

