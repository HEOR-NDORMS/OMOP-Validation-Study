# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Differences between OMOP and Raw using cohort data
# Date: 05.02.2025
# Last date edited: 05.02.2025
# ****************************************************************************** #

# Load packages ----
library(here)
library(dplyr)
library(ggplot2)
library(VennDiagram)
library(patchwork)
library(eulerr)
library(tidylog)

# Import data ----
# Raw data that has been saved from the CleanCohort_Raw.R
cohort_table_raw <- rio::import(here("CPRD Raw Analysis", "Results","cohort_table_raw.xlsx")) %>% 
  mutate(patid = bit64::as.integer64(patid))

glimpse(cohort_table_raw)

# Load the OMOP data from the CleanCohort_OMOP.R
cohort_table_omop <- cohort_table

glimpse(cohort_table_omop)

# Count of patients ----
(omop_cohort <- n_distinct(cohort_table_omop$subject_id))
(raw_cohort <- n_distinct(cohort_table_raw$patid))

# Identify overlapping and missing subjects ----
overlap_subjects <- intersect(cohort_table_omop$subject_id, cohort_table_raw$patid)
only_in_omop <- setdiff(cohort_table_omop$subject_id, cohort_table_raw$patid)
only_in_raw <- setdiff(cohort_table_raw$patid, cohort_table_omop$subject_id)	


cat("Subjects in both datasets:", length(overlap_subjects), "\n")
cat("Subjects only in OMOP:", length(only_in_omop), "\n")
cat("Subjects only in Raw:", length(only_in_raw), "\n") 

# Compute values
overlap_count <- length(overlap_subjects)   # Subjects in both OMOP and Raw
only_in_omop_count <- length(only_in_omop)  # Subjects only in OMOP
only_in_raw_count <- length(only_in_raw)    # Subjects only in Raw

# Total counts
total_omop <- overlap_count + only_in_omop_count  # Total in OMOP
total_raw <- overlap_count + only_in_raw_count    # Total in Raw
total_both <- total_omop + only_in_raw_count      # Total in OMOP + Raw

# Create a contingency table with row/column totals
(contingency_table <- matrix(c(
  only_in_omop_count, overlap_count, total_omop,
  overlap_count, only_in_raw_count, total_raw,
  total_omop, total_raw, total_both
), 
nrow = 3, byrow = TRUE, 
dimnames = list(
  c("OMOP", "Raw", "Total"),
  c("OMOP", "Raw", "Total")
)))

# Export 
contingency_table_df <- as.data.frame(contingency_table)
rio::export(contingency_table_df, here::here("contigency_table_cohort_comparison.xlsx"))

## Venn Diagram ----
venn_plot <- draw.pairwise.venn(area1 = nrow(cohort_table_omop), area2 = nrow(cohort_table_raw), cross.area = length(overlap_subjects),
                                category = c("OMOP", "Raw"), fill = c("#1E90FF", "#FF34B3"), alpha = 0.4)
grid.draw(venn_plot)

# Compare cohort/aurum start and end dates ----
cohort_table_omop <- cohort_table_omop %>% 
  mutate(cohort_start_date = as.Date(cohort_start_date, format = "%Y-%m-%d"),
         cohort_end_date = as.Date(cohort_end_date, format = "%Y-%m-%d"),
         condition_start_date = as.Date(condition_start_date, format = "%Y-%m-%d"))

cohort_table_raw <- cohort_table_raw %>% 
  mutate(aurum_min_accept_date = as.Date(aurum_min_accept_date, format = "%Y-%m-%d"),
         aurum_max_accept_date = as.Date(aurum_max_accept_date, format = "%Y-%m-%d"),
         fx_date = as.Date(fx_date, format = "%Y-%m-%d"))

summary(cohort_table_omop$cohort_start_date)
summary(cohort_table_raw$aurum_min_accept_date)
summary(cohort_table_omop$cohort_end_date)
summary(cohort_table_raw$aurum_max_accept_date)


# Visualization: Distribution of cohort start dates
ggplot() +
  geom_density(data = cohort_table_omop, aes(x = cohort_start_date), fill = "blue", alpha = 0.4) +
  geom_density(data = cohort_table_raw, aes(x = aurum_min_accept_date), fill = "red", alpha = 0.1) +
  labs(title = "Comparison of Cohort Start Dates", x = "Cohort Start Date", y = "Density") +
  theme(
    legend.position = "top"
  )

# Overlap Records ----
overlap_df <- inner_join(cohort_table_omop %>%
                           dplyr::rename("death_date_omop" = "death_date"),
                         cohort_table_raw %>% 
                                select(patid, aurum_min_accept_date, 
                                       aurum_max_accept_date, site, fx_date, 
                                       yob, death_date) %>% 
                           rename("death_date_raw" = "death_date"), 
                              by = c("subject_id" = "patid"))

## Compare cohort/aurum start and end dates ---- 
summary(overlap_df$cohort_start_date)
summary(overlap_df$aurum_min_accept_date)
summary(overlap_df$cohort_end_date)
summary(overlap_df$aurum_max_accept_date)

# Visualization: Distribution of cohort start dates
(dp_o <- ggplot() +
    geom_density(data = overlap_df, aes(x = cohort_start_date, y = ..density..), 
                 fill = "#69b3a2", alpha = 0.6) +
    geom_label(aes(x = as.Date("1990-01-01"), y = 0.0015, label="Start date"), color="#69b3a2") +
    geom_density(data = overlap_df, aes(x = cohort_end_date, y = ..density..), 
                 fill = "#404080", alpha = 0.6) +
    geom_label(aes(x = as.Date("2011-01-01"), y = 0.0015, label="End date"), color="#404080") +
    xlim(as.Date("1910-07-02", format = "%Y-%m-%d"), as.Date("2022-03-21", format = "%Y-%m-%d")) +
    labs(title = "OMOP", x = "Cohort Start Date", y = "Density") +
    theme_minimal())

(dp_r <- ggplot() +
    geom_density(data = overlap_df, aes(x = aurum_min_accept_date, y = ..density..), 
                 fill = "#69b3a2", alpha = 0.6) +
    geom_label(aes(x = as.Date("1990-01-01"), y = 0.0015, label="Start date"), color="#69b3a2") +
    geom_density(data = overlap_df, aes(x = aurum_max_accept_date, y = ..density..), 
                 fill = "#404080", alpha = 0.6) +
    geom_label(aes(x = as.Date("2011-01-01"), y = 0.0015, label="End date"), color="#404080") +
    xlim(as.Date("1910-07-02", format = "%Y-%m-%d"), as.Date("2022-03-21", format = "%Y-%m-%d"))+
    labs(title = "Raw", x = "Cohort Start Date", y = "Density") +
    theme_minimal()
)

(cohort_date_plot <- dp_o + dp_r + plot_annotation(title = "Comparison of Cohort start and end dates at the records level: OMOP vs Raw"))


## Assess Age ----
# Calculate Age at Fracture date

overlap_df.1 <- overlap_df %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  mutate(fracture_site = recode(fracture_site, NHNS = "NHNV")) %>% 
  mutate(site = recode(site, hip = "Hip", vert = "Vertebra", nhnv = "NHNV")) %>% 
  mutate(age_fracture_o = lubridate::year(condition_start_date) - year_of_birth) %>%
  mutate(age_fracture_r = lubridate::year(fx_date) - yob) %>%
  select(!c(gender_concept_id, month_of_birth, day_of_birth, birth_datetime,
            race_concept_id, ethnicity_concept_id, location_id, provider_id, care_site_id,
            person_source_value, gender_source_value, gender_source_concept_id, 
            race_source_value, race_source_concept_id, ethnicity_source_value,
            ethnicity_source_concept_id)) %>% 
  mutate(age_diff = age_fracture_o - age_fracture_r)

# Boxplot to compare Age at Fracture
(stats_o <- overlap_df.1 %>%
  summarise(
    Q1 = quantile(age_fracture_o, 0.25),
    Q3 = quantile(age_fracture_o, 0.75),
    IQR = IQR(age_fracture_o),
    lower_whisker = Q1 - 1.5 * IQR,
    upper_whisker = Q3 + 1.5 * IQR,
    median = median(age_fracture_o)
  ))

(bp_plot_o <- ggplot() +
    geom_boxplot(data = overlap_df.1, aes(x = "OMOP", y = age_fracture_o), fill = "#FFC1C1", alpha = 0.5, outlier.color = "black") +
    labs(title = "",
         y = "Age at Fracture",
         x = "") +
    scale_y_continuous(limits = c(0, 130)) +
    theme_minimal() +
    geom_text(data = stats_o, aes(x = 1, y = lower_whisker, label = paste0("lower whisker:", round(lower_whisker, 1))), vjust = 1.5, color = "#104E8B") +
    geom_text(data = stats_o, aes(x = 1, y = upper_whisker, label = paste0("upper whisker:", round(upper_whisker, 1))), vjust = -0.5, color = "#104E8B") +
    geom_text(data = stats_o, aes(x = 1, y = Q1, label = paste0("Q1:", round(Q1, 1))), vjust = 1.5, color = "#8B1A1A") +
    geom_text(data = stats_o, aes(x = 1, y = Q3, label = paste0("Q3:", round(Q3, 1))), vjust = -0.5, color = "#8B1A1A") +
    geom_text(data = stats_o, aes(x = 1, y = median, label = paste0("Median:", round(median, 1))), vjust = -0.5, color = "#458B00")) 

stats_r <- overlap_df.1 %>%
  summarise(
    Q1 = quantile(age_fracture_r, 0.25),
    Q3 = quantile(age_fracture_r, 0.75),
    IQR = IQR(age_fracture_r),
    lower_whisker = Q1 - 1.5 * IQR,
    upper_whisker = Q3 + 1.5 * IQR,
    median = median(age_fracture_r)
  )

(bp_plot_r <- ggplot() +
    geom_boxplot(data = overlap_df.1, aes(x = "Raw", y = age_fracture_r), fill = "plum1", alpha = 0.5, outlier.color = "black") +
    labs(title = "",
         y = "Age at Fracture",
         x = "") +
    scale_y_continuous(limits = c(0, 130)) +
    theme_minimal() +
    geom_text(data = stats_r, aes(x = 1, y = lower_whisker, label = paste0("lower whisker:", round(lower_whisker, 1))), vjust = 1.5, color = "#104E8B") +
    geom_text(data = stats_r, aes(x = 1, y = upper_whisker, label = paste0("upper whisker:", round(upper_whisker, 1))), vjust = -0.5, color = "#104E8B") +
    geom_text(data = stats_r, aes(x = 1, y = Q1, label = paste0("Q1:", round(Q1, 1))), vjust = 1.5, color = "#8B1A1A") +
    geom_text(data = stats_r, aes(x = 1, y = Q3, label = paste0("Q3:", round(Q3, 1))), vjust = -0.5, color = "#8B1A1A") +
    geom_text(data = stats_r, aes(x = 1, y = median, label = paste0("Median:", round(median, 1))), vjust = -0.5, color = "#458B00"))

(age_plot <- bp_plot_o + bp_plot_r + plot_annotation(title = "Comparison of Age at Fracture: OMOP vs Raw"))


## for the same fx date, same site, different ages? ----
mismatched_ages_1 <- overlap_df.1 %>%
  filter(condition_start_date == fx_date,
         fracture_site == site,
         age_fracture_o != age_fracture_r,
         ) %>%
  select(subject_id,condition_concept_id, condition_start_date, fx_date,
         fracture_site, site, year_of_birth, yob, age_fracture_o, age_fracture_r)

n_distinct(mismatched_ages_1$subject_id)

## How many patients and % had different ages for the same fracture? ---- 

mismatched_ages_2 <- overlap_df.1 %>%
  filter(fracture_site == site,
         age_fracture_o != age_fracture_r) %>%
  select(subject_id,condition_concept_id, condition_start_date, fx_date,
         fracture_site, site,
         year_of_birth, yob, age_fracture_o, age_fracture_r) 
n_distinct(mismatched_ages_2$subject_id)

## How many patients and % had same site for different dates? ----
mismatched_fx_1 <- overlap_df.1 %>%
  filter(
    fracture_site == site,
    condition_start_date != fx_date) %>%
  select(subject_id,condition_concept_id, condition_start_date, fx_date,
         fracture_site, site) 
n_distinct(mismatched_fx_1$subject_id)

## How many patients and % had same dates for the same site? ----
mismatched_fx_2 <- overlap_df.1 %>%
  filter(
    fracture_site == site,
    condition_start_date == fx_date) %>%
  select(subject_id,condition_concept_id, condition_start_date, fx_date,
         fracture_site, site) 
n_distinct(mismatched_fx_2$subject_id)

## How many patients and % had same dates for the different site? ----
mismatched_fx_3 <- overlap_df.1 %>%
  filter(
    condition_start_date == fx_date,
    fracture_site != site
    ) %>%
  select(subject_id,condition_concept_id, condition_start_date, fx_date,
         fracture_site, site) 
n_distinct(mismatched_fx_3$subject_id)


## Assessing death date ----
overlap_df.1 <- overlap_df.1 %>% 
  mutate(death_date_raw = as.Date(death_date_raw, format = "%Y-%m-%d"))

# Calculate mean and median
mean_death_date_omop <- as.Date(mean(overlap_df.1$death_date_omop, na.rm = T), format = "%Y-%m-%d")
median_death_date_omop <- as.Date(median(overlap_df.1$death_date_omop, na.rm = T), format = "%Y-%m-%d")

(hp_o <- ggplot(data = overlap_df.1, aes(x = death_date_omop)) +
    geom_histogram(bins = 30, fill = "darkred", alpha = 0.6) +
    geom_vline(aes(xintercept = as.numeric(mean_death_date_omop)), color = "#104E8B", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = as.numeric(median_death_date_omop)), color = "#008B00", linetype = "dotted", size = 1) +
    annotate("text", x = mean_death_date_omop, y = Inf, label = paste("Mean:", mean_death_date_omop), color = "#104E8B", vjust = 2, hjust = 1.1) +
    annotate("text", x = median_death_date_omop, y = Inf, label = paste("Median:", median_death_date_omop), color = "#008B00", vjust = 2, hjust = -0.1) +
    labs(title = "OMOP",
         x = "Death Date",
         y = "Frequency") +
    # scale_y_continuous(limits = c(0, 700)) +
    theme_minimal())

mean_death_date_raw <- as.Date(mean(overlap_df.1$death_date_raw, na.rm = T), format = "%Y-%m-%d")
median_death_date_raw <- as.Date(median(overlap_df.1$death_date_raw, na.rm = T), format = "%Y-%m-%d")

(hp_r <- ggplot(data = overlap_df.1, aes(x = death_date_raw)) +
    geom_histogram(bins = 30, fill = "#551A8B", alpha = 0.6) +
    geom_vline(aes(xintercept = as.numeric(mean_death_date_raw)), color = "#104E8B", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = as.numeric(median_death_date_raw)), color = "#008B00", linetype = "dotted", size = 1) +
    annotate("text", x = mean_death_date_raw, y = Inf, label = paste("Mean:", mean_death_date_raw), color = "#104E8B", vjust = 2, hjust = 1.1) +
    annotate("text", x = median_death_date_raw, y = Inf, label = paste("Median:", median_death_date_raw), color = "#008B00", vjust = 2, hjust = -0.1) +
    labs(title = "Raw",
         x = "Death Date",
         y = "Frequency") +
    # scale_y_continuous(limits = c(0, 700)) +
    theme_minimal())

(death_plot <- hp_o + hp_r + plot_annotation(title = "Comparison of Death Date: OMOP vs Raw"))

## How many patients have different death date in OMOP and Raw?----
mismatched_deaths <- overlap_df.1 %>% 
  group_by(subject_id) %>%
  filter(!is.na(death_date_omop) | !is.na(death_date_raw)) %>% 
  filter(death_date_omop != death_date_raw) %>% 
  ungroup() %>% 
    select(subject_id, death_date_omop, death_date_raw)

mismatched_deaths_1 <- overlap_df.1 %>% 
  group_by(subject_id) %>%
  filter(!is.na(death_date_omop) | !is.na(death_date_raw)) %>% 
  filter(death_date_omop == death_date_raw) %>% 
  ungroup() %>% 
  select(subject_id, death_date_omop, death_date_raw)

## Table 1 ----
# Using gtsummary::tbl_summary, we can summarise the table as follows
(gt_summary_table <- table_one_omop %>% 
   gtsummary::tbl_summary(
     # select the variables of interest
     include = c(age_fracture, fracture_site, follow_up_reason, follow_up_time),
     # rename the variable names
     label = list(
       age_fracture = "Age at fracture occurrence",
       fracture_site = "Fracture site", 
       follow_up_reason = "Censoring event",
       follow_up_time = "Follow up time"
     ),
     # indicate whether we want the missing values to be seen in the summary table
     missing = "no"
   ))  

# Only Raw and Only OMOP ----
                         
only_in_omop_df <- cohort_table_omop %>% 
  filter(subject_id %in% only_in_omop)

only_in_raw_df <- cohort_table_raw %>% 
  filter(patid %in% only_in_raw)

# OR
only_in_omop_df_v2 <- only_in_omop_df %>% 
  select(!c(condition_concept_id, observation_period_end_date)) %>%
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  dplyr::mutate(age_fracture = lubridate::year(condition_start_date) - year_of_birth) %>% 
  select(!c(
    gender_concept_id, year_of_birth, month_of_birth,
    day_of_birth, birth_datetime, race_concept_id, ethnicity_concept_id,
    location_id, provider_id, care_site_id, person_source_value, 
    gender_source_value, gender_source_concept_id, race_source_value,
    race_source_concept_id, ethnicity_source_value, 
    ethnicity_source_concept_id)) %>% 
  mutate(dataset = "OMOP") %>% 
  mutate(fracture_site = recode(fracture_site, NHNS = "NHNV")) %>% 
  mutate(follow_up_reason = case_when(
    follow_up_reason %in% c("after_index, observation_period_end_date, death_date",
                            "death_date",
                            "observation_period_end_date, cancer_date, death_date",
                            "observation_period_end_date, death_date") ~ "Death",
    follow_up_reason == "after_index" ~ "Max of 730 days from index date",
    follow_up_reason == "after_index, cancer_date" ~ "Max of 730 days from index date, Cancer",
    follow_up_reason == "after_index, fracture_after_index" ~ "Max of 730 days from index date, Fracture ensuing incident (index) fracture",
    follow_up_reason == "after_index, mbd_date" ~ "Max of 730 days from index date, Paget’s disease of the bone, or other metabolic bone diseases",
    follow_up_reason == "after_index, observation_period_end_date" ~ "Max of 730 days from index date, End of data collection period",
    follow_up_reason == "cancer_date" ~ "Cancer",
    follow_up_reason == "cancer_date, fracture_after_index" ~ "Cancer, Fracture ensuing incident (index) fracture",
    follow_up_reason == "cancer_date, mbd_date" ~ "Cancer, Paget’s disease of the bone, or other metabolic bone diseases",
    follow_up_reason == "fracture_after_index" ~ "Fracture ensuing incident (index) fracture",
    follow_up_reason == "mbd_date" ~ "Paget’s disease of the bone, or other metabolic bone diseases",
    follow_up_reason == "mbd_date, fracture_after_index" ~ "Paget’s disease of the bone, or other metabolic bone diseases, Fracture ensuing incident (index) fracture",
    follow_up_reason == "observation_period_end_date" ~ "End of data collection period",
    follow_up_reason == "observation_period_end_date, cancer_date" ~ "End of data collection period, Cancer",
    follow_up_reason == "observation_period_end_date, fracture_after_index" ~ "End of data collection period, Fracture ensuing incident (index) fracture")) 
  

only_in_raw_df_v2 <- only_in_raw_df %>% 
  select(!c(gender, yob, death, days_prior_obs, days_after_obs)) %>% 
  rename("subject_id" = "patid",
         "cohort_start_date" = "aurum_min_accept_date",
         "cohort_end_date" = "aurum_max_accept_date",
         "condition_start_date" = "fx_date",
         "fracture_site" = "site",
         "class" = "type",
         "mbd_date" = "paget_fa_index_dt",
         "cancer_date" = "cancer_fa_index_dt") %>% 
  mutate(fracture_site = recode(fracture_site, NHNS = "NHNV")) %>% 
  mutate(follow_up_reason = case_when(
    follow_up_reason %in% c("after_index, observation_period_end_date, death_date",
                            "death_date",
                            "observation_period_end_date, cancer_date, death_date",
                            "observation_period_end_date, death_date") ~ "Death",
    follow_up_reason == "after_index" ~ "Max of 730 days from index date",
    follow_up_reason == "after_index, cancer_date" ~ "Max of 730 days from index date, Cancer",
    follow_up_reason == "after_index, fracture_after_index" ~ "Max of 730 days from index date, Fracture ensuing incident (index) fracture",
    follow_up_reason == "after_index, mbd_date" ~ "Max of 730 days from index date, Paget’s disease of the bone, or other metabolic bone diseases",
    follow_up_reason == "after_index, observation_period_end_date" ~ "Max of 730 days from index date, End of data collection period",
    follow_up_reason == "cancer_date" ~ "Cancer",
    follow_up_reason == "cancer_date, fracture_after_index" ~ "Cancer, Fracture ensuing incident (index) fracture",
    follow_up_reason == "cancer_date, mbd_date" ~ "Cancer, Paget’s disease of the bone, or other metabolic bone diseases",
    follow_up_reason == "fracture_after_index" ~ "Fracture ensuing incident (index) fracture",
    follow_up_reason == "mbd_date" ~ "Paget’s disease of the bone, or other metabolic bone diseases",
    follow_up_reason == "mbd_date, fracture_after_index" ~ "Paget’s disease of the bone, or other metabolic bone diseases, Fracture ensuing incident (index) fracture",
    follow_up_reason == "observation_period_end_date" ~ "End of data collection period",
    follow_up_reason == "observation_period_end_date, cancer_date" ~ "End of data collection period, Cancer",
    follow_up_reason == "observation_period_end_date, fracture_after_index" ~ "End of data collection period, Fracture ensuing incident (index) fracture")) %>% 
  mutate(dataset = "Raw")
  
  
only_omop_raw_df <- rbind(only_in_omop_df_v2, only_in_raw_df_v2)


