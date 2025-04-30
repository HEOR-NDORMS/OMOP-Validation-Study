library(dplyr)
library(ggplot2)
library(VennDiagram)
library(patchwork)
library(eulerr)

# Import raw data ----
fracture_table_raw <- rio::import(here("CPRD Raw Analysis", "fracture_table_raw.xlsx"))

fracture_table_omop <- fracture_table %>% 
  mutate(subject_id = as.numeric(subject_id)) 


# Find overlapping records ----
overlap_records <- inner_join(fracture_table_1 ,
                              fracture_table_raw %>% 
                                select(patid, aurum_min_accept_date, aurum_max_accept_date, site, fx_date, yob, death_date), 
                              by = c("subject_id" = "patid"),
                              relationship = "many-to-many")
n_distinct(overlap_records$subject_id)

# Count of records
cat("Number of overlapping records:", nrow(overlap_records), "\n")
cat("Number of records only in OMOP:", nrow(fracture_table_1), "\n")
cat("Number of records only in Raw:", nrow(fracture_table_raw), "\n")

# Venn diagram to visualize subject overlap ----


# Define the sizes of the datasets and their intersection
fit_records <- euler(c("OMOP" = nrow(fracture_table_1), 
                       "Raw" = nrow(fracture_table_raw),
                       "OMOP&Raw" = nrow(overlap_records)))

# Plot the Euler diagram
plot(fit_records, fills = list(fill = c("blue", "red"), alpha = 0.5),
     quantities = TRUE, labels = list(font = 2),
     main = "Comparing Fractures: OMOP & Raw")



# Identify overlapping and missing subjects ----
overlap_subjects <- intersect(fracture_table_1$subject_id, fracture_table_raw$patid)
only_in_omop <- setdiff(fracture_table_1$subject_id, fracture_table_raw$patid)
only_in_raw <- setdiff(fracture_table_raw$patid, fracture_table_1$subject_id)	


cat("Subjects in both datasets:", length(overlap_subjects), "\n")
cat("Subjects only in OMOP:", length(only_in_omop), "\n")
cat("Subjects only in Raw:", length(only_in_raw), "\n") 


# Venn diagram to visualize subject overlap ----
# venn.plot <- draw.pairwise.venn(area1 = nrow(fracture_table_1), area2 = nrow(fracture_table_raw), cross.area = length(overlap_subjects),
#                                 category = c("OMOP", "Raw"), fill = c("#1E90FF", "#FF34B3"), alpha = 0.4)
# grid.draw(venn.plot)

# Define the sizes of the datasets and their intersection
fit_pts <- euler(c("OMOP" = length(only_in_omop), 
                   "Raw" = length(only_in_raw), 
                   "OMOP&Raw" = length(overlap_subjects)))


# Plot the Euler diagram
plot(fit_pts, fills = list(fill = c("#1E90FF", "#FF34B3"), alpha = 0.5),
     quantities = TRUE, labels = list(font = 4),
     main = "Comparing Patients: OMOP & Raw")

# Compare cohort/aurum start and end dates ----
fracture_table_1 <- fracture_table_1 %>% 
  mutate(cohort_start_date = as.Date(cohort_start_date, format = "%Y-%m-%d"),
         cohort_end_date = as.Date(cohort_end_date, format = "%Y-%m-%d"),
         condition_start_date = as.Date(condition_start_date, format = "%Y-%m-%d"))

fracture_table_raw <- fracture_table_raw %>% 
  mutate(aurum_min_accept_date = as.Date(aurum_min_accept_date, format = "%Y-%m-%d"),
         aurum_max_accept_date = as.Date(aurum_max_accept_date, format = "%Y-%m-%d"),
         fx_date = as.Date(fx_date, format = "%Y-%m-%d"))

summary(fracture_table_1$cohort_start_date)
summary(fracture_table_raw$aurum_min_accept_date)
summary(fracture_table_1$cohort_end_date)
summary(fracture_table_raw$aurum_max_accept_date)


# Visualization: Distribution of cohort start dates
ggplot() +
  geom_density(data = fracture_table_1, aes(x = cohort_start_date), fill = "blue", alpha = 0.4) +
  geom_density(data = fracture_table_raw, aes(x = aurum_min_accept_date), fill = "red", alpha = 0.4) +
  labs(title = "Comparison of Cohort Start Dates", x = "Cohort Start Date", y = "Density") +
  theme(
    legend.position = "top"
  )		 

# Overlap patients in both omop and raw ----
# Compare condition dates for matched subjects
matched_omop <- fracture_table_1 %>% filter(subject_id %in% overlap_subjects)
matched_raw <- fracture_table_raw %>% filter(patid %in% overlap_subjects)


## Compare cohort/aurum start and end dates ----
matched_omop <- matched_omop %>% 
  mutate(cohort_start_date = as.Date(cohort_start_date, format = "%Y-%m-%d"),
         cohort_end_date = as.Date(cohort_end_date, format = "%Y-%m-%d"),
         condition_start_date = as.Date(condition_start_date, format = "%Y-%m-%d"))

matched_raw <- matched_raw %>% 
  mutate(aurum_min_accept_date = as.Date(aurum_min_accept_date, format = "%Y-%m-%d"),
         aurum_max_accept_date = as.Date(aurum_max_accept_date, format = "%Y-%m-%d"),
         fx_date = as.Date(fx_date, format = "%Y-%m-%d")) 

summary(matched_omop$cohort_start_date)
summary(matched_raw$aurum_min_accept_date)
summary(matched_omop$cohort_end_date)
summary(matched_raw$aurum_max_accept_date)

# Visualization: Distribution of cohort start dates
(dp_o <- ggplot() +
    geom_density(data = matched_omop, aes(x = cohort_start_date, y = ..density..), 
                 fill = "#69b3a2", alpha = 0.6) +
    geom_label(aes(x = as.Date("1990-01-01"), y = 0.0015, label="Start date"), color="#69b3a2") +
    geom_density(data = matched_omop, aes(x = cohort_end_date, y = ..density..), 
                 fill = "#404080", alpha = 0.6) +
    geom_label(aes(x = as.Date("2011-01-01"), y = 0.0015, label="End date"), color="#404080") +
    xlim(as.Date("2010-04-01", format = "%Y-%m-%d"), as.Date("2018-03-31", format = "%Y-%m-%d")) +
    labs(title = "OMOP", x = "Cohort Start Date", y = "Density") +
    theme_minimal())

(dp_r <- ggplot() +
    geom_density(data = matched_raw, aes(x = aurum_min_accept_date, y = ..density..), 
                 fill = "#69b3a2", alpha = 0.6) +
    geom_label(aes(x = as.Date("1990-01-01"), y = 0.0015, label="Start date"), color="#69b3a2") +
    geom_density(data = matched_raw, aes(x = aurum_max_accept_date, y = ..density..), 
                 fill = "#404080", alpha = 0.6) +
    geom_label(aes(x = as.Date("2011-01-01"), y = 0.0015, label="End date"), color="#404080") +
    xlim(as.Date("1910-07-02", format = "%Y-%m-%d"), as.Date("2022-03-21", format = "%Y-%m-%d"))+
    labs(title = "Raw", x = "", y = "Density") +
    theme_minimal()
)

(cohort_date_plot <- dp_o + dp_r + plot_annotation(title = "Comparison of Cohort start and end dates at the records level: OMOP vs Raw"))

## Assess Age ----
# Calculate Age at Fracture date
matched_omop.1 <- matched_omop %>%
  mutate(subject_id = bit64::as.integer64(subject_id)) %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  mutate(age_fracture = lubridate::year(condition_start_date) - year_of_birth) %>% 
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, year_of_birth, fracture_site, age_fracture)

matched_raw.1 <- matched_raw %>%
  mutate(patid = bit64::as.integer64(patid)) %>%
  mutate(age_fracture = lubridate::year(fx_date) - yob) 

# Boxplot to compare Age at Fracture
(bp_plot_o <- ggplot() +
    geom_boxplot(data = matched_omop.1, aes(x = "OMOP", y = age_fracture), fill = "#FFC1C1", alpha = 0.5, outlier.color = "black") +
    labs(title = "",
         y = "Age at Fracture",
         x = "") +
    scale_y_continuous(limits = c(0, 130)) +
    theme_minimal())

(bp_plot_r <- ggplot() +
    geom_boxplot(data = matched_raw.1, aes(x = "Raw", y = age_fracture), fill = "plum1", alpha = 0.5, outlier.color = "black") +
    labs(title = "",
         y = "Age at Fracture",
         x = "") +
    scale_y_continuous(limits = c(0, 130)) +
    theme_minimal())

(age_plot <- bp_plot_o + bp_plot_r + plot_annotation(title = "Comparison of Age at Fracture at the records level: OMOP vs Raw"))

# Similar median and IQR: Both datasets have similar distributions of fracture ages.
# OMOP has more outliers: There are more extremely low fracture ages in OMOP, suggesting possible errors or more granular data capture.
# Slightly wider range in OMOP: The whiskers extend further, indicating OMOP may include more extreme values.


matched_omop.1.1 <- matched_omop %>%
  mutate(subject_id = bit64::as.integer64(subject_id)) %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  mutate(age_fracture = lubridate::year(condition_start_date) - year_of_birth) %>% 
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, age_fracture) %>% 
  distinct(subject_id, .keep_all = T)

matched_raw.1.1 <- matched_raw %>%
  mutate(patid = bit64::as.integer64(patid)) %>%
  mutate(age_fracture = lubridate::year(fx_date) - yob) %>% 
  dplyr::select(patid, aurum_min_accept_date, aurum_max_accept_date, type, fx_date, site, age_fracture) %>% 
  distinct(patid, .keep_all = T)


# Boxplot to compare Age at Fracture
(bp_plot_o.1 <- ggplot() +
    geom_boxplot(data = matched_omop.1.1, aes(x = "OMOP", y = age_fracture), fill = "#FFC1C1", alpha = 0.5, outlier.color = "black") +
    labs(title = "",
         y = "Age at Fracture",
         x = "") +
    scale_y_continuous(limits = c(0, 130)) +
    theme_minimal())

(bp_plot_r.1 <- ggplot() +
    geom_boxplot(data = matched_raw.1.1, aes(x = "Raw", y = age_fracture), fill = "plum1", alpha = 0.5, outlier.color = "black") +
    labs(title = "",
         y = "Age at Fracture",
         x = "") +
    scale_y_continuous(limits = c(0, 130)) +
    theme_minimal())

(age_plot.1 <- bp_plot_o.1 + bp_plot_r.1 + plot_annotation(title = "Comparison of Age at Fracture at the pt level: OMOP vs Raw"))

### checking where age differs
overlap_records.1 <- overlap_records %>% 
  mutate(subject_id = bit64::as.integer64(subject_id)) %>% 
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  mutate(age_fracture_o = lubridate::year(condition_start_date) - year_of_birth) %>%
  mutate(age_fracture_r = lubridate::year(fx_date) - yob) %>%
  # filter(site != 0) %>% 
  select(!c(gender_concept_id, month_of_birth, day_of_birth, birth_datetime,
            race_concept_id, ethnicity_concept_id, location_id, provider_id, care_site_id,
            person_source_value, gender_source_value, gender_source_concept_id, 
            race_source_value, race_source_concept_id, ethnicity_source_value,
            ethnicity_source_concept_id)) %>% 
  mutate(age_diff = age_fracture_o - age_fracture_r)

mismatched_ages <- overlap_records.1 %>%
  filter(condition_start_date == fx_date,
         age_fracture_o != age_fracture_r) %>%
  select(subject_id,condition_concept_id, condition_start_date, fx_date, year_of_birth, yob, age_fracture_o, age_fracture_r) 

## Assessing death date ----

matched_omop.2 <- matched_omop %>%
  mutate(subject_id = bit64::as.integer64(subject_id)) %>%
  dplyr::left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  dplyr::group_by(subject_id) %>%
  # dplyr::filter(is.na(death_date)| death_date > condition_start_date) %>%
  dplyr::ungroup() %>% 
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, death_date) %>% 
  distinct(subject_id, death_date, .keep_all = T)

matched_raw.2 <- matched_raw %>% 
  mutate(patid = bit64::as.integer64(patid)) %>%
  group_by(patid) %>% 
  # filter(is.na(death_date)| death_date > fx_date) %>%
  ungroup() %>% 
  dplyr::select(patid, aurum_min_accept_date, aurum_max_accept_date, type, fx_date, site, death_date) %>% 
  mutate(death_date = as.Date(death_date, format = "%Y-%m-%d")) %>% 
  distinct(patid, death_date, .keep_all = T)

# histograms
# Calculate mean and median
mean_death_date_omop <- as.Date(mean(matched_omop.2$death_date, na.rm = T), format = "%Y-%m-%d")
median_death_date_omop <- as.Date(median(matched_omop.2$death_date, na.rm = T), format = "%Y-%m-%d")

(hp_o <- ggplot(data = matched_omop.2, aes(x = death_date)) +
    geom_histogram(bins = 30, fill = "darkred", alpha = 0.6) +
    geom_vline(aes(xintercept = as.numeric(mean_death_date_omop)), color = "#104E8B", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = as.numeric(median_death_date_omop)), color = "#008B00", linetype = "dotted", size = 1) +
    annotate("text", x = mean_death_date_omop, y = Inf, label = paste("Mean:", mean_death_date_omop), color = "#104E8B", vjust = 2, hjust = 1.1) +
    annotate("text", x = median_death_date_omop, y = Inf, label = paste("Median:", median_death_date_omop), color = "#008B00", vjust = 2, hjust = -0.1) +
    labs(title = "OMOP",
         x = "Death Date",
         y = "Frequency") +
    scale_y_continuous(limits = c(0, 700)) +
    theme_minimal())

mean_death_date_raw <- as.Date(mean(matched_raw.2$death_date, na.rm = T), format = "%Y-%m-%d")
median_death_date_raw <- as.Date(median(matched_raw.2$death_date, na.rm = T), format = "%Y-%m-%d")

(hp_r <- ggplot(data = matched_raw.2, aes(x = death_date)) +
    geom_histogram(bins = 30, fill = "#551A8B", alpha = 0.6) +
    geom_vline(aes(xintercept = as.numeric(mean_death_date_raw)), color = "#104E8B", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = as.numeric(median_death_date_raw)), color = "#008B00", linetype = "dotted", size = 1) +
    annotate("text", x = mean_death_date_raw, y = Inf, label = paste("Mean:", mean_death_date_raw), color = "#104E8B", vjust = 2, hjust = 1.1) +
    annotate("text", x = median_death_date_raw, y = Inf, label = paste("Median:", median_death_date_raw), color = "#008B00", vjust = 2, hjust = -0.1) +
    labs(title = "Raw",
         x = "Death Date",
         y = "Frequency") +
    scale_y_continuous(limits = c(0, 700)) +
    theme_minimal())

(death_plot <- hp_o + hp_r + plot_annotation(title = "Comparison of Death Date (before removing fx after death) at pt level: OMOP vs Raw"))

matched_omop.2.1 <- matched_omop %>%
  mutate(subject_id = bit64::as.integer64(subject_id)) %>%
  dplyr::left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  dplyr::group_by(subject_id) %>%
  dplyr::filter(is.na(death_date)| death_date > condition_start_date) %>%
  dplyr::ungroup() %>% 
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, death_date) %>% 
  distinct(subject_id, death_date, .keep_all = T)

matched_raw.2.1 <- matched_raw %>% 
  mutate(patid = bit64::as.integer64(patid)) %>%
  group_by(patid) %>% 
  filter(is.na(death_date)| death_date > fx_date) %>%
  ungroup() %>% 
  dplyr::select(patid, aurum_min_accept_date, aurum_max_accept_date, type, fx_date, site, death_date) %>% 
  mutate(death_date = as.Date(death_date, format = "%Y-%m-%d")) %>% 
  distinct(patid, death_date, .keep_all = T)

# histograms
# Calculate mean and median
mean_death_date_omop.1 <- as.Date(mean(matched_omop.2.1$death_date, na.rm = T), format = "%Y-%m-%d")
median_death_date_omop.1 <- as.Date(median(matched_omop.2.1$death_date, na.rm = T), format = "%Y-%m-%d")

(hp_o.1 <- ggplot(data = matched_omop.2.1, aes(x = death_date)) +
    geom_histogram(bins = 30, fill = "darkred", alpha = 0.6) +
    geom_vline(aes(xintercept = as.numeric(mean_death_date_omop.1)), color = "#104E8B", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = as.numeric(median_death_date_omop.1)), color = "#008B00", linetype = "dotted", size = 1) +
    annotate("text", x = mean_death_date_omop.1, y = Inf, label = paste("Mean:", mean_death_date_omop.1), color = "#104E8B", vjust = 2, hjust = 1.1) +
    annotate("text", x = median_death_date_omop.1, y = Inf, label = paste("Median:", median_death_date_omop.1), color = "#008B00", vjust = 2, hjust = -0.1) +
    labs(title = "OMOP",
         x = "Death Date",
         y = "Frequency") +
    scale_y_continuous(limits = c(0, 700)) +
    theme_minimal())

mean_death_date_raw.1 <- as.Date(mean(matched_raw.2.1$death_date, na.rm = T), format = "%Y-%m-%d")
median_death_date_raw.1 <- as.Date(median(matched_raw.2.1$death_date, na.rm = T), format = "%Y-%m-%d")

(hp_r.1 <- ggplot(data = matched_raw.2.1, aes(x = death_date)) +
    geom_histogram(bins = 30, fill = "#551A8B", alpha = 0.6) +
    geom_vline(aes(xintercept = as.numeric(mean_death_date_raw.1)), color = "#104E8B", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = as.numeric(median_death_date_raw.1)), color = "#008B00", linetype = "dotted", size = 1) +
    annotate("text", x = mean_death_date_raw.1, y = Inf, label = paste("Mean:", mean_death_date_raw.1), color = "#104E8B", vjust = 2, hjust = 1.1) +
    annotate("text", x = median_death_date_raw.1, y = Inf, label = paste("Median:", median_death_date_raw.1), color = "#008B00", vjust = 2, hjust = -0.1) +
    labs(title = "Raw",
         x = "Death Date",
         y = "Frequency") +
    scale_y_continuous(limits = c(0, 700)) +
    theme_minimal())

(death_plot.1 <- hp_o.1 + hp_r.1 + plot_annotation(title = "Comparison of Death Date (after removing fx after death) at pt level: OMOP vs Raw"))

matched_omop.2.2 <- matched_omop %>%
  mutate(subject_id = bit64::as.integer64(subject_id)) %>%
  dplyr::left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  dplyr::group_by(subject_id) %>%
  dplyr::filter(is.na(death_date)| death_date > condition_start_date) %>%
  dplyr::ungroup() %>% 
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, death_date) 

matched_raw.2.2 <- matched_raw %>% 
  mutate(patid = bit64::as.integer64(patid)) %>%
  group_by(patid) %>% 
  filter(is.na(death_date)| death_date > fx_date) %>%
  ungroup() %>% 
  dplyr::select(patid, aurum_min_accept_date, aurum_max_accept_date, type, fx_date, site, death_date) %>% 
  mutate(death_date = as.Date(death_date, format = "%Y-%m-%d"))

# histograms
# Calculate mean and median
mean_death_date_omop.2 <- as.Date(mean(matched_omop.2.2$death_date, na.rm = T), format = "%Y-%m-%d")
median_death_date_omop.2 <- as.Date(median(matched_omop.2.2$death_date, na.rm = T), format = "%Y-%m-%d")

(hp_o.2 <- ggplot(data = matched_omop.2.2, aes(x = death_date)) +
    geom_histogram(bins = 30, fill = "darkred", alpha = 0.6) +
    geom_vline(aes(xintercept = as.numeric(mean_death_date_omop.2)), color = "#104E8B", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = as.numeric(median_death_date_omop.2)), color = "#008B00", linetype = "dotted", size = 1) +
    annotate("text", x = mean_death_date_omop.1, y = Inf, label = paste("Mean:", mean_death_date_omop.2), color = "#104E8B", vjust = 2, hjust = 1.1) +
    annotate("text", x = median_death_date_omop.1, y = Inf, label = paste("Median:", median_death_date_omop.2), color = "#008B00", vjust = 2, hjust = -0.1) +
    labs(title = "OMOP",
         x = "Death Date",
         y = "Frequency") +
    scale_y_continuous(limits = c(0, 4000)) +
    theme_minimal())

mean_death_date_raw.2 <- as.Date(mean(matched_raw.2.2$death_date, na.rm = T), format = "%Y-%m-%d")
median_death_date_raw.2 <- as.Date(median(matched_raw.2.2$death_date, na.rm = T), format = "%Y-%m-%d")

(hp_r.2 <- ggplot(data = matched_raw.2.2, aes(x = death_date)) +
    geom_histogram(bins = 30, fill = "#551A8B", alpha = 0.6) +
    geom_vline(aes(xintercept = as.numeric(mean_death_date_raw.2)), color = "#104E8B", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = as.numeric(median_death_date_raw.2)), color = "#008B00", linetype = "dotted", size = 1) +
    annotate("text", x = mean_death_date_raw.2, y = Inf, label = paste("Mean:", mean_death_date_raw.2), color = "#104E8B", vjust = 2, hjust = 1.1) +
    annotate("text", x = median_death_date_raw.2, y = Inf, label = paste("Median:", median_death_date_raw.2), color = "#008B00", vjust = 2, hjust = -0.1) +
    labs(title = "Raw",
         x = "Death Date",
         y = "Frequency") +
    scale_y_continuous(limits = c(0, 4000)) +
    theme_minimal())

(death_plot.2 <- hp_o.2 + hp_r.2 + plot_annotation(title = "Comparison of Death Date (after removing fx after death) at records level: OMOP vs Raw"))

mismatched_deaths <- overlap_records %>% 
  mutate(subject_id = bit64::as.integer64(subject_id)) %>%
  dplyr::left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>%
  rename("death_date_r" = "death_date.x",
         "death_date_o" = "death_date.y") 

mismatched_deaths.1 <- mismatched_deaths %>% 
  group_by(subject_id) %>%
  filter(death_date_o != death_date_r | is.na(death_date_o) | is.na(death_date_r)) %>% 
  ungroup()

## How many patients and % had different dates for the same fracture? ---- 