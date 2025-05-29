library(dplyr)
library(tidyr)
library(lubridate)
library(tidylog)
library(data.table)

# Import data ----

visit_data_omop <- fread("H:/Projects/Validation/Data/visits_data_omop.csv")
# glimpse(visit_data_omop)
# cohort_data_omop <- rio::import("H:/Projects/Validation/Data/cohort_table_cprd.csv")

visits_data_raw <- fread("H:/Projects/Validation/Data/visits_data_raw.csv")
# cohort_data_raw <- rio::import("H:/Projects/Validation/Data/cohort_table_raw.csv")


visit_data_omop <- visit_data_omop |> 
  mutate(
    index_date_omop = as.Date(index_date),
    follow_up_end_omop = as.Date(follow_up_end)
  )

visits_data_raw <- visits_data_raw |> 
  mutate(
    index_date_source = as.Date(index_date),
    follow_up_end_source = as.Date(follow_up_end)
  )

merged_visits_data <- visit_data_omop |>  
  select(subject_id, index_date, visit_detail_start_date, follow_up_reason, follow_up_end_omop,
         follow_up_time,specialty, visit_count) |> 
  rename("patid" = "subject_id",
         "index_date_omop" = "index_date",
         "follow_up_reason_omop" = "follow_up_reason",
         "follow_up_time_omop" = "follow_up_time",
         "specialty_omop" = "specialty",
         "visit_count_omop" = "visit_count"
         ) |> 
  inner_join(visits_data_raw |> 
               select(patid, index_date, consdate, follow_up_reason, follow_up_end_source,
                      follow_up_time, specialty, visit_count) |> 
               rename("index_date_source" = "index_date",
                      "follow_up_reason_source" = "follow_up_reason",
                      "follow_up_time_source" = "follow_up_time",
                      "specialty_source" = "specialty",
                      "visit_count_source" = "visit_count"
               ) , by = "patid")

# Drop duplicates when merging:
merged_visits_data.1 <- merged_visits_data |> 
  distinct()

merged_visits_data.1 <- merged_visits_data.1 |>
  mutate(
    index_date_omop = ymd(index_date_omop),
    follow_up_end_omop = ymd(follow_up_end_omop),
    index_date_source = ymd(index_date_source),
    follow_up_end_source = ymd(follow_up_end_source),  # corrected column name
    
    omop_duration = as.numeric(follow_up_end_omop - index_date_omop),
    source_duration = as.numeric(follow_up_end_source - index_date_source),
    duration_diff = abs(omop_duration - source_duration)
  )

top_discrepancies <- merged_visits_data.1 |>
  arrange(desc(duration_diff)) |>
  slice_head(n = 10) 
  # select(patid, omop_duration, source_duration, duration_diff)
top_discrepancies <- merged_visits_data.1 |>
  group_by(patid) |>
  slice_max(order_by = duration_diff, n = 1) |>
  ungroup() |>
  arrange(desc(duration_diff)) |>
  slice_head(n = 10)

top_patids <- merged_visits_data.1 %>%
  group_by(patid) %>%
  summarise(max_duration_diff = max(duration_diff, na.rm = TRUE)) %>%
  arrange(desc(max_duration_diff)) %>%
  slice_head(n = 10) %>%
  pull(patid)

top_discrepancies <- merged_visits_data.1 %>%
  filter(patid %in% top_patids)

# rio::export(top_discrepancies, "H:/Projects/Validation/Data/top_discrepancies.xlsx")

# visits timing
visits_timing <- merged_visits_data.1  |> 
  mutate(
    visit_date = ymd(visit_detail_start_date),
    visit_relative_to_omop = case_when(
      visit_date <= follow_up_end_omop ~ "Within OMOP Window",
      visit_date <= follow_up_end_source ~ "Within Source Window Only",
      TRUE ~ "After Source Window"
    ),
    days_since_index = as.numeric(visit_date - index_date_omop)
  )

library(ggplot2)
ggplot(visits_timing, aes(x = days_since_index, fill = visit_relative_to_omop)) +
  geom_histogram(binwidth = 30, color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("Within OMOP Window" = "green",
                               "Within Source Window Only" = "gold",
                               "After Source Window" = "red")) +
  labs(
    title = "Distribution of Visit Dates Relative to Index Date",
    x = "Days Since Index Date",
    y = "Number of Visits",
    fill = "Visit Timing"
  ) +
  theme_minimal()
