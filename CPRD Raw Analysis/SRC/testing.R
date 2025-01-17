library(tidyverse)

sample_df <- all_patients %>% 
  mutate(patid = as.numeric(patid)) %>%  # Convert integer64 to numeric
  filter(patid %in% c(23520018, 8920018, 243220018, 575320018))

sample_df.1 <- sample_df %>% 
  select(-c(contains("cardiovascular_lpon"), contains("heart_failure_lpon"), contains("mi_"),
            contains("osteoporosis_"), contains("ra_"), contains("cancer_"), contains("paget_"))) %>% 
  rename("index_dt" = "index_date") 


# Reshape the data
reshaped_data <- sample_df.1 %>%
  pivot_longer(cols = c(index_hip_fx, index_vert_fx, index_nhnv_fx),
               names_to = "site_type", values_to = "index_flag") %>%
  mutate(site = case_when(
    site_type == "index_hip_fx" ~ "hip",
    site_type == "index_vert_fx" ~ "vert",
    site_type == "index_nhnv_fx" ~ "nhnv"
  ),
  Type = "index",
  value = index_dt) %>%
  select(patid, gender, yob, death, death_date, aurum_min_accept_date, aurum_max_accept_date, site, Type, value,index_flag)

# Add fa and trauma rows
additional_rows <- sample_df.1 %>%
  pivot_longer(cols = c(hip_fx_fa_index_dt, vert_fx_fa_index_dt, nhnv_fx_fa_index_dt, trauma_faon_index_dt),
               names_to = "site_type", values_to = "value") %>%
  mutate(site = case_when(
    site_type == "hip_fx_fa_index_dt" ~ "hip",
    site_type == "vert_fx_fa_index_dt" ~ "vert",
    site_type == "nhnv_fx_fa_index_dt" ~ "nhnv",
    site_type == "trauma_faon_index_dt" ~ "trauma"
  ),
  Type = case_when(
    site_type == "trauma_faon_index_dt" ~ "0",
    TRUE ~ "fa"
  )) %>%
  select(patid, gender, yob, death, death_date, aurum_min_accept_date, aurum_max_accept_date, site, Type, value) %>% 
  filter(!is.na(value))

# Combine the reshaped data with the additional rows
final_data <- bind_rows(reshaped_data, additional_rows)

final_data.1 <- final_data %>% 
  filter(index_flag != 0 | is.na(index_flag))



# Assuming your large dataset is loaded into a variable called `data`
# For example, if you are reading from a CSV file:
# data <- read.csv("large_dataset.csv", sep = "\t")

sample_df2 <- all_patients %>% 
  mutate(patid = as.numeric(patid)) %>%  # Convert integer64 to numeric
  filter(patid %in% c(612620018, 893620018, 918020018, 938520018, 950320018))

sample_df2.1 <- sample_df2 %>% 
  select(-c(contains("cardiovascular_lpon"), contains("heart_failure_lpon"), contains("mi_"),
            contains("osteoporosis_"), contains("ra_"), contains("cancer_"), contains("paget_"))) %>% 
  rename("index_dt" = "index_date")


# Reshape the data
reshaped_data2 <- sample_df2.1 %>%
  pivot_longer(cols = c(index_hip_fx, index_vert_fx, index_nhnv_fx),
               names_to = "site_type", values_to = "index_flag") %>%
  mutate(site = case_when(
    site_type == "index_hip_fx" ~ "hip",
    site_type == "index_vert_fx" ~ "vert",
    site_type == "index_nhnv_fx" ~ "nhnv"
  ),
  Type = "index",
  value = index_dt) %>%
  select(patid, gender, yob, death, death_date, aurum_min_accept_date, aurum_max_accept_date, site, Type, value,index_flag)

# Add fa and trauma rows
additional_rows2 <- sample_df2.1 %>%
  pivot_longer(cols = c(hip_fx_fa_index_dt, vert_fx_fa_index_dt, nhnv_fx_fa_index_dt, trauma_faon_index_dt),
               names_to = "site_type", values_to = "value") %>%
  mutate(site = case_when(
    site_type == "hip_fx_fa_index_dt" ~ "hip",
    site_type == "vert_fx_fa_index_dt" ~ "vert",
    site_type == "nhnv_fx_fa_index_dt" ~ "nhnv",
    site_type == "trauma_faon_index_dt" ~ "0"
  ),
  Type = case_when(
    site_type == "trauma_faon_index_dt" ~ "trauma",
    TRUE ~ "fa"
  )) %>%
  select(patid, gender, yob, death, death_date, aurum_min_accept_date, aurum_max_accept_date, site, Type, value) %>% 
  filter(!is.na(value))

# Combine the reshaped data with the additional rows
final_data2 <- bind_rows(reshaped_data2, additional_rows2)

final_data2.1 <- final_data2 %>% 
  filter(index_flag != 0 | is.na(index_flag))


sample_df3 <- all_patients %>% 
  mutate(patid = as.numeric(patid)) %>%  # Convert integer64 to numeric
  filter(patid %in% c(23520018, 8920018, 243220018, 575320018,
                      612620018, 893620018, 918020018, 938520018, 950320018))

sample_df3.1 <- sample_df3 %>% 
  select(-c(contains("cardiovascular_lpon"), contains("heart_failure_lpon"), contains("mi_"),
            contains("osteoporosis_"), contains("ra_"), contains("cancer_"), contains("paget_"))) %>% 
  rename("index_dt" = "index_date")


# Reshape the data
reshaped_data3 <- sample_df3.1 %>%
  pivot_longer(cols = c(index_hip_fx, index_vert_fx, index_nhnv_fx),
               names_to = "site_type", values_to = "index_flag") %>%
  mutate(site = case_when(
    site_type == "index_hip_fx" ~ "hip",
    site_type == "index_vert_fx" ~ "vert",
    site_type == "index_nhnv_fx" ~ "nhnv"
  ),
  Type = "index",
  value = index_dt) %>%
  select(patid, gender, yob, death, death_date, aurum_min_accept_date, aurum_max_accept_date, site, Type, value,index_flag)

# Add fa and trauma rows
additional_rows3 <- sample_df3.1 %>%
  pivot_longer(cols = c(hip_fx_fa_index_dt, vert_fx_fa_index_dt, nhnv_fx_fa_index_dt, trauma_faon_index_dt),
               names_to = "site_type", values_to = "value") %>%
  mutate(site = case_when(
    site_type == "hip_fx_fa_index_dt" ~ "hip",
    site_type == "vert_fx_fa_index_dt" ~ "vert",
    site_type == "nhnv_fx_fa_index_dt" ~ "nhnv",
    site_type == "trauma_faon_index_dt" ~ "0"
  ),
  Type = case_when(
    site_type == "trauma_faon_index_dt" ~ "trauma",
    TRUE ~ "fa"
  )) %>%
  select(patid, gender, yob, death, death_date, aurum_min_accept_date, aurum_max_accept_date, site, Type, value) %>% 
  filter(!is.na(value))

# Combine the reshaped data with the additional rows
final_data3 <- bind_rows(reshaped_data3, additional_rows3)

final_data3.1 <- final_data3 %>% 
  filter(index_flag != 0 | is.na(index_flag))
