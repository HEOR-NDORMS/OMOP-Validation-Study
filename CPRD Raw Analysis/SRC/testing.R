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

# Remove fractures that are on the same date as the trauma ones
# Process Data
final_data.1_filtered <- final_data.1 %>%
  group_by(patid, value) %>% 
  mutate(trauma_present = any(Type == "0")) %>% 
  filter(!(Type %in% c("fa", "index") & trauma_present)) %>%  # Remove fa and index if trauma exists on the same date
  # mutate(fa_sites = n_distinct(site[Type == "fa"])) %>% 
  # filter(!(Type == "fa" & fa_sites >= 3)) %>%  # Remove fa if 3 or more distinct sites exist on the same date
  ungroup()

final_data.1_filtered.1 <- final_data.1_filtered %>% 
  group_by(patid, value) %>% 
  mutate(number_sites = n_distinct(site)) %>% 
  filter(number_sites == 3) %>% 
  ungroup()
  
# Identify fractures that occur on three sites on the same date and mark them as trauma
final_data.3 <- final_data.2 %>%
  group_by(patid, value) %>%
  mutate(trauma_flag = n_distinct(site) >= 3) %>%
  ungroup() %>%
  mutate(Type = ifelse(trauma_flag, "trauma", Type)) %>%
  select(-trauma_flag)

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

# Remove fractures that are on the same date as the trauma ones
# Process Data
final_data.3_filtered <- final_data3.1 %>%
  group_by(patid, value) %>% 
  mutate(trauma_present = any(site == 0)) %>% 
  filter(!(Type %in% c("fa", "index") & trauma_present)) %>%  # Remove fa and index if trauma exists on the same date
  # mutate(fa_sites = n_distinct(site[Type == "fa"])) %>% 
  # filter(!(Type == "fa" & fa_sites >= 3)) %>%  # Remove fa if 3 or more distinct sites exist on the same date
  ungroup()

final_data.3_filtered.1 <- final_data.3_filtered %>% 
  group_by(patid, value) %>% 
  summarise(number_sites = n_distinct(site), .groups = "drop") %>% 
  filter(number_sites == 3) %>% 
  ungroup()
