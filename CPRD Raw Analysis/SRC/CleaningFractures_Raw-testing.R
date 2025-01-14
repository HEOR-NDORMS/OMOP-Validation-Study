


# Sample data


sample_df <- all_patients %>% 
  filter(patid %in% as.integer64(c(8920018L, 23520018L, 243220018L, 370320018L, 575320018L)))



# Select columns containing lpon
lpon_sample_df <- sample_df %>% 
  select(patid, contains("lpon")) 

# Load necessary libraries
library(tidyr)
library(dplyr)

# Pivot data to long format for `_index` columns
long_lpon_sample_df <- lpon_sample_df %>%
  pivot_longer(
    cols = -patid, # Keep `patid` as is
    names_to = c("condition", ".value"), # Split column names into `condition` and `_index` or `_dt`
    names_sep = "_lpon_" # Use `_lpon_` as the separator
  ) %>% 
  rename("lpon_index" = "index",
         "lpon_index_dt" = "index_dt",
         "lpon_condition" =  "condition")



# Select columns containing fa
fa_sample_df <- sample_df %>% 
  select(patid, contains("_fa_")) 

# Pivot data to long format for `_index` columns
long_fa_sample_df <- fa_sample_df %>%
  pivot_longer(
    cols = -patid, # Keep `patid` as is
    names_to = c("condition", ".value"), # Split column names into `condition` and `_index` or `_dt`
    names_sep = "_fa_" # Use `_fa_` as the separator
  ) %>% 
  rename("fa_index" = "index",
         "fa_index_dt" = "index_dt",
         "fa_condition" =  "condition")


other_cols_df <- sample_df %>% 
  select(patid, gender, yob, death, death_date, aurum_min_accept_date, 
         aurum_max_accept_date, index_date, index_hip_fx, index_vert_fx,
         index_nhnv_fx, trauma_faon_index, trauma_faon_index_dt)

library(tidylog)

full_long_sample_df.1 <- full_join(
  demo_df, long_lpon_sample_df, by = "patid"
)

full_long_sample_df.2 <- full_join(
  full_long_sample_df.1, long_fa_sample_df, by = "patid"
)


lpon_fa_df <- sample_df %>% 
  select(patid, contains("_fa_"), contains("lpon"))

long_lpon_fa_df <- lpon_fa_df %>% 
  pivot_longer(
    cols = -patid, # Keep `patid` as is
    names_to = c("condition", ".value"), # Split column names into `condition` and `_index` or `_dt`
    names_sep = "_(fa|lpon)_" # Use `_fa_` as the separator
  )
