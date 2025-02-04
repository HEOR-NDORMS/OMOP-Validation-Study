# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: Table 1: description of the patients
# Date: 31.01.2025
# Last date edited: 31.01.2025
# ****************************************************************************** #

# Number of patients
# Age (median, IQR)
# Follow-up time in years, median (IQR)
# Site of index fracture
# hip (n%)
# vertebra (n%)
# NHNS (n%)
# Censoring events:
# fracture ensuing incident (index) fracture (n%)
# cancer (except non-melanoma skin cancer) (n%)
# Paget’s disease of the bone, or other metabolic bone diseases (n%)
# death (n%)
# end of data collection period (n%)
# maximum of 730 days from index date (n%)

# For the first 3 items, it’s def patients. Then the index 
# I think that we should also have one per patient but not sure 
# if by the end of the cleaning we still have cases of more than one site (hip,vertebra or nhns) possibly associated with the same index fx.
# Let’s check. 
# If yes, then let’s count the number of fractures instead. 
# Same for censoring event, check if there is anyone with more than one censoring reason and if so report the n of events.
# I think in most cases there should be just one event per person as it’s unlikely that they have for example cancer and fracture on the same day.
# Definitely impossible that they die and have fracture on the same day. Or at least if we have these cases then it’s not good.

# Table 1 ----

# Starting with cohort_table_g_index.3

table_one_raw <- cohort_table_g_index.3 %>% 
  mutate(site = recode(site, hip = "Hip", vert = "Vertical", nhnv = "NHNV"))

(gt_table <- table_one_raw %>%
    group_by(patid) %>%  # Ensure unique patients
    summarise(
      across(
        c(age_fracture, site, fracture_after_index, follow_up_end,
          cancer_lpon_index_dt, paget_lpon_index_dt, censoring_event,
          follow_up_end_new, follow_up_time_new), 
        ~ ifelse(all(is.na(.)), NA, first(na.omit(.)))  # Robust NA handling
      ), 
      .groups = "drop" # Correct placement
    ) %>% 
    gtsummary::tbl_summary(
      include = c(age_fracture, site, fracture_after_index, follow_up_end,
                  cancer_lpon_index_dt, paget_lpon_index_dt, censoring_event,
                  follow_up_end_new, follow_up_time_new),
      missing = "ifany",  # Includes missing values in the summary
      statistic = list(
        fracture_after_index ~ "{N_nonmiss} ({p_nonmiss}%) / {N_miss} ({p_miss}%)",
        cancer_lpon_index_dt ~ "{N_nonmiss} ({p_nonmiss}%) / {N_miss} ({p_miss}%)",
        paget_lpon_index_dt ~ "{N_nonmiss} ({p_nonmiss}%) / {N_miss} ({p_miss}%)",
        follow_up_end_new ~ "{N_nonmiss} ({p_nonmiss}%) / {N_miss} ({p_miss}%)",
        follow_up_end ~ "{N_nonmiss} ({p_nonmiss}%) / {N_miss} ({p_miss}%)"
      )
    ))

## Number of patients ----
(number_of_patients <- n_distinct(table_one_raw$patid))

## Age (median, IQR) ----
(median_age <- median(table_one_raw$age_fracture))
(iqr_age <- IQR(table_one_raw$age_fracture))
(results_age <- paste0(median_age, "(", iqr_age, ")"))

table_one_raw_pt <- table_one_raw %>% 
  distinct(patid, .keep_all = TRUE)

(median_age_pt <- median(table_one_raw_pt$age_fracture))
(iqr_age_pt <- IQR(table_one_raw_pt$age_fracture))

## Site of index fracture ----
# hip (n%)
# vertebra (n%)
# NHNS (n%)
(site_count_pts <- table_one_raw %>% 
   group_by(site) %>%
   summarise(count_of_pts = n_distinct(patid)) %>%
   mutate(percent = round(count_of_pts / sum(count_of_pts) * 100, 2),
          result = paste0(count_of_pts, "(", percent, "%", ")")) %>% 
   ungroup())

(site_count_fx <- table_one_raw %>% 
    group_by(site) %>%
    summarise(count_of_pts = n(), .groups = "drop"))

(site_count_pt <- table_one_raw_pt %>% 
    distinct(patid, site) %>%            
    group_by(site) %>%
    summarise(count_of_pts = dplyr::n_distinct(patid), .groups = "drop"))

## Censoring events ----
# fracture ensuing incident (index) fracture (n%)
# cancer (except non-melanoma skin cancer) (n%)
# Paget’s disease of the bone, or other metabolic bone diseases (n%)
# death (n%)
# end of data collection period (n%)
# maximum of 730 days from index date (n%)

(censoring_events_count_pts <- table_one_raw %>% 
   group_by(censoring_event) %>%
   summarise(count_of_pts = n_distinct(patid), .groups = "drop"))

(censoring_events_count_fx <- table_one_raw_pt %>% 
    group_by(censoring_event) %>%
    summarise(count_of_pts = n(), .groups = "drop"))
