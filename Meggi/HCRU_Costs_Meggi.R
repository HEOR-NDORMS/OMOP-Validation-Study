# load necessary functions
source("functions.R")

# 1. Identify the possible values for staff roles

# visit_specialty_source_value <- joined_visit_provider_tables %>% 
#   group_by(specialty_source_value) %>%
#   summarise(
#     number_visit = n(),
#     number_distinct_provider  = n_distinct(provider_id))%>%
#   mutate(freq_visit = (number_visit / sum (number_visit)))%>%
#   mutate(freq_provider = (number_distinct_provider / sum (number_distinct_provider)))


# 2. Prepare visit data for health economics (HE) analysis

## Join visit_detail and provider tables
joined_visit_provider_tables <- cdm$visit_detail %>% 
  dplyr::inner_join(cdm$provider) 

specialty_names <- provider_cost_inputs %>% dplyr::filter(Include == 1) %>% dplyr::pull(specialty_source_value)

## Sub-setting the visit data
  cdm[["visit_data"]] <- joined_visit_provider_tables %>%
    dplyr::inner_join(provider_cost_inputs %>% dplyr::filter(Include == "1"), by = "specialty_source_value", copy=TRUE) %>% # Filter for only meaningful specialties
    dplyr::select(person_id, provider_id, specialty_source_value, visit_detail_start_date, visit_detail_id, unit_cost, visit_detail_concept_id) %>%
    dplyr::filter(visit_detail_start_date >= study_start_date & # include only visits in the study period
                    visit_detail_start_date <= study_end_date) %>%
    dplyr::filter(visit_detail_concept_id == 581477) %>% # including only primary care
    dplyr::rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
    dplyr::distinct(visit_detail_start_date, subject_id, specialty_source_value, .keep_all=TRUE) %>% # Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
    dplyr::rename(specialty = specialty_source_value) %>%  # renaming for consistency with other countries as this variable is used for the HCRU and costs too
    computeQuery()

# 3. HE Analysis
  
cohort_meggi<- fracture_table_rq4 %>% select(subject_id, index_date, follow_up_end, fracture_site, condition_start_date )

# HCRU -----
meggi_results_hcru <- analyse_visits(cohort_combined = cohort_meggi, visit_data = cdm[["visit_data"]]) 

results_user <-meggi_results$user_only_summary %>% dplyr::collect()
results_all  <-meggi_results$all_summary %>% dplyr::collect()
non_service_users <-meggi_results$non_service_users %>% dplyr::collect()

# summary HCRU
meggi_summary <- cohort_summary(cohort_combined, "Target", non_service_users)

data_all <- meggi_results_hcru[["visits_count_wide"]] %>% 
  dplyr::select(-fracture_site, -condition_start_date) %>% 
  pivot_longer(cols = -c(subject_id, index_date, follow_up_end)) %>% 
  dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
  dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cohort = "Cohort",
                tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                tot_visits_per_yr = tot_visits/tot_exposed_yrs)

data_user <- data_all %>% 
  dplyr::filter(tot_visits > 0)

meggi_results_hcru[["user_only_summary_statistics"]] <- tibble(mean_visit_per_year=(sum(data_user$tot_visits)/sum(data_user$tot_exposed_yrs)),
                                                                  min_visit_per_year = min(data_user$tot_visits_per_yr),
                                                                  max_visit_per_year = max(data_user$tot_visits_per_yr),
                                                                  sd_visit_per_year = signif(sd(data_user$tot_visits_per_yr), 4),
                                                                  median_visit_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.5)), 4),
                                                                  lower_q_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.25)), 4),
                                                                  upper_q_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.75)), 4))

meggi_results_hcru[["all_summary_statistics"]] <- tibble(mean_visit_per_year=(sum(data_all$tot_visits)/sum(data_all$tot_exposed_yrs)),
                                                            min_visit_per_year = min(data_all$tot_visits_per_yr),
                                                            max_visit_per_year = max(data_all$tot_visits_per_yr),
                                                            sd_visit_per_year = signif(sd(data_all$tot_visits_per_yr),4),
                                                            median_visit_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.5)), 4),
                                                            lower_q_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.25)), 4), 
                                                            upper_q_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.75)), 4))
# save HCRU results
write.xlsx(meggi_results_hcru[-5], file = "hcru_results.xlsx")

# Costs -----

meggi_results_cost <- analyse_visits_cost(cohort_combined, cdm[["visit_data"]]) 
results_user_cost <-meggi_results_cost$user_only_cost_summary %>% dplyr::collect()
results_all_cost <-meggi_results_cost$all_cost_summary %>% dplyr::collect()

# summary costs

results_cost_output <- list()
for (name in (names(meggi_results_cost)[1:length(meggi_results_cost)])){
  results_cost_output[[name]]<-meggi_results_cost[[name]]
}

data_all <- results_cost_output[["visits_cost_wide"]] %>% 
  dplyr::select(-fracture_site, -condition_start_date) %>% 
  pivot_longer(cols = -c(subject_id, index_date, follow_up_end)) %>% 
  dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
  dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cohort = "Cohort",
                tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                tot_visits_per_yr = tot_visits/tot_exposed_yrs)

data_user <- data_all %>% 
  dplyr::filter(tot_visits > 0)

results_cost_output[["user_only_summary_statistics"]] <- tibble(mean_cost_visit_per_year=(sum(data_user$tot_visits)/sum(data_user$tot_exposed_yrs)),
                                                                       min_cost_visit_per_year = min(data_user$tot_visits_per_yr),
                                                                       max_cost_visit_per_year = max(data_user$tot_visits_per_yr),
                                                                       sd_cost_visit_per_year = signif(sd(data_user$tot_visits_per_yr), 4),
                                                                       median_cost_visit_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.5)), 4),
                                                                       lower_q_cost_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.25)), 4),
                                                                       upper_q_cost_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.75)), 4))

results_cost_output[["all_summary_statistics"]] <- tibble(mean_cost_visit_per_year=(sum(data_all$tot_visits)/sum(data_all$tot_exposed_yrs)),
                                                                 min_visit_cost_per_year = min(data_all$tot_visits_per_yr),
                                                                 max_visit_cost_per_year = max(data_all$tot_visits_per_yr),
                                                                 sd_visit_cost_per_year = signif(sd(data_all$tot_visits_per_yr),4),
                                                                 median_cost_visit_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.5)), 4),
                                                                 lower_q_cost_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.25)), 4), 
                                                                 upper_q_cost_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.75)), 4))
# save Costs results
write.xlsx(results_cost_output[-4], file =  "target_results_cost.xlsx")
