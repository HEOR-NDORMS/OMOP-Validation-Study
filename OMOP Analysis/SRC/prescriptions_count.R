drug_exposure_table <- cdm[["drug_exposure"]] %>% 
  dplyr::left_join(cdm[["visit"]], by = c("subject_id" = "person_id"), copy = T) %>%
  computeQuery()
