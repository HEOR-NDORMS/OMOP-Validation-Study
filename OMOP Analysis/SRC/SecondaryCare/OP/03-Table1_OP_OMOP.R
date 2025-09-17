# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: OP Table 1: description of the patients
# Date: 01.09.2025
# Last date edited: 01.09.2025
# ****************************************************************************** #

# Prepare the OP appts data ----
# Filter visit_detail for outpatient visits within study period
cdm[["visit_detail_filtered"]] <- cdm[["visit_detail"]] %>%
  dplyr::filter(
    visit_detail_concept_id == 9202,
    visit_detail_start_date >= study_start_date,
    visit_detail_start_date <= study_end_date
  ) %>%
  dplyr::select(
    visit_detail_id, person_id, provider_id,
    visit_detail_start_date, visit_detail_concept_id, preceding_visit_detail_id
  ) |>
  dplyr::compute()

# Join with visit_occurrence to get visit_type_concept_id
cdm[["visit_with_type"]] <- cdm[["visit_detail_filtered"]] %>%
  dplyr::left_join(
    cdm[["visit_occurrence"]] %>%
      dplyr::select(person_id, visit_type_concept_id),
    by = "person_id"
  ) %>% dplyr::compute()

# Join with provider to get specialty info
cdm[["appt_data"]] <- cdm[["visit_with_type"]] %>%
  dplyr::left_join(
    cdm[["provider"]] %>%
      dplyr::select(provider_id, specialty_concept_id, specialty_source_value),
    by = "provider_id"
  ) %>%
  dplyr::select(
    visit_detail_id, person_id, provider_id,
    specialty_concept_id, specialty_source_value,
    visit_detail_start_date, visit_detail_concept_id,
    preceding_visit_detail_id, visit_type_concept_id
  ) %>% dplyr::compute()

# Merge cohort with visit data
cohort_appt_data <- cohort_table %>%
  dplyr::left_join(
    cdm[["appt_data"]] %>% dplyr::collect(),
    by = c("subject_id" = "person_id")
  ) %>%
  dplyr::filter(# this will keep only patients with visits
    visit_detail_start_date >= index_date &
      visit_detail_start_date <= follow_up_end)

# Minimise the data variables
cohort_appt_data_min <- cohort_appt_data %>% 
  select(subject_id, index_date, follow_up_end,  visit_detail_id, provider_id,
         specialty_concept_id, specialty_source_value, 
         visit_detail_start_date)


# ----------------------
# Lookup tables as named vectors
# ----------------------

# Mainspef lookup
mainspef_lookup <- c(
  "General Surgery" = "100", 
  "Urology" = "101",
  "Trauma and Orthopaedics" = "110",
  "Ear Nose and Throat" = "120",
  "Ophthalmology" = "130", 
  "Oral Surgery" = "140", 
  "Oral Surgery" = "145", 
  "Neurosurgery" = "150", 
  "Plastic Surgery" = "160",
  "Emergency Medicine" = "180",
  "Anaesthetics" = "190", 
  "General Internal Medicine" = "300",
  "Gastroenterology" = "301", 
  "Endocrinology and Diabetes" = "302",
  "Clinical Haematology" = "303", 
  "Cardiology" = "320",
  "Acute Internal Medicine" = "326",
  "Dermatology" = "330", 
  "Respiratory Medicine" = "340",
  "Infectious Diseases" = "350",
  "Renal Medicine" = "361",
  "Medical Oncology" = "370",
  "Neurology" = "400",
  "Rheumatology" = "410",
  "Geriatric Medicine" = "430",
  "Obstetrics" = "501",
  "Gynaecology" = "502",
  "Adult Mental Illness" = "710",
  "Clinical Oncology" = "800", 
  "Radiology" = "810",
  "Radiology" = "823",
  "Restorative Dentistry" = "141",
  "Orthodontics" = "143", 
  "Clinical Physiology" = "304",
  "Audio Vestibular Medicine" = "310", 
  "Clinical Genetics" = "311", 
  "Clinical Immunology" = "313" ,
  "Rehabilitation Medicine" =  "314",
  "Clinical Neurophysiology" = "401", 
  "Midwifery" = "560", 
  "Learning Disability" = "700",
  "Medical Microbiology" = "833",
  "Medical Psychotherapy" = "713",
  "Paediatrics" = "420", 
  "Community Medicine" = "900",
  "Nursing " = "950",
  "Allied Health Professional" = "960",
  "Old Age Psychiatry" = "715",
  "Cardiothoracic Surgery" = "170",
  "General Medical Practice" = "600",
  "Palliative Medicine" = "315",
  "Dental Medicine" = "450",
  "Nuclear Medicine" = "371",
  "Clinical Pharmacology" = "305",
  "Chemical Pathology" = "822",
  "General Pathology" = "820",
  "Medical Ophthalmology" = "460",
  "Intensive Care Medicine" = "192",
  "Occupational Medicine" = "901",
  "Prosthodontics" = "148",
  "Periodontics" = "147",
  "Immunopathology" = "830",
  "Genitourinary Medicine" = "360",
  "Child and Adolescent Psychiatry" = "711",
  "Tropical Medicine" = "352",
  "Sport and Exercise Medicine" = "325"
)

# Tretspef lookup
tretspef_lookup <- c(
  "General Surgery Service" = "100", 
  "Urology Service" = "101", 
  "Urology Service" = "103", 
  "Upper Gastrointenstinal Surgery Service" = "106", 
  "Colorectal Surgery Service" = "104", 
  "Vascular Surgery Service" = "107",
  "Spinal Surgery Service" = "108",
  "Trauma and Orthopaedic Service" = "110", 
  "Ophthalmology Service" = "130", 
  "Oral Surgery Service" = "140", 
  "Maxillofacial Surgery Service" = "144", 
  "Neurosurgical Service" = "150", 
  "Plastic Surgery Service" = "160", 
  "Emergency Medicine Service" = "180", 
  "Anaesthetic Service" = "190", 
  "Pain Management Service" = "191",
  "Intensive Care Medicine Service" = "192",
  "Paediatric Trauma and Orthopaedic Service" = "214", 
  "General Internal Medicine Service" = "300", 
  "Gastroenterology Service" = "301", 
  "Clinical Haematology Service" = "303", 
  "Endocrinology Service" = "302", 
  "Hepatology Service" = "306", 
  "Cardiology Service" = "320",
  "Transient Ischaemic Attack Service" = "329", 
  "Dermatology Service" = "330", 
  "Respiratory Medicine Service" = "340", 
  "Respiratory Physiology Service" = "341",
  "Medical Oncology Service" = "370", 
  "Neurology Service" = "400", 
  "Rheumatology Service" = "410", 
  "Obstetrics Service" = "501", 
  "Gynaecology Service" = "502", 
  "Gynaecological Oncology Service" = "503",
  "Adult Mental Health Service" = "710",  
  "Clinical Oncology Service" = "800", 
  "Interventional Radiology Service" = "811",
  "Ear Nose and Throat Service" = "120",
  "Hepatobiliary and Pancreatic Surgery Service" = "105",
  "Bariatric Surgery Service" = "109",
  "Restorative Dentistry Service" = "141",
  "Orthodontic Service" = "143",
  "Oral and Maxillofacial Surgery Service"  = "145",
  "Paediatric Diabetes Service" = "263",
  "Clinical Physiology Service" = "304",
  "Diabetes Service" = "307",
  "Blood and Marrow Transplantation Service" = "308",
  "Audio Vestibular Medicine Service" = "310",
  "Clinical Genetics Service" = "311",
  "Clinical Immunology and Allergy Service" = "313",
  "Rehabilitation Medicine Service" = "314",
  "Allergy Service" = "317",
  "Anticoagulant Service" = "324",
  "Congenital Heart Disease Service" = "331",
  "Specialist Rehabilitation Service" = "345", 
  "Infectious Diseases Service" = "350",
  "Renal Medicine Service" = "361",
  "Nuclear Medicine Service" = "371",
  "Clinical Neurophysiology Service" = "401",
  "Elderly Medicine Service" = "430",
  "Medical Ophthalmology Service" = "460",
  "Fetal Medicine Service" = "505",
  "Midwifery Service" = "560",
  "Physiotherapy Service" = "650",
  "Occupational Therapy Service" = "651",
  "Speech and Language Therapy Service" = "652",
  "Podiatry Service" = "653",
  "Dietetics Service" = "654",
  "Orthoptics Service" = "655",
  "Clinical Psychology Service" = "656",
  "Orthotics Service" = "658",
  "Optometry Service" = "662",
  "Podiatric Surgery Service" = "663",
  "Cardiac Physiology Service" = "675",
  "Learning Disability Service" = "700",
  "Diagnostic Imaging Service" = "812",
  "Medical Virology Service" = "834",
  "Audiology Service" = "840",
  "Diabetic Education Service" = "920",
  "Intermediate Care Service" = "318",
  "Old Age Psychiatry Service" = "715",
  "Palliative Medicine Service" = "315",
  "Dental Medicine Service" = "450",
  "Cardiac Rehabilitation Service" = "327",
  "Liaison Psychiatry Service" = "722",
  "Chemical Pathology Service" = "822",
  "Stroke Medicine Service" = "328",
  "Cardiac Surgery Service" = "172",
  "Cardiothoracic Transplantation Service" = "174",
  "Cardiothoracic Surgery Service" = "170",
  "Burns Care Service" = "161",
  "Thoracic Surgery Service" = "173",
  "Clinical Immunology Service" = "316",
  "Spinal Injuries Service" = "323",
  "Transplant Surgery Service" = "102",
  "Clinical Pharmacology Service" = "305",
  "Paediatric Ophthalmology Service" = "216",
  "Haemophilia Service" = "309",
  "Clinical Microbiology Service" = "322",
  "Pulmonary Rehabilitation Service" = "342",
  "Genitourinary Medicine Service" = "360",
  "Prosthetics Service" = "657",
  "Medical Psychotherapy Service" = "713",
  "Sport and Exercise Medicine Service" = "325",
  "Complex Specialised Rehabilitation Service" = "344",
  "Genitourinary Medicine Service" = "360",
  "Tropical Medicine Service" = "352",
  "Child and Adolescent Psychiatry Service" = "711",
  "Paediatric Service" = "420",
  "Paediatric Ear Nose and Throat Service" = "215"
)

# ----------------------
# Apply mapping
# ----------------------

cohort_appt_data_min.1 <- cohort_appt_data_min %>%
  mutate(
    # mainspef
    mainspef_code = mainspef_lookup[specialty_source_value],
    mainspef_desc = ifelse(!is.na(mainspef_code), specialty_source_value, NA),
    
    # tretspef
    tretspef_code = tretspef_lookup[specialty_source_value],
    tretspef_desc = ifelse(!is.na(tretspef_code), specialty_source_value, NA)
  )
