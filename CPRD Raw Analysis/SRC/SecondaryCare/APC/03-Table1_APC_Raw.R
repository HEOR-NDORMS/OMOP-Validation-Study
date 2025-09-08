# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: AE Table 2: description of the patients' AE attendances
# Date: 05.07.2025
# Last date edited: 08.09.2025
# ****************************************************************************** #

# From Antonella:

# 2.	The following tables have been added
# validation._hes_ccare
# validation._hes_diagnosis_epi
# validation._hes_episodes
# validation._hes_hospital
# validation._hes_hrg
# validation._hes_procedures_epi
# validation._hesae_attendance
# validation._hesae_hrg
# validation._hesae_investigation
# validation._hesae_treatment
# validation._hesop_appointment
# validation._hesop_cliinical

# In OMOP, spells are in visit_occurrence whilst episodes in visit_detail

# 
# 3.	The AURUM validation tables have not been modified

# Starting with cohort_table from CleaningCohort_Raw_new.R ----
table_two_apc_raw <- cohort_table %>% 
  # Select the variables of interest
  select(patid, index_date, follow_up_end)

# Importing the APC datasets ----
validation_apc_episodes <- my_dm$validation_hes_episodes %>% collect() 
table_two_apc_raw <- left_join(table_two_apc_raw, validation_apc_episodes, 
                               by = "patid")
validation_apc_diagnosis_epi <- my_dm$validation_hes_diagnosis_epi %>% collect()

table_two_apc_raw <- left_join(table_two_apc_raw, validation_apc_diagnosis_epi, 
                                 by = c("patid", "spno", "epikey"))
validation_apc_procedures_epi <- my_dm$validation_hes_procedures_epi %>% collect()

table_two_apc_raw <- left_join(table_two_apc_raw, validation_apc_procedures_epi, 
                               by = c("patid", "spno", "epikey"))
validation_apc_hospital <- my_dm$validation_hes_hospital %>% collect()
table_two_apc_raw <- left_join(table_two_apc_raw, validation_apc_hospital,
                                 by = c("patid", "spno"))

# Filter data ----
table_two_apc_raw <- table_two_apc_raw %>% 
  # Filter such that we have the episodes between the study start and study end dates
  filter(admidate >= study_start_date & discharged <= study_end_date) %>% 
  # in order to count all the episodes  from index date until follow-up end date.
  filter(epistart >= index_date & epiend <= follow_up_end) 

# Merge back to cohort data ----
table_two_apc_raw <- left_join(cohort_table %>% select(patid, index_date, follow_up_end), 
                               table_two_apc_raw, 
                               by = c("patid", "index_date", "follow_up_end"))

n_distinct(table_two_apc_raw$patid)

# Recode variables ----
table_two_apc_raw_recoded <- table_two_apc_raw %>% 
  rename(diag_01 = icd,
         opertn_01 = opcs,
         disdate = discharged) %>% 
  mutate_at(c("admimeth","admisorc", "mainspef", "tretspef","dismeth",
              "disdest", "diag_01", "opertn_01"), as.factor) %>% 
  mutate(
    admimeth_recoded = forcats::fct_recode(
      admimeth,
      "Waiting list" = "11",
      "Booked" = "12",
      "Planned" = "13",
      "Emergency - A&E or dental" = "21",
      "Emergency - GP direct" = "22",
      "Emergency - Bed bureau" = "23",
      "Emergency - Consultant clinic" = "24",
      "Emergency - Mental Health Crisis Team" = "25",
      "Emergency - Other provider A&E (not admitted)" = "2A",
      "Emergency - Transfer from another provider" = "2B",
      "Emergency - Baby born at home as intended" = "2C",
      "Emergency - Other" = "2D",
      "Emergency - Other means (retired)" = "28",
      "Maternity - Ante-partum" = "31",
      "Maternity - Post-partum" = "32",
      "Other - Birth in this provider" = "82",
      "Other - Baby born outside provider (not home)" = "83",
      "Other - Transfer from another provider (non-emergency)" = "81",
      "Other - HSPH Admissions Panel" = "84",
      "Other - HSPH Waiting List" = "89",
      "Not applicable" = "98",
      "Unknown method" = "99"
      ),
    admisorc_recoded = fct_recode(
      admisorc, 
      
      "Usual residence" = "19",
      "Temporary residence" = "29",
      "Repatriation from HSPH" = "30",
      "Court" = "37",
      "Police station (1999–2007)" = "38",
      "Penal establishment / court / police (retired)" = "39",
      "Penal establishment" = "40",
      "Court (retired)" = "41",
      "Police station / custody suite" = "42",
      "HSPH Scotland" = "48",
      "NHS HSPH accommodation" = "49",
      "NHS medium secure unit" = "50",
      "NHS general ward / A&E" = "51",
      "NHS maternity / neonates ward" = "52",
      "NHS mental health / learning disabilities ward" = "53",
      "NHS-run care home (retired)" = "54",
      "Care home with nursing" = "55",
      "Care home without nursing" = "56",
      "Local authority residential care (retired)" = "65",
      "Local authority foster care" = "66",
      "Local authority home/care (1989–1996)" = "69",
      "Baby born in/on way to hospital" = "79",
      "Non-NHS care home (retired)" = "85",
      "Non-NHS nursing home" = "86",
      "Independent sector hospital" = "87",
      "Hospice" = "88",
      "Non-NHS institution (1989–1996)" = "89",
      "Not applicable" = "98",
      "Admission source unknown" = "99"
    ),
    dismeth_recoded = fct_recode(
      dismeth, 
      "Discharged on clinical advice" = "1",
      "Discharged self or by relative/advocate (retired)" = "2",
      "Discharged by tribunal, Home Secretary or court" = "3",
      "Patient died" = "4",
      "Stillbirth" = "5",
      "Discharged self (from 2022-23)" = "6",
      "Discharged by relative/advocate (from 2022-23)" = "7",
      "Not applicable – spell not finished" = "8",
      "Method of discharge not known" = "9"
    ),
    disdest_recoded = fct_recode(
      disdest,
      
      "Usual residence (incl. no fixed abode)" = "19",
      "Temporary residence" = "29",
      "Repatriation from HSPH" = "30",
      "Penal establishment - court" = "37",
      "Penal establishment - police station" = "38",
      "Penal establishment - court and police excluded" = "39",
      "Penal establishment" = "40",
      "Police station / custody suite" = "42",
      "HSPH Scotland" = "48",
      "NHS HSPH accommodation" = "49",
      "NHS medium secure unit" = "50",
      "NHS general ward / younger physically disabled" = "51",
      "NHS maternity / neonates ward" = "52",
      "NHS mental health / learning disabilities ward" = "53",
      "NHS-run care home (retired)" = "54",
      "Care home with nursing" = "55",
      "Care home without nursing" = "56",
      "Local authority residential care (retired)" = "65",
      "Local authority foster care" = "66",
      "Local authority home/care (1989–1996)" = "69",
      "Patient died or stillbirth" = "79",
      "Independent sector medium secure unit" = "84",
      "Non-NHS care home (retired)" = "85",
      "Non-NHS nursing home" = "86",
      "Independent sector hospital (excl. medium secure)" = "87",
      "Hospice" = "88",
      "Organisation responsible for forced repatriation" = "89",
      "Not applicable – spell not finished" = "98",
      "Destination of discharge not known" = "99"
      ),
    mainspef_recoded = fct_recode(
      mainspef,
      "General Surgery" = "100",
      "Urology" = "101",
      "Vascular Surgery" = "107",
      "Trauma and Orthopaedics" = "110",
      "Ear Nose and Throat" = "120",
      "Ophthalmology" = "130",
      "Oral Surgery" = "140",
      "Restorative Dentistry" = "141",
      "Paediatric Dentistry" = "142",
      "Orthodontics" = "143",
      "Oral and Maxillofacial Surgery" = "145",
      "Endodontics" = "146",
      "Periodontics" = "147",
      "Prosthodontics" = "148",
      "Surgical Dentistry" = "149",
      "Neurosurgery" = "150",
      "Plastic Surgery" = "160",
      "Cardiothoracic Surgery" = "170",
      "Paediatric Surgery" = "171",
      "Pain Management (retired)" = "191",
      "Emergency Medicine" = "180",
      "Anaesthetics" = "190",
      "Intensive Care Medicine" = "192",
      "Aviation and Space Medicine" = "200",
      "General Internal Medicine" = "300",
      "Gastroenterology" = "301",
      "Endocrinology and Diabetes" = "302",
      "Clinical Haematology" = "303",
      "Clinical Physiology" = "304",
      "Clinical Pharmacology" = "305",
      "Audio Vestibular Medicine" = "310",
      "Clinical Genetics" = "311",
      "Clinical Cytogenetics and Molecular Genetics (retired)" = "312",
      "Clinical Immunology" = "313",
      "Rehabilitation Medicine" = "314",
      "Palliative Medicine" = "315",
      "Allergy" = "317",
      "Cardiology" = "320",
      "Paediatric Cardiology" = "321",
      "Sport and Exercise Medicine" = "325",
      "Acute Internal Medicine" = "326",
      "Dermatology" = "330",
      "Respiratory Medicine" = "340",
      "Infectious Diseases" = "350",
      "Tropical Medicine" = "352",
      "Genitourinary Medicine" = "360",
      "Renal Medicine" = "361",
      "Medical Oncology" = "370",
      "Nuclear Medicine" = "371",
      "Neurology" = "400",
      "Clinical Neurophysiology" = "401",
      "Rheumatology" = "410",
      "Paediatrics" = "420",
      "Paediatric Neurology" = "421",
      "Geriatric Medicine" = "430",
      "Dental Medicine" = "450",
      "Special Care Dentistry" = "451",
      "Obstetrics" = "501",
      "Gynaecology" = "502",
      "Community Sexual and Reproductive Health" = "504",
      "Antenatal Clinic (retired)" = "510",
      "Postnatal Clinic (retired)" = "520",
      "Midwifery" = "560",
      "General Medical Practice" = "600",
      "General Dental Practice" = "601",
      "Maternity Function (retired)" = "610",
      "Other than Maternity (retired)" = "620",
      "Learning Disability" = "700",
      "Adult Mental Illness" = "710",
      "Child and Adolescent Psychiatry" = "711",
      "Forensic Psychiatry" = "712",
      "Medical Psychotherapy" = "713",
      "Old Age Psychiatry" = "715",
      "Clinical Oncology" = "800",
      "Radiology" = "810",
      "General Pathology" = "820",
      "Blood Transfusion" = "821",
      "Chemical Pathology" = "822",
      "Haematology" = "823",
      "Histopathology" = "824",
      "Immunopathology" = "830",
      "Medical Microbiology and Virology" = "831",
      "Neuropathology (retired)" = "832",
      "Medical Microbiology" = "833",
      "Medical Virology" = "834",
      "Community Medicine" = "900",
      "Occupational Medicine" = "901",
      "Community Health Services Dental" = "902",
      "Public Health Medicine" = "903",
      "Public Health Dental" = "904",
      "Nursing" = "950",
      "Allied Health Professional" = "960",
      "Joint Consultant Clinics (retired)" = "990",
      "Non-UK Provider – Surgical" = "199",
      "Medical Ophthalmology" = "460",
      "Non-UK Provider – Medical" = "499",
      "Other Maternity Event" = "Null",
      "Not Known" = "&"
    ),
    tretspef_recoded = fct_recode(tretspef, 
                                  "General Surgery Service" = "100",
                                  "Urology Service" = "101",
                                  "Transplant Surgery Service (From 1 April 2004)" = "102",
                                  "Breast Surgery Service (From 1 April 2004)" = "103",
                                  "Colorectal Surgery Service (From 1 April 2004)" = "104",
                                  "Hepatobiliary and Pancreatic Surgery Service (From 1 April 2004)" = "105",
                                  "Upper Gastrointestinal Surgery Service (From 1 April 2004)" = "106",
                                  "Vascular Surgery Service (From 1 April 2004)" = "107",
                                  "Spinal Surgery Service (From April 2013)" = "108",
                                  "Bariatric Surgery Service (From 1 April 2021)" = "109",
                                  "Trauma and Orthopaedic Service" = "110",
                                  "Orthopaedic Service (From 1 April 2021)" = "111",
                                  "Endocrine Surgery Service (From 1 April 2021)" = "113",
                                  "Trauma Surgery Service (From 1 April 2021)" = "115",
                                  "Ear Nose and Throat Service" = "120",
                                  "Ophthalmology Service" = "130",
                                  "Oral Surgery Service" = "140",
                                  "Restorative Dentistry Service" = "141",
                                  "Paediatric Dentistry Service (From 1999-2000)" = "142",
                                  "Orthodontic Service" = "143",
                                  "Maxillofacial Surgery Service (From 1 April 2004)" = "144",
                                  "Oral and Maxillofacial Surgery Service (From 1 April 2021)" = "145",
                                  "Neurosurgical Service" = "150",
                                  "Plastic Surgery Service" = "160",
                                  "Burns Care Service (From 1 April 2004)" = "161",
                                  "Cardiothoracic Surgery Service" = "170",
                                  "Paediatric Surgery Service" = "171",
                                  "Cardiac Surgery Service (From 1 April 2004)" = "172",
                                  "Thoracic Surgery Service (From 1 April 2004)" = "173",
                                  "Cardiothoracic Transplantation Service (From 1 April 2004)" = "174",
                                  "Emergency Medicine Service" = "180",
                                  "Anaesthetic Service" = "190",
                                  "Pain Management Service (From 1998-99)" = "191",
                                  "Intensive Care Medicine Service (From 1 April 2004)" = "192",
                                  "Non-UK Provider; specialty function not known, treatment mainly surgical" = "199",
                                  "Aviation and Space Medicine Service (From 1 April 2021)" = "200",
                                  "Paediatric Urology Service (From 2006-07)" = "211",
                                  "Paediatric Transplantation Surgery Service (From 2006-07)" = "212",
                                  "Paediatric Gastrointestinal Surgery Service (From 2006-07)" = "213",
                                  "Paediatric Trauma and Orthopaedic Service (From 2006-07)" = "214",
                                  "Paediatric Ear Nose and Throat Service (From 2006-07)" = "215",
                                  "Paediatric Ophthalmology Service (From 2006-07)" = "216",
                                  "Paediatric Oral and Maxillofacial Surgery Service (From 2006-07)" = "217",
                                  "Paediatric Neurosurgery Service (From 2006-07)" = "218",
                                  "Paediatric Plastic Surgery Service (From 2006-07)" = "219",
                                  "Paediatric Burns Care Service (From 2006-07)" = "220",
                                  "Paediatric Cardiac Surgery Service (From 2006-07)" = "221",
                                  "Paediatric Thoracic Surgery Service (From 2006-07)" = "222",
                                  "Paediatric Epilepsy Service (From April 2013)" = "223",
                                  "Paediatric Clinical Pharmacology Service (From 1 April 2021)" = "230",
                                  "Paediatric Palliative Medicine Service (From 1 April 2021)" = "240",
                                  "Paediatric Pain Management Service (From 2006-07)" = "241",
                                  "Paediatric Intensive Care Service (From 2006-07)" = "242",
                                  "Paediatric Hepatology Service (From 1 April 2021)" = "250",
                                  "Paediatric Gastroenterology Service (From 2006-07)" = "251",
                                  "Paediatric Endocrinology Service (From 2006-07)" = "252",
                                  "Paediatric Clinical Haematology Service (From 2006-07)" = "253",
                                  "Paediatric Audio Vestibular Medicine Service (From 2006-07)" = "254",
                                  "Paediatric Clinical Immunology and Allergy Service (From 2006-07)" = "255",
                                  "Paediatric Infectious Diseases Service (From 2006-07)" = "256",
                                  "Paediatric Dermatology Service (From 2006-07)" = "257",
                                  "Paediatric Respiratory Medicine Service (From 2006-07)" = "258",
                                  "Paediatric Nephrology Service (From 2006-07)" = "259",
                                  "Paediatric Medical Oncology Service (From 2006-07)" = "260",
    "Paediatric Inherited Metabolic Medicine Service (From 2006-07)" = "261",
    "Paediatric Rheumatology Service (From 2006-07)" = "262",
    "Paediatric Diabetes Service (From 1 April 2004)" = "263",
    "Paediatric Cystic Fibrosis Service (From 1 April 2004)" = "264",
    "Paediatric Emergency Medicine Service (From 1 April 2021)" = "270",
    "Paediatric Interventional Radiology Service (From 2006-07)" = "280",
    "Community Paediatric Service (From 2006-07)" = "290",
    "Paediatric Neurodisability Service (From 2006-07)" = "291",
    "General Internal Medicine Service" = "300",
    "Gastroenterology Service" = "301",
    "Endocrinology Service" = "302",
    "Clinical Haematology Service" = "303",
    "Clinical Physiology Service (From 2008-09)" = "304",
    "Clinical Pharmacology Service" = "305",
    "Hepatology Service (From 1 April 2004)" = "306",
    "Diabetes Service (From 1 April 2004)" = "307",
    "Blood and Marrow Transplantation Service (From 1 April 2004)" = "308",
    "Haemophilia Service (From 1 April 2004)" = "309",
    "Audio Vestibular Medicine Service" = "310",
    "Clinical Genetics Service" = "311",
    "Clinical Immunology and Allergy Service (From 1991-92)" = "313",
    "Rehabilitation Medicine Service (From 1991-92)" = "314",
    "Palliative Medicine Service" = "315",
    "Clinical Immunology Service (From 1 April 2004)" = "316",
    "Allergy Service (From 1 April 2004)" = "317",
    "Intermediate Care Service (From 1 April 2004)" = "318",
    "Respite Care Service (From 1 April 2004)" = "319",
    "Cardiology Service" = "320",
    "Paediatric Cardiology Service (From 1 April 2004)" = "321",
    "Clinical Microbiology Service (From 1 April 2004)" = "322",
    "Spinal Injuries Service (From 2006-07)" = "323",
    "Anticoagulant Service (From 1 April 2004)" = "324",
    "Sport and Exercise Medicine Service (From 1 April 2004)" = "325",
    "Acute Internal Medicine Service (From 1 April 2021)" = "326",
    "Cardiac Rehabilitation Service (From 1 April 2004)" = "327",
    "Stroke Medicine Service (From 1 April 2004)" = "328",
    "Transient Ischaemic Attack Service (From 1 April 2004)" = "329",
    "Dermatology Service" = "330",
    "Congenital Heart Disease Service (From April 2013)" = "331",
    "Rare Disease Service (From 1 April 2021)" = "333",
    "Inherited Metabolic Medicine Service (From 1 April 2021)" = "335",
    "Respiratory Medicine Service" = "340",
    "Respiratory Physiology Service (From 1 April 2004)" = "341",
    "Pulmonary Rehabilitation Service (From 1 April 2004)" = "342",
    "Adult Cystic Fibrosis Service (From 1 April 2004)" = "343",
    "Complex Specialised Rehabilitation Service (From April 2013)" = "344",
    "Specialist Rehabilitation Service (From April 2013)" = "345",
    "Local Specialist Rehabilitation Service (From April 2013)" = "346",
    "Sleep Medicine Service (From 1 April 2021)" = "347",
    "Post-COVID-19 Syndrome Service (From 1 April 2021)" = "348",
    "Infectious Diseases Service" = "350",
    "Tropical Medicine Service (From 1 April 2004)" = "352",
    "Genitourinary Medicine Service" = "360",
    "Renal Medicine Service" = "361",
    "Medical Oncology Service" = "370",
    "Nuclear Medicine Service (From 2008-09)" = "371",
    "Neurology Service" = "400",
    "Clinical Neurophysiology Service (From 2008-09)" = "401",
    "Rheumatology Service" = "410",
    "Paediatric Service" = "420",
    "Paediatric Neurology Service" = "421",
    "Neonatal Critical Care Service (From 1 April 2004)" = "422",
    "Well Baby Service (From 1 April 2004)" = "424",
    "Elderly Medicine Service" = "430",
    "Orthogeriatric Medicine Service (From 1 April 2021)" = "431",
    "Dental Medicine Service (From 1990-91)" = "450",
    "Special Care Dentistry Service (From 1 April 2021)" = "451",
    "Medical Ophthalmology Service (From 1993-94)" = "460",
    "Ophthalmic and Vision Science Service (From 1 April 2021)" = "461",
    "Non-UK Provider; specialty function not known, treatment mainly medical" = "499",
    "Obstetrics Service" = "501",
    "Gynaecology Service" = "502",
    "Gynaecological Oncology Service (From 1 April 2004)" = "503",
    "Community Sexual and Reproductive Health Service (From 1 April 2021)" = "504",
    "Fetal Medicine Service (From 1 April 2021)" = "505",
    "Midwifery Service (From October 1995)" = "560",
    "Physiotherapy Service (From 2006-07)" = "650",
    "Occupational Therapy Service (From 2006-07)" = "651",
    "Speech and Language Therapy Service (From 2006-07)" = "652",
    "Podiatry Service (From 2006-07)" = "653",
    "Dietetics Service (From 2006-07)" = "654",
    "Orthoptics Service (From 2006-07)" = "655",
    "Clinical Psychology Service (From 2006-07)" = "656",
    "Prosthetics Service (From 1 April 2004)" = "657",
    "Orthotics Service (From 1 April 2004)" = "658",
    "Dramatherapy Service (From 1 April 2004)" = "659",
    "Art Therapy Service (From 1 April 2004)" = "660",
    "Music Therapy Service (From 1 April 2004)" = "661",
    "Optometry Service (From 1 April 2004)" = "662",
    "Podiatric Surgery Service (From April 2013)" = "663",
    "Urological Physiology Service (From 1 April 2021)" = "670",
    "Vascular Physiology Service (From 1 April 2021)" = "673",
    "Cardiac Physiology Service (From 1 April 2021)" = "675",
    "Gastrointestinal Physiology Service (From 1 April 2021)" = "677",
    "Learning Disability Service" = "700",
    "Adult Mental Health Service" = "710",
    "Child and Adolescent Psychiatry Service" = "711",
    "Forensic Psychiatry Service" = "712",
    "Medical Psychotherapy Service" = "713",
    "Old Age Psychiatry Service (From 1990-91)" = "715",
    "Eating Disorders Service (From 2006-07)" = "720",
    "Addiction Service (From 2006-07)" = "721",
    "Liaison Psychiatry Service (From 2006-07)" = "722",
    "Psychiatric Intensive Care Service (From 2006-07)" = "723",
    "Perinatal Mental Health Service (From 2006-07)" = "724",
    "Mental Health Recovery and Rehabilitation Service (From April 2013)" = "725",
    "Mental Health Dual Diagnosis Service (From April 2013)" = "726",
    "Dementia Assessment Service (From April 2013)" = "727",
    "Neuropsychiatry Service (From 1 April 2021)" = "730",
    "Clinical Oncology Service" = "800",
    "Interventional Radiology Service (From 1 April 2004)" = "811",
    "Diagnostic Imaging Service (From 2008-09)" = "812",
    "Chemical Pathology Service" = "822",
    "Medical Virology Service (From 1 April 2004)" = "834",
    "Audiology Service (From 2008-09)" = "840",
    "Diabetic Education Service (From April 2013)" = "920",
    "Other Maternity Event" = "Null",
    "Not known" = "&")
  ) %>% 
  mutate(diag_01_cleaned = gsub("\\.", "", diag_01))


## Codes data  ----
# ICD-10 Codes and Labels are maintained by the World Health Organisation. For more information please visit www.who.int/classifications/icd
# 4th edition
diag_codes_data <- import(here("HRG_ICD10_OPCS_ICD10.csv")) %>% 
  clean_names() %>% 
  select(!icd_10_edition)

table_two_apc_raw_recoded <- table_two_apc_raw_recoded %>% 
  left_join(diag_codes_data %>% rename("diag_01_cleaned" = "icd_10_code",
                                       "diag_01_description" =  "description"), 
            by = "diag_01_cleaned")

table_two_apc_raw_recoded <- table_two_apc_raw_recoded %>% 
  mutate(collapsed_diag_01 = str_sub(diag_01_cleaned, 1, 4)) %>% 
  mutate(diag_01_code_description = paste(
    collapsed_diag_01 , diag_01_description, sep = "-"))


# For more information please visit systems.hscic.gov.uk/data/clinicalcoding/codingstandards/opcs4 
# OPSC
opertn_codes_data <- import(here("HRG_ICD10_OPCS_OPCS4.csv")) %>% 
  clean_names() 

table_two_apc_raw_recoded <- table_two_apc_raw_recoded %>% 
  left_join(opertn_codes_data %>% rename("opertn_01" = "opcs_code",
                                         "opertn_01_description" =  "description"), 
            by = "opertn_01")

table_two_apc_raw_recoded <- table_two_apc_raw_recoded %>% 
  mutate(collapsed_opertn_01 = str_sub(opertn_01, 1, 4)) %>% 
  mutate(opertn_01_description = if_else(!is.na(collapsed_opertn_01),
                                         paste(collapsed_opertn_01, opertn_01_description, sep = "-"), NA))
# Minimise the columns
table_two_apc_raw_recoded_min <- table_two_apc_raw_recoded %>% 
  select(patid, epikey,epistart, epiend, admidate, disdate,
         admimeth_recoded, admisorc_recoded, mainspef_recoded,
         tretspef_recoded, dismeth_recoded, disdest_recoded,
         diag_01_code_description, opertn_01_description
         ) %>% 
  mutate(
    user_status = if_else(is.na(admidate) & is.na(disdate), "Non-User", "User")
  )


## Reorder factor by frequency ----
# table_two_apc_raw_recoded_min <- table_two_apc_raw_recoded_min %>%
#   mutate_at(c("mainspef_recoded", "tretspef_recoded"), as.factor) %>%
#   mutate(admimeth_recoded = forcats::fct_infreq(admimeth_recoded),
#          admisorc_recoded = forcats::fct_infreq(admisorc_recoded),
#          dismeth_recoded = forcats::fct_infreq(dismeth_recoded),
#          disdest_recoded = forcats::fct_infreq(disdest_recoded),
#          mainspef_recoded = forcats::fct_infreq(mainspef_recoded),
#          tretspef_recoded = forcats::fct_infreq(tretspef_recoded),
#          diag_01_code_description = forcats::fct_infreq(diag_01_code_description),
#          opertn_01_description = forcats::fct_infreq(opertn_01_description))



## Gtsumaary table ----
# (apc_gt_summary_table <- table_two_apc_raw_recoded_min %>% 
#    gtsummary::tbl_summary(
#      include = c(admimeth_recoded,
#                  admisorc_recoded, dismeth_recoded, 
#                  disdest_recoded,
#                  mainspef_recoded,
#                  tretspef_recoded,diag_01_code_description, 
#                  opertn_01_description,user_status),
#      label = list(
#        admimeth_recoded = "Method of Admission",
#        admisorc_recoded = "Source of Admission",
#        dismeth_recoded = "Method of Discharge",
#        disdest_recoded = "Destination on Discharge",
#        mainspef_recoded = "Main Specialty",
#        tretspef_recoded = "Treatment Specialty",
#        diag_01_code_description = "Main Diagnosis",
#        opertn_01_description = "Main Procedure",
#        user_status = "OP HealthCare Status"
#      ),
#      missing = "no"
#    ) %>%
#    gtsummary::modify_header(label = "**Variable**"))
# 
# apc_gt_summary_table <- table_two_apc_raw_recoded_min %>%
#   gtsummary::tbl_summary(
#     by = epikey,   # ensures counting at encounter-level
#     include = c(admimeth_recoded,
#                 admisorc_recoded, dismeth_recoded, 
#                 disdest_recoded,
#                 mainspef_recoded,
#                 tretspef_recoded,
#                 diag_01_code_description, 
#                 opertn_01_description,
#                 user_status),
#     statistic = all_categorical() ~ "{n}",  # only count distinct encounters
#     label = list(
#       admimeth_recoded = "Method of Admission",
#       admisorc_recoded = "Source of Admission",
#       dismeth_recoded = "Method of Discharge",
#       disdest_recoded = "Destination on Discharge",
#       mainspef_recoded = "Main Specialty",
#       tretspef_recoded = "Treatment Specialty",
#       diag_01_code_description = "Main Diagnosis",
#       opertn_01_description = "Main Procedure",
#       user_status = "OP HealthCare Status"
#     ),
#     missing = "no"
#   ) %>%
#   gtsummary::modify_header(label = "**Variable**")
# 
# 
# # Export ----
# apc_gt_summary_table_df <- as_tibble(apc_gt_summary_table) %>% 
#   rename("Variable" = "**Variable**",
#          "N (Appointments) = 170,321" = "**N = 170,321**") %>% 
#   mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>% 
#   filter(Variable != "&")
# 
# rio::export(apc_gt_summary_table_df, here::here("CPRD Raw Analysis", "Results",
#                                                 "SecondaryCareAnalysis", "APC", "Table1_APC_Raw.xlsx"))

# List of summaries
summaries_apc <- list(
  admimeth_recoded_summary = table_two_apc_raw_recoded_min %>% 
    select(patid, epikey, admimeth_recoded) %>% distinct() %>%
    filter(!is.na(admimeth_recoded)) %>%
    count(admimeth_recoded, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "Method of Admission",
           Category = admimeth_recoded),
  
  admisorc_recoded_summary = table_two_apc_raw_recoded_min %>% 
    select(patid, epikey, admisorc_recoded) %>% distinct() %>%
    filter(!is.na(admisorc_recoded)) %>%
    count(admisorc_recoded, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "Source of Admission",
           Category = admisorc_recoded),
  
  main_diag_summary = table_two_apc_raw_recoded_min %>%
    select(patid, epikey, diag_01_code_description) %>% 
    distinct() %>%
    filter(diag_01_code_description != "NA-NA") %>%
    count(diag_01_code_description, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "Main Diagnosis",
           Category = diag_01_code_description),
  
  main_proc_summary = table_two_apc_raw_recoded_min %>%
    select(patid, epikey, opertn_01_description) %>% 
    distinct() %>%
    filter(opertn_01_description != "NA-NA") %>%
    count(opertn_01_description, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "Main Procedure",
           Category = opertn_01_description),
  
  mainspef_recoded_summary = table_two_apc_raw_recoded_min %>% 
    select(patid, epikey, mainspef_recoded) %>% distinct() %>%
    filter(!is.na(mainspef_recoded)) %>%
    count(mainspef_recoded, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "Main Specialty",
           Category = mainspef_recoded),
  
  tretspef_recoded_summary = table_two_apc_raw_recoded_min %>% 
    select(patid, epikey, tretspef_recoded) %>% distinct() %>%
    filter(!is.na(tretspef_recoded)) %>%
    count(tretspef_recoded, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "Treatment Specialty",
           Category = tretspef_recoded),
  
  dismeth_recoded_summary = table_two_apc_raw_recoded_min %>% 
    select(patid, epikey, dismeth_recoded) %>% distinct() %>%
    filter(!is.na(dismeth_recoded)) %>%
    count(dismeth_recoded, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "Method of Discharge",
           Category = dismeth_recoded),
  
  disdest_recoded_summary = table_two_apc_raw_recoded_min %>% 
    select(patid, epikey, disdest_recoded) %>% distinct() %>%
    filter(!is.na(disdest_recoded)) %>%
    count(disdest_recoded, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "Destination on Discharge",
           Category = disdest_recoded),
  
  user_status_summary = table_two_apc_raw_recoded_min %>% 
    select(patid, user_status) %>% distinct() %>%
    count(user_status, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "APC Health Care Service Status",
           Category = user_status)
)

# Combine them
table2_combined_summary <- bind_rows(summaries_apc, .id = "summary_name") %>%
  select(Variable, Category, count_of_epi, percent_epi) %>%
  arrange(Variable, desc(count_of_epi)) %>% 
  mutate(Result = paste0(count_of_epi, " (", percent_epi, "%)")) %>% 
  select(!c(count_of_epi, percent_epi))

rio::export(table2_combined_summary, here("CPRD Raw Analysis", "Results",
                                          "SecondaryCareAnalysis", "APC", "Table1", "Table1_APC_Raw.xlsx"))
