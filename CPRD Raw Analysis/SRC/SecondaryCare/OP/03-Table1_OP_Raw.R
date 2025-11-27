# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: OP Table 1: description of the patients
# Date: 16.07.2025
# Last date edited: 08.09.2025
# ****************************************************************************** #

# Some info about the variables

# attended – Attended or Did Not Attend
# This variable indicates whether the patient attended the appointment.
# The values are:

# 2 = appointment cancelled by or on behalf of the patient
# 3 = didn't attended - no advance warning given
# 4 = Appointment cancelled or postponed by the health care provider
# 5 = seen, having attended on time or, if late, before the relevant care professional care was ready to see the patient
# 6 = arrived late after the relevant care professional was ready to see the patient, but was seen
# 7 = didn't attended - patient arrived late
# 9 = not known

# atentype – Attendance Type
# This describes the nature of the appointment:

# 1 = Attended first appointment
# 2 = Attended subsequent appointment
# 3 = Attended but first/subsequent/tele unknown
# 4 = Did not attend first appointment
# 5 = Did not attend subsequent appointment
# 6 = Did not attend but first/subsequent/tele unknown
# 7 = Patient cancelled first appointment
# 8 = Patient cancelled subsequent appointment
# 9 = Patient cancelled but first/subsequent/tele unknown
# 10 = Hospital postponed/cancelled first appointment
# 11 = Hospital postponed/cancelled subsequent appointment
# 12 = Hospital postponed/cancelled but first/subsequent unknown
# 13 = Not known
# 21 = Attended first tele consultation
# 22 = Attended subsequent tele consultation
# 24 = Did not attend first tele consultation
# 25 = Did not attend subsequent tele consultation
# 27 = Patient cancelled first tele consultation
# 28 = Patient cancelled subsequent tele consultation
# 30 = Hospital postponed/cancelled first tele consultation
# 31 = Hospital postponed/cancelled subsequent tele consultation

# firstatt – First Attendance
# This is a character field that further categorizes the appointment:

# 1 = First attendance face to face
# 2 = Follow-up attendance face to face
# 3 = First telephone or telemedicine consultation
# 4 = Follow-up telephone or telemedicine consultation
# X = Not known (from 2008/09)
# 9 = Not known (prior to 2008/09)

# Starting with cohort_table from CleaningCohort_Raw_new.R ----
table_one_op_raw <- cohort_table

# Importing the appointments data ----
validation_appointment <- my_dm$validation_hesop_appointment %>% collect() %>% 
  # Filter such that we have the visits between the study start and study end dates
  filter(apptdate >= study_start_date & apptdate <= study_end_date)

# Merge with cohort data ----
table_one_op_raw_appt <- left_join(table_one_op_raw, 
                                   validation_appointment, 
                                   by = "patid") %>% 
  # in order to count all the visits  from index date until follow-up end date.
  filter(apptdate >= index_date & apptdate <= follow_up_end)

# Merge with clinical table ----
validation_clinical <- my_dm$validation_hesop_clinical %>% collect()

table_one_op_raw_appt <- left_join(table_one_op_raw_appt, 
                                   validation_clinical,
                                     by = c("patid", "attendkey"))

# Remove unattended appointments ----
names(table_one_op_raw_appt)
table(table_one_op_raw_appt$attended)

# 2 = appointment cancelled by or on behalf of the patient
# 3 = didn't attended - no advance warning given
# 4 = Appointment cancelled or postponed by the health care provider
# 5 = seen, having attended on time or, if late, before the relevant care professional care was ready to see the patient
# 6 = arrived late after the relevant care professional was ready to see the patient, but was seen
# 7 = didn't attended - patient arrived late
# 9 = not known

## Check if there are unattended appts ----
# If true, then we are unattended appts
any(
  table_one_op_raw_appt %>% 
    filter(!(attended %in% c(5, 6))) %>% 
    summarise(n = n()) %>% 
    pull(n) > 0
)

# Following the document shared by Antonella: https://unioxfordnexus-my.sharepoint.com/:w:/g/personal/orms0952_ox_ac_uk/EW9FQGi8RZVDitmfMqPdMV4BzPDTYbF_6fGUede_-XSExw?e=dctIGD
# We will only keep attended = 5, because; 
# From Antonella, “If attended <> 5 discard visit appointment detail (we accept only attended = 5)" 
# we include only attended=5 because in the mapped data that's the only type of appointment included.
table_one_op_raw_appt <- table_one_op_raw_appt %>% 
  filter(attended == 5)

# Merge cohort data
# merge back to cohort data, inorder to have patients have been filtered out 
# in the previous step
table_one_op_raw_appt <- left_join(
  cohort_table %>% select(patid, index_date, follow_up_end), table_one_op_raw_appt,
  by = c(
    "patid", "index_date","follow_up_end"
  )
)

# Recode variables ----
table_one_op_raw_appt_recoded <- table_one_op_raw_appt %>% 
  mutate(mainspef = factor(trimws(as.character(mainspef)))) %>% 
  mutate_at(c("attended", "atentype", "firstatt", "mainspef", "tretspef"), as.factor) %>%
  mutate(
    attended_recoded = fct_recode(
      attended,
      # "Appointment cancelled by or on behalf of the patient" = "2",
      # "Did not attend – no advance warning given" = "3",
      # "Appointment cancelled or postponed by the health care provider" = "4",
      "Seen – attended on time or, if late, before the relevant care professional was ready to see the patient" = "5"
      # "Arrived late after the relevant care professional was ready to see the patient, but was seen" = "6",
      # "Did not attend – patient arrived late" = "7",
      # "Attendance status not known" = "9"
      )) %>% 
  mutate(
    atentype_recoded = fct_recode(
      atentype,
      "Attended first appointment" = "1",
      "Attended subsequent appointment" = "2",
      "Attended but first/subsequent/tele unknown" = "3",
      "Did not attend first appointment" = "4",
      "Did not attend subsequent appointment" = "5",
      "Did not attend but first/subsequent/tele unknown" = "6",
      "Patient cancelled first appointment" = "7",
      "Patient cancelled subsequent appointment" = "8",
      "Patient cancelled but first/subsequent/tele unknown" = "9",
      "Hospital postponed/cancelled first appointment" = "10",
      "Hospital postponed/cancelled subsequent appointment" = "11",
      "Hospital postponed/cancelled but first/subsequent unknown" = "12",
      "Not known" = "13",
      "Attended first tele consultation" = "21",
      "Attended subsequent tele consultation" = "22",
      "Did not attend first tele consultation" = "24",
      "Did not attend subsequent tele consultation" = "25",
      "Patient cancelled first tele consultation" = "27",
      "Patient cancelled subsequent tele consultation" = "28",
      "Hospital postponed/cancelled first tele consultation" = "30",
      "Hospital postponed/cancelled subsequent tele consultation" = "31"
    )
  ) %>% 
  mutate(
    firstatt_recoded = fct_recode(
      firstatt,
      "First attendance face to face" = "1",
      "Follow-up attendance face to face" = "2",
      "First telephone or telemedicine consultation" = "3",
      "Follow-up telephone or telemedicine consultation" = "4",
      "Not known (from 2008/09)" = "X",
      "Not known (prior to 2008/09)" = "9"
    )
  ) %>% 
  
  mutate(
    mainspef_recoded = fct_recode(
      mainspef, 
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
    ),
    tretspef_recoded = fct_recode(
      tretspef, 
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
  )%>% 
  mutate(
    opertn_01 = ifelse(
      opertn_01 == "&" |opertn_01 == "-", NA, opertn_01
    )
  ) %>% 
  mutate(
    user_status = if_else(is.na(apptdate), "Non-User", "User")
    )%>%

  mutate(
    mainspef_recoded = if_else(mainspef_recoded == "&", NA, mainspef_recoded)
    ) %>%
  mutate(
    tretspef_recoded = if_else(tretspef_recoded == "&", NA, tretspef_recoded)
    )

# count of patients with appointments/or not
(users_pts_appts <- table_one_op_raw_appt_recoded %>% 
  group_by(user_status) %>% 
  summarise(n_pts = n_distinct(patid),
            .groups = "drop")
)

# Count of patients users
users_status_appts <- table_one_op_raw_appt_recoded %>% 
  filter(user_status == "User") %>% 
  group_by(attended_recoded) %>% 
  summarise(
   n_pts = n_distinct(patid),
   n_appts = n(),
   .groups = "drop"
  )

## Cleaning Diagnosis codes ----
diag_codes_data <- rio::import("HRG_ICD10_OPCS_ICD10.csv") %>% 
  janitor::clean_names() %>% 
  select(!icd_10_edition)

table_one_op_raw_appt_recoded <- table_one_op_raw_appt_recoded %>% 
  left_join(diag_codes_data %>% rename("diag_01" = "icd_10_code",
                                       "diag_01_description" =  "description"), 
            by = "diag_01")

table_one_op_raw_appt_recoded <- table_one_op_raw_appt_recoded %>% 
  mutate(collapsed_diag_01 = str_sub(diag_01, 1, 4)) %>% 
  mutate(diag_01_description = paste(collapsed_diag_01 , diag_01_description, sep = "-")) %>% 
  mutate(diag_01_description = case_when(diag_01_description == "R69X-NA" ~ "R69X-Unknown and unspecified causes of morbidity",
                                         TRUE ~ diag_01_description)) %>% 
  mutate(diag_01_description = if_else(diag_01_description == "NA-NA", NA, diag_01_description))

## Cleaning Procedures codes ----
# For more information please visit systems.hscic.gov.uk/data/clinicalcoding/codingstandards/opcs4 
# OPSC
opertn_codes_data <- rio::import("HRG_ICD10_OPCS_OPCS4.csv") %>% 
  janitor::clean_names() 


table_one_op_raw_appt_recoded <- table_one_op_raw_appt_recoded %>% 
  left_join(opertn_codes_data %>% rename("opertn_01" = "opcs_code",
                                         "opertn_01_description" =  "description"), 
            by = "opertn_01")

table_one_op_raw_appt_recoded <- table_one_op_raw_appt_recoded %>% 
  mutate(collapsed_opertn_01 = str_sub(opertn_01, 1, 4)) %>% 
  mutate(opertn_01_description = if_else(!is.na(collapsed_opertn_01),
                                         paste(collapsed_opertn_01, opertn_01_description, sep = "-"), NA))

## Reorder factor by frequency ----
# table_one_op_raw_appt_recoded <- table_one_op_raw_appt_recoded %>%
#   mutate_at(c("mainspef_recoded", "tretspef_recoded"), as.factor) %>%
#   mutate(follow_up_reason = forcats::fct_infreq(follow_up_reason),
#          attended_recoded = forcats::fct_infreq(attended_recoded),
#          atentype_recoded = forcats::fct_infreq(atentype_recoded),
#          firstatt_recoded = forcats::fct_infreq(firstatt_recoded),
#          mainspef_recoded = forcats::fct_infreq(mainspef_recoded),
#          tretspef_recoded = forcats::fct_infreq(tretspef_recoded),
#          diag_01_description = forcats::fct_infreq(diag_01_description),
#          opertn_01_description = forcats::fct_infreq(opertn_01_description)) %>% 
#   mutate(follow_up_time = as.numeric(follow_up_time))

## Gtsumaary table ----
# (op_gt_summary_table <- table_one_op_raw_appt_recoded %>% 
#     gtsummary::tbl_summary(
#       include = c(attended_recoded,
#                   atentype_recoded, firstatt_recoded, mainspef_recoded,
#                   tretspef_recoded,diag_01_description, 
#                   opertn_01_description,user_status),
#       label = list(
#         attended_recoded = "Attended or Did Not Attend",
#         atentype_recoded = "Attendance Type",
#         firstatt_recoded = "First Attendance",
#         mainspef_recoded = "Main Specialty",
#         tretspef_recoded = "Treatment Specialty",
#         diag_01_description = "Main Diagnosis",
#         opertn_01_description = "Main Procedure",
#         user_status = "OP HealthCare Status"
#       ),
#       missing = "no"
#     ) %>%
#     gtsummary::modify_header(label = "**Variable**"))

# List of summaries
summaries_op <- list(
  attended_recoded_summary = table_one_op_raw_appt_recoded %>% 
    select(patid, attendkey, attended_recoded) %>% distinct() %>%
    # filter(!is.na(attended_recoded)) %>%
    count(attended_recoded, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "Attended or Did Not Attend",
           Category = attended_recoded),
  
  atentype_recoded_summary = table_one_op_raw_appt_recoded %>% 
    select(patid, attendkey, atentype_recoded) %>% distinct() %>%
    # filter(!is.na(atentype_recoded)) %>%
    count(atentype_recoded, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "Attendance Type",
           Category = atentype_recoded),
  
  firstatt_recoded_summary = table_one_op_raw_appt_recoded %>%
    select(patid, attendkey, firstatt_recoded) %>% 
    distinct() %>%
    # filter(firstatt_recoded != "NA-NA") %>%
    count(firstatt_recoded, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "First Attendance",
           Category = firstatt_recoded),
  
 mainspef_recoded_summary = table_one_op_raw_appt_recoded %>% 
    select(patid, attendkey, mainspef_recoded) %>% distinct() %>%
    # filter(!is.na(mainspef_recoded)) %>%
    count(mainspef_recoded, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "Main Specialty",
           Category = mainspef_recoded) %>% 
   mutate(Category = if_else(percent_epi < 1, "Other Main Specialty", Category)) %>% 
   group_by(Variable, Category) %>%
   summarise(count_of_epi = sum(count_of_epi),
             percent_epi = round(sum(percent_epi), 2),
             .groups = "drop"),
  
  tretspef_recoded_summary = table_one_op_raw_appt_recoded %>% 
    select(patid, attendkey, tretspef_recoded) %>% distinct() %>%
    # filter(!is.na(tretspef_recoded)) %>%
    count(tretspef_recoded, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "Treatment Specialty",
           Category = tretspef_recoded) %>% 
   mutate(Category = if_else(percent_epi < 1, "Other Treatment Specialty", Category)) %>% 
   group_by(Variable, Category) %>%
   summarise(count_of_epi = sum(count_of_epi),
             percent_epi = round(sum(percent_epi), 2),
             .groups = "drop"),
 
 diag_01_description_summary = table_one_op_raw_appt_recoded %>%
   select(patid, attendkey, diag_01_description) %>% 
   distinct() %>%
   # filter(diag_01_description != "NA-NA") %>%
   count(diag_01_description, name = "count_of_epi") %>%
   mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
          Variable = "Main Diagnosis",
          Category = diag_01_description) %>% 
   mutate(Category = if_else(percent_epi < 1, "Other Main Diagnosis", Category)) %>% 
   group_by(Variable, Category) %>%
   summarise(count_of_epi = sum(count_of_epi),
             percent_epi = round(sum(percent_epi), 2),
             .groups = "drop"),
 
 opertn_01_description_summary = table_one_op_raw_appt_recoded %>%
   select(patid, attendkey, opertn_01_description) %>% 
   distinct() %>%
   # filter(opertn_01_description != "NA-NA") %>%
   count(opertn_01_description, name = "count_of_epi") %>%
   mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
          Variable = "Main Procedure",
          Category = opertn_01_description) %>% 
   mutate(Category = if_else(percent_epi < 1, "Other Main Procedure", Category)) %>% 
   group_by(Variable, Category) %>%
   summarise(count_of_epi = sum(count_of_epi),
             percent_epi = round(sum(percent_epi), 2),
             .groups = "drop"),
  
  user_status_summary = table_one_op_raw_appt_recoded %>% 
    select(patid, user_status) %>% distinct() %>%
    count(user_status, name = "count_of_epi") %>%
    mutate(percent_epi = round(count_of_epi / sum(count_of_epi) * 100, 2),
           Variable = "OP Health Care Service Status",
           Category = user_status)
)

# Combine them
table2_combined_summary_op <- bind_rows(summaries_op, .id = "summary_name") %>%
  select(Variable, Category, count_of_epi, percent_epi) %>%
  arrange(Variable, desc(count_of_epi)) %>% 
  mutate(Result = paste0(count_of_epi, " (", percent_epi, "%)")) %>% 
  select(!c(count_of_epi, percent_epi)) %>% 
  mutate(Category = if_else(is.na(Category), "Missing", Category))


# Export ----
# op_gt_summary_table_df <- as_tibble(op_gt_summary_table) %>% 
#   rename("Variable" = "**Variable**",
#          "N (Appointments) = 170,321" = "**N = 170,321**") %>% 
#   mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>% 
#   filter(Variable != "&")

rio::export(table2_combined_summary_op, here::here("CPRD Raw Analysis", "Results",
                                               "SecondaryCareAnalysis", "OP","Table1_OP_Raw.xlsx"))
