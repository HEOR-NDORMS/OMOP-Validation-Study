# ****************************************************************************** #
#	Project name: Validation Study			    	
#	Author: Njoki Njuki	
# Do file name: AE Table 2: description of the patients' AE attendances
# Date: 16.07.2025
# Last date edited: 30.10.2025
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

# Starting with cohort_table from CleaningCohort_Raw_new.R ----
table_two_ae_raw <- cohort_table %>% 
  # Select the variables of interest
  select(patid, index_date, follow_up_end)


# Importing the AE attendance data ----
validation_ae_attendance <- my_dm$validation_hesae_attendance %>% collect() %>%
  # Filter such that we have the visits between the study start and study end dates
  filter(arrivaldate >= study_start_date & arrivaldate <= study_end_date)

# Merge with cohort data ----
table_two_ae_raw_a <- left_join(table_two_ae_raw, validation_ae_attendance, 
                                by = "patid") %>%
  # in order to count all the visits  from index date until follow-up end date.
  filter(arrivaldate >= index_date & arrivaldate <= follow_up_end)

# Import other AE datasets and merge with attendance data ----
## Treatment data ----
validation_ae_treatment <- my_dm$validation_hesae_treatment %>% collect()
table_two_ae_raw_t <- left_join(table_two_ae_raw_a,
                                validation_ae_treatment, 
                                by = c("patid", "aekey")) %>% 
  distinct(patid, aekey, treat, .keep_all = TRUE)

## Investigation data ----
validation_ae_investigation <- my_dm$validation_hesae_investigation %>% collect()
table_two_ae_raw_i <- left_join(table_two_ae_raw_a,
                                validation_ae_investigation, 
                                by = c("patid", "aekey")) %>% 
  distinct(patid, aekey, invest, .keep_all = TRUE)

# Merge the treatment and investigations tables
# the id variable here is preventing duplication resulted in merging
table_two_ae_raw_final <- full_join(table_two_ae_raw_t %>% group_by(patid, aekey) %>% 
                                      mutate(id = row_number()) %>% ungroup(), 
                                    table_two_ae_raw_i %>% group_by(patid, aekey) %>% 
                                      mutate(id = row_number()) %>% ungroup(),
                                     by = c("patid", "aekey", "id","index_date",
                                            "arrivaldate", "follow_up_end"))

# Merge back to cohort data, so that we can have the number of the cohort pts
# This will allow us to have users and non-users
table_two_ae_raw_final <- left_join(cohort_table %>% 
                                      select(patid, index_date, follow_up_end), 
                                    table_two_ae_raw_final, 
                                    by = c("patid", "index_date", "follow_up_end"))

# Create the users status
table_two_ae_raw_final <- table_two_ae_raw_final %>% 
  mutate(
    user_status = if_else(is.na(arrivaldate), "Non-User", "User")
  )

# Cleaning the data variables ----
# clean invest_01 variable
# Example R script to clean 'invest' variable

# Create a named vector (mapping detailed codes to grouped codes)
invest_mapping <- c(
  # 06 group
  "06" = "06", "0601" = "06", "061" = "06", "062" = "06", "063" = "06", "06A" = "06", "06N" = "06",
  
  # 21 group
  "21" = "21", "211" = "21",
  
  # 02 group
  "02" = "02", "0201" = "02", "020306" = "02", "021" = "02", "022" = "02", "02201" = "02", "02202" = "02", "02203" = "02", 
  "023" = "02", "02A" = "02", "02N" = "02",
  
  # 05 group
  "05" = "05", "0501" = "05", "050102" = "05", "0501A" = "05", "0501N" = "05",
  "0502" = "05", "0502N" = "05", "0503" = "05", "050303" = "05", "0503A" = "05", "0503N" = "05",
  "0504" = "05", "0504N" = "05", "0505" = "05", "0506" = "05",
  "051" = "05", "0514" = "05", "0517" = "05",
  "052" = "05", "053" = "05", "054" = "05", "055" = "05", "056" = "05", "058" = "05",
  "05C" = "05", "05G" = "05",
  
  # 99 group
  "99" = "99", "9905" = "99", "9905A" = "99", "993" = "99", "994" = "99", "995" = "99", "997" = "99", "99A" = "99",
  
  # 17 group
  "17" = "17", "1701" = "17",
  
  # 16 group
  "16" = "16", "1601" = "16", "1601N" = "16", "16A" = "16",
  
  # 18 group
  "18" = "18",
  
  # 15 group
  "15" = "15",
  
  # 20 group
  "20" = "20",
  
  # 03 group
  "03" = "03", "0301" = "03", "0301N" = "03", "0302" = "03", "030301" = "03", "030302" = "03", "030303" = "03", 
  "030305" = "03", "030317" = "03", "0304" = "03", "0305" = "03",
  "031" = "03", "0314" = "03", "032" = "03", "033" = "03", "034" = "03",
  "03A" = "03", "03B" = "03",
  
  # 14 group
  "14" = "14", "1401" = "14",
  
  # 04 group
  "04" = "04", "041" = "04", "04A" = "04",
  
  # 07 group
  "07" = "07", "0719" = "07", "072" = "07", "073" = "07", "074" = "07",
  
  # 08 group
  "08" = "08",
  
  # 10 group
  "10" = "10", "10N" = "10",
  # 14 group
  "14" = "14", "1401"  = "14",
  
  # 19 group
  "19" = "19",
  
  # 23 group
  "23" = "23", "23C" = "23",
  
  # 22 group
  "22" = "22",
  
  # 01 group
  "01" = "01", "010301" = "01", "010408" = "01", "011" = "01", "013" = "01", "014" = "01", "015" = "01",
  "01A" = "01", "01A12" = "01", "01A212" = "01", "01A22" = "01", "01A23" = "01", "01A26" = "01", "01A261" = "01", "01A262" = "01", "01A291" = "01", 
  "01A4" = "01", "01A42" = "01", "01A44" = "01", "01A45" = "01", "01A5" = "01", "01A51" = "01", "01A53" = "01", "01A57" = "01", "01A59" = "01",
  "01B" = "01", "01H03A" = "01", "01L" = "01", "01L01A" = "01", "01L02" = "01", "01L02A" = "01", "01L02N" = "01", "01L04A" = "01", "01L05" = "01", "01L05A" = "01", "01L05N" = "01", "01L06" = "01", "01L06A" = "01", "01L06N" = "01", "01L08" = "01", "01L08A" = "01", "01L08N" = "01",
  "01S" = "01", "01S01" = "01", "01S01N" = "01", "01S02" = "01", "01S02A" = "01", "01S04A" = "01",
  "01T" = "01", "01T01" = "01", "01T01A" = "01", "01T02" = "01", "01T02A" = "01", "01T02N" = "01", "01T03" = "01", "01T03A" = "01", "01T03N" = "01",
  "01U" = "01", "01U01" = "01", "01U01A" = "01", "01U01N" = "01", "01U03A" = "01", "01U04" = "01", "01U04A" = "01", "01U04N" = "01",
  "01U05" = "01", "01U05A" = "01", "01U05N" = "01", "01U06" = "01", "01U06A" = "01", "01U07" = "01", "01U07A" = "01", "01U08" = "01", "01U08A" = "01", "01U08N" = "01", "01U10" = "01", "01U10A" = "01", "01U10N" = "01",
  
  # 10 group
  "10" = "10", "10N" = "10",
  
  # 12 group
  "12" = "12", "1201" = "12", "1201N" = "12", "1202" = "12", "1202A" = "12", "1202N" = "12",
  "121" = "12", "12A" = "12", "12B" = "12", "12C" = "12",
  
  # 11 group
  "11" = "11",
  
  # 24 group
  "24" = "24", "241" = "24", "25" = "24"
)

# Apply mapping
table_two_ae_raw_final$invest_clean <- invest_mapping[as.character(table_two_ae_raw_final$invest)]

# Check result
table(table_two_ae_raw_final$invest_clean, useNA = "ifany")

# Create mapping for treat
treat_mapping <- c(
  # 40 group
  "40" = "40",
  
  # 25 group
  "25" = "25", "245" = "25", "246" = "25", "246001" = "25", "259" = "25", "268" = "25",
  
  # 38 group
  "38" = "38",
  
  # 231 group
  "231" = "231",
  
  # 41 group
  "41" = "41", "041" = "41", "0411" = "41",
  
  # 15 group
  "15" = "15",
  
  # 16 group
  "16" = "16",
  
  # 21 group
  "21" = "21", "201406" = "21", "201412" = "21", "201504" = "21",
  
  # 12 group
  "12" = "12",
  
  # 43 group
  "43" = "43", "43 M" = "43", "43 O" = "43",
  
  # 44 group
  "44" = "44",
  
  # 42 group
  "42" = "42",
  
  # 13 group
  "13" = "13",
  
  # 182 group
  "182" = "182",
  
  # 181 group
  "181" = "181",
  
  # 19 group
  "19" = "19",
  
  # 49 group
  "49" = "49",
  
  # 50 group
  "50" = "50",
  
  # 232 group
  "232" = "232",
  
  # 234 group
  "234" = "234",
  
  # 233 group
  "233" = "233",
  
  # 235 group
  "235" = "235",
  
  # 511 group
  "511" = "511", "511001" = "511", "51106" = "511", "5111" = "511", "511A" = "511", "511AA" = "511", "511K" = "511", "511LLL" = "511",
  
  # 291 group
  "291" = "291", "29103" = "291", "29104" = "291", "29106" = "291", "29108" = "291", "2911" = "291",
  
  # 292 group
  "292" = "292", "29202" = "292", "29204" = "292", "29211" = "292", "292J" = "292",
  
  # 57 group
  "57" = "57", "57 YY" = "57",
  
  # 14 group
  "14" = "14",
  
  # 282 group
  "282" = "282",
  
  # 17 group
  "17" = "17",
  
  # 27 group
  "27" = "27", "274" = "27",
  
  # 20 group
  "20" = "20",
  
  # 46 group
  "46" = "46",
  
  # 08 group
  "08" = "08",
  
  # 011 group
  "011" = "011", "01105" = "011", "01106" = "011", "01107" = "011",
  
  # 042 group
  "042" = "042", "0422" = "042",
  
  # 043 group
  "043" = "043", "0432" = "043",
  
  # 031 group
  "031" = "031", "03104" = "031", "03108" = "031", "0311" = "031", "03118" = "031",
  
  # 032 group
  "032" = "032",
  
  # 033 group
  "033" = "033",
  
  # 243 group
  "243" = "243",
  
  # 244 group
  "244" = "244",
  
  # 051 group
  "051" = "051", "0511" = "051", "0512" = "051", "051202" = "051", "051203" = "051", "051204" = "051", "051206" = "051", "0513" = "051", "051A" = "051", "051B" = "051",
  
  # 052 group
  "052" = "052", "0522" = "052",
  
  # 06 group
  "06" = "06", "06B" = "06",
  
  # 36 group
  "36" = "36", "360" = "36",
  
  # 53 group
  "53" = "53", "53A" = "53",
  
  # 101 group
  "101" = "101", "1013" = "101", "1017" = "101", "1018" = "101",
  
  # 102 group
  "102" = "102",
  
  # 103 group
  "103" = "103",
  
  # 47 group
  "47" = "47",
  
  # 37 group
  "37" = "37",
  
  # 56 group
  "56" = "56",
  
  # 551/552/553/554/555 group
  "551" = "551", "552" = "552", "553" = "553", "554" = "554", "555" = "555",
  
  # 221 group
  "221" = "221", "2211" = "221",
  
  # 521 group
  "521" = "521",
  
  # 522 group
  "522" = "522",
  
  # 092 group
  "092" = "092",
  
  # 091 group
  "091" = "091", "0911" = "091",
  
  # 54 group
  "54" = "54",
  
  # 99 group
  "99" = "99"
)

# Apply mapping
table_two_ae_raw_final$treat_clean <- treat_mapping[as.character(table_two_ae_raw_final$treat)]

# Check result
table(table_two_ae_raw_final$treat_clean, useNA = "ifany")

# Recode variables ----
table_two_ae_raw_recoded <- table_two_ae_raw_final %>%
  mutate_at(c("invest_clean", "treat_clean"), as.factor) %>% 
  mutate(invest_recoded = fct_recode(invest_clean,
                                     "X-ray" = "01",
                                     "ECG" = "02",
                                     "Haematology" = "03",
                                     "Cross match" = "04",
                                     "Biochemistry" = "05",
                                     "Urine" = "06",
                                     "Bacteriology" = "07",
                                     "Histology" = "08",
                                     "Computerised Tomography" = "09",
                                     "Ultrasound" = "10",
                                     "Magnetic Resonance Imaging (MRI)" = "11",
                                     "Other" = "99",
                                     "None" = "24",
                                     "Refraction, orthoptic tests and computerised visual fields" = "23",
                                     "Dental investigation" = "22",
                                     "Pregnancy test" = "21",
                                     "Serology" = "20",
                                     "Blood culture" = "19",
                                     "Taxicology" = "18",
                                     "Arterial/capillary blood gas" = "17",
                                     "Cardiac enzymes" = "16",
                                     "Immunology" = "15",
                                     "Clotting studies" = "14",
                                     "Computerised tomography (excluding genito urinary contrast examination/tomography)" = "12",
                                     
                                     
  ),
  
  treat_recoded = fct_recode(treat_clean,
                             "Dressing" = "01",
                             "Bandage" = "02",
                             "Sutures" = "03",
                             "Wound closure - steristrips" = "041",
                             "Wound closure - wound glue" = "042",
                             "Wound closure - other" = "043",
                             "Plaster of Paris" = "05",
                             "Splint" = "06",
                             "Prescription" = "07",
                             "Removal foreign body" = "08",
                             "Physiotherapy - manipulation" = "091",
                             "Physiotherapy - other" = "092",
                             "Reduction" = "10",
                             "I&D (Incision & Drainage)" = "11",
                             "Intravenous cannula" = "12",
                             "Central line" = "13",
                             "Lavage/Emesis" = "14",
                             "Intubation" = "15",
                             "Chest drain" = "16",
                             "Urinary catheter" = "17",
                             "Defibrillation/pacing" = "18",
                             "Resuscitation" = "19",
                             "Minor surgery" = "20",
                             "Observation" = "21",
                             "Guidance - written" = "221",
                             "Guidance - verbal" = "222",
                             "Anaesthesia - general anaesthetic" = "231",
                             "Anaesthesia - local anaesthetic" = "232",
                             "Anaesthesia - regional block" = "233",
                             "Anaesthesia - entonox" = "234",
                             "Anaesthesia - sedation" = "235",
                             "Anaesthesia - other" = "236",
                             "Tetanus - immune" = "241",
                             "Tetanus - tetanus toxoid course" = "242",
                             "Tetanus - tetanus toxoid booster" = "243",
                             "Tetanus - human immunoglobulin" = "244",
                             "Tetanus - combined tetanus/diphtheria course" = "245",
                             "Tetanus - combined tetanus/diphtheria booster" = "246",
                             "Nebuliser" = "25",
                             "Parenteral thrombolysis" = "28",
                             "Other Parenteral drugs" = "29",
                             "Other" = "27",
                             "None" = "99",
                             "Dressing minor wound/burn/eye" = "011",
                             "Primary sutures" = "031",
                             "Secondary/complex sutures" = "032",
                             "Removal of sutures/clips" = "033",
                             "Application plaster of paris" = "051",
                             "Removal plaster of paris" = "052",
                             "Manipulation of upper limb fracture" = "101",
                             "Manipulation of lower limb fracture" = "102",
                             "Manipulation of dislocation" = "103",
                             "Defibrillation" = "181",
                             "External pacing" = "182",
                             "Prescription/medicines prepared to take away" = "57",
                             "Dental treatment" = "56",
                             "Eye - epilation of lashes" = "554",
                             "Eye - laser of retina or posterior capsule" = "552",
                             "Eye - orthoptic exercises" = "551",
                             "Social work intervention" = "54",
                             "Loan of walking aid (cructches)" = "53",
                             "Occupational therapy - OT activities of daily living equipment provision" = "522",
                             "Occupational therapy - OT functional assessment" = "521",
                             "Medication administered - oral" = "511",
                             "Cooling - control body temperature" = "50",
                             "Active rewarming of the hypothermic patient" = "49",
                             "Joint aspiration" = "47",
                             "Lumbarr puncture" = "46",
                             "Blood product transfusion" = "44",
                             "Infusion fluids" = "43",
                             "aterial line" = "46",
                             "Continuous positive airways pressure/nasal intermittent positive pressure ventilation/bag valve mask" = "41",
                             "supplemental oxygen" = "40",
                             "Nasal airway" = "38",
                             "Epistaxis control" = "37",
                             "Sling/collar cuff/broad arm sling" = "36",
                             "Other Parental drugs - intravenous drug" = "291",
                             "Parental thrombolysis" = "282",
                             
                             
                             
                             
                             
                             
                             
  )
  
  )

# List of summaries
summaries_ae <- list(
  invest_recoded = table_two_ae_raw_recoded %>% 
    select(patid, aekey, invest_recoded) %>% distinct() %>%
    count(invest_recoded, name = "count_of_att") %>%
    mutate(percent_att = round(count_of_att / sum(count_of_att) * 100, 2),
           Variable = "Main Investigation",
           Category = invest_recoded) %>% 
    mutate(Category = if_else(percent_att < 1, "Other Main Investigation", Category)) %>% 
    group_by(Variable, Category) %>%
    summarise(count_of_att = sum(count_of_att),
              percent_att = round(sum(percent_att), 2),
              .groups = "drop"),
  
  treat_recoded = table_two_ae_raw_recoded %>% 
    select(patid, aekey, treat_recoded) %>% distinct() %>%
    count(treat_recoded, name = "count_of_att") %>%
    mutate(percent_att = round(count_of_att / sum(count_of_att) * 100, 2),
           Variable = "Main Treatment",
           Category = treat_recoded) %>% 
    mutate(Category = if_else(percent_att < 1, "Other Main Treatment", Category)) %>% 
    group_by(Variable, Category) %>%
    summarise(count_of_att = sum(count_of_att),
              percent_att = round(sum(percent_att), 2),
              .groups = "drop"),
  
  user_status_summary = table_two_ae_raw_recoded %>% 
    select(patid, user_status) %>% distinct() %>%
    count(user_status, name = "count_of_att") %>%
    mutate(percent_att = round(count_of_att / sum(count_of_att) * 100, 2),
           Variable = "AE Health Care Service Status",
           Category = user_status)
)

# Combine them
table2_combined_summary_ae <- bind_rows(summaries_ae, .id = "summary_name") %>%
  select(Variable, Category, count_of_att, percent_att) %>%
  arrange(Variable, desc(count_of_att)) %>% 
  mutate(Result = paste0(count_of_att, " (", percent_att, "%)")) %>% 
  select(!c(count_of_att, percent_att)) %>% 
  mutate(Category = if_else(is.na(Category), "Missing", Category))


# Export ----
rio::export(table2_combined_summary_ae, 
            here::here("CPRD Raw Analysis", "Results",
                       "SecondaryCareAnalysis", "AE","Table1_AE_Raw.xlsx"))
