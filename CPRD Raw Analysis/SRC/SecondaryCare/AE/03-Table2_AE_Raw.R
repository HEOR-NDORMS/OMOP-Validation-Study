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

# Create mapping for treat
treat_mapping <- c(
  # 40 group
  "40" = "40",
  
  # 25 group
  "25" = "25", "245" = "25", "246" = "25", "246001" = "25", 
  "259" = "25", "268" = "25",
  
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
  "511" = "511", "511001" = "511", "51106" = "511", "5111" = "511", 
  "511A" = "511", "511AA" = "511", "511K" = "511", "511LLL" = "511",
  
  # 291 group
  "291" = "291", "29103" = "291", "29104" = "291", "29106" = "291", 
  "29108" = "291", "2911" = "291",
  
  # 292 group
  "292" = "292", "29202" = "292", "29204" = "292", "29211" = "292", 
  "292J" = "292",
  
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
  "01" = "011", "00" = "011",
  
  # 042 group
  "042" = "042", "0422" = "042",
  
  # 043 group
  "043" = "043", "0432" = "043",
  
  # 031 group
  "031" = "031", "03104" = "031", "03108" = "031", "0311" = "031", 
  "03118" = "031", "03" = "031",
  
  # 032 group
  "032" = "032",
  
  # 033 group
  "033" = "033",
  
  # 243/244 group
  "243" = "243", "244" = "244",

  # 051 group
  "051" = "051", "0511" = "051", "0512" = "051", "051202" = "051", 
  "051203" = "051", "051204" = "051", "051206" = "051", "0513" = "051", 
  "051A" = "051", "051B" = "051", "51" = "051",
  
  # 052 group
  "052" = "052", "0522" = "052",
  
  # 06 group
  "06" = "06", "06B" = "06",
  
  # 36 group
  "36" = "36", "360" = "36", "036" = "36",
  
  # 53 group
  "53" = "53", "53A" = "53",
  
  # 101 group
  "101" = "101", "1013" = "101", "1017" = "101", "1018" = "101",
  
  # 102/103 group
  "102" = "102", "103" = "103",

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
  "521" = "521", "52" = "521",
  
  # 522 group
  "522" = "522",
  
  # 092 group
  "092" = "092",
  
  # 091 group
  "091" = "091", "0911" = "091",
  
  # 54 group
  "54" = "54",
  
  # 99 group
  "99" = "99",
  
  # 222 group
  "2221" = "222", "222001" = "222", "2222" = "222", "222A" = "222",

  # 02 group
  "02B" = "02", "022" = "02", "02D" = "02", 
  
  # 101 group
  "10" = "101",
  
  # 291 group
  "29" = "291",
  
  # 091 group
  "09" = "091", "91" = "091",
  
  # 221 group
  "22" = "221",
  
  # 513 group
  "51301" = "513",
  
  # 513 group
  "51301" = "513",
  
  # 511 group
  "51" = "51",
  
  # 241 group
  "24" = "241",
  
  # 291 group
  "29" = "291",
  
  # 233 group
  "233001" = "233",
  
  # 011 group
  "01" = "011",
  
  # 521 group
  "52" = "521",
  
  # 34 group
  "34B" = "34",
  
  # 092 group
  "92" = "092",
 
  # 55 group
  "55" = "551",
  
  # 232001 group
  "232001" = "232",

  # 04 group
  "04" = "041",
  
  # 512 group
  "51201" = "512", "512I" = "512"
  
)

# Apply mapping
validation_ae_treatment <- validation_ae_treatment %>%
  mutate(
    treat_clean = if_else(
      !is.na(treat_mapping[as.character(treat)]),
      treat_mapping[as.character(treat)],
      as.character(treat)
    )
  )

# Reshape the treat variable into long format
validation_ae_treatment_wide <- validation_ae_treatment %>%
  arrange(patid, aekey, treat_clean) %>%        # set order of codes inside each group
  group_by(patid, aekey) %>%
  mutate(treat_seq = row_number()) %>%               # 1,2,3... per patient+aekey
  ungroup() %>%
  pivot_wider(
    id_cols = c(patid, aekey),
    names_from = treat_seq,
    values_from = treat_clean,
    # make zero-padded column names like treat_01, treat_02, ...
    names_glue = "treat_{sprintf('%02d', treat_seq)}",
    values_fill = NA
  )

# Merge to the attendance data
table_two_ae_raw_t <- left_join(table_two_ae_raw_a,
                                validation_ae_treatment_wide, 
                                by = c("patid", "aekey")) 

## Investigation data ----
validation_ae_investigation <- my_dm$validation_hesae_investigation %>% collect()

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
validation_ae_investigation <- validation_ae_investigation %>%
  mutate(
    invest_clean = if_else(
      !is.na(invest_mapping[as.character(invest)]),
      invest_mapping[as.character(invest)],
      as.character(invest)
    )
  )

# Reshape the invest variable into long format
validation_ae_investigation_wide <- validation_ae_investigation %>%
  arrange(patid, aekey, invest_clean) %>%        # set order of codes inside each group
  group_by(patid, aekey) %>%
  mutate(invest_seq = row_number()) %>%               # 1,2,3... per patient+aekey
  ungroup() %>%
  pivot_wider(
    id_cols = c(patid, aekey),
    names_from = invest_seq,
    values_from = invest_clean,
    # make zero-padded column names like treat_01, treat_02, ...
    names_glue = "invest_{sprintf('%02d', invest_seq)}",
    values_fill = NA
  )

# Merge to the attendance data
table_two_ae_raw_i <- left_join(table_two_ae_raw_a,
                                validation_ae_investigation_wide, 
                                by = c("patid", "aekey")) 

# Merge the treatment and investigations tables
table_two_ae_raw_final <- full_join(table_two_ae_raw_t, 
                                    table_two_ae_raw_i,
                                     by = c("patid", "aekey", "index_date",
                                            "arrivaldate", "follow_up_end"))

# Merge back to cohort data, so that we can have the number of the cohort
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
