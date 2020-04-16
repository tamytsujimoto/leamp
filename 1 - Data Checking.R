library(dplyr)

# AMP_PROC_20181102.csv: Patients undergoing amputation as well as pre-operative data, 
# peri-operative data and 30 day outcomes.
# AMP_LTF_20181102.csv: Long term follow-up data

proc = 
  data.table::fread("../../1 - From PI/AMP_20181102/AMP_PROC_20181102.csv") %>% 
  select(PATIENTID,
         PRIMPROCID,
         SURGYEAR,
         SURGMONTH,
         PROC_SURVIVALDAYS,
         DEAD,
         ANESTHESIA,
         AGE,
         GENDER,
         RACE,
         SURGERYSIDE,
         INDICATION_R,
         INDICATION_L,
         PREOP_SMOKING,
         PREOP_DIABETES,
         HTCM,
         WTKG,
         HTN,
         PRIOR_CAD,
         PRIOR_CHF,
         COPD,
         PREOP_CREAT,
         DIALYSIS,
         POSTOP_MI,
         POSTOP_RESPIRATORY,
         POSTOP_DYSRHYTHMIA,
         POSTOP_CHF,
         RTORBLEED,
         RTOR,
         POSTOP_LOS,
         TOTALPROCTIME,
         LEVEL_R,
         LEVEL_L)


ltf = 
  data.table::fread("../../1 - From PI/AMP_20181102/AMP_LTF_20181102.csv") %>% 
  select(PATIENTID,
         PRIMPROCID,
         LTF_ID,
         LTF_DAYS,
         LTF_MORTCAUSE,
         LTF_CONTACT,
         LTF_PROC_SURVIVALDAYS = PROC_SURVIVALDAYS,
         LTF_DEAD = DEAD,
         LTF_SURGERYSIDE = SURGERYSIDE) 

#################################
# Checking unicity of both data #
#################################

proc %>% 
  group_by(PATIENTID, PRIMPROCID) %>% # unique with these keys
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  glimpse

proc %>% 
  group_by(PATIENTID) %>% 
  summarise(n = n()) %>% 
  glimpse  # 10,018 patients

ltf %>% 
  group_by(PATIENTID, PRIMPROCID, LTF_ID) %>% # unique with these keys
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  glimpse

ltf %>% 
  group_by(PATIENTID) %>% 
  summarise(n = n()) %>% 
  glimpse  # 5,063 patients

#################################
# Creating and merging datasets #
#################################

# Selecting first amputation procedure performed per patient

proc2 =
  proc %>% 
  mutate(SURGMONTH_n = car::Recode(SURGMONTH, 
                                   "'jan' = 1;
                                    'feb' = 2;
                                    'mar' = 3;
                                    'apr' = 4;
                                    'may' = 5;
                                    'jun' = 6; 
                                    'jul' = 7;
                                    'aug' = 8;
                                    'sep' = 9;
                                    'oct' = 10;
                                    'nov' = 11;
                                    'dec' = 12")) %>% 
  arrange(PATIENTID, SURGYEAR, SURGMONTH_n) %>% 
  group_by(PATIENTID) %>% 
  filter(row_number()==1) %>% 
  ungroup

# Selecting most updated follow up information

ltf2 =
  ltf %>% 
  arrange(PATIENTID, PRIMPROCID, desc(LTF_PROC_SURVIVALDAYS)) %>% 
  group_by(PATIENTID, PRIMPROCID) %>% 
  filter(row_number() == 1) %>% 
  mutate(flag_ltf = 1)

# Merging the two datasets
proj1 = 
  proc2 %>% 
  left_join(ltf2, by = c('PATIENTID', 'PRIMPROCID'))

#################################################
# Checking information - survival and mortality #
#################################################

proj1 %>% filter(is.na(PROC_SURVIVALDAYS)) %>% glimpse # 4,368 patients w/ PROC_SURVIVALDAYS
proj1 %>% filter(!is.na(PROC_SURVIVALDAYS)) %>% glimpse # 5,650 patients w/o PROC_SURVIVALDAYS

proj1 %>% 
  filter(!is.na(PROC_SURVIVALDAYS)) %>% 
  filter(PROC_SURVIVALDAYS != LTF_PROC_SURVIVALDAYS) %>% 
  glimpse # 0 patients not matching PROC_SURVIVALDAYS

proj1 %>% 
  filter(is.na(PROC_SURVIVALDAYS)) %>% 
  filter(flag_ltf == 1, !is.na(LTF_PROC_SURVIVALDAYS)) %>% 
  glimpse # 0 patients found in ltf with proc_survivaldays


proj1 %>% filter(is.na(DEAD)) %>% glimpse # 4,368 patients w/ DEAD
proj1 %>% filter(!is.na(DEAD)) %>% glimpse # 5,650 patients w/o DEAD

proj1 %>% 
  filter(!is.na(DEAD)) %>% 
  filter(DEAD != LTF_DEAD) %>% 
  glimpse # 0 patients not matching DEAD

proj1 %>% 
  filter(is.na(DEAD)) %>% 
  filter(flag_ltf == 1, !is.na(LTF_DEAD)) %>% 
  glimpse # 0 patients found in ltf with proc_survivaldays

############################
# Saving complete data set #
############################

saveRDS(proc2, file = "../data/pad_data.rds") # Using information from PROC database only


