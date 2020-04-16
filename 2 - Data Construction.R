library(dplyr)

pad = 
  readRDS("../data/pad_data.rds") %>% 
  filter(!is.na(PROC_SURVIVALDAYS)) # Filtering cases without any follow up

# COVARIATES #

pad2 =
  pad %>% 
  mutate(GROUP = ifelse(is.na(ANESTHESIA), NA, 
                        ifelse(ANESTHESIA == 3, 1, 2)),
         RACE_B = ifelse(is.na(RACE), NA, 
                         ifelse(RACE == 3, 1, 0)),
         TISSUE_LOSS = ifelse(SURGERYSIDE %in% c(1,3) & INDICATION_R <= 2, 1,
                              ifelse(SURGERYSIDE %in% c(2,3) & INDICATION_L <= 2, 1, 0)),
         PREOP_SMOKING_D = ifelse(is.na(PREOP_SMOKING), NA,
                               ifelse(PREOP_SMOKING == 2, 1, 0)),
         PREOP_DIABETES_D = ifelse(is.na(PREOP_DIABETES), NA,
                                 ifelse(PREOP_DIABETES == 0, 0, 1)),
         PREOP_BMI = WTKG/(HTCM/100)^2,
         PREOP_OBESE = ifelse(is.na(PREOP_BMI), NA,
                              ifelse(PREOP_BMI >= 30, 1, 0)),
         PREOP_CAD = ifelse(is.na(PRIOR_CAD), NA,
                              ifelse(PRIOR_CAD == 0, 0, 1)),
         PREOP_CHF = ifelse(is.na(PRIOR_CHF), NA,
                            ifelse(PRIOR_CHF == 0, 0, 1)),
         PREOP_COPD = ifelse(is.na(COPD), NA,
                             ifelse(COPD == 0, 0, 1)),
         PREOP_CKD = ifelse(is.na(PREOP_CREAT), NA,
                            ifelse(PREOP_CREAT >= 1.8, 1, 0)),
         PREOP_DIALYSIS = ifelse(is.na(DIALYSIS), NA,
                            ifelse(DIALYSIS == 2, 1, 0)),
         FLAG_BKA = ifelse(LEVEL_R %in% 4, 1, 
                           ifelse(LEVEL_L %in% 4, 1, 0)),
         FLAG_AKA = ifelse(LEVEL_R %in% 6, 1, 
                           ifelse(LEVEL_L %in% 6, 1, 0)),
         LEVEL_AMP = ifelse(FLAG_BKA+FLAG_AKA == 0, '1. NO BKA AKA', 
                        ifelse(FLAG_BKA+FLAG_AKA==2, '4. BKA and AKA', 
                               ifelse(FLAG_BKA == 1, '2. BKA only', '3. AKA only'))))  

# OUTCOMES #

pad2 =
  pad2 %>% 
  mutate(POSTOP_MI_D = ifelse(is.na(POSTOP_MI), NA, 
                              ifelse(POSTOP_MI == 0, 0, 1)),
         POSTOP_RESPIR = ifelse(is.na(POSTOP_RESPIRATORY), NA, 
                              ifelse(POSTOP_RESPIRATORY == 0, 0, 1)),
         POSTOP_BLEED = ifelse(is.na(RTORBLEED), NA, 
                                   ifelse(RTORBLEED == 0, 0, 1)),
         POSTOP_RTOR = ifelse(is.na(RTOR), NA, 
                              ifelse(RTOR == 0, 0, 1)),
         POSTOP_MACE = ifelse(is.na(POSTOP_MI_D) & is.na(POSTOP_DYSRHYTHMIA) & is.na(POSTOP_CHF), NA, 
                              ifelse(POSTOP_MI_D == 1 | POSTOP_DYSRHYTHMIA == 1 | POSTOP_CHF == 1, 1, 0)),
         DEAD_30 = ifelse(DEAD == 1 & PROC_SURVIVALDAYS <= 30, 1, 0),
         POSTOP_MACE_DEAD = ifelse(POSTOP_MACE == 1, 1, 
                                   ifelse(DEAD_30 == 1, 1, 0))
         )

# Saving Final database

pad2 %>% 
  select(PATIENTID,
         GROUP,
         AGE,
         GENDER,
         RACE_B,
         TISSUE_LOSS,
         PREOP_SMOKING_D,
         PREOP_DIABETES_D,
         PREOP_BMI,
         PREOP_OBESE,
         HTN,
         PREOP_CAD,
         PREOP_CHF,
         PREOP_COPD,
         PREOP_CKD,
         PREOP_DIALYSIS,
         POSTOP_MI_D,
         POSTOP_RESPIR,
         POSTOP_BLEED,
         FLAG_BKA,
         FLAG_AKA,
         LEVEL_AMP,
         DEAD,
         DEAD_30,
         POSTOP_RTOR,
         POSTOP_LOS,
         TOTALPROCTIME,
         PROC_SURVIVALDAYS,
         POSTOP_DYSRHYTHMIA,
         POSTOP_CHF,
         POSTOP_MACE,
         POSTOP_MACE_DEAD) %>% 
  saveRDS(file = "../data/pad_final.rds") 

# Exporting to csv

readRDS("../data/pad_final.rds") %>% 
  write.csv("../data/pad_final.csv", row.names = FALSE)

# Saving complete case dataset

pad_final_comp = 
  pad2 %>% 
  select(PATIENTID,
         PRIMPROCID,
         PROC_SURVIVALDAYS,
         POSTOP_MACE,
         POSTOP_MACE_DEAD,
         DEAD,
         DEAD_30,
         AGE,
         RACE_B,
         PREOP_BMI,
         PREOP_OBESE,
         GROUP, 
         PREOP_DIABETES_D,
         PREOP_DIALYSIS,
         PREOP_SMOKING_D,
         PREOP_CHF,
         PREOP_CAD,
         GENDER,
         PREOP_COPD,
         HTN) %>% 
  filter(complete.cases(.))

saveRDS(pad_final_comp, file = "../data/pad_final_complete.rds")
