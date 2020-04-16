require(dplyr)
require(tidyr)
require(data.table)
require(summarytools)

pad_final = 
  readRDS("../data/pad_final.rds")

pad_comp = 
  readRDS("../data/pad_final_complete.rds") 
  
pad_comp2 =
  pad_comp %>% 
  select(-c(PATIENTID, PRIMPROCID)) %>% 
  mutate_at(vars(-c(PROC_SURVIVALDAYS, AGE, PREOP_BMI)), ~factor(.)) %>% 
  as.data.frame()

# PREOP X GROUP

pad_comp2 %>% with(ctable(GENDER, GROUP, chisq = TRUE, prop = 'c', headings = FALSE))
pad_comp2 %>% with(ctable(RACE_B, GROUP, chisq = TRUE, prop = 'c', headings = FALSE))
pad_comp2 %>% with(ctable(PREOP_OBESE, GROUP, chisq = TRUE, prop = 'c', headings = FALSE))
pad_comp2 %>% with(ctable(PREOP_DIABETES_D, GROUP, chisq = TRUE, prop = 'c', headings = FALSE))
pad_comp2 %>% with(ctable(PREOP_DIALYSIS, GROUP, chisq = TRUE, prop = 'c', headings = FALSE))
pad_comp2 %>% with(ctable(PREOP_SMOKING_D, GROUP, chisq = TRUE, prop = 'c', headings = FALSE))
pad_comp2 %>% with(ctable(PREOP_CHF, GROUP, chisq = TRUE, prop = 'c', headings = FALSE))
pad_comp2 %>% with(ctable(PREOP_CAD, GROUP, chisq = TRUE, prop = 'c', headings = FALSE))
pad_comp2 %>% with(ctable(PREOP_COPD, GROUP, chisq = TRUE, prop = 'c', headings = FALSE))
pad_comp2 %>% with(ctable(HTN, GROUP, chisq = TRUE, prop = 'c', headings = FALSE))


pad_comp2 %>% with(descr(AGE, stats = c('mean', 'sd', 'q1','med', 'q3')))
pad_comp2 %>% with(stby(AGE, GROUP, descr, stats = c('mean', 'sd', 'q1','med', 'q3')))
t.test(pad_comp2$AGE ~ pad_comp2$GROUP)

pad_comp2 %>% with(descr(PREOP_BMI, stats = c('mean', 'sd', 'q1','med', 'q3')))
pad_comp2 %>% with(stby(PREOP_BMI, GROUP, descr, stats = c('mean', 'sd', 'q1','med', 'q3')))
t.test(pad_comp2$AGE ~ pad_comp2$GROUP)


# POSTOP X GROUP

pad_comp3 =
  pad_comp %>% 
  left_join(select(pad_final, 
                   PATIENTID,
                   TISSUE_LOSS,
                   POSTOP_RESPIR,
                   POSTOP_BLEED,
                   POSTOP_RTOR,
                   POSTOP_LOS,
                   POSTOP_MI_D,
                   POSTOP_DYSRHYTHMIA,
                   POSTOP_CHF,
                   FLAG_BKA,
                   FLAG_AKA,
                   LEVEL_AMP), by = 'PATIENTID')

pad_comp3 %>% with(stby(POSTOP_LOS, GROUP, descr, stats = c('mean', 'sd', 'q1','med', 'q3')))
t.test(pad_comp3$POSTOP_LOS ~ pad_comp3$GROUP)

pad_comp3 %>% with(ctable(LEVEL_AMP, GROUP, chisq = TRUE, headings = FALSE, prop = 'c'))
pad_comp3 %>% with(ctable(TISSUE_LOSS, GROUP, chisq = TRUE, headings = FALSE, prop = 'c'))
pad_comp3 %>% with(ctable(POSTOP_RESPIR, GROUP, chisq = TRUE, headings = FALSE, prop = 'c'))
pad_comp3 %>% with(ctable(POSTOP_BLEED, GROUP, chisq = TRUE, headings = FALSE, prop = 'c'))
pad_comp3 %>% with(ctable(POSTOP_RTOR, GROUP, chisq = TRUE, headings = FALSE, prop = 'c'))
pad_comp3 %>% with(ctable(POSTOP_MI_D, GROUP, chisq = TRUE, headings = FALSE, prop = 'c'))
pad_comp3 %>% with(ctable(POSTOP_DYSRHYTHMIA, GROUP, chisq = TRUE, headings = FALSE, prop = 'c'))
pad_comp3 %>% with(ctable(POSTOP_CHF, GROUP, chisq = TRUE, headings = FALSE, prop = 'c')) 
pad_comp3 %>% with(ctable(DEAD_30, GROUP, chisq = TRUE, headings = FALSE, prop = 'c'))
pad_comp3 %>% with(ctable(DEAD, GROUP, chisq = TRUE, headings = FALSE, prop = 'c'))
pad_comp3 %>% with(ctable(POSTOP_MACE, GROUP, chisq = TRUE, headings = FALSE, prop = 'c'))
pad_comp3 %>% with(ctable(POSTOP_MACE_DEAD, GROUP, chisq = TRUE, headings = FALSE, prop = 'c'))

# Saving dataset for Corey
pad_comp3 %>% 
  write.csv(file = '../data/pad_complete_04152020.csv')

