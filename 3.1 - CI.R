library(tidyverse)

pad_final =
  readRDS("../data/pad_final.rds") 

pad_comp = 
  readRDS("../data/pad_final_complete.rds") 

pad_comp =
  pad_comp %>% 
  mutate(DEAD_yr = ifelse(DEAD == 1 & PROC_SURVIVALDAYS <= 365, 1, 0)) %>% 
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
                   LEVEL_AMP), by = 'PATIENTID')

# Total
pad_comp %>% 
  group_by(LEVEL_AMP) %>% 
  summarise(n=n()) %>% 
  mutate(p = n/sum(n),
         p_l = p - qnorm(.975)*sqrt(p*(1-p)/n),
         p_u = p + qnorm(.975)*sqrt(p*(1-p)/n))

pad_comp %>% 
  summarise(m = mean(PREOP_BMI),
            sd = sd(PREOP_BMI)) %>% 
  mutate(l = m - qnorm(.975)*sd,
         u = m + qnorm(.975)*sd)


# GROUP
pad_comp %>% 
  group_by(GROUP, POSTOP_MACE_DEAD) %>% 
  summarise(n=n()) %>% 
  group_by(GROUP) %>% 
  mutate(p = n/sum(n),
         p_l = p - qnorm(.975)*sqrt(p*(1-p)/n),
         p_u = p + qnorm(.975)*sqrt(p*(1-p)/n))

pad_comp %>% 
  group_by(GROUP) %>% 
  summarise(m = mean(AGE),
            sd = sd(AGE)) %>% 
  mutate(l = m - qnorm(.975)*sd,
         u = m + qnorm(.975)*sd)

