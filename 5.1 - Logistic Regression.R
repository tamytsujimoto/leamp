library(tidyverse)

pad = 
  readRDS("../data/pad_final_complete.rds") %>% 
  select(POSTOP_MACE_DEAD,
         POSTOP_MACE,
         AGE,
         PREOP_BMI,
         RACE_B,
         GROUP, 
         PREOP_DIABETES_D,
         PREOP_DIALYSIS,
         PREOP_SMOKING_D,
         PREOP_CHF,
         PREOP_CAD,
         GENDER,
         PREOP_COPD,
         HTN) %>% 
  mutate_at(vars(RACE_B:HTN), funs(factor(.)))

fit.mace = 
  pad %>% 
  select(-POSTOP_MACE_DEAD) %>% 
  glm(POSTOP_MACE ~ ., data = ., family = binomial)

fit.mace.dead = 
  pad %>% 
  select(-POSTOP_MACE) %>% 
  glm(POSTOP_MACE_DEAD ~ ., data = ., family = binomial)