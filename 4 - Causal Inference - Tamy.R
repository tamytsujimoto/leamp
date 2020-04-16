library(tidyverse)
require(geepack)
library(CausalGAM)

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

## Propensity Score

covs = "AGE+I(AGE^2)+
         PREOP_BMI+I(PREOP_BMI^2)+
         RACE_B+
         PREOP_DIABETES_D+
         PREOP_DIALYSIS+
         PREOP_SMOKING_D+
         PREOP_CHF+
         PREOP_CAD+
         GENDER+
         PREOP_COPD+
         HTN"

p.score.form = as.formula(paste("GROUP", covs, sep = '~'))
data = pad

fit.denom = glm(as.formula(p.score.form), family = binomial, data = pad)
fit.num = glm(GROUP ~ 1, family = binomial, data = data)

ps.denom <- ifelse(data$GROUP == 2, 
                   predict(fit.denom, type = "response"),
                   1 - predict(fit.denom, type = "response"))

ps.num = ifelse(data$GROUP == 2, 
                predict(fit.num, type = "response"),
                1 - predict(fit.num, type = "response"))

sw = ps.num/ps.denom
mean(sw)

# Computing IPW estimator

n = dim(data)[1]

fit.MACE = geeglm(POSTOP_MACE ~ GROUP, 
                       family = binomial,
                       weights = sw, id = 1:n, data = pad)
summary(fit.MACE)


fit.MACE.dead = geeglm(POSTOP_MACE_DEAD ~ GROUP, 
                       family = binomial,
                       weights = sw, id = 1:n, data = pad)
summary(fit.MACE.dead)




