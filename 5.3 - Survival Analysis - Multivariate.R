require(survival)
require(dplyr)
require(tidyr)
require(survminer)

pad = 
  readRDS("pad_final_complete.rds") %>% 
  select(PROC_SURVIVALDAYS,
         DEAD,
         AGE,
         PREOP_BMI,
         GROUP, 
         PREOP_DIABETES_D,
         PREOP_DIALYSIS,
         PREOP_SMOKING_D,
         #PREOP_CKD,
         PREOP_CHF,
         PREOP_CAD,
         #PREOP_OBESE,
         GENDER,
         PREOP_COPD,
         HTN) %>% 
  mutate_at(vars(GROUP:HTN), list(~factor(.)))

###############
## LONG TERM ##
###############

## Cox Proportional hazards model

f0 = as.formula(paste("Surv(PROC_SURVIVALDAYS, DEAD) ~ ",
                      paste0(names(pad)[-c(1:2, 15, 16)], collapse = '+'))) 

f1 = as.formula(paste("Surv(PROC_SURVIVALDAYS, DEAD) ~ ",
                      paste0(names(pad)[-c(1:2, 15, 16)], collapse = '+'), "+",
                      paste0("GROUP:",names(pad)[-c(1,2,5,15,16)], collapse = ' + '))) 

cox0 = coxph(f0, data = pad); summary(cox0)
cox1 = coxph(f1, data = pad); summary(cox1)
anova(cox1, cox0) # LRT: do not reject main effects model

## Diagnostic proportionality 
# SHOENFELD RESIDUAL

cox.zph(cox0)
plot(cox.zph(cox0), col.lab = "transparent")
title(main="Time (days)", ylab="Schoenfeld Residuals")
#legend('topleft', legend = paste("p-value", ifelse(round(cox.zph(cox0)$table[1,3],3)>=0.001, format(round(cox.zph(cox1)$table[1,3], 3), nsmall = 3), '< 0.001'), sep = ": "), box.lty = 0)
#abline(0, 0, col = 'red', lwd = 2)

fit <- survfit(Surv(PROC_SURVIVALDAYS, DEAD) ~ GROUP + strata(PREOP_DIABETES_D), data = pad)

ggsurvplot(
  fit, 
  palette = c(rep('gray50', 2),rep('black', 2)),
  ggtheme = theme_bw()) 

## Cox Proportional hazards model - with stratification

cox2 = update(cox0, ~.-PREOP_DIABETES_D + strata(PREOP_DIABETES_D)); summary(cox2)
cox.zph(cox2)

## KM plot with adjusted and unadjusted p-values

km <- survfit(Surv(PROC_SURVIVALDAYS, DEAD) ~ GROUP, data=pad)
g <- ggsurvplot(km, 
                risk.table = TRUE,
                surv.scale = 'percent',
                xscale = 365.25,
                break.time.by = 365.25,
                xlab = 'Time (years)',
                xlim = c(0, 2192),
                legend = 'top',
                legend.title = '',
                legend.labs = c('General', 'Regional'),
                censor = FALSE,
                data = com)

logrank <- survdiff(form, rho=0, data=com)
p.log <- 1 - pchisq(logrank$chisq, 1)
p.cox <- summary(cox2)$coeff[3,5]

g$plot <- g$plot + 
  ggplot2::annotate("text", 
                    x = 450, y = .1, # x and y coordinates of the text
                    label = paste("Unadjusted p-value =", round(p.log, 3), "\n Adjusted p-value = ", round(p.cox, 3)), 
                    size = 5)

pdf('Figures/km_GROUP_adj.pdf')
print(g, newpage = FALSE)
dev.off()

#############
## 30 DAYS ##
#############

## Cox Proportional hazards model

# f0 = as.formula(paste("Surv(PROC_SURVIVALDAYS_30, DEAD_30) ~ ",
#                       paste0(names(pad)[-c(1:2, 15, 16)], collapse = '+'))) 
# 
# f1 = as.formula(paste("Surv(PROC_SURVIVALDAYS_30, DEAD_30) ~ ",
#                       paste0(names(pad)[-c(1:2, 15, 16)], collapse = '+'), "+",
#                       paste0("GROUP:",names(pad)[-c(1,2,5,15, 16)], collapse = ' + '))) 
# 
# cox0 = coxph(f0, data = pad); summary(cox0)
# cox1 = coxph(f1, data = pad); summary(cox1)
# anova(cox1, cox0) # LRT: do not reject main effects model
# 
# pad2 = 
#   pad %>% 
#   mutate(PREOP_BMI_GROUP = ifelse(PREOP_BMI < 18.5, "1: Underweight", 
#                                   ifelse(PREOP_BMI < 25, "2: Normal weight", 
#                                          ifelse(PREOP_BMI < 30, "3: Overweight", "4: Obese"))))
#          
# fit <- survfit(Surv(PROC_SURVIVALDAYS_30, DEAD_30) ~ GROUP + strata(PREOP_BMI_GROUP), data = pad2)
# fit <- survfit(Surv(PROC_SURVIVALDAYS_30, DEAD_30) ~ PREOP_SMOKING_D, data = pad)
# 
# ggsurvplot(fit, ggtheme = theme_bw()) 
# 
# ## Diagnostic proportionality 
# # SHOENFELD RESIDUAL
# 
# cox.zph(cox0)
# plot(cox.zph(cox0), col.lab = "transparent")
# title(main="Time (days)", ylab="Schoenfeld Residuals")
