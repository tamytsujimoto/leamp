require(survival)
require(dplyr)
require(tidyr)
require(survminer)

pad_com = 
  readRDS("../data/pad_final_complete.rds")

com = 
  pad_com %>% 
  select(PROC_SURVIVALDAYS,
         DEAD,
         GROUP, 
         PREOP_DIABETES_D,
         PREOP_DIALYSIS,
         PREOP_SMOKING_D,
         AGE,
         #PREOP_CKD,
         PREOP_CHF,
         PREOP_CAD,
         PREOP_BMI,
         PREOP_OBESE,
         GENDER,
         PREOP_COPD,
         HTN) %>% 
  mutate(PREOP_BMI_GROUP = ifelse(PREOP_BMI < 18.5, "1: Underweight", 
                                  ifelse(PREOP_BMI < 25, "2: Normal weight", 
                                         ifelse(PREOP_BMI < 30, "3: Overweight", "4: Obese"))),
         AGE_GROUP = ifelse(AGE < 65, "1: < 65", 
                            ifelse(AGE < 75, "2: 65 - 74",
                                   ifelse(AGE < 85, "3: 75 - 84", "4: >= 85"))),
         GROUP = car::Recode(GROUP, "1 = 'General'; 2 = 'Regional'"),
         GENDER = car::Recode(GENDER, "1 = 'Male'; 2 = 'Female'")) %>% 
  select(-c(AGE, PREOP_BMI)) %>% 
  mutate_at(vars(PREOP_DIABETES_D:PREOP_OBESE, PREOP_COPD:HTN), funs(car::Recode(., "1 = 'Yes'; 0 = 'No'"))) %>% 
  mutate_at(vars(GROUP:AGE_GROUP), funs(factor)) %>% 
  data.frame()

p <- dim(com)[2]
names <- names(com)

# GGPLOT #
for(i in 3:p){
  var <- com[,i]
  if(class(var) == 'factor'){
    #l <- length(levels(var))
    form <- formula(paste('Surv(PROC_SURVIVALDAYS, DEAD) ~ ', names[i]))
    km <- survfit(form, data=com)
    km$call$formula <- form

    pdf(paste('../Figures/km_', names[i],'.pdf',sep=''))
    #par(mar = c(4, 4, 2, 2) + 0.1)
    
    g <- ggsurvplot(km, 
               risk.table = TRUE,
               surv.scale = 'percent',
               xscale = 365.25,
               break.time.by = 365.25,
               xlab = 'Time (years)',
               xlim = c(0, 2192),
               pval = TRUE,
               pval.coord = c(1900, 1),
               legend = 'top',
               legend.title = '',
               legend.labs = c(levels(var)),
               censor = FALSE,
               data = com)
    
    print(g, newpage = FALSE)
    
    dev.off()
  }
}

# Total 

pdf('../Figures/km_Total.pdf')
g <- ggsurvplot(survfit(Surv(PROC_SURVIVALDAYS, DEAD) ~ 1, data=com), 
                risk.table = TRUE,
                fun = "cumhaz",
                surv.scale = 'percent',
                xscale = 365.25,
                break.time.by = 365.25,
                xlab = 'Time (years)',
                xlim = c(0, 2192),
                legend = 'top',
                legend.title = '',
                legend.labs = c(levels(var)),
                censor = FALSE,
                data = com)
print(g, newpage = FALSE)
dev.off()

# 1yr mortality
round(km$cumhaz[km$time == 365],3)
round(km$cumhaz[km$time == 365] - qnorm(0.975)*km$std.chaz[km$time == 365],3)
round(km$cumhaz[km$time == 365] + qnorm(0.975)*km$std.chaz[km$time == 365],3)


