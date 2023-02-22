#reproduce
set.seed(5)

#packages
library(tidyverse)
library(readxl)
library(sandwich)
library(lmtest)
library(texreg)

#data
setwd("~/Desktop/SDP 2")

cb =
  read_xlsx("Copy of Data Codebook_updated.xlsx", skip = 2) 
dat = 
  read_csv("Copy of dual_enrl_dataset_updated.csv")
full =
  read_csv("datasets/full.csv")
hs = 
  read_csv("datasets/hs.psw.csv") 
college = 
  read_csv("datasets/college.psw.csv")

#----------------------------- logit model -----------------------------------#
cu <-
  dat %>% 
  select(sdpsid, collegename_enrl_1oct_grad_1)
hs.psw <-
  hs %>% 
  mutate(chrt_ninth = as.factor(chrt_ninth))
college.psw <-
  college %>% 
  left_join(cu) %>% 
  mutate(chrt_ninth = as.factor(chrt_ninth))

#controlling for school-level factors
glmFormula.hs <-
  paste("enrl_1oct_grad_yr1_any ~ ", 
        paste0(hs.psw %>% 
                 select(dual_enrl_taken, chrt_ninth:hs_name) %>% 
                 names(), 
               collapse = " + "))

glmFormula.college <-
  paste("remedial_taken ~ ", 
        paste0(college.psw %>% 
                 select(dual_enrl_hours.Q, chrt_ninth, collegename_enrl_1oct_grad_1) %>% 
                 names(), 
               collapse = " + "))

#treatment effect
glm.hs <-
  glm(glmFormula.hs, family = quasibinomial, 
      weights = weightATE.gbm, data = hs.psw)

efx_clustered.hs <-
  coeftest(glm.hs,
           vcov = vcovCL,
           type = "HC1",
           cluster = ~ hs_name,
           save = T)
pars.hs <-
  efx_clustered.hs[,] %>% 
  as_tibble() %>% 
  mutate(Variable = rownames(efx_clustered.hs),
         .before = Estimate) %>% 
  mutate(Estimate = exp(Estimate))


college.psw$dual_enrl_hours.Q <-
  as.factor(college.psw$dual_enrl_hours.Q)
glm.college <-
  glm(glmFormula.college, family = quasibinomial, 
      weights = weightATE.gbm, data = college.psw)

efx_clustered.college <-
  coeftest(glm.college,
           vcov = vcovCL,
           type = "HC1",
           cluster = ~ collegename_enrl_1oct_grad_1,
           save = T)
pars.college <-
  efx_clustered.college[,] %>% 
  as_tibble() %>% 
  mutate(Variable = rownames(efx_clustered.college),
         .before = Estimate) %>% 
  mutate(Estimate = exp(Estimate))


#progressive levels
qs <-
  list()
ps <-
  list()
for (i in c("Q1", "Q2", "Q3")) {
  
  #vs qi
  college.psw$dual_enrl_hours.Q <-
    relevel(college.psw$dual_enrl_hours.Q, ref = i)
  tmp <-
    glm(glmFormula.college, family = quasibinomial, 
        weights = weightATE.gbm, data = college.psw)
  
  qs[[paste0("glm.college_", i)]] <-
    tmp
  
  tmp1 <-
    coeftest(tmp,
             vcov = vcovCL,
             type = "HC1",
             cluster = ~ collegename_enrl_1oct_grad_1,
             save = T)
  tmp2 <-
    tmp1[,] %>% 
    as_tibble() %>% 
    mutate(Variable = rownames(tmp1),
           .before = Estimate) %>% 
    mutate(Estimate = exp(Estimate))
  
  ps[[paste0("pars.college_", i)]] <-
    tmp2
  
}

#summary
screenreg(list(glm.hs, glm.college, qs$glm.college_Q1, qs$glm.college_Q2, qs$glm.college_Q3),
          single.row = T, 
          digits = 3,
          stars = c(0.001, 0.01, 0.05, 0.1),
          custom.header = list("Dual Enrollment Courses | College Enrollemnt (CE) & Remedial Work In College (RWIC)" = 1:5),
          custom.model.names = c("CE", "RWIC (vs no DE)", "RWIC (vs Q1)",
                                 "RWIC (vs Q2)", "RWIC (vs Q3)"), 
          groups = list("Treatment Effect" = 1:6),
          override.coef = list(pars.hs$Estimate, pars.college$Estimate, ps$pars.college_Q1$Estimate,
                               ps$pars.college_Q2$Estimate, ps$pars.college_Q3$Estimate),
          override.se = list(pars.hs$`Std. Error`, pars.college$`Std. Error`, ps$pars.college_Q1$`Std. Error`,
                             ps$pars.college_Q2$`Std. Error`, ps$pars.college_Q3$`Std. Error`),
          override.pvalues = list(pars.hs$`Pr(>|z|)`, pars.college$`Pr(>|z|)`, ps$pars.college_Q1$`Pr(>|z|)`,
                                  ps$pars.college_Q2$`Pr(>|z|)`, ps$pars.college_Q3$`Pr(>|z|)`),
          reorder.coef = c(1, 6, 2:5), 
          omit.coef = "(Intercept)|(hs_name)|(collegename)|(chrt)",
          custom.note = 'MLE Odds Ratios | (Clustered HC1 SE) | *** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1')

mean(hs.psw$dual_enrl_taken) * 100
full %>% 
  group_by(dual_enrl_taken) %>% 
  reframe(min(dual_enrl_hours),
          max(dual_enrl_hours),
          median(dual_enrl_hours),
          n(),
          mean(enrl_1oct_grad_yr1_any) * 100)

#cut points
college.psw %>% 
  mutate(dual_enrl_hours.Q = factor(dual_enrl_hours.Q, levels = c("Q0", "Q1", 
                                                                  "Q2", "Q3", 
                                                                  "Q4"))) %>% 
  group_by(dual_enrl_taken, dual_enrl_hours.Q) %>% 
  reframe(min(dual_enrl_hours),
          max(dual_enrl_hours),
          mean(dual_enrl_hours),
          n())











