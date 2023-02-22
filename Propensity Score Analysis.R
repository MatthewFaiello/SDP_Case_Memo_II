#reproduce
set.seed(5)

#packages
library(tidyverse)
library(readxl)
library(WeightIt)

#data
setwd("~/Desktop/SDP 2")

cb =
  read_xlsx("Copy of Data Codebook_updated.xlsx", skip = 2) 
dat = 
  read_csv("Copy of dual_enrl_dataset_updated.csv")
full =
  read_csv("datasets/full.csv")

#---------------------------- descriptive ------------------------------------#
#correlations
corr <-
  full %>% 
  select(lep_ever:read_8_scaled, dual_enrl_hours:remedial_taken,
         s_frpl_ever:hs_diploma, late_grad)
corr2 <-
  cor(corr) %>% 
  as_tibble() %>% 
  mutate(var = rownames(cor(corr)),
         .before = lep_ever) %>% 
  select(var, dual_enrl_hours:dual_enrl_taken) %>% 
  filter(!var %in% c("dual_enrl_hours", "dual_enrl_taken"))

#aggregation of dual enrollment by school year and i
for (i in c("chrt_ninth", "district_name", "hs_area", "hs_size")) {
  
  if (i == "chrt_ninth") {
    assign(paste0("enroll.all.", i),
           full %>% 
             group_by(chrt_ninth) %>% 
             summarise(`Dual Enrollment` = mean(dual_enrl_taken)) %>% 
             ungroup() %>% 
             arrange(desc(`Dual Enrollment`)))
  } else {
    assign(paste0("enroll.all.", i),
           full %>% 
             group_by(chrt_ninth, !!sym(i)) %>% 
             summarise(`Dual Enrollment` = mean(dual_enrl_taken)) %>% 
             ungroup() %>% 
             arrange(desc(`Dual Enrollment`)))
  }
  
}

#aggregation of dual enrollment hours by school year and i
full.de <-
  full %>% 
  filter(dual_enrl_taken == 1)

for (i in c("chrt_ninth", "district_name", "hs_area", "hs_size")) {
  
  if (i == "chrt_ninth") {
    assign(paste0("enroll.de.", i),
           full.de %>% 
             group_by(chrt_ninth) %>% 
             summarise(`Hours in Dual Enrollment Courses.mean` = mean(dual_enrl_hours),
                       `Hours in Dual Enrollment Courses.median` = median(dual_enrl_hours),
                       `Hours in Dual Enrollment Courses.max` = max(dual_enrl_hours),
                       `Hours in Dual Enrollment Courses.sd` = sd(dual_enrl_hours)) %>% 
             ungroup() %>% 
             arrange(desc(`Hours in Dual Enrollment Courses.median`)))
  } else {
    assign(paste0("enroll.de.", i),
           full.de %>% 
             group_by(chrt_ninth, !!sym(i)) %>% 
             summarise(`Hours in Dual Enrollment Courses.mean` = mean(dual_enrl_hours),
                       `Hours in Dual Enrollment Courses.median` = median(dual_enrl_hours),
                       `Hours in Dual Enrollment Courses.max` = max(dual_enrl_hours),
                       `Hours in Dual Enrollment Courses.sd` = sd(dual_enrl_hours)) %>% 
             ungroup() %>% 
             arrange(desc(`Hours in Dual Enrollment Courses.median`)))
  }
  
}

###############################################################################
#------------------------------- pro score -----------------------------------#
#controlling for student level factors
hs <-
  full %>% 
  select(sdpsid:chrt_ninth,
         hs_name, s_male:s_frpl_ever, 
         lep_ever:read_8_scaled, enrl_1oct_grad_yr1_any, 
         dual_enrl_taken)

c <-
  full %>% 
  filter(public_enrl_1oct_grad_yr1 == 1) %>% 
  select(sdpsid:chrt_ninth,
         hs_name, s_male:s_frpl_ever, 
         lep_ever:read_8_scaled, dual_enrl_hours, 
         dual_enrl_taken, remedial_taken)

qrtls <-
  c %>% 
  filter(dual_enrl_taken == 1)
quantile(qrtls$dual_enrl_hours, probs = c(0.25, 0.50, 0.75))

college <-
  c %>% 
  group_by(dual_enrl_taken) %>% 
  mutate(dual_enrl_hours.Q = case_when(dual_enrl_hours == 0 ~ "Q0",
                                       dual_enrl_hours %in% 1:21 ~ "Q1",
                                       dual_enrl_hours %in% 22:36 ~ "Q2",
                                       dual_enrl_hours %in% 37:56 ~ "Q3",
                                       dual_enrl_hours >= 57 ~ "Q4")) %>% 
  ungroup()

#cut points
college %>% 
  group_by(dual_enrl_taken, dual_enrl_hours.Q) %>% 
  reframe(min(dual_enrl_hours),
          max(dual_enrl_hours),
          mean(dual_enrl_hours),
          n())

#ps models
psFormula.hs <-
  formula(paste("dual_enrl_taken ~ ", 
                paste0(hs %>% 
                         select(s_male:read_8_scaled) %>% 
                         names(), 
                       collapse = " + ")))
psFormula.college <-
  formula(paste("dual_enrl_hours.Q ~ ", 
                paste0(hs %>% 
                         select(s_male:read_8_scaled) %>% 
                         names(), 
                       collapse = " + ")))

#Generalized Boosted Models weights
psw.hs <- 
  weightit(psFormula.hs, 
           method = "gbm", 
           stop.method = "es.max",
           estimand = "ATE", 
           data = hs)
summary(psw.hs)
hs.psw <-
  hs %>% 
  mutate(weightATE.gbm = psw.hs$weights)
summary(hs.psw$weightATE.gbm)
write_csv(hs.psw, "datasets/hs.psw.csv")

psw.college <- 
  weightit(psFormula.college, 
           method = "gbm", 
           stop.method = "es.max",
           estimand = "ATE", 
           data = college)
summary(psw.college)
college.psw <-
  college %>% 
  mutate(weightATE.gbm = psw.college$weights)
summary(college.psw$weightATE.gbm)
write_csv(college.psw, "datasets/college.psw.csv")









