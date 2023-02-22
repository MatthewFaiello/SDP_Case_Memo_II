#reproduce
set.seed(5)

#packages
library(tidyverse)
library(readxl)
library(naniar)
library(mice)
library(mitools)
library(fastDummies)

#data
setwd("~/Desktop/SDP 2")

#create folder dir.create("datasets")

cb =
  read_xlsx("Copy of Data Codebook_updated.xlsx", skip = 2) 
dat = 
  read_csv("Copy of dual_enrl_dataset_updated.csv")
data <-
  dat %>% 
  mutate(across(enrl_1oct_grad_yr1_2yr:enrl_1oct_grad_yr1_vtech, ~ if_else(is.na(.), 0, .)),
         status_enrl_1oct_grad_yr1 = case_when(status_enrl_1oct_grad_yr1 == 1 ~ "Full time", 
                                               status_enrl_1oct_grad_yr1 == 2 ~ "Half time", 
                                               status_enrl_1oct_grad_yr1 == 3 ~ "Less than half time", 
                                               status_enrl_1oct_grad_yr1 == 4 ~ "Withdrew", 
                                               status_enrl_1oct_grad_yr1 == 5 ~ "Leave of absence", 
                                               status_enrl_1oct_grad_yr1 == 6 ~ "Deceased",
                                               is.na(status_enrl_1oct_grad_yr1) ~ "No HS diploma"),
         hs_size = sub(".*: ", "", hs_location),
         hs_area = sub(":.*", "", hs_location),
         female = if_else(s_male == "Female", 1, 0),
         chrt_ninth_2008 = if_else(chrt_ninth == 2008, 1, 0),
         district_name = str_to_title(str_remove(district_name, " DISTRICT")),
         .before = hs_location) %>% 
  dummy_cols(select_columns = c("s_raceethnicity",
                                "district_name",
                                "hs_size",
                                "hs_area")) %>% 
  rename_with(~ str_replace_all(., " |,", "_"))

#imputation set
dataImp <-
  data %>% 
  select(-c(chrt_ninth:s_raceethnicity, hs_diploma_date, 
            hs_id:district_name, hs_size:hs_area,
            hs_location, enrl_1oct_grad_yr1_2yr:act_score_composite, 
            remedial_taken))

#missing
miss <-
  data %>% 
  group_by(hs_location) %>% 
  miss_var_summary() %>% 
  filter(variable %in% c("math_8_scaled", "read_8_scaled", "lep_ever"))

#------------------------------- impute --------------------------------------#
missing.indicator <- 
  data.frame(is.na(dataImp))

propMissing <- 
  apply(missing.indicator, 2, mean)

names(missing.indicator)[propMissing > 0] <- 
  paste(names(dataImp)[propMissing > 0], "NA", sep = "_")

for (var in 1:ncol(missing.indicator)) {
  missing.indicator[, var] <- 
    as.numeric(missing.indicator[, var])} 

allDummy <- 
  bind_cols(dataImp, 
            missing.indicator[, propMissing > 0]) %>% 
  rename_with(~ str_replace_all(., " |,", "_"))

#predictors
predictor.selection <- 
  quickpred(allDummy, 
            mincor = 0.1, 
            method = 'pearson',  
            minpuc = 0.5, 
            exclude = "sdpsid")

#multivariate imputation by chained equations
imputation <- 
  mice(allDummy,
       m = 1, 
       method = "pmm", 
       visitSequence = "monotone", 
       predictorMatrix = predictor.selection)

#extract
long.imputation <-
  complete(imputation, action = "long")

full <-
  data %>% 
  select(-c(lep_ever, math_8_scaled:read_8_scaled)) %>% 
  left_join(long.imputation)

write_csv(full, "datasets/full.csv")

#------------------------------ app data prep --------------------------------#
viz <-
  full %>% 
  select(sdpsid, dual_enrl_taken, dual_enrl_hours,
         District = district_name, 
         Size = hs_size, 
         Location = hs_area, 
         School = hs_name,
         Cohort = chrt_ninth, 
         `Math 8th` = math_8_scaled, 
         `ELA 8th` = read_8_scaled,
         `IEP Ever` = sped_ever,
         `LEP Ever` = lep_ever, 
         Graduated = hs_diploma, 
         `On-Time Graduation` = ontime_grad, 
         `College Enrollment` = enrl_1oct_grad_yr1_any,
         Female = female,
         `African American` = s_raceethnicity_African_American,
         `Asian American` = s_raceethnicity_Asian_American,
         `Hispanic` = s_raceethnicity_Hispanic,
         `White, Not Hispanic` = s_raceethnicity_White__Not_Hispanic,
         `Other Race/Ethnicity` = s_raceethnicity_Other,
         `FRPL Ever` = s_frpl_ever) %>% 
  mutate(`Dual Enrollment` = if_else(dual_enrl_taken == 1, "Participants", "Non-Participants"),
         School = str_to_title(School))
write_csv(viz, "SDP_APP/viz.csv")














