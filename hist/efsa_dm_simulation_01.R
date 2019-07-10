### EFSA experiment data management ###

# Hi there!

#loading packages

library(tidyverse)
library(car)
library(stringr)
library(lubridate)
library(sjPlot)
library(sjmisc)

#loading raw data CSV file
efsa_raw.s_00 <- read_csv("C:/SAAR/UNIVERSITY/R/EFSA/data/EFSA_survey_simulations.csv")


#removing observations that did not complete any question in the survey. 
  efsa_raw.s_00 %>% 
  filter(Q1.4==1) ->
    efsa_raw.s

#replacing NAs with 0 (practical later)
  efsa_raw.s[is.na(efsa_raw.s)] <- 0

#creating a new dataset
efsa_00.s <-
  efsa_raw.s %>% 
  select(id = V1) %>%  
  mutate(start.date = efsa_raw.s$V8,
         end.date = efsa_raw.s$V9) 


#experiment groups
efsa_00.s <-
  efsa_00.s %>%
  mutate(t1 = efsa_raw.s$Q5.1 %>% Recode("1='approve-pesticide treatment'"),
         t2 = efsa_raw.s$Q6.1 %>% Recode("1='ban-pesticide treatment'"),
         t3 = efsa_raw.s$Q7.1 %>% Recode("1='approve-pesticide control'"),
         t4 = efsa_raw.s$Q8.1 %>% Recode("1='ban-pesticide control'")) %>% 
  mutate(condition = paste0(t1,t2,t3,t4) %>% str_replace_all("NA|0","") %>% Recode("''=NA")) %>% 
  select(-t1,-t2,-t3,-t4) %>% 
  
  
  mutate(treatment = ifelse(str_detect(condition,"treatment")==T,1,
                            ifelse(str_detect(condition,"control")==T,0,NA)),
         ban.pesticide = ifelse(str_detect(condition,"ban-")==T,1,
                            ifelse(str_detect(condition,"approve-")==T,0,NA)))

efsa_00 <- efsa_00 %>% 
  mutate(treatment.start = ifelse(efsa_raw$Q2.1==1,"treatment",
                                  ifelse(efsa_raw$Q3.1==1,"control",NA)))


