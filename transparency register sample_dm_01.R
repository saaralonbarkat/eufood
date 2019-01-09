library(tidyverse)
library(translateR)
library(googleLanguageR)
gl_auth("C:/SAAR/UNIVERSITY/R/eufood/sample building/code/google_api.json")
library(cld2)
library(jsonlite)

raw_sample <- read_csv("C:/SAAR/UNIVERSITY/R/eufood/sample building/data/transparency register/raw_sample_xx.csv") 
xx <- read.csv("C:/SAAR/UNIVERSITY/R/eufood/sample building/data/transparency register/raw_sample_xx.csv")

#cleaning the text a bit 
colnames(raw_sample) <- colnames(xx)

sample_00 <- raw_sample



sample_00 <- sample_00 %>% 
  select(id = 1,
         name = X.Organisation..name,
         fields = Fields.of.interest,
         goals = Goals...remit,
         initiatives = EU.initiatives) %>%
  mutate(info = tolower(paste(goals,initiatives)) %>% str_replace_all("\r|\n",""))


#Detect the language of the text: 

sample_00 <- sample_00 %>% 
  mutate(language = as.character(cld2::detect_language(info))) #%>% 

sjmisc::frq(sample_00$language,out="viewer")


#create a subset of only non english
df <- sample_00 %>% 
  filter(language!="en") %>%
  select(id,info) 

#translate this subset
##translation.english <- gl_translate(df$info,target = "en")[,1]
##save(translation.english,file="translation")

load("C:/SAAR/UNIVERSITY/R/eufood/sample building/temp/translation")

t1 <- translation.english[,1] %>% flatten() %>% data.frame()

df$info.en <- t1$translatedText


#join this to the main dataset
sample_01 <- sample_00 %>% 
  left_join(df) %>% 
mutate(info.en.1 = ifelse(language=="en",info,info.en)) %>% 
  mutate(url = raw_sample$Website.address.,
         person.head.name = raw_sample$Person.with.legal.responsibility,
         person.head.descr = raw_sample$Position.,
         person.eu.name = raw_sample$Person.in.charge.of.EU.relations,
         person.eu.descr = raw_sample$Position..1,
         organization.section = raw_sample$Section,
         organization.subsection = raw_sample$Subsection)


write.csv(sample_01,file="C:/SAAR/UNIVERSITY/R/eufood/sample building/data/transparency register/sample_food_en.csv")
