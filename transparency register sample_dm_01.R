library(tidyverse)
library(translateR)
library(googleLanguageR)
gl_auth("google_api.json")
library(cld2)
library(jsonlite)

raw_sample <- read.csv("raw_sample.csv") 


#cleaning the text a bit 
sample_00 <- read.csv("raw_sample.csv") %>% 
  select(id = Identification.number.,
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
##t1 <- gl_translate(df$info,target = "en")[,1]
load("translation")
t1 <- df$x1
class(t1)
t2 <- as.data.frame(t1)
class(t2)
#t2 <- as.data.frame(flatten(t1))

df2$info.en <- t2[,1]


#join this to the main dataset
sample_01 <- left_join(sample_00,df2) %>% 
mutate(info.en.1 = ifelse(language=="en",info,info.en))

sample_01 <- sample_01 %>% 
  mutate(url = raw_sample$Website.address.,
         person.head.name = raw_sample$Person.with.legal.responsibility,
         person.head.descr = raw_sample$Position.,
         person.eu.name = raw_sample$Person.in.charge.of.EU.relations,
         person.eu.descr = raw_sample$Position..1)


write.cs(sample_01,file="C:/SAAR/UNIVERSITY/R/eufood/sample building/data/transparency register/sample_food_en.csv")
