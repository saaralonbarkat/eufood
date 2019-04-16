### EFSA experiment data management ###

# Hi there!

#loading packages

library(tidyverse)
library(car)
library(stringr)
library(lubridate)




#files <- list.files(path = "C:/Users/OWNER/surfdrive/research/My studies/Study 2 - reputation and standpoint on issues/data/raw_data/new/")
#f <- list()
#for (i in 1:length(files)) {
#  f[[i]] <- read_csv(str_c("C:/Users/OWNER/surfdrive/research/My studies/Study 2 - reputation and standpoint on issues/data/raw_data/new/",files[i]))%>%.[-1,] %>%  
#    mutate(survey.name=files[i])}




#t1 <- bind_rows(f)%>%
#  data.frame() %>%
#  select(-(V11:V222)) %>%
#  select(-LocationLatitude,
#         -LocationLongitude,
#         -LocationAccuracy,
#         -V6) %>% 
#  write.csv(file = "C:/SAAR/UNIVERSITY/R/EFSA/data/efsa_data.csv")



#loading raw data CSV file
efsa_raw_old <- read_csv("C:/SAAR/UNIVERSITY/R/EFSA/data/efsa_data_old.csv") %>% 
  mutate(Q105=NA)
efsa_raw_new <- read_csv("C:/SAAR/UNIVERSITY/R/EFSA/data/efsa_data_new.csv") %>% 
  mutate(survey.name = NA)

efsa_raw_00 <- rbind(efsa_raw_old,
                     efsa_raw_new)

#removing observations that did not complete any question in the survey. 
  efsa_raw_00 %>% 
  filter(Q1.4==1) ->
    efsa_raw

#replacing NAs with 0 (practical later)
  efsa_raw[is.na(efsa_raw)] <- 0

#creating a new dataset
efsa_00 <-
  efsa_raw %>% 
  select(id = V1) %>%  
  mutate(start.date = dmy_hm(efsa_raw$V8),
         end.date = dmy_hm(efsa_raw$V9)) 


#Demographics
efsa_00 <- efsa_00 %>% 
  mutate(age = efsa_raw$Q13.2 %>% Recode("1=20;
                                       2=30;
                                       3=40;
                                       4=50;
                                       5=60;
                                       6=70;
                                       0=NA"),
         female = efsa_raw$Q13.3 %>% Recode("1=0;
                                          2=1;
                                          3=NA"),
         education = efsa_raw$Q13.4 %>% Recode("1='1 High-school';
                                              2='2 Bachlor';
                                              3='3 Master';
                                              4='4 Doctorat';
                                              0=NA"))

  
#experiment groups
efsa_00 <-
  efsa_00 %>%
  mutate(t1 = efsa_raw$Q5.1 %>% Recode("1='low risk treatment'"),
         t2 = efsa_raw$Q6.1 %>% Recode("1='high risk treatment'"),
         t3 = efsa_raw$Q7.1 %>% Recode("1='low risk control'"),
         t4 = efsa_raw$Q8.1 %>% Recode("1='high risk control'")) %>% 
  mutate(condition = paste0(t1,t2,t3,t4) %>% str_replace_all("NA|0","") %>% Recode("''=NA")) %>% 
  select(-t1,-t2,-t3,-t4) %>% 
 
  
  mutate(treatment = ifelse(str_detect(condition,"treatment")==T,1,
                            ifelse(str_detect(condition,"control")==T,0,NA)),
         high.risk = ifelse(str_detect(condition,"high")==T,1,
                            ifelse(str_detect(condition,"low")==T,0,NA)))

efsa_00 <- efsa_00 %>% 
  mutate(treatment.start = ifelse(efsa_raw$Q2.1==1,"treatment",
                                  ifelse(efsa_raw$Q3.1==1,"control",NA)))


#Native language

  select(efsa_00,1) %>%
  mutate(native.language.english = ifelse(efsa_raw$Q13.7==1,"English",NA),
         native.language.French = ifelse(efsa_raw$Q13.8_1==1,"French",NA),
         native.language.German = ifelse(efsa_raw$Q13.8_2==1,"German",NA),
         native.language.Spanish = ifelse(efsa_raw$Q13.8_4==1,"Spanish",NA),
         native.language.Italian = ifelse(efsa_raw$Q13.8_5==1,"Italian",NA),
         native.language.Dutch = ifelse(efsa_raw$Q13.8_6==1,'Dutch',NA),
         native.language.Portuguese = ifelse(efsa_raw$Q13.8_7==1,"Portuguese",NA),
         native.language.other = efsa_raw$Q13.8_8_TEXT) %>%
  mutate(native.language = paste(native.language.english,
                                   native.language.French,
                                   native.language.German,
                                   native.language.Spanish,
                                   native.language.Italian,
                                   native.language.Dutch,
                                   native.language.Portuguese,
                                   native.language.other,
                                   sep = ", ")) %>%
  mutate(native.language = native.language %>% str_replace_all(", NA|NA, |0","")) ->
    t1
  
efsa_00 <- efsa_00 %>% 
  mutate(native.language = t1$native.language)
  
rm(t1)

#Job task

select(efsa_00,1) %>%
  mutate(job.task.1 = ifelse(efsa_raw$Q13.6_1==1,"Research",NA),
         job.task.2 = ifelse(efsa_raw$Q13.6_2==1,"Human resource management",NA),
         job.task.3 = ifelse(efsa_raw$Q13.6_3==1,"Public communication or public relations",NA),
         job.task.4 = ifelse(efsa_raw$Q13.6_4==1,"Government relations, lobbying or advocacy",NA),
         job.task.5 = ifelse(efsa_raw$Q13.6_5==1,"Financing, marketing and sales",NA),
         job.task.6 = ifelse(efsa_raw$Q13.6_6==1,"Project management",NA),
         job.task.7 = ifelse(efsa_raw$Q13.6_7==1,"Administration",NA),
         job.task.other = efsa_raw$Q13.6_8_TEXT) %>%
  mutate(job.task = paste(job.task.1,
                                 job.task.2,
                                 job.task.3,
                                 job.task.4,
                                 job.task.5,
                                 job.task.6,
                                 job.task.7,
                                 job.task.other,
                                 sep = "; ")) %>%
  mutate(job.task = job.task %>% str_replace_all("; NA|NA; |0","")) ->
  t1

efsa_00 <- efsa_00 %>% 
  mutate(job.task = t1$job.task)

rm(t1)


#Education discipline

select(efsa_00,1) %>%
  mutate(education.discipline.1 = ifelse(efsa_raw$Q13.5_1==1,"Economics or Business Management",NA),
         education.discipline.2 = ifelse(efsa_raw$Q13.5_2==1,"Law",NA),
         education.discipline.3 = ifelse(efsa_raw$Q13.5_3==1,"Biology",NA),
         education.discipline.4 = ifelse(efsa_raw$Q13.5_4==1,"Chemistry",NA),
         education.discipline.5 = ifelse(efsa_raw$Q13.5_5==1,"Physics",NA),
         education.discipline.6 = ifelse(efsa_raw$Q13.5_6==1,"Engineering or Computer Science",NA),
         education.discipline.7 = ifelse(efsa_raw$Q13.5_7==1,"Political Science, Governance, Public Policy or Public Administration",NA),
         education.discipline.8 = ifelse(efsa_raw$Q13.5_8==1,"Communication",NA),
         education.discipline.9 = ifelse(efsa_raw$Q13.5_9==1,"Sociology",NA),
         education.discipline.10 = ifelse(efsa_raw$Q13.5_10==1,"Psychology",NA),
         education.discipline.other= efsa_raw$Q13.5_11_TEXT) %>% 
   mutate(education.discipline = paste(education.discipline.1,
                                       education.discipline.2,
                                       education.discipline.3,
                                       education.discipline.4,
                                       education.discipline.5,
                                       education.discipline.6,
                                       education.discipline.7,
                                       education.discipline.8,
                                       education.discipline.9,
                                       education.discipline.10,
                                       education.discipline.other,
                          sep = "; ")) %>%
  mutate(education.discipline = education.discipline %>% str_replace_all("; NA|NA; |0","")) ->
  t1

efsa_00 <- efsa_00 %>% 
  mutate(education.discipline = t1$education.discipline)

rm(t1)


#Organization areas

select(efsa_00,1) %>%
  mutate(organization.area.1 = ifelse(efsa_raw$Q1.5_1==1,"Environment",NA),
         organization.area.2 = ifelse(efsa_raw$Q1.5_2==1,"Telecommunications",NA),
         organization.area.3 = ifelse(efsa_raw$Q1.5_3==1,"Food and Agriculture",NA),
         organization.area.4 = ifelse(efsa_raw$Q1.5_4==1,"Health",NA),
         organization.area.5 = ifelse(efsa_raw$Q1.5_5==1,"Financial services",NA),
         organization.area.6 = ifelse(efsa_raw$Q1.5_6==1,"Energy",NA),
         organization.area.other = efsa_raw$Q1.5_7_TEXT) %>% 
  mutate(organization.area = paste(organization.area.1,
                                   organization.area.2,
                                   organization.area.3,
                                   organization.area.4,
                                   organization.area.5,
                                   organization.area.6,
                                   organization.area.other,
                                      sep = "; ")) %>%
  mutate(organization.area = organization.area %>% str_replace_all("; NA|NA; |0","")) ->
  t1

efsa_00 <- efsa_00 %>% 
  mutate(organization.area = t1$organization.area,
         organization.area.food = ifelse(efsa_raw$Q1.5_3==1,1,0))

rm(t1)

#Organization type

efsa_00 <- efsa_00 %>% 
  mutate(organization.type.raw = efsa_raw$Q1.6 %>% Recode("1='consultancy firm';
                                                          2='company';
                                                          3='trade association';
                                                          4='professional association';
                                                          5='non-governmental nonprofit organization';
                                                          6='research institute';
                                                          7='other';
                                                          8='Law firm'"))

#Attitudes about EU agencies
efsa_00 <- efsa_00 %>% 
  mutate(credibility.eea.pre = efsa_raw$Q2.3_1 + efsa_raw$Q3.3_1 -6,
         credibility.efsa.pre = efsa_raw$Q2.3_2 + efsa_raw$Q3.3_2 -6,
         credibility.ema.pre = efsa_raw$Q2.3_3 + efsa_raw$Q3.3_3 -6,
         credibility.echa.pre = efsa_raw$Q2.3_4 + efsa_raw$Q3.3_4 - 6) %>% 
  mutate_at(vars(starts_with("credibility.e")),list(~Recode(.,"-6=NA;6=0")))


#Attitudes about EU institutions
efsa_00 <- efsa_00 %>% 
  mutate(trust.eu.commission = efsa_raw$Q2.2_1+efsa_raw$Q3.2_1-6,
         trust.eu.parliament = efsa_raw$Q2.2_2+efsa_raw$Q3.2_2-6,
         trust.eu.council = efsa_raw$Q2.2_3+efsa_raw$Q3.2_3-6) %>% 
  mutate_at(vars(starts_with("trust.eu.")),list(~Recode(.,"-6=NA")))
 


#Moderators

##Perceived greediness of companies
efsa_00 <- efsa_00 %>% 
  mutate(greediness.industry = efsa_raw$Q4.2-6,
         greediness.food.industry = efsa_raw$Q4.5-6) %>% 
  mutate_at(vars(starts_with("greediness.")),list(~Recode(.,"-6=NA")))


##Attitudes about innovations in food industry
efsa_00 <- efsa_00 %>% 
  mutate(supports.gmo = efsa_raw$Q4.7_3-6,
         supports.pesticides = efsa_raw$Q4.7_5-6,
         supports.additives = efsa_raw$Q4.7_7-6) %>% 
  mutate_at(vars(starts_with("supports.")),list(~Recode(.,"-6=NA")))




##Relations with agencies

efsa_00 <- efsa_00 %>% 
  mutate(informed.eea = efsa_raw$Q12.2_1-6,
         informed.ema = efsa_raw$Q12.2_3-6,
         informed.echa = efsa_raw$Q12.2_4-6,
         informed.efsa = efsa_raw$Q12.2_5-6) %>% 

  mutate(affect.decisions.eea = efsa_raw$Q12.3_1-6,
         affect.decisions.ema = efsa_raw$Q12.3_3-6,
         affect.decisions.echa = efsa_raw$Q12.3_4-6,
         affect.decisions.efsa = efsa_raw$Q12.3_5-6) %>% 
  mutate_at(vars(starts_with("affect.decisions.")),list(~Recode(.,"-6=NA;6=0"))) %>% 
  mutate_at(vars(starts_with("informed.")),list(~Recode(.,"-6=NA;6=0")))


efsa_00 <- efsa_00 %>%   
  mutate(interact.eea = efsa_raw$Q12.4_1,
         interact.ema = efsa_raw$Q12.4_2,
         interact.echa = efsa_raw$Q12.4_3,
         interact.efsa = efsa_raw$Q12.4_4) %>% 
  mutate_at(c("interact.eea",
              "interact.ema",
              "interact.echa",
              "interact.efsa"),list(~Recode(.,"0=NA;
                                              1='1. not at all';
                                              2='2. About once a year';
                                              3='3. About twice a year';
                                              4='4. About once a month';
                                              5='5. A few times a month';
                                              6='6. I dont know';
                                              7='7. I prefer not to answer'")))
  
  
  
#Manipulation checks


##compregension check
efsa_00 %>% select(high.risk) %>% 
  mutate(t1 = efsa_raw$Q13.9,
         t2 = efsa_raw$Q13.10) %>% 
  mutate(comprehension.check = ifelse(high.risk==1&t2==1|high.risk==0&t1==2,1,
                                      ifelse(t1==0&t2==0,NA,0))) -> 
  t1

efsa_00 <- efsa_00 %>% 
  mutate(comprehension.check = t1$comprehension.check %>% as.numeric())

rm(t1)

efsa_00 <- efsa_00 %>% 
  mutate(familiarity.pesticide = ifelse(efsa_raw$Q13.11+efsa_raw$Q13.12==1,1,
                                        ifelse(efsa_raw$Q13.11+efsa_raw$Q13.12==2,0,NA)))

##viewing time
efsa_00 <- efsa_00 %>% 
  mutate(viewtime.intro = efsa_raw$Q5.3_3+
                          efsa_raw$Q6.3_3+
                          efsa_raw$Q7.3_3+
                          efsa_raw$Q8.3_3,
         viewtime.manipulation = efsa_raw$Q5.5_3+
                                  efsa_raw$Q6.5_3+
                                  efsa_raw$Q7.5_3+
                                  efsa_raw$Q8.5_3,
         viewtime.pesticide = efsa_raw$Q5.7_3+
                                  efsa_raw$Q6.7_3+
                                  efsa_raw$Q7.7_3+
                                  efsa_raw$Q8.7_3) %>% 
  mutate(viewtime.case.total = viewtime.intro+
                                viewtime.manipulation+
                                viewtime.pesticide)

##perceived independence
efsa_00 <- efsa_00 %>% 
  mutate(idependence.polit.eea = (efsa_raw$Q12.5_1-6)*-1,
         idependence.polit.ema = (efsa_raw$Q12.5_3-6)*-1,
         idependence.polit.echa = (efsa_raw$Q12.5_4-6)*-1,
         
         idependence.industry.eea = (efsa_raw$Q12.6_1-6)*-1,
         idependence.industry.ema = (efsa_raw$Q12.6_3-6)*-1,
         idependence.industry.echa = (efsa_raw$Q12.6_4-6)*-1) %>%
  
  mutate_at(vars(starts_with("idependence.")),list(~Recode(.,"6=NA; -6=0"))) %>% 
  
mutate(idependence.polit.efsa = (efsa_raw$Q10.2-6)*-1,
       idependence.industry.efsa = (efsa_raw$Q10.3-6)*-1) %>% 
  mutate(idependence.polit.efsa = idependence.polit.efsa %>% na_if(6),
         idependence.industry.efsa = idependence.industry.efsa %>% na_if(6))


#Outcome variable
efsa_00 <- efsa_00 %>% 
  mutate(credibility.pest.q1 = efsa_raw$Q9.2 - 6,
         credibility.pest.q2 = efsa_raw$Q9.3 - 6,
         credibility.pest.q3 = efsa_raw$Q9.4 - 6,
         credibility.pest.q4 = efsa_raw$Q9.5 - 6,
         credibility.pest.q5 = efsa_raw$Q9.6 - 6,
         credibility.pest.q6 = efsa_raw$Q9.7 - 6) %>%
  mutate_at(vars(starts_with("credibility.pest.")),list(~Recode(.,"-6=NA"))) %>% 

  mutate(credibility.efsa.post = efsa_raw$Q10.4 - 6,
         credibility.efsa.post.commitment = efsa_raw$Q105 - 6) %>%
  mutate_at(vars(starts_with("credibility.efsa.post")),list(~Recode(.,"-6=NA")))


efsa_00 <- efsa_00 %>% 
  mutate(reputation.efsa.q1 = efsa_raw$Q10.5 -6,
         reputation.efsa.q2 = efsa_raw$Q10.6 -6,
         reputation.efsa.q3 = efsa_raw$Q10.7 -6,
         reputation.efsa.q4 = efsa_raw$Q10.8 -6,
         reputation.efsa.q5 = efsa_raw$Q10.9 -6,
         reputation.efsa.q6 = efsa_raw$Q10.10 -6,
         reputation.efsa.q7 = efsa_raw$Q10.11 -6,
         reputation.efsa.q8 = efsa_raw$Q10.12 -6,
         reputation.efsa.q9 = efsa_raw$Q10.13 -6,
         reputation.efsa.q10 = efsa_raw$Q10.14 -6,
         reputation.efsa.q11 = efsa_raw$Q10.15 -6,
         reputation.efsa.open = efsa_raw$Q11.1) %>% 
mutate_at(vars(starts_with("reputation.efsa.")),list(~Recode(.,"-6=NA")))

efsa_00 <- efsa_00 %>%
  mutate(open.text = efsa_raw$Q13.15)


  



