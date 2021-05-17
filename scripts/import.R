library(tidyverse)
library(psych)

data <- read.csv('./data/cleaned.csv')

quantile(data$time_sum)

data <- data %>% filter(time_sum > quantile(time_sum, 0.25))

data$treatment_group <-  factor(data$treatment_group,
                                levels = c(1, 2, 3, 4),
                                labels = c("Inevitable Doom", "Evitable Doom",
                                           "Inevitable Gloom", "Evitable Gloom") )
data$gender <-  factor(data$gender, 
                       levels=c(1, 2, 3), 
                       labels=c("female","male","other"), ordered=FALSE)
data$education <-  factor(data$education, 
                          levels=c(1,2,3,4,5,6,7,8,-9),
                          labels=c("Still in school","Finished school with no qualifications","Vocational secondary certification (completion of specialized secondary school/college)","A-levels/International Baccalaureate, High-School subject-related higher education entrance qualification","Bachelor degree","Master degree","PhD or comperable","Other school-leaving qualification:","[NA] nicht beantwortet"), ordered=FALSE)
data$occupation <-  factor(data$occupation,
                           levels=c(1,2,3,4,5,6,7,8,9,10,-9),
                           labels=c("Pupil/in school","Training/apprenticeship","University student","Employee","Civil servant","Self-employed","Unemployed/seeking employment","Works in own household (also care of adult relatives, 100% parental leave)","Other:","Retired (including sickness benefit, activity allowance, 100% long-term sick leave, 100% sickness benefit, early retirement)","[NA] nicht beantwortet"), ordered=FALSE)
data$income <-  factor(data$income,
                       levels=c(1,2,3,4,5,6,7,8,9,10,11,12,-9),
                       labels=c("I do not have a personal income","less than 250 €","250 € up to 500 €","500 € up to 1000 €","1000 € up to 1500 €","1500 € up to 2000 €","2000 € up to 2500 €","2500 € up to 3000 €","3000 € up to 3500 €","3500 € up to 4000 €","4000 € or more","I do not wish to answer","[NA] nicht beantwortet"), ordered=FALSE)
data$consent <-  factor(data$consent, 
                        levels=c(1,2,-9), 
                        labels=c("No, I do not agree (do not participate in this study)","Yes, I agree","[NA] nicht beantwortet"), ordered=FALSE)
data$dq_overall <-  factor(data$dq_overall, 
                           levels=c(1,2,-9),
                           labels=c("Yes, I have correctly answered all questions. My data can be used for the analyses.","No, I wanted to \'just look\', take part repeatedly, or do not want my data to be used for analyses.","[NA] nicht beantwortet"), ordered=FALSE)
data$dq_meaningless <-  factor(data$dq_meaningless,
                               levels=c(1,2,3,-9), 
                               labels=c("I have completed all the tasks as requested in the instructions.","I sometimes clicked something because I was unmotivated or simply did not know.","I have often clicked something, so I am quickly done.","[NA] nicht beantwortet"), ordered=FALSE)
data$residence_country <-  factor(data$residence_country, levels=c(
    "40", "56", "76", "124", "170", "818", "250", "276", "300", "348", "356", "372", "380", "410", "484", "528", "566", "642", "643", 
    "688", "724", "752", "756", "804", "784", "826", "840", "862",'-1','-2','-9'),
    labels=c("Austria", "Belgium", "Brasil", "Canada", "Colombia", "Egypt", "France", "Germany", "Greece", "Hungary", "India", "Ireland", "Italia", "South Korea", "Mexico", "Netherlands", "Nigeria", "Romania", "Russia", "Serbia", "Spain", "Sweden", "Switzerland", "Ukraine", 
             "United Arab Emirates", "United Kingdom", "United States", "Venezuela", 'No answer','sonstige Texteingabe','nicht beantwortet'), ordered=FALSE)

data$started <- as.Date(data$started)

# Comine Risk Perception Scales -----------------------------------------------------------

#Generate keys for mapping items to sub-scales
keys.list <-  list(risk_self = c('rp2_living_self',
                                 'rp4_starvation_self',
                                 'rp7_disease_self'),
                   
                   risk_other = c("rp1_living_others",
                                  "rp3_starvation_others",
                                  "rp5_financial_aid",
                                  "rp6_disease_others",   
                                  "rp7_disease_self"),
                   
                   risk_total = c("rp1_living_others",
                                  "rp2_living_self",
                                  "rp3_starvation_others",
                                  "rp4_starvation_self",
                                  "rp5_financial_aid",
                                  "rp6_disease_others",
                                  "rp7_disease_self")
)
scale_metrics <- scoreItems(keys.list, data)
round(scale_metrics$alpha, digits = 2)

scale_scores <-  as.data.frame(scale_metrics$score)
data <-  bind_cols(data, scale_scores)










