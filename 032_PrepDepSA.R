########################################################
##### Analysis of mortality differences by pathway #####
########################################################


# 0.1 packages

library(tidyverse)
library(data.table)
library(foreign)
library(survival)
library(broom)
library(stargazer)
# Use for a parametric model
library(flexsurv)
# for the cool plots
library(survminer)
# for checking for different distributions
library(fitdistrplus)

# 0.3 Set right working directory
dir()
setwd("C:/Users/y4956294S/Documents/LONGPOP/Subproject 2 - SE differences in transition to dependency/R code/EDAD")

# 0.2 data
# --------------------------------------------
# new data
load(file='010_mayor50.link.RData')


# select only the dependent individuals! #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# table(link.may50$Dependientes13, useNA = "always")
# table(link.may50$L_1, useNA = "always")
# table(link.may50$DependientesReales44, useNA = "always")
# 
# # Select the dependent individuals
# link.may50 <- link.may50 %>% filter(L_1=="Sí") 



### Dataset for trial and error

# diffdata <- as.data.frame(link.may50[,c(6,8,625,626:639)])
# diffdata <- diffdata %>% 
#   mutate(diff1 = ifelse(is.na(diff1), 999, diff1)) %>% mutate(diff2 = ifelse(is.na(diff2), 999, diff2)) %>% 
#   mutate(diff3 = ifelse(is.na(diff3), 999, diff3)) %>% mutate(diff4 = ifelse(is.na(diff4), 999, diff4)) %>% 
#   mutate(diff5 = ifelse(is.na(diff5), 999, diff5)) %>% mutate(diff6 = ifelse(is.na(diff6), 999, diff6)) %>% 
#   mutate(diff7 = ifelse(is.na(diff7), 999, diff7)) %>% mutate(diff8 = ifelse(is.na(diff8), 999, diff8)) %>% 
#   mutate(diff9 = ifelse(is.na(diff9), 999, diff9)) %>% mutate(diff10 = ifelse(is.na(diff10), 999, diff10)) %>% 
#   mutate(diff11 = ifelse(is.na(diff11), 999, diff11)) %>% mutate(diff12 = ifelse(is.na(diff12), 999, diff12)) %>% 
#   mutate(diff13 = ifelse(is.na(diff13), 999, diff13))
# diffdata$DIFF_2X <- apply(diffdata[,4:16], 1, FUN = function(x) {min(x[x > 0])})
# diffdata <- diffdata %>% mutate(DIFF_2X = as.numeric(ifelse(DIFF_2X==999, 0, DIFF_2X)))
# 
# # now to get an idea which one may have had an progressive onset
# hist(diffdata$DIFF_2X)
# # Now just those with 2 or more disabilities at the same onset age - filters those who had only 1 or 2 disabilities overall
# hist(diffdata$DIFF_2X[diffdata$dis_golpe==1])
# summary(diffdata$DIFF_2X)
# summary(diffdata$DIFF_2X[diffdata$dis_golpe==1])
# table(diffdata$DIFF_2X, useNA = "always")

# the distribution of disability onset ages supports the idea (0-catastrophicos, 1+ progressivos)

## Onset age of disability as age group variable

hist(link.may50$DISCA13_AGE)
summary(link.may50$DISCA13_AGE)

link.may50 <- link.may50 %>% mutate(DISCA13_AGEGR = ifelse(DISCA13_AGE<61, "50-60", ifelse(DISCA13_AGE<71, "60-70",
                                                                                           ifelse(DISCA13_AGE<81, "70-80", "80+"))))

table(link.may50$DISCA13_AGEGR)

# ----------------- #
# Duration variable #   !!! Duration variable cannot be used due to the categorical nature of "Edadinicio_cuidado"
# ----------------- #
link.may50 <- link.may50 %>% mutate(dur_dis = EDAD - DISCA13_AGE) %>% filter(dur_dis>=0)
summary(link.may50$dur_dis)
hist(link.may50$dur_dis)

####
####

# 2.5 Separate analysis by sex
link.may_M <- link.may50 %>% filter(SEXO=="Varón") %>% mutate(SEXO="Male")
link.may_F <- link.may50 %>% filter(SEXO=="Mujer") %>% mutate(SEXO="Female")



#### 1. Change categories and referecnces

# Create English equivalent with less categories
link.may_M <- link.may_M %>% mutate(education = as.factor(ifelse(Estudios4=="Analfabeto/estudios incompletos", "Incomplete Educ.",
                                                                 ifelse(Estudios4=="Estudios primarios", "Primary Educ.", "Secondary Educ or higher"))))

link.may_F <- link.may_F %>% mutate(education = as.factor(ifelse(Estudios4=="Analfabeto/estudios incompletos", "Incomplete Educ.",
                                                                 ifelse(Estudios4=="Estudios primarios", "Primary Educ.", "Secondary Educ or higher"))))

table(link.may_M$education, useNA = "always")
# change the reference category
link.may_M <- within(link.may_M, education <- relevel(education, ref = "Secondary Educ or higher")) 
link.may_F <- within(link.may_F, education <- relevel(education, ref = "Secondary Educ or higher")) 

### CIVIL STATUS
table(link.may_F$Ecivil4, useNA = "always")
table(link.may_M$Ecivil4, useNA = "always")
# make them factors
link.may_M <- link.may_M %>% mutate(civil = as.factor(ifelse(Ecivil4=="Casado", "Married",
                                                             ifelse(Ecivil4=="viudo", "Widowed", "Others"))))

link.may_F <- link.may_F %>% mutate(civil = as.factor(ifelse(Ecivil4=="Casado", "Married",
                                                             ifelse(Ecivil4=="viudo", "Widowed", "Others"))))
# change the reference category
link.may_M <- within(link.may_M, civil <- relevel(civil, ref = "Widowed")) 
link.may_F <- within(link.may_F, civil <- relevel(civil, ref = "Widowed")) 
table(link.may_M$civil, useNA = "always")
table(link.may_F$civil)

## Income
table(link.may_M$IM_MENS, useNA = "always")
table(link.may_F$IM_MENS, useNA = "always") # What?


link.may_M <- link.may_M %>% mutate(income = as.factor(ifelse(IM_MENS=="Menos de 500 euros" | IM_MENS=="De 500 a menos de 1000 euros", "$<$ 1000 Euro",
                                                              ifelse(IM_MENS=="De 1000 a menos de 1500 euros" | IM_MENS=="De 1500 a menos de 2000 euros ", "1000-2000 Euro", "$>$ 2000 Euro"))))
link.may_F <- link.may_F %>% mutate(income = as.factor(ifelse(IM_MENS=="Menos de 500 euros" | IM_MENS=="De 500 a menos de 1000 euros", "$<$ 1000 Euro",
                                                              ifelse(IM_MENS=="De 1000 a menos de 1500 euros" | IM_MENS=="De 1500 a menos de 2000 euros ", "1000-2000 Euro", "$>$ 2000 Euro"))))
link.may_M <- within(link.may_M, income <- relevel(income, ref = "$>$ 2000 Euro")) 
link.may_F <- within(link.may_F, income <- relevel(income, ref = "$>$ 2000 Euro")) 

# Care giver type
# ---------------

table(link.may_M$CP)
table(link.may_F$CP)
class(link.may_M$CP)

# make them factors
link.may_M$CP <- as.factor(link.may_M$CP)
link.may_F$CP <- as.factor(link.may_F$CP)

# 2.8 Cohabitation with the partner
# ---------------------------------
table(link.may_M$PAREJA)
table(link.may_F$PAREJA)

link.may_M <- link.may_M %>% mutate(PAREJA=ifelse(PAREJA=="NC",NA,PAREJA)) %>% filter(!is.na(PAREJA))
link.may_F <- link.may_F %>% mutate(PAREJA=ifelse(PAREJA=="NC",NA,PAREJA)) %>% filter(!is.na(PAREJA))

# Create English equivalent with less categories
link.may_M <- link.may_M %>% mutate(CV = as.factor(ifelse(PAREJA==1, "Lives with Partner", "Doesn´t live with partner")))
link.may_F <- link.may_F %>% mutate(CV = as.factor(ifelse(PAREJA==1, "Lives with Partner", "Doesn´t live with partner")))

# change the reference category
link.may_M <- within(link.may_M, CV <- relevel(CV, ref = "Lives with Partner")) 
link.may_F <- within(link.may_F, CV <- relevel(CV, ref = "Lives with Partner")) 


### Kinship - variable (Does a relative live in the same household?)

### two categorizations based on I_1_1 to I_1_7
  # one: differentiation between close (children and siblings) and extented kin (everybody else)
  # two: geographical distance (shared environment) - same household, same city etc

# children (the geographically closest)
link.may_M <- link.may_M %>% mutate(kin_child = ifelse(I_1_2=="En el mismo domicilio" | I_1_2=="En el mismo edificio", "same household or building",
                                                       ifelse(I_1_2=="En el mismo barrio o pueblo" | I_1_2=="En la misma ciudad", "same village or city",
                                                              ifelse(I_1_2=="En la misma provincia"| I_1_2=="En distinta provincia", "in Spain",
                                                                     ifelse(I_1_2=="En otro país","lives abroad", "does not exists/unknown"))))) %>% 
# siblings (the geographically closest)
mutate(kin_sib = ifelse(I_1_3=="En el mismo domicilio" | I_1_3=="En el mismo edificio", "same household or building",
                       ifelse(I_1_3=="En el mismo barrio o pueblo" | I_1_3=="En la misma ciudad", "same village or city",
                              ifelse(I_1_3=="En la misma provincia"| I_1_3=="En distinta provincia","in Spain",
                                     ifelse(I_1_3=="En otro país","lives abroad", "does not exists/unknown"))))) %>% 
# grand children (the geographically closest)
mutate(kin_grandc = ifelse(I_1_4=="En el mismo domicilio" | I_1_4=="En el mismo edificio", "same household or building",
                          ifelse(I_1_4=="En el mismo barrio o pueblo" | I_1_4=="En la misma ciudad", "same village or city",
                                ifelse(I_1_4=="En la misma provincia"| I_1_4=="En distinta provincia","in Spain",
                                       ifelse(I_1_4=="En otro país","lives abroad", "does not exists/unknown"))))) %>% 
# extented kin (the geographically closest)
mutate(kin_ext = ifelse(I_1_6=="En el mismo domicilio" | I_1_6=="En el mismo edificio", "same household or building",
                        ifelse(I_1_6=="En el mismo barrio o pueblo" | I_1_6=="En la misma ciudad", "same village or city",
                              ifelse(I_1_6=="En la misma provincia"| I_1_6=="En distinta provincia","in Spain",
                                     ifelse(I_1_6=="En otro país","lives abroad", "does not exists/unknown"))))) %>% 
# friends who are not neighbors (the geographically closest)
mutate(kin_amigo = ifelse(I_1_7=="En el mismo domicilio" | I_1_7=="En el mismo edificio", "same household or building",
                         ifelse(I_1_7=="En el mismo barrio o pueblo" | I_1_7=="En la misma ciudad", "same village or city",
                               ifelse(I_1_7=="En la misma provincia"| I_1_7=="En distinta provincia","in Spain",
                                      ifelse(I_1_7=="En otro país","lives abroad", "does not exists/unknown")))))

### Now summarise (close and extented kin)

# close kin
link.may_M <- link.may_M %>% mutate(kin_close = ifelse(kin_child=="same household or building"|kin_sib=="same household or building",
                                                       "same household or building",
                                                ifelse(kin_child=="same village or city"|kin_sib=="same village or city", "same village or city",
                                                ifelse(kin_child=="in Spain"|kin_sib=="in Spain"|kin_child=="lives abroad"|kin_sib=="lives abroad",
                                                                                                        "different province or abroad" ,"does not exists/unknown"))))

# extented kin
link.may_M <- link.may_M %>% mutate(kin_extent = ifelse(kin_grandc=="same household or building"|kin_ext=="same household or building"|kin_amigo=="same household or building",
                                                       "same household or building",
                                                       ifelse(kin_grandc=="same village or city"|kin_ext=="same village or city"|kin_amigo=="same village or city", "same village or city",
                                                              ifelse(kin_grandc=="in Spain"|kin_ext=="in Spain"|kin_amigo=="in Spain"|kin_grandc=="lives abroad"|kin_ext=="lives abroad"|kin_amigo=="lives abroad",
                                                                     "different province or abroad","does not exists/unknown"))))

table(link.may_M$kin_close)
table(link.may_M$kin_extent)

### Same for females
# children (the geographically closest)
link.may_F <- link.may_F %>% mutate(kin_child = ifelse(I_1_2=="En el mismo domicilio" | I_1_2=="En el mismo edificio", "same household or building",
                                                       ifelse(I_1_2=="En el mismo barrio o pueblo" | I_1_2=="En la misma ciudad", "same village or city",
                                                              ifelse(I_1_2=="En la misma provincia"| I_1_2=="En distinta provincia", "in Spain",
                                                                     ifelse(I_1_2=="En otro país","lives abroad", "does not exists/unknown"))))) %>% 
  # siblings (the geographically closest)
  mutate(kin_sib = ifelse(I_1_3=="En el mismo domicilio" | I_1_3=="En el mismo edificio", "same household or building",
                          ifelse(I_1_3=="En el mismo barrio o pueblo" | I_1_3=="En la misma ciudad", "same village or city",
                                 ifelse(I_1_3=="En la misma provincia"| I_1_3=="En distinta provincia","in Spain",
                                        ifelse(I_1_3=="En otro país","lives abroad", "does not exists/unknown"))))) %>% 
  # grand children (the geographically closest)
  mutate(kin_grandc = ifelse(I_1_4=="En el mismo domicilio" | I_1_4=="En el mismo edificio", "same household or building",
                             ifelse(I_1_4=="En el mismo barrio o pueblo" | I_1_4=="En la misma ciudad", "same village or city",
                                    ifelse(I_1_4=="En la misma provincia"| I_1_4=="En distinta provincia","in Spain",
                                           ifelse(I_1_4=="En otro país","lives abroad", "does not exists/unknown"))))) %>% 
  # extented kin (the geographically closest)
  mutate(kin_ext = ifelse(I_1_6=="En el mismo domicilio" | I_1_6=="En el mismo edificio", "same household or building",
                          ifelse(I_1_6=="En el mismo barrio o pueblo" | I_1_6=="En la misma ciudad", "same village or city",
                                 ifelse(I_1_6=="En la misma provincia"| I_1_6=="En distinta provincia","in Spain",
                                        ifelse(I_1_6=="En otro país","lives abroad", "does not exists/unknown"))))) %>% 
  # friends who are not neighbors (the geographically closest)
  mutate(kin_amigo = ifelse(I_1_7=="En el mismo domicilio" | I_1_7=="En el mismo edificio", "same household or building",
                            ifelse(I_1_7=="En el mismo barrio o pueblo" | I_1_7=="En la misma ciudad", "same village or city",
                                   ifelse(I_1_7=="En la misma provincia"| I_1_7=="En distinta provincia","in Spain",
                                          ifelse(I_1_7=="En otro país","lives abroad", "does not exists/unknown")))))

### Now summarise (close and extented kin)


# close kin
link.may_F <- link.may_F %>% mutate(kin_close = ifelse(kin_child=="same household or building"|kin_sib=="same household or building",
                                                       "same household or building",
                                                       ifelse(kin_child=="same village or city"|kin_sib=="same village or city", "same village or city",
                                                              ifelse(kin_child=="in Spain"|kin_sib=="in Spain"|kin_child=="lives abroad"|kin_sib=="lives abroad",
                                                                     "different province or abroad" ,"does not exists/unknown"))))

# extented kin
link.may_F <- link.may_F %>% mutate(kin_extent = ifelse(kin_grandc=="same household or building"|kin_ext=="same household or building"|kin_amigo=="same household or building",
                                                        "same household or building",
                                                        ifelse(kin_grandc=="same village or city"|kin_ext=="same village or city"|kin_amigo=="same village or city", "same village or city",
                                                               ifelse(kin_grandc=="in Spain"|kin_ext=="in Spain"|kin_amigo=="in Spain"|kin_grandc=="lives abroad"|kin_ext=="lives abroad"|kin_amigo=="lives abroad",
                                                                      "different province or abroad","does not exists/unknown"))))

table(link.may_F$kin_close)
table(link.may_F$kin_extent)




## Age groups
table(link.may_M$DISCA13_AGEGR)
table(link.may_F$DISCA13_AGEGR)
# # change the reference category
link.may_M <- within(link.may_M, DISCA13_AGEGR <- relevel(as.factor(DISCA13_AGEGR), ref = "50-60"))
link.may_F <- within(link.may_F, DISCA13_AGEGR <- relevel(as.factor(DISCA13_AGEGR), ref = "50-60"))

#link.may <- within(link.may, Income <- relevel(Income, ref = "625+ eur")) 

# 2.3 Accident in the last 12 months #
# ---------------------------------- #

class(link.may_F$K_4)   
table(link.may_F$K_4, useNA = "always")

link.may_F <- link.may_F %>% mutate(Accident12 = as.factor(ifelse(K_4=="Sí", "Accident 12 mo", ifelse(K_4=="NC", NA, "No Accident"))))
link.may_M <- link.may_M %>% mutate(Accident12 = as.factor(ifelse(K_4=="Sí", "Accident 12 mo", ifelse(K_4=="NC", NA, "No Accident"))))

link.may_F <- within(link.may_F, Accident12 <- relevel(Accident12, ref = "No Accident")) 
link.may_M <- within(link.may_M, Accident12 <- relevel(Accident12, ref = "No Accident"))

# 2.4 Body and attitude #
# ---------------------- #
table(link.may_F$K_7, useNA = "always")

link.may_F <- link.may_F %>% mutate(DailyAct = ifelse(K_7=="Sí", "Daily activity", ifelse(K_7=="NC", NA, "No daily act.")))
link.may_M <- link.may_M %>% mutate(DailyAct = ifelse(K_7=="Sí", "Daily activity", ifelse(K_7=="NC", NA, "No daily act.")))

###### 2.5 Co-morbidity
###### ----------------

# # Cardiovascular diseases (includes stroke)
# link.may_F <- link.may_F %>% mutate(D1_CVD = ifelse(K_3_2=="Sí" | K_3_3=="Sí" | K_3_5=="Sí","CVD", "No CVD")) %>% 
#   # Now bring the missings back
#   mutate(D1_CVD = ifelse(K_3_2=="NC" & K_3_3=="NC" & K_3_5=="NC", NA, D1_CVD))
# # Cardiovascular diseases (includes stroke)
# link.may_M <- link.may_M %>% mutate(D1_CVD = ifelse(K_3_2=="Sí" | K_3_3=="Sí" | K_3_5=="Sí","CVD", "No CVD")) %>% 
#   # Now bring the missings back
#   mutate(D1_CVD = ifelse(K_3_2=="NC" & K_3_3=="NC" & K_3_5=="NC", NA, D1_CVD))
# 
# # Cancer
# link.may_F <- link.may_F %>% mutate(D2_C = ifelse(K_3_12!="Sí","Cancer", ifelse(K_3_12=="NC", NA, "No Cancer")))
# link.may_M <- link.may_M %>% mutate(D2_C = ifelse(K_3_12!="Sí","Cancer", ifelse(K_3_12=="NC", NA, "No Cancer")))
# 
# # Mental diseases
# link.may_F <- link.may_F %>% mutate(D3_MD = ifelse(K_3_15=="Sí"| K_3_16=="Sí" | K_3_17=="Sí","Mental disease", "No mental disease")) %>% 
#   # Now bring the missings back
#   mutate(D3_MD = ifelse(K_3_15=="NC" & K_3_16=="NC" & K_3_17=="NC", NA, D3_MD))  
# 
# link.may_M <- link.may_M %>% mutate(D3_MD = ifelse(K_3_15=="Sí"| K_3_16=="Sí" | K_3_17=="Sí","Mental disease", "No mental disease")) %>% 
#   # Now bring the missings back
#   mutate(D3_MD = ifelse(K_3_15=="NC" & K_3_16=="NC" & K_3_17=="NC", NA, D3_MD))  
# 
# ### Change references
# 
# # CVD
# link.may_M <- link.may_M %>% mutate(D1_CVD = as.factor(D1_CVD))
# link.may_F <- link.may_F %>% mutate(D1_CVD = as.factor(D1_CVD))
# 
# link.may_M <- within(link.may_M, D1_CVD <- relevel(D1_CVD, ref = "No CVD")) 
# link.may_F <- within(link.may_F, D1_CVD <- relevel(D1_CVD, ref = "No CVD")) 
# 
# # Mental diseases
# link.may_M <- link.may_M %>% mutate(D3_MD = as.factor(D3_MD))
# link.may_F <- link.may_F %>% mutate(D3_MD = as.factor(D3_MD))
# 
# link.may_M <- within(link.may_M, D3_MD <- relevel(D3_MD, ref = "No mental disease")) 
# link.may_F <- within(link.may_F, D3_MD <- relevel(D3_MD, ref = "No mental disease"))

# 2.X Co-morbidity variables
# --------------------------
link.may_M <- within(link.may_M, CoMorb <- relevel(CoMorb, ref = "no multi morbidity")) 
link.may_F <- within(link.may_F, CoMorb <- relevel(CoMorb, ref = "no multi morbidity")) 

# 2.Y. Disability Count - progressive vs. catastrophic (NEW!)
# -----------------------------------------------------
# 
# link.may_M <- within(link.may_M, catastrophic <- relevel(catastrophic, ref = "progressive onset"))
# link.may_F <- within(link.may_F, catastrophic <- relevel(catastrophic, ref = "progressive onset"))

# link.may_M <- within(link.may_M, CatPro <- relevel(CatPro, ref = "progressive"))
# link.may_F <- within(link.may_F, CatPro <- relevel(CatPro, ref = "progressive"))

# 2.Z Onset of severity   !!!! New !!!
# ---------------------

link.may_M <- within(link.may_M, SEVEREDIS <- relevel(as.factor(SEVEREDIS), ref = "mild")) %>% filter(!is.na(SEVEREDIS))
link.may_F <- within(link.may_F, SEVEREDIS <- relevel(as.factor(SEVEREDIS), ref = "mild")) %>% filter(!is.na(SEVEREDIS))

# 2.Z2 Onset of severity with only two categories !!!! New !!!
# -----------------------------------------------

link.may_M <- link.may_M %>% mutate(SEVEREDIS2 = ifelse(SEVEREDIS=="mild" | SEVEREDIS=="moderate", "mild disability", "severe disability"))
link.may_M <- within(link.may_M, SEVEREDIS2 <- relevel(as.factor(SEVEREDIS2), ref = "mild disability")) %>% filter(!is.na(SEVEREDIS2))
table(link.may_M$SEVEREDIS2)

link.may_F <- link.may_F %>% mutate(SEVEREDIS2 = ifelse(SEVEREDIS=="mild" | SEVEREDIS=="moderate", "mild disability", "severe disability"))
link.may_F <- within(link.may_F, SEVEREDIS2 <- relevel(as.factor(SEVEREDIS2), ref = "mild disability")) %>% filter(!is.na(SEVEREDIS2))
table(link.may_F$SEVEREDIS2)

# duration age groups
# -------------------
# summary(link.may_F$dur_dis)
# summary(link.may_M$dur_dis)
# hist(link.may_M$dur_dis, breaks=50)
# hist(link.may_F$dur_dis)
# link.may_F <- link.may_F %>% mutate(dur_disGR = ifelse(dur_dis<=4,"0-4 years", ifelse(dur_dis<=7, "5-7 years", "7 + years")))
# link.may_M <- link.may_M %>% mutate(dur_disGR = ifelse(dur_dis<=3,"0-3 years", ifelse(dur_dis<=6, "4-6 years", "6 + years")))
# 
# # Visualization
# SDD <- link.may_M %>% mutate(event = as.factor(event)) %>% 
#   ggplot(aes(x=dur_disGR, fill=SEVEREDIS)) +
#   geom_bar(aes(y = (..count..)/sum(..count..))) +
#   scale_y_continuous(name = "Relative Frequency", labels = scales::percent) +
#   scale_x_discrete(name = "Duration Disability (Groups)") +
#   scale_fill_manual(name = "", values=c("#0072B2", "#D55E00", "#009E73")) +
#   theme_bw()
# SDD + theme(axis.text=element_text(size=12),
#             axis.title=element_text(size=14,face="bold"), strip.text.y = element_text(size=12, face="bold"))

### ------------------- ###
### Grouping categories ###
### ------------------- ###

# make the necessary checks

# a possible group in between (like accelerated)
link.may_F %>% filter(dis_golpe<=2 & DIFF_2>1) %>% count(SEVEREDIS2)
link.may_M %>% filter(dis_golpe<=2 & DIFF_2>1) %>% count(SEVEREDIS2)

# 1. idea - make 3 groups
  # catastrophic - high severity, more than 2 disabilities aquired at the same time or less than one years in between
  # accelerated - high severity, 1 or 2 disabilities acquired at the same time and more than one year in between (muy pocos)
  # gradual - the rest mild disability

link.may_F <- link.may_F %>% mutate(DIS_GRP = ifelse(dis_golpe<=2 & DIFF_2>1, "accelerated", 
                                                     ifelse(dis_golpe>2 & SEVEREDIS2=="severe disability","catastrophic","mild-gradual")))


link.may_M <- link.may_M %>% mutate(DIS_GRP = ifelse(dis_golpe<=2 & DIFF_2>1, "accelerated", 
                                                     ifelse(dis_golpe>2 & SEVEREDIS2=="severe disability","catastrophic","mild-gradual")))

###### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ######
# link.may_F <- link.may_F %>% mutate(DIS_GRP = ifelse(SEVEREDIS2=="mild disability", "mild-gradual", 
#                                               ifelse(SEVEREDIS2=="severe disability" & dis_golpe==1,"accelerated", "catastrophic")))
# 
# table(link.may_F$DIS_GRP)
# 
# link.may_M <- link.may_M %>% mutate(DIS_GRP = ifelse(SEVEREDIS2=="mild disability", "mild-gradual", 
#                                                      ifelse(SEVEREDIS2=="severe disability" & dis_golpe==1,"accelerated", "catastrophic")))
# 
# table(link.may_M$DIS_GRP)
###### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ######
# check age groups
summary(link.may_F$EDAD[link.may_F$DIS_GRP=="accelerated"])
summary(link.may_F$EDAD[link.may_F$DIS_GRP=="catastrophic"])

# see event distribution - looks like accelerated decline leads to much higher number of deaths
round(prop.table(table(link.may_F$event[link.may_F$DIS_GRP=="accelerated"])),3)
round(prop.table(table(link.may_F$event[link.may_F$DIS_GRP=="catastrophic"])),3)

# Differences in distance between first and second limitation
summary(link.may_F$DIFF_2[link.may_F$DIS_GRP=="mild-gradual"]) # NA - mostly only one disability - doublecheck!
table(link.may_F$NumDiscas13[link.may_F$DIS_GRP=="mild-gradual"])
summary(link.may_F$DIFF_2[link.may_F$DIS_GRP=="accelerated"])  # median 3


# missing value exploration
# -------------------------

library(Amelia)

# Make data set with variables for the analysis (No more missings)
training.data_M <- link.may_M %>% dplyr::select(EDAD, CoMorb, catastrophic, SEVEREDIS, ADL, civil, education, Accident12, DailyAct)

missmap(training.data_M, main = "Missing values vs observed")

training.data_F <- link.may_F %>% dplyr::select(EDAD, catastrophic, SEVEREDIS, ADL, civil, income, education, Accident12, DailyAct)

missmap(training.data_F, main = "Missing values vs observed")


# Output table for paper - summary stats on the age variables   !!! (Make all of them numeric arguments)
# ------------------------------------------------------------------------------------------------------ #
names(link.may_M)
stargazer(link.may_M[,c(9, 463, 513, 527, 541, 625)])
stargazer(link.may_F[,c(9, 463, 513, 527, 541, 625)])

# save file for the next step - KME and Survival Models

save(link.may_M,file='010_mayorDEP_M.link.RData')
save(link.may_F,file='010_mayorDEP_F.link.RData')
