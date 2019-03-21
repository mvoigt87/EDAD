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
# load(file='010_mayor.link.RData')

# prepared datasets (see R file 025_CleanCluster)

# Disca 44 file
# load(file = 'datasets/030_linkmay_M.RData')
# load(file = 'datasets/030_linkmay_F.RData')

# A 12 data
# load(file = '030_linkmay_M_12A.RData')
# load(file = '030_linkmay_F_12A.RData')

# link.may_M2 <- link.may_M
# link.may_F2 <- link.may_F

# D 13 Data
# load(file = 'datasets/030_linkmay_M_50ADL.RData')
# load(file = 'datasets/030_linkmay_F_50ADL.RData')

# new data
load(file='010_mayor50.link.RData')


# select only the dependent individuals! #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
table(link.may50$Dependientes13, useNA = "always")
table(link.may50$L_1, useNA = "always")
table(link.may50$DependientesReales44, useNA = "always")

# Select the dependent individuals
link.may50 <- link.may50 %>% filter(L_1=="Sí") 

### Create variable disability de golpe

link.may50 <- link.may50 %>%
  mutate(d_1_a = ifelse(Disca13=="Si" & DIS_1_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(d_2_a = ifelse(Disca13=="Si" & DIS_2_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(d_3_a = ifelse(Disca13=="Si" & DIS_3_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(d_4_a = ifelse(Disca13=="Si" & DIS_4_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(d_5_a = ifelse(Disca13=="Si" & DIS_5_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(d_6_a = ifelse(Disca13=="Si" & DIS_6_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(d_7_a = ifelse(Disca13=="Si" & DIS_7_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(d_8_a = ifelse(Disca13=="Si" & DIS_8_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(d_9_a = ifelse(Disca13=="Si" & DIS_9_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(d_10_a = ifelse(Disca13=="Si" & DIS_10_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(d_11_a = ifelse(Disca13=="Si" & DIS_11_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(d_12_a = ifelse(Disca13=="Si" & DIS_12_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(d_13_a = ifelse(Disca13=="Si" & DIS_13_A==DISCA13_AGE, 1, 0)) %>% 
  mutate(dis_golpe = as.numeric(d_1_a + d_2_a + d_3_a + d_4_a + d_5_a + d_6_a +
                                  d_7_a + d_8_a + d_9_a + d_10_a + d_11_a + d_12_a + d_13_a))

summary(link.may50$dis_golpe)
hist(link.may50$dis_golpe)

### New definition of catastrophics = more than one at the time + if the one is eating + if there is a year or less between 1-2-3

# First: for those with one "first" disability, exclude those with eating problems (DIS_10)

link.may50 <- link.may50 %>% mutate(catastrophic = ifelse(dis_golpe<=2,0,1)) %>% 
  # make it a factor variable for later use
    mutate(catastrophic= as.factor(ifelse(catastrophic==1, "catastrophic onset", "progressive onset")))

table(link.may50$catastrophic)



# Second: calculate the difference between every onset age and DISCA13_AGE (A second way to describe pathways)
          # ignore the zeros
link.may50 <- link.may50 %>% 
  mutate(diff1 = ifelse(DIS_1_A<999, DIS_1_A - DISCA13_AGE, NA)) %>% mutate(diff2 = ifelse(DIS_2_A<999,DIS_2_A - DISCA13_AGE, NA)) %>%
  mutate(diff3 = ifelse(DIS_3_A<999, DIS_3_A - DISCA13_AGE, NA)) %>% mutate(diff4 = ifelse(DIS_4_A<999, DIS_4_A - DISCA13_AGE, NA)) %>%
  mutate(diff5 = ifelse(DIS_5_A<999, DIS_5_A - DISCA13_AGE, NA)) %>% mutate(diff6 = ifelse(DIS_6_A<999, DIS_6_A - DISCA13_AGE, NA)) %>%
  mutate(diff7 = ifelse(DIS_7_A<999, DIS_7_A - DISCA13_AGE, NA)) %>% mutate(diff8 = ifelse(DIS_8_A<999, DIS_8_A - DISCA13_AGE, NA)) %>%
  mutate(diff9 = ifelse(DIS_9_A<999, DIS_9_A - DISCA13_AGE, NA)) %>% mutate(diff10 = ifelse(DIS_10_A<999, DIS_10_A - DISCA13_AGE, NA)) %>%
  mutate(diff11 = ifelse(DIS_11_A<999, DIS_11_A - DISCA13_AGE, NA)) %>% mutate(diff12 = ifelse(DIS_12_A<999, DIS_12_A - DISCA13_AGE, NA)) %>%
  mutate(diff13 = ifelse(DIS_13_A<999, DIS_13_A - DISCA13_AGE, NA))
# find the smallest difference to find the onset of disability number 2

  # mutate(DIFF_2 = pmin(diff1>0, diff2>0, diff3>0, diff4>0, diff5>0, diff6>0, diff7>0, diff8>0,
  #                      diff9>0, diff10>0, diff11>0, diff12>0, diff13>0))

  link.may50$DIFF_2 <- apply(link.may50[,627:639], 1, FUN = function(x) {min(x[x > 0])})
  
  hist(link.may50$DIFF_2)
  table(link.may50$DIFF_2)
  
  # the distribution of disability onset ages supports the idea (0-catastrophicos, 1+ progressivos)
  
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


# make them factors
link.may_M <- link.may_M %>% mutate(civil = as.factor(ifelse(Ecivil4=="Casado", "Married",
                                                             ifelse(Ecivil4=="viudo", "Widowed", "Others"))))

link.may_F <- link.may_F %>% mutate(civil = as.factor(ifelse(Ecivil4=="Casado", "Married",
                                                             ifelse(Ecivil4=="viudo", "Widowed", "Others"))))
# change the reference category
link.may_M <- within(link.may_M, civil <- relevel(civil, ref = "Married")) 
link.may_F <- within(link.may_F, civil <- relevel(civil, ref = "Married")) 
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


## Age groups
# table(link.may_M$edad5Suecia)
# table(link.may_F$edad5Suecia)
# 
# link.may_M <- link.may_M %>% mutate(age.gr = as.factor(ifelse(edad5Suecia == "45-64","45-64",
#                                              ifelse(edad5Suecia ==  "65-80", "65-80", "81+"))))
# 
# link.may_F <- link.may_F %>% mutate(age.gr = as.factor(ifelse(edad5Suecia == "45-64","45-64",
#                                                               ifelse(edad5Suecia ==  "65-80", "65-80", "81+"))))
# 
# # change the reference category
# link.may_M <- within(link.may_M, age.gr <- relevel(age.gr, ref = "45-64")) 
# link.may_F <- within(link.may_F, age.gr <- relevel(age.gr, ref = "45-64")) 

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

link.may_M <- within(link.may_M, CatPro <- relevel(CatPro, ref = "progressive"))
link.may_F <- within(link.may_F, CatPro <- relevel(CatPro, ref = "progressive"))

# 2.Z Onset of severity   !!!! New !!!
# ---------------------

link.may_M <- within(link.may_M, SEVEREDIS <- relevel(as.factor(SEVEREDIS), ref = "mild")) %>% filter(!is.na(SEVEREDIS))
link.may_F <- within(link.may_F, SEVEREDIS <- relevel(as.factor(SEVEREDIS), ref = "mild")) %>% filter(!is.na(SEVEREDIS))

# ----------------- #
# Duration variable #   !!! Duration variable cannot be used due to the categorical nature of "Edadinicio_cuidado"
# ----------------- #
link.may_F <- link.may_F %>% mutate(dur_dis = Edadinicio_cuidado - DISCA13_AGE)
link.may_M <- link.may_M %>% mutate(dur_dis = Edadinicio_cuidado - DISCA13_AGE)
summary(link.may_F$dur_dis)
summary(link.may_M$dur_dis)
hist(link.may_M$dur_dis, breaks=50)

# Probably better to be a categorical variable
# link.may_F <- link.may_F %>% mutate(dur_dis_cat = as.factor(ifelse(dur_dis<(-1), "$> 1$ years before", 
#                                                          ifelse(dur_dis<1, "same time",
#                                                                 ifelse(dur_dis<5, "$< 3$ years after", "$> 3$ years after")))))
# 
# table(link.may_F$dur_dis_cat)
# 
# 
# # Probably better to be a categorical variable
# link.may_M <- link.may_M %>% mutate(dur_dis_cat = as.factor(ifelse(dur_dis<(-1), "$> 1$ years before", 
#                                                          ifelse(dur_dis<1, "same time",
#                                                                 ifelse(dur_dis<5, "$< 3$ years after", "$> 3$ years after")))))
# 
# table(link.may_M$dur_dis_cat)

# relevel the categories
# ----------------------

# link.may_F <- within(link.may_F, dur_dis_cat <- relevel(dur_dis_cat, ref = "$> 1$ years before")) 
# link.may_M <- within(link.may_M, dur_dis_cat <- relevel(dur_dis_cat, ref = "$> 1$ years before"))


# relevel the categories
# ----------------------

# link.may_F <- within(link.may_F, EntryGrave13_cat <- relevel(as.factor(EntryGrave13_cat), ref = "85+")) 
# link.may_M <- within(link.may_M, EntryGrave13_cat <- relevel(as.factor(EntryGrave13_cat), ref = "85+"))



# missing value exploration
# -------------------------

library(Amelia)

# Make data set with variables for the analysis (No more missings)
training.data_M <- link.may_M %>% dplyr::select(EDAD, CoMorb, catastrophic, SEVEREDIS, ADL, civil, CV, education, Accident12, DailyAct)

missmap(training.data_M, main = "Missing values vs observed")

training.data_F <- link.may_F %>% dplyr::select(EDAD, catastrophic, SEVEREDIS, ADL, civil, CV, income, education, Accident12, DailyAct)

missmap(training.data_F, main = "Missing values vs observed")


# Output table for paper - summary stats on the age variables   !!! (Make all of them numeric arguments)
# ------------------------------------------------------------------------------------------------------ #
names(link.may_M)
stargazer(link.may_M[,c(9, 463, 513, 527, 541, 625)])
stargazer(link.may_F[,c(9, 463, 513, 527, 541, 625)])

###############################
### Exploratory Cox Regression
###############################

## Males
## -----

# exit time as time in years (month/12+year)
link.may_M <- link.may_M %>% mutate(t.salida = (a_salida+(m_salida/12)) - 2006.99)
hist(link.may_M$t.salida)

# entry time (month/12+year) - onset of disability
link.may_M <- link.may_M %>% mutate(t.entrada.dis =  2006.99 - (EDAD-DISCA13_AGE)) # same story with inicio Disca44
link.may_M <- link.may_M %>% mutate(t.salida.dis = (a_salida+(m_salida/12)))

# entry time (month/12+year) - onset of dependency
link.may_M <- link.may_M %>% mutate(t.entrada.dep =  2006.99 - (EDAD-Edadinicio_cuidado)) 



## Females
## -------

# exit time (month/12+year)
link.may_F <- link.may_F %>% mutate(t.salida = (a_salida+(m_salida/12)) - 2006.99)
hist(link.may_F$t.salida)


# entry time (month/12+year) - onset of disability
link.may_F <- link.may_F %>% mutate(t.entrada.dis =  2006.99 - (EDAD-DISCA13_AGE)) # same story with inicio Disca44
link.may_F <- link.may_F %>% mutate(t.salida.dis = (a_salida+(m_salida/12)))

# entry time (month/12+year) - onset of dependency
link.may_F <- link.may_F %>% mutate(t.entrada.dep =  2006.99 - (EDAD-Edadinicio_cuidado)) 


#### 2. Cox Models
#### -------------


##### Using time between disability onset and dependency onset as explanatory variable (severity onset) #######
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################

#########
# males #
#########


Cox_M_1 <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGE,
                 data=link.may_M)

summary(Cox_M_1)

# Add co-morbidity information

Cox_M_2 <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGE + SEVEREDIS,
                 data=link.may_M)

summary(Cox_M_2)


Cox_M_F <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGE + SEVEREDIS  + CoMorb + Accident12 + DailyAct ,
                   data=link.may_M)

summary(Cox_M_F)

# + education + civil + CV


# 2. step - add severity and comorbidity


# females


Cox_F_1 <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGE,
                 data=link.may_F)

summary(Cox_F_1)

# Add co-morbidity information

Cox_F_2 <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGE + SEVEREDIS,
                 data=link.may_F)

summary(Cox_F_2)


# And add socio-demographics in a final step

Cox_F_F <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGE + SEVEREDIS + CoMorb + Accident12 + DailyAct,
                 data=link.may_F)

summary(Cox_F_F)

# + education + civil + CV

# Put the output together

# males
# -----
stargazer(Cox_M_1,Cox_M_2, Cox_M_F, title="Cox PH Regression Models",no.space=F,
          ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative risk of dying"),
          covariate.labels=c("Onset age", "Moderate Severity (ICF)", "High Severity (ICF)", 
                             "Suffers from multiple diseases" ,"Had accident in last 12 mo.",
                             "No daily activity"),
          single.row=FALSE, apply.coef = exp)
# , "Incomplete Ed." ,"Primary Ed.", "Single/Div","Widowed", "No cohabitation"


# females
# -------
stargazer(Cox_F_1,Cox_F_2, Cox_F_F, title="Cox PH Regression Models",no.space=F,
          ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative risk of dying"),
          covariate.labels=c("Onset age" , "Moderate Severity (ICF)", "High Severity (ICF)", 
                             "Suffers from multiple diseases" ,"Had accident in last 12 mo.",
                             "No daily activity"),
          single.row=FALSE, apply.coef = exp)


# , "Incomplete Ed." ,"Primary Ed.", "Single/Div","Widowed", "No cohabitation"

############################
############################
##### Gompertz Model #######
############################
############################



## Females
GOMP_F <- flexsurvreg(Surv(time=EDAD,
                           time2=age.ex,
                           event=event) ~ dur_dis_cat + education + CP + civil + EntryGrave13, data = link.may_F,
                          dist = "gompertz")
GOMP_F
# survival curve
plot(GOMP_F, xlim=c(50,100))
legend("topright",legend=c("KME","Gompertz Curve"), 
       lty=c(1,1),col=c("black","red"), cex=0.75)

# hazard plot # doesn´t work for left-truncated data
# plot(GOMP_F, xlim=c(50,100),ylim=c(-0.1,1),type = "hazard")

GOMP_M <- flexsurvreg(Surv(time=EDAD,
                          time2=age.ex,
                          event=event) ~ dur_dis_cat + education + CP + civil + EntryGrave13, data = link.may_M,
                     dist = "gompertz")
GOMP_M

# survival curve
plot(GOMP_M, xlim=c(50,100))
legend("topright",legend=c("KME","Gompertz Curve"), 
       lty=c(1,1),col=c("black","red"), cex=0.75)





# estimated shape and scale
shape <- 0.096
rate <- 0.0000641
# vector of quantiles
time<-seq(50,100,1)

### Base survival - Gompertz function (http://www.statsathome.com/2017/06/07/fitting-non-linear-groth-curves-in-r/)

gompertz <- function(time, a, mu, lambda){
  y <- a*exp(-exp(mu*exp(1)/a*(lambda-time)+1))
  return(data.frame(time=time, y=y))
}

fit <- gompertz(time = time, a = 2 ,mu = shape ,lambda = rate)

plot(fit)


# function for the hazard rate
# ---------------------------- #
# 
# haz.Gompertz <- function(x, shape, rate) {
#   hgompertz(x, shape = shape, rate = rate)
# }
# 
# 
# x <- seq(50,100,1)             ## to make it the same time scale
# # parameters
# shape.m <- GOMP_F$coefficients[1]
# rate.m <- 0.0000641                  ## something went wrong with the rate (always negative)
# 
# 
# haz.Gomp <- as.vector(haz.Gompertz(x=x, shape = shape.m, rate = rate.m))
# 
# plot(x=x,y=log(haz.Gomp), type="l")  

# # Gompertz survival function
# Gomp.Surv <- function(time,a=0,b=0,c=0){
#   y(t) <- a*exp(-b*exp(-c*t))
#   return(y)
# }  



## Males
## -----

GOMP_M <- flexsurvreg(Surv(time=EDAD,
                           time2=age.ex,
                           event=event) ~ dur_dis_cat + education + CP + civil + EntryGrave13, data = link.may_M,
                      dist = "gompertz")






# Just the transitions

Cox.CFP.a <- coxph(Surv(time=EDAD,
                        time2=age.ex,
                        event=event) ~ group.d.d, 
                   data=subset(link.may))

summary(Cox.CFP.a)

# Plus a few ses variables
Cox.CFP.b <- coxph(Surv(time=EDAD,
                        time2=age.ex,
                        event=event) ~ group.d.d + Sex + Education + Income, 
                   data=subset(link.may))

summary(Cox.CFP.b)

# table for CFP
# -------------

stargazer(Cox.CFP.a, Cox.CFP.b, title ="Cox PH Model",no.space=F, 
          ci=T, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Hazard Ratios"),
          covariate.labels=c("Acute Transition", "Moderate Transition", "Male", "Low Education",
                             "Low Income (<625 Eur)"),
          single.row=T, apply.coef = exp)

