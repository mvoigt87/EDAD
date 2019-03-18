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


# 2.5 Separate analysis by sex
link.may_M <- link.may50 %>% filter(SEXO=="Varón") %>% mutate(Sex="Male")
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


table(link.may_M$CP)
table(link.may_F$CP)
class(link.may_M$CP)

# make them factors
link.may_M$CP <- as.factor(link.may_M$CP)
link.may_F$CP <- as.factor(link.may_F$CP)

# change the reference category
link.may_M <- within(link.may_M, CP <- relevel(CP, ref = "Lives with Partner")) 
link.may_F <- within(link.may_F, CP <- relevel(CP, ref = "Lives with Partner")) 


## Age groups
table(link.may_M$edad5Suecia)
table(link.may_F$edad5Suecia)

link.may_M <- link.may_M %>% mutate(age.gr = as.factor(ifelse(edad5Suecia == "45-64","45-64",
                                             ifelse(edad5Suecia ==  "65-80", "65-80", "81+"))))

link.may_F <- link.may_F %>% mutate(age.gr = as.factor(ifelse(edad5Suecia == "45-64","45-64",
                                                              ifelse(edad5Suecia ==  "65-80", "65-80", "81+"))))

# change the reference category
link.may_M <- within(link.may_M, age.gr <- relevel(age.gr, ref = "45-64")) 
link.may_F <- within(link.may_F, age.gr <- relevel(age.gr, ref = "45-64")) 

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

# 2.Y. Disability Count - progressive vs. catastrophic
# -----------------------------------------------------

link.may_M <- within(link.may_M, CatPro <- relevel(CatPro, ref = "progressive")) 
link.may_F <- within(link.may_F, CatPro <- relevel(CatPro, ref = "progressive"))

# 2.Z Onset of severity
# ---------------------

link.may_M <- within(link.may_M, EntryGrave13_cat <- relevel(EntryGrave13_cat, ref = "no severe disability")) 
link.may_F <- within(link.may_F, EntryGrave13_cat <- relevel(EntryGrave13_cat, ref = "no severe disability")) 

# ----------------- #
# Duration variable #   !!! Duration variable cannot be used due to the categorical nature of "Edadinicio_cuidado"
# ----------------- #
link.may_F <- link.may_F %>% mutate(dur_dis = Edadinicio_cuidado - DISCA13_AGE)
link.may_M <- link.may_M %>% mutate(dur_dis = Edadinicio_cuidado - DISCA13_AGE)
summary(link.may_F$dur_dis)
summary(link.may_M$dur_dis)
hist(link.may_M$dur_dis, breaks=50)

# Probably better to be a categorical variable
link.may_F <- link.may_F %>% mutate(dur_dis_cat = as.factor(ifelse(dur_dis<(-1), "$> 1$ years before", 
                                                         ifelse(dur_dis<1, "same time",
                                                                ifelse(dur_dis<5, "$< 3$ years after", "$> 3$ years after")))))

table(link.may_F$dur_dis_cat)


# Probably better to be a categorical variable
link.may_M <- link.may_M %>% mutate(dur_dis_cat = as.factor(ifelse(dur_dis<(-1), "$> 1$ years before", 
                                                         ifelse(dur_dis<1, "same time",
                                                                ifelse(dur_dis<5, "$< 3$ years after", "$> 3$ years after")))))

table(link.may_M$dur_dis_cat)

# relevel the categories
# ----------------------

link.may_F <- within(link.may_F, dur_dis_cat <- relevel(dur_dis_cat, ref = "$> 1$ years before")) 
link.may_M <- within(link.may_M, dur_dis_cat <- relevel(dur_dis_cat, ref = "$> 1$ years before"))


# relevel the categories
# ----------------------

link.may_F <- within(link.may_F, EntryGrave13_cat <- relevel(as.factor(EntryGrave13_cat), ref = "85+")) 
link.may_M <- within(link.may_M, EntryGrave13_cat <- relevel(as.factor(EntryGrave13_cat), ref = "85+"))



# missing value exploration
# -------------------------

library(Amelia)

# Make data set with variables for the analysis (No more missings)
training.data_M <- link.may_M %>% dplyr::select(EDAD, CoMorb, CatPro, EntryGrave13_cat, ADL, civil, income, education, Accident12, DailyAct)

missmap(training.data_M, main = "Missing values vs observed")

training.data_F <- link.may_F %>% dplyr::select(EDAD, CoMorb, CatPro, EntryGrave13_cat, ADL, civil, income, education, Accident12, DailyAct)

missmap(training.data_F, main = "Missing values vs observed")


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
                        event=event) ~ CatPro + EntryGrave13_cat + CoMorb + ADL + Accident12 + DailyAct + education + 
                                       CP + civil,
                   data=link.may_M)

summary(Cox_M_1)


# 2. step - add severity and comorbidity


# females
Cox_F_1 <- coxph(Surv(time=EDAD,
                        time2 = age.ex,
                        event=event) ~ CatPro + EntryGrave13_cat + CoMorb + ADL + Accident12 + DailyAct + education + 
                                       CP + civil,
                   data=link.may_F)

summary(Cox_F_1)


# Put the output together
stargazer(Cox_F_1,Cox_M_1, title="Cox PH Regression Models",no.space=F,
          ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative risk of dying"),
          covariate.labels=c("catastrophic disability", "Onset sev. dis. $>$ 85", "Onset sev. dis. $<$ 65", "Onset sev. dis. 65-75",
                             "Onset sev. dis. 75-85", "Suffers from multiple diseases", "First Dis = ADL", "Had accident in last 12 mo.",
                             "No daily activity", "Incomplete Ed." ,"Primary Ed.", "Formal caregiver",
                             "Single/Div","Widowed"),
          single.row=FALSE, apply.coef = exp)

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

