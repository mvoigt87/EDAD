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
load(file = 'datasets/030_linkmay_M_50ADL.RData')
load(file = 'datasets/030_linkmay_F_50ADL.RData')


#### 1. Change categories and referecnces


table(link.may_M$education)
table(link.may_F$education)

# make them factors
link.may_M$education <- as.factor(link.may_M$education)
link.may_F$education <- as.factor(link.may_F$education)
# change the reference category
link.may_M <- within(link.may_M, education <- relevel(education, ref = "Secondary Educ or higher")) 
link.may_F <- within(link.may_F, education <- relevel(education, ref = "Secondary Educ or higher")) 


table(link.may_M$civil)
table(link.may_F$civil)

# make them factors
link.may_M$civil <- as.factor(link.may_M$civil)
link.may_F$civil <- as.factor(link.may_F$civil)
# change the reference category
link.may_M <- within(link.may_M, civil <- relevel(civil, ref = "Married")) 
link.may_F <- within(link.may_F, civil <- relevel(civil, ref = "Married")) 


table(link.may_M$work)
table(link.may_F$work)

# make them factors
link.may_M$work <- as.factor(link.may_M$work)
link.may_F$work <- as.factor(link.may_F$work)
# change the reference category
link.may_M <- within(link.may_M, work <- relevel(work, ref = "Self-employed")) 
link.may_F <- within(link.may_F, work <- relevel(work, ref = "self-employed")) 


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

## Males - time since EDAD

Cox.CFP.a <- coxph(Surv(time=t.salida,
                        event=event) ~ cluster2 + EDAD, 
                   data=subset(link.may_M))

Cox.CFP.a

Cox.CFP.a2 <- coxph(Surv(time=t.salida,
                        event=event) ~ cluster2 + age.gr, 
                   data=subset(link.may_M))

summary(Cox.CFP.a2)

## Females - Time since EDAD

Cox.CFP.b <- coxph(Surv(time=t.salida,
                        event=event) ~ cluster2 + EDAD, 
                   data=subset(link.may_F))

Cox.CFP.b


Cox.CFP.b2 <- coxph(Surv(time=t.salida,
                        event=event) ~ cluster2 + age.gr, 
                   data=subset(link.may_F))

summary(Cox.CFP.b2)


# Plus SES variables Time since EDDA

# males
Cox.CFP.e <- coxph(Surv(time=t.salida,
                        event=event) ~ cluster2 + age.gr + education + CP + civil,
                   data=subset(link.may_M))

summary(Cox.CFP.e)

# females
Cox.CFP.f <- coxph(Surv(time=t.salida,
                         event=event) ~ cluster2 + age.gr + education + CP + civil,
                   data=subset(link.may_F))

summary(Cox.CFP.f)





#### Age at onset of Dependency

## Males

Cox.CFP.a <- coxph(Surv(time=EDAD,
                        time2 = age.ex,
                        event=event) ~ cluster2, 
                   data=subset(link.may_M))
summary(Cox.CFP.a)


## Females

Cox.CFP.b <- coxph(Surv(time=EDAD,
                        time2 = age.ex,
                        event=event) ~ cluster2, 
                   data=subset(link.may_F))
summary(Cox.CFP.b)


# Plus SES variables Age since onset

# males
Cox.CFP.e <- coxph(Surv(time=EDAD,
                        time2 = age.ex,
                        event=event) ~  cluster2 + education + CP + civil,
                   data=subset(link.may_M))

summary(Cox.CFP.e)

# females
Cox.CFP.f <- coxph(Surv(time=EDAD,
                        time2 = age.ex,
                        event=event) ~ cluster2 + education + CP + civil,
                   data=subset(link.may_F))

summary(Cox.CFP.f)
















# Additional variables

# males
Cox.CFP.e <- coxph(Surv(time=t.entrada,
                        time2=t.salida,
                        event=event) ~ cluster4 + Education + Income + EDAD,
                   data=subset(link.may_M))

summary(Cox.CFP.e)

# females
Cox.CFP.f <- coxph(Surv(time=t.entrada,
                        time2=t.salida,
                        event=event) ~ cluster3 + Education + Income + EDAD,
                   data=subset(link.may_F))

summary(Cox.CFP.f)




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

