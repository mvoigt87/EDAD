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

# A 12 data
load(file = '030_linkmay_M_12A.RData')
load(file = '030_linkmay_F_12A.RData')

link.may_M2 <- link.may_M
link.may_F2 <- link.may_F

# Disca 44 file
load(file = 'datasets/030_linkmay_M.RData')
load(file = 'datasets/030_linkmay_F.RData')





###############################
### Exploratory Cox Regression
###############################

# Preliminary step: Change Reference Category and Category names
link.may$group.d.d <- as.factor(link.may$group.d.d)
link.may <- within(link.may, group.d.d <- relevel(group.d.d, ref = "gradual"))  
link.may$Sex <- as.factor(link.may$Sex)
link.may <- within(link.may, Sex <- relevel(Sex, ref = "Female"))  
link.may <- within(link.may, Education <- relevel(Education, ref = "High"))  
link.may <- within(link.may, Income <- relevel(Income, ref = "625+ eur"))  




# exit time (month/12+year)
link.may_M <- link.may_M %>% mutate(t.salida = a_salida+(m_salida/12))
#hist(link.may_F$t.salida)
# entry time (month/12+year) - onset of disability
link.may_M <- link.may_M %>% mutate(t.entrada = 2006.99 - (EDAD-Edadinicio_cuidado)) # same story with inicio Disca44

# exit time (month/12+year)
link.may_F <- link.may_F %>% mutate(t.salida = a_salida+(m_salida/12))
#hist(link.may_F$t.salida)
# entry time (month/12+year) - onset of disability
link.may_F <- link.may_F %>% mutate(t.entrada = 2006.99 - (EDAD-Edadinicio_cuidado)) # same story with inicio Disca44


# males
Cox.CFP.a <- coxph(Surv(time=t.entrada,
                        time2=t.salida,
                        event=event) ~ cluster2, 
                   data=subset(link.may_M))

Cox.CFP.a

# females
Cox.CFP.b <- coxph(Surv(time=t.entrada,
                        time2=t.salida,
                        event=event) ~ cluster2, 
                   data=subset(link.may_F))

Cox.CFP.b





# Plus SES variables

# males
Cox.CFP.e <- coxph(Surv(time=t.entrada,
                        time2=t.salida,
                        event=event) ~ cluster2 + Education + Income + EDAD,
                   data=subset(link.may_M))

Cox.CFP.e

# females
Cox.CFP.f <- coxph(Surv(time=t.entrada,
                        time2=t.salida,
                        event=event) ~ cluster2 + Education + Income + EDAD,
                   data=subset(link.may_F))

Cox.CFP.f






# BIGGER CLUSTERS

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

