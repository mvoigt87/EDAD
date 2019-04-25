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

# new data
load(file='010_mayorDEP_M.link.RData')
load(file='010_mayorDEP_F.link.RData')


#### ---------------------------- ####
#### KME for exploratory overview ####
#### ---------------------------- ####

# the three severity groups

mfit.1a <- survfit(coxph(Surv(time=EDAD,
                              time2=age.ex,
                              event=event) ~ 1, data=subset(link.may50,SEVEREDIS =="mild")), 
                   data=subset(link.may50,SEVEREDIS =="mild"),type="kaplan-meier")

mfit.1b <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,SEVEREDIS =="moderate")), 
                   data=subset(link.may50,SEVEREDIS =="moderate"),type="kaplan-meier")

mfit.1c <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,SEVEREDIS =="severe")), 
                   data=subset(link.may50,SEVEREDIS =="severe"),type="kaplan-meier")



KM.LIM.a <- tidy(mfit.1a) %>% dplyr::select(estimate, time) %>% mutate(SEV = "mild")
help.KM1 <- data.frame(1,50,"mild")
names(help.KM1) <- c("estimate", "time", "SEV")
KM.LIM.a <- union(KM.LIM.a, help.KM1)

KM.LIM.b <- tidy(mfit.1b) %>% dplyr::select(estimate, time) %>% mutate(SEV = "moderate")
help.KM2 <- data.frame(1,50,"moderate")
names(help.KM2) <- c("estimate", "time", "SEV")
KM.LIM.b <- union(KM.LIM.b, help.KM2)

KM.LIM.c <- tidy(mfit.1c) %>% dplyr::select(estimate, time) %>% mutate(SEV = "severe")
help.KM3 <- data.frame(1,50,"severe")
names(help.KM3) <- c("estimate", "time", "SEV")
KM.LIM.c <- union(KM.LIM.c, help.KM3)

### ADD a starting value!!!

KM.LIM <- union(KM.LIM.a, KM.LIM.b) %>% union(KM.LIM.c)


km.1 <- KM.LIM %>% dplyr::filter(time >= 50) %>% 
  ggplot() +
  geom_step(mapping=aes(x=time, y=estimate, color=SEV)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("#0072B2", "#D55E00", "#009E73"), name="")     +
#  Optional Grey Scales for the paper
# scale_colour_manual(values = c("#000000", "grey50", "grey75"), name="")     +
  theme_bw()

km.1 +  theme(legend.position = c(0.85, 0.80)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold")) +
       scale_shape_discrete(guide=FALSE)



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
                      event=event) ~ DISCA13_AGEGR,
                 data=link.may_M)

summary(Cox_M_1)

# Add co-morbidity information

Cox_M_2 <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGEGR + SEVEREDIS  + CoMorb,
                 data=link.may_M)

summary(Cox_M_2)


Cox_M_F <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGEGR + SEVEREDIS  + CoMorb + Accident12 + DailyAct + income + education + civil,
                   data=link.may_M)

summary(Cox_M_F)

# + education + civil + CV


# 2. step - add severity and comorbidity


# females


Cox_F_1 <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGEGR,
                 data=link.may_F)

summary(Cox_F_1)

# Add co-morbidity information

Cox_F_2 <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGEGR + SEVEREDIS,
                 data=link.may_F)

summary(Cox_F_2)


# And add socio-demographics in a final step

Cox_F_F <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGEGR + SEVEREDIS + CoMorb + Accident12 + DailyAct +  income + education + civil,
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
                           event=event) ~ DISCA13_AGEGR + SEVEREDIS + CoMorb + Accident12 + DailyAct, data = link.may_F,
                          dist = "gompertz")
GOMP_F
# survival curve
plot(GOMP_F, xlim=c(50,100), main="Estimated Gompertz Survival curve vs. KME", xlab="Age",
     ylab="Survival Probability", cex=1.5)
legend("topright",legend=c("KME","Gompertz Curve"), 
       lty=c(1,1),col=c("black","red"), cex=1)

# hazard plot # doesn´t work for left-truncated data
# plot(GOMP_F, xlim=c(50,100),ylim=c(-0.1,1),type = "hazard")

GOMP_M <- flexsurvreg(Surv(time=EDAD,
                          time2=age.ex,
                          event=event) ~ DISCA13_AGEGR + SEVEREDIS + CoMorb + Accident12 + DailyAct, data = link.may_M,
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

