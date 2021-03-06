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
library(parfm)

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

# 1) KMEs

# connect the male and female data sets for shared variables
link.may50 <- union(link.may_F, link.may_M)

#### ---------------------------- ####
#### KME for exploratory overview ####
#### ---------------------------- ####

# the three severity groups

mfit.1a <- survfit(coxph(Surv(time=EDAD,
                              time2=age.ex,
                              event=event) ~ 1, data=subset(link.may50,DIS_GRP =="mild-gradual")), 
                   data=subset(link.may50,DIS_GRP =="mild-gradual"),type="kaplan-meier")

mfit.1b <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,DIS_GRP =="accelerated")), 
                   data=subset(link.may50,DIS_GRP =="accelerated"),type="kaplan-meier")

mfit.1c <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,DIS_GRP =="catastrophic")), 
                   data=subset(link.may50,DIS_GRP =="catastrophic"),type="kaplan-meier")



KM.LIM.a <- tidy(mfit.1a) %>% dplyr::select(estimate, time) %>% mutate(SEV = "mild-gradual")
help.KM1 <- data.frame(1,50,"mild-gradual")
names(help.KM1) <- c("estimate", "time", "SEV")
KM.LIM.a <- union(KM.LIM.a, help.KM1)

KM.LIM.b <- tidy(mfit.1b) %>% dplyr::select(estimate, time) %>% mutate(SEV = "accelerated")
help.KM2 <- data.frame(1,50,"accelerated")
names(help.KM2) <- c("estimate", "time", "SEV")
KM.LIM.b <- union(KM.LIM.b, help.KM2)

KM.LIM.c <- tidy(mfit.1c) %>% dplyr::select(estimate, time) %>% mutate(SEV = "catastrophic")
help.KM3 <- data.frame(1,50,"catastrophic")
names(help.KM3) <- c("estimate", "time", "SEV")
KM.LIM.c <- union(KM.LIM.c, help.KM3)

### ADD a starting value!!!

KM.LIM <- union(KM.LIM.a, KM.LIM.b) %>% union(KM.LIM.c)


km.1 <- KM.LIM %>% dplyr::filter(time >= 65) %>% 
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

### now the same graph by sex (facetting)
### -------------------------------------

# females
# -------

mfit.2a <- survfit(coxph(Surv(time=EDAD,
                              time2=age.ex,
                              event=event) ~ 1, data=subset(link.may50,SEVEREDIS =="mild" & SEXO=="Female")), 
                   data=subset(link.may50,SEVEREDIS =="mild" & SEXO=="Female"),type="kaplan-meier")

mfit.2b <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,SEVEREDIS =="moderate" & SEXO=="Female")), 
                   data=subset(link.may50,SEVEREDIS =="moderate" & SEXO=="Female"),type="kaplan-meier")

mfit.2c <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,SEVEREDIS =="severe" & SEXO=="Female")), 
                   data=subset(link.may50,SEVEREDIS =="severe" & SEXO=="Female"),type="kaplan-meier")


KM.LIM.a <- tidy(mfit.2a) %>% dplyr::select(estimate, time) %>% mutate(SEV = "mild") %>% mutate(sex="female")
help.KM1 <- data.frame(1,50,"mild", "female")
names(help.KM1) <- c("estimate", "time", "SEV", "sex")
KM.LIM.a <- union(KM.LIM.a, help.KM1)

KM.LIM.b <- tidy(mfit.2b) %>% dplyr::select(estimate, time) %>% mutate(SEV = "moderate") %>% mutate(sex="female")
help.KM2 <- data.frame(1,50,"moderate","female")
names(help.KM2) <- c("estimate", "time", "SEV","sex")
KM.LIM.b <- union(KM.LIM.b, help.KM2)

KM.LIM.c <- tidy(mfit.2c) %>% dplyr::select(estimate, time) %>% mutate(SEV = "severe") %>% mutate(sex="female")
help.KM3 <- data.frame(1,50,"severe","female")
names(help.KM3) <- c("estimate", "time", "SEV", "sex")
KM.LIM.c <- union(KM.LIM.c, help.KM3)

KM.LIM_F <- union(KM.LIM.a, KM.LIM.b) %>% union(KM.LIM.c)

# males
# -----

mfit.3a <- survfit(coxph(Surv(time=EDAD,
                              time2=age.ex,
                              event=event) ~ 1, data=subset(link.may50,SEVEREDIS =="mild" & SEXO=="Male")), 
                   data=subset(link.may50,SEVEREDIS =="mild" & SEXO=="Male"),type="kaplan-meier")

mfit.3b <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,SEVEREDIS =="moderate" & SEXO=="Male")), 
                   data=subset(link.may50,SEVEREDIS =="moderate" & SEXO=="Male"),type="kaplan-meier")

mfit.3c <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,SEVEREDIS =="severe" & SEXO=="Male")), 
                   data=subset(link.may50,SEVEREDIS =="severe" & SEXO=="Male"),type="kaplan-meier")

KM.LIM.a <- tidy(mfit.3a) %>% dplyr::select(estimate, time) %>% mutate(SEV = "mild") %>% mutate(sex="male")
help.KM1 <- data.frame(1,50,"mild", "male")
names(help.KM1) <- c("estimate", "time", "SEV", "sex")
KM.LIM.a <- union(KM.LIM.a, help.KM1)

KM.LIM.b <- tidy(mfit.3b) %>% dplyr::select(estimate, time) %>% mutate(SEV = "moderate") %>% mutate(sex="male")
help.KM2 <- data.frame(1,50,"moderate","male")
names(help.KM2) <- c("estimate", "time", "SEV","sex")
KM.LIM.b <- union(KM.LIM.b, help.KM2)

KM.LIM.c <- tidy(mfit.3c) %>% dplyr::select(estimate, time) %>% mutate(SEV = "severe") %>% mutate(sex="male")
help.KM3 <- data.frame(1,50,"severe","male")
names(help.KM3) <- c("estimate", "time", "SEV", "sex")
KM.LIM.c <- union(KM.LIM.c, help.KM3)

KM.LIM_M <- union(KM.LIM.a, KM.LIM.b) %>% union(KM.LIM.c)

# Put them together
KM.LIM_MF <- union(KM.LIM_F, KM.LIM_M)

km.2 <- KM.LIM_MF %>% dplyr::filter(time >= 50) %>% 
  ggplot() +
  geom_step(mapping=aes(x=time, y=estimate, color=SEV)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  # scale_colour_manual(values = c("#0072B2", "#D55E00", "#009E73"), name="")     +
  #  Optional Grey Scales for the paper
  scale_colour_manual(values = c("#000000", "grey50", "grey75"), name="")     +
  facet_grid(.~sex) +
  theme_bw()

km.2 +  theme(legend.position = c(0.85, 0.80)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold")) +
  scale_shape_discrete(guide=FALSE)



###############################
### Exploratory Cox Regression
###############################

## Males
## -----

  # # exit time as time in years (month/12+year)
  # link.may_M <- link.may_M %>% mutate(t.salida = (a_salida+(m_salida/12)) - 2006.99)
  # hist(link.may_M$t.salida)
  # 
  # # entry time (month/12+year) - onset of disability
  # link.may_M <- link.may_M %>% mutate(t.entrada.dis =  2006.99 - (EDAD-DISCA13_AGE)) # same story with inicio Disca44
  # link.may_M <- link.may_M %>% mutate(t.salida.dis = (a_salida+(m_salida/12)))
  # 
  # # entry time (month/12+year) - onset of dependency
  # link.may_M <- link.may_M %>% mutate(t.entrada.dep =  2006.99 - (EDAD-Edadinicio_cuidado)) 



## Females
## -------

  # # exit time (month/12+year)
  # link.may_F <- link.may_F %>% mutate(t.salida = (a_salida+(m_salida/12)) - 2006.99)
  # hist(link.may_F$t.salida)
  # 
  # 
  # # entry time (month/12+year) - onset of disability
  # link.may_F <- link.may_F %>% mutate(t.entrada.dis =  2006.99 - (EDAD-DISCA13_AGE)) # same story with inicio Disca44
  # link.may_F <- link.may_F %>% mutate(t.salida.dis = (a_salida+(m_salida/12)))
  # 
  # # entry time (month/12+year) - onset of dependency
  # link.may_F <- link.may_F %>% mutate(t.entrada.dep =  2006.99 - (EDAD-Edadinicio_cuidado)) 


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
                      event=event) ~ DISCA13_AGEGR + SEVEREDIS,
                 data=link.may_M)

summary(Cox_M_2)



Cox_M_3 <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGEGR + SEVEREDIS + catastrophic,
                 data=link.may_M)

summary(Cox_M_3)


Cox_M_F <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGEGR + SEVEREDIS + catastrophic + CoMorb + Accident12 + DailyAct + income + education + civil + kin_close,
                   data=link.may_M)

summary(Cox_M_F)

### Best fitting model (method: trial and error)

Cox_M_BF <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGEGR + SEVEREDIS2 + DailyAct + civil + kin_close,
                 data=link.may_M)

summary(Cox_M_BF)

# + education + civil + CV

############# --------------------------------- #######################
# BY GROUPS - Females

Cox_F_BF_A <- coxph(Surv(time=EDAD,
                         time2 = age.ex,
                         event=event) ~ DailyAct + civil + kin_close + income + education + CoMorb + Accident12,
                    data=subset(link.may_F, SEVEREDIS2=="mild disability"))

summary(Cox_F_BF_A)

Cox_F_BF_B <- coxph(Surv(time=EDAD,
                         time2 = age.ex,
                         event=event) ~ DailyAct + civil + kin_close + income + education + CoMorb + Accident12,
                    data=subset(link.may_F, SEVEREDIS2=="severe disability"))

summary(Cox_F_BF_B)


# BY GROUPS - Males

Cox_M_BF_A <- coxph(Surv(time=EDAD,
                       time2 = age.ex,
                       event=event) ~ DailyAct + civil + kin_close + income + education + CoMorb + Accident12,
                  data=subset(link.may_M, SEVEREDIS2=="mild disability"))

summary(Cox_M_BF_A)

Cox_M_BF_B <- coxph(Surv(time=EDAD,
                         time2 = age.ex,
                         event=event) ~ DailyAct + civil + kin_close + income + education + CoMorb + Accident12,
                    data=subset(link.may_M, SEVEREDIS2=="severe disability"))

summary(Cox_M_BF_B)


############# --------------------------------- #######################

# 2. step - add severity and comorbidity


# females


Cox_F_1 <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGEGR,
                 data=link.may_F)

summary(Cox_F_1)

# Add Severity

Cox_F_2 <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGEGR + SEVEREDIS,
                 data=link.may_F)

summary(Cox_F_2)

# Add Onset

Cox_F_3 <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGEGR + SEVEREDIS + catastrophic,
                 data=link.may_F)

summary(Cox_F_3)

# And add socio-demographics in a final step

Cox_F_F <- coxph(Surv(time=EDAD,
                      time2 = age.ex,
                      event=event) ~ DISCA13_AGEGR + SEVEREDIS + catastrophic + CoMorb + Accident12 + DailyAct + income + education + civil + kin_close,
                 data=link.may_F)

summary(Cox_F_F)

# + education + civil + CV

### Best fitting model (method: trial and error)

Cox_F_BF <- coxph(Surv(time=EDAD,
                       time2 = age.ex,
                       event=event) ~ DISCA13_AGEGR + SEVEREDIS2 + DailyAct + civil + CoMorb,
                  data=link.may_F)

summary(Cox_F_BF)
AIC(Cox_F_BF)

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
                           event=event) ~ DISCA13_AGEGR + SEVEREDIS2 + DailyAct + civil + CoMorb, data = link.may_F,
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
                          event=event) ~ DISCA13_AGEGR + SEVEREDIS2 + DailyAct + civil + kin_close, data = link.may_M,
                     dist = "gompertz")
GOMP_M

# survival curve
plot(GOMP_M, xlim=c(50,100))
legend("topright",legend=c("KME","Gompertz Curve"), 
       lty=c(1,1),col=c("black","red"), cex=0.75)

# By group:
GOMP_M_A <- flexsurvreg(Surv(time=EDAD,
                           time2=age.ex,
                           event=event) ~ DailyAct + civil + kin_close, data = subset(link.may_M,SEVEREDIS2=="mild disability"),
                      dist = "gompertz")
GOMP_M_A

GOMP_M_B <- flexsurvreg(Surv(time=EDAD,
                             time2=age.ex,
                             event=event) ~ DailyAct + civil + kin_close, data = subset(link.may_M,SEVEREDIS2=="severe disability"),
                        dist = "gompertz")
GOMP_M_B


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

###########################################################################################################################


#### Gompertz - Gamma Frailty Model

# First step - every individual needs an individual identifier


###
# males
###

link.may_M$ID <- seq.int(nrow(link.may_M))

# run model
GGF_M1 <- parfm(Surv(time=EDAD,
                     time2=age.ex,
                     event=event) ~ DISCA13_AGEGR + SEVEREDIS2 + DailyAct + civil + kin_close, cluster = "ID",
          data = link.may_M, dist = "gompertz", frailty = "gamma", method = "Nelder-Mead")

###
# females
###

link.may_F$ID <- seq.int(nrow(link.may_F))

# run model
GGF_F1 <- parfm(Surv(time=EDAD,
                     time2=age.ex,
                     event=event) ~ DISCA13_AGEGR + SEVEREDIS2 + DailyAct + civil + kin_close, cluster = "ID",
                data = link.may_F, dist = "gompertz", frailty = "gamma", method = "Nelder-Mead")
