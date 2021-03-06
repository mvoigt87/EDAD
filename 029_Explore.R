### -------------------- ###
### Exploratory Analysis ###
### -------------------- ###


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
library(gmodels)

dir()
setwd("C:/Users/y4956294S/Documents/LONGPOP/Subproject 2 - SE differences in transition to dependency/R code/EDAD")

# 0.2 data
# --------------------------------------------
# new data
load(file='010_mayor50.link.RData')

# Age distributions (at survey year and at death) general #
# ------------------------------------------------------- #
summary(link.may50$EDAD)
hist(link.may50$EDAD, breaks = 60)
# make age groups
link.may50 <- link.may50 %>% mutate(AGEGR = ifelse(EDAD<61, "50-60", ifelse(EDAD<71, "60-70",
                                                    ifelse(EDAD<81, "70-80", ifelse(EDAD<91, "80-90","90+")))))             

table(link.may50$AGEGR)
round(prop.table(table(link.may50$AGEGR)),3)


# Age at disability onset
# -----------------------
hist(link.may50$DISCA13_AGE)
summary(link.may50$DISCA13_AGE)

link.may50 <- link.may50 %>% mutate(DISCA13_AGEGR = ifelse(DISCA13_AGE<61, "50-60", ifelse(DISCA13_AGE<71, "60-70",
                                                                                           ifelse(DISCA13_AGE<81, "70-80", "80+"))))

table(link.may50$DISCA13_AGEGR)

## 3. Age at death
## ---------------

# make age groups
link.may50 <- link.may50 %>% mutate(AGE_X_GR = ifelse(age.ex<61, "50-60", ifelse(age.ex<71, "60-70",
                                                                                 ifelse(age.ex<81, "70-80", ifelse(age.ex<91, "80-90","90+")))))             

table(link.may50$AGE_X_GR)
round(prop.table(table(link.may50$AGE_X_GR)),3)


SD <- link.may50 %>% mutate(event = as.factor(ifelse(event==1, "muerto", "vive"))) %>% 
  ggplot(aes(x=AGEGR, fill=event)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(name = "Relative Frequency", labels = scales::percent) +
  scale_x_discrete(name = "Age Groups") +
  scale_fill_manual(name = "", values=c("#0072B2", "#D55E00")) +
  theme_bw()

SD + theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"), strip.text.y = element_text(size=12, face="bold"))

# By Sex
# ------
hist.muerte <- link.may50 %>% mutate(event = as.factor(ifelse(event==1, "muerto", "vive"))) %>% 
  ggplot(aes(age.ex, fill = event))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(name = "", values=c("#0072B2", "#D55E00")) +
  theme_bw() +
  facet_grid(.~SEXO)
hist.muerte + theme(axis.text=element_text(size=12),
                   axis.title=element_text(size=14,face="bold"), strip.text.y = element_text(size=12, face="bold"))


# females
table(link.may50$AGE_X_GR[link.may50$SEXO=="Mujer" & link.may50$event==1])
round(prop.table(table(link.may50$AGE_X_GR[link.may50$SEXO=="Mujer" & link.may50$event==1])),3)
table(link.may50$AGE_X_GR[link.may50$SEXO=="Mujer" & link.may50$event==0])
round(prop.table(table(link.may50$AGE_X_GR[link.may50$SEXO=="Mujer" & link.may50$event==0])),3)

# try gmodels CrossTable command
CrossTable(link.may50$AGE_X_GR, link.may50$event, prop.t=TRUE, prop.r=TRUE, prop.c=F)


#### 4. Disability Groups (by sex and age group) !!!! New classification

# 2 year difference = accelerated
link.may50 <- link.may50 %>% mutate(DIS_GRP = ifelse(dis_golpe<=2 & DIFF_2>=2, "accelerated", 
                                                     ifelse(dis_golpe>2,"catastrophic","mild-gradual")))
# check age groups
summary(link.may50$EDAD[link.may50$DIS_GRP=="accelerated"])
summary(link.may50$EDAD[link.may50$DIS_GRP=="catastrophic"])

CrossTable(link.may50$AGEGR, link.may50$DIS_GRP, prop.t=TRUE, prop.r=TRUE, prop.c=F)


# 1 year difference = accelerated
link.may50 <- link.may50 %>% mutate(DIS_GRP2 = ifelse(dis_golpe<=2 & DIFF_2>=1, "accelerated", 
                                                     ifelse(dis_golpe>2,"catastrophic","mild-gradual")))

# check age groups
summary(link.may50$EDAD[link.may50$DIS_GRP2=="accelerated"])
summary(link.may50$EDAD[link.may50$DIS_GRP2=="catastrophic"])

CrossTable(link.may50$AGEGR, link.may50$DIS_GRP2, prop.t=TRUE, prop.r=TRUE, prop.c=F)

### Check mean/medium duration in disability until 2008/2018/death by group

# 1. Average time spent in disability by group and event #
# ------------------------------------------------------ #

# create a duration variable (EDAD)
link.may50 <- link.may50 %>% mutate(DurDisEDAD = EDAD-DISCA13_AGE) %>% mutate(DurDisEDAD=ifelse(DurDisEDAD<0,0,DurDisEDAD)) %>% 
  # and the same for the exit (death/censoring)
mutate(DurDisEXIT = age.ex - DISCA13_AGE) %>% filter(DurDisEXIT>=0)

summary(link.may50$DurDisEDAD) # negative numbers are strange
summary(link.may50$DurDisEXIT)

# table(link.may50$DurDisEDAD[link.may50$DurDisEDAD<0]) # 150 with exactly -1 year (interviews in 2007?)
# table(link.may50$DurDisEXIT[link.may50$DurDisEXIT<0]) # 1 case

# histogram for average duration and event occurrence by disability groups (CHANGE BETWEEN EDAD AND EXIT for different durations)

hist_dur <- link.may50 %>% mutate(event = as.factor(ifelse(event==1, "muerto", "vive"))) %>% 
  ggplot(aes(DurDisEDAD, fill = event))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(name = "", values=c("#0072B2", "#D55E00")) +
  theme_bw() +
  facet_grid(.~ DIS_GRP2)
hist_dur + theme(axis.text=element_text(size=12),
                    axis.title=element_text(size=14,face="bold"), strip.text.y = element_text(size=12, face="bold"))

# summary stats
summary(link.may50$DurDisEDAD[link.may50$DIS_GRP2=="mild-gradual"])
summary(link.may50$DurDisEXIT[link.may50$DIS_GRP2=="mild-gradual"])

summary(link.may50$DurDisEDAD[link.may50$DIS_GRP2=="accelerated"])
summary(link.may50$DurDisEXIT[link.may50$DIS_GRP2=="accelerated"])

summary(link.may50$DurDisEDAD[link.may50$DIS_GRP2=="catastrophic"])
summary(link.may50$DurDisEXIT[link.may50$DIS_GRP2=="catastrophic"])

# 2. SURVIVAL - by the three severity groups #
# --------------------------------------- #

### Grouping 1

mfit.1a <- survfit(coxph(Surv(time=EDAD,
                              time2=age.ex,
                              event=event) ~ 1, data=subset(link.may50,DIS_GRP2 =="mild-gradual" & age.ex>64)), 
                   data=subset(link.may50,DIS_GRP =="mild-gradual" & age.ex>64),type="kaplan-meier")

mfit.1b <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,DIS_GRP2 =="accelerated" & age.ex>64)), 
                   data=subset(link.may50,DIS_GRP =="accelerated" & age.ex>64),type="kaplan-meier")

mfit.1c <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,DIS_GRP2 =="catastrophic" & age.ex>64)), 
                   data=subset(link.may50,DIS_GRP =="catastrophic" & age.ex>64),type="kaplan-meier")



KM.LIM.a <- tidy(mfit.1a) %>% dplyr::select(estimate, time) %>% mutate(DG = "mild-gradual")
help.KM1 <- data.frame(1,65,"mild-gradual")
names(help.KM1) <- c("estimate", "time", "DG")
KM.LIM.a <- union(KM.LIM.a, help.KM1)

KM.LIM.b <- tidy(mfit.1b) %>% dplyr::select(estimate, time) %>% mutate(DG = "accelerated")
help.KM2 <- data.frame(1,65,"accelerated")
names(help.KM2) <- c("estimate", "time", "DG")
KM.LIM.b <- union(KM.LIM.b, help.KM2)

KM.LIM.c <- tidy(mfit.1c) %>% dplyr::select(estimate, time) %>% mutate(DG = "catastrophic")
help.KM3 <- data.frame(1,65,"catastrophic")
names(help.KM3) <- c("estimate", "time", "DG")
KM.LIM.c <- union(KM.LIM.c, help.KM3)

### ADD a starting value!!!

KM.LIM <- union(KM.LIM.a, KM.LIM.b) %>% union(KM.LIM.c)


km.1 <- KM.LIM %>% dplyr::filter(time >= 65) %>% 
  ggplot() +
  geom_step(mapping=aes(x=time, y=estimate, color=DG)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("#0072B2", "#D55E00", "#009E73"), name="")     +
  #  Optional Grey Scales for the paper
  # scale_colour_manual(values = c("#000000", "grey50", "grey75"), name="")     +
  theme_bw()

km.1 +  theme(legend.position = c(0.85, 0.80)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold")) +
  scale_shape_discrete(guide=FALSE)





###########################
### Now the same by sex ###
###########################





mfit.1a <- survfit(coxph(Surv(time=EDAD,
                              time2=age.ex,
                              event=event) ~ 1, data=subset(link.may50,DIS_GRP2 =="mild-gradual" & age.ex>64 & SEXO=="Mujer")), 
                   data=subset(link.may50,DIS_GRP =="mild-gradual" & age.ex>64 & SEXO=="Mujer"),type="kaplan-meier")

mfit.1b <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,DIS_GRP2 =="accelerated" & age.ex>64 & SEXO=="Mujer")), 
                   data=subset(link.may50,DIS_GRP =="accelerated" & age.ex>64 & SEXO=="Mujer"),type="kaplan-meier")

mfit.1c <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,DIS_GRP2 =="catastrophic" & age.ex>64 & SEXO=="Mujer")), 
                   data=subset(link.may50,DIS_GRP =="catastrophic" & age.ex>64 & SEXO=="Mujer"),type="kaplan-meier")

# varones
# -------
mfit.2a <- survfit(coxph(Surv(time=EDAD,
                              time2=age.ex,
                              event=event) ~ 1, data=subset(link.may50,DIS_GRP2 =="mild-gradual" & age.ex>64 & SEXO=="Varón")), 
                   data=subset(link.may50,DIS_GRP =="mild-gradual" & age.ex>64 & SEXO=="Varón"),type="kaplan-meier")

mfit.2b <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,DIS_GRP2 =="accelerated" & age.ex>64 & SEXO=="Varón")), 
                   data=subset(link.may50,DIS_GRP =="accelerated" & age.ex>64 & SEXO=="Varón"),type="kaplan-meier")

mfit.2c <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may50,DIS_GRP2 =="catastrophic" & age.ex>64 & SEXO=="Varón")), 
                   data=subset(link.may50,DIS_GRP =="catastrophic" & age.ex>64 & SEXO=="Varón"),type="kaplan-meier")

# females
KM.LIM.a <- tidy(mfit.1a) %>% dplyr::select(estimate, time) %>% mutate(DG = "mild-gradual") %>% mutate(sexo="mujer")
help.KM1 <- data.frame(1,65,"mild-gradual","mujer")
names(help.KM1) <- c("estimate", "time", "DG", "sexo")
KM.LIM.a <- union(KM.LIM.a, help.KM1)

KM.LIM.b <- tidy(mfit.1b) %>% dplyr::select(estimate, time) %>% mutate(DG = "accelerated") %>% mutate(sexo="mujer")
help.KM2 <- data.frame(1,65,"accelerated", "mujer")
names(help.KM2) <- c("estimate", "time", "DG", "sexo")
KM.LIM.b <- union(KM.LIM.b, help.KM2)

KM.LIM.c <- tidy(mfit.1c) %>% dplyr::select(estimate, time) %>% mutate(DG = "catastrophic") %>% mutate(sexo="mujer")
help.KM3 <- data.frame(1,65,"catastrophic","mujer")
names(help.KM3) <- c("estimate", "time", "DG", "sexo")
KM.LIM.c <- union(KM.LIM.c, help.KM3)

# males
KM.LIM.d <- tidy(mfit.2a) %>% dplyr::select(estimate, time) %>% mutate(DG = "mild-gradual") %>% mutate(sexo="varón")
help.KM1 <- data.frame(1,65,"mild-gradual","varón")
names(help.KM1) <- c("estimate", "time", "DG", "sexo")
KM.LIM.d <- union(KM.LIM.d, help.KM1)

KM.LIM.e <- tidy(mfit.2b) %>% dplyr::select(estimate, time) %>% mutate(DG = "accelerated") %>% mutate(sexo="varón")
help.KM2 <- data.frame(1,65,"accelerated","varón")
names(help.KM2) <- c("estimate", "time", "DG","sexo")
KM.LIM.e <- union(KM.LIM.e, help.KM2)

KM.LIM.f <- tidy(mfit.2c) %>% dplyr::select(estimate, time) %>% mutate(DG = "catastrophic") %>% mutate(sexo="varón")
help.KM3 <- data.frame(1,65,"catastrophic","varón")
names(help.KM3) <- c("estimate", "time", "DG", "sexo")
KM.LIM.f <- union(KM.LIM.f, help.KM3)

# combine the data sets and plot them
KM.LIM <- union(KM.LIM.a, KM.LIM.b) %>% union(KM.LIM.c) %>% union(KM.LIM.d) %>% union(KM.LIM.e) %>% union(KM.LIM.f)


km.1 <- KM.LIM %>% dplyr::filter(time >= 65) %>% 
  ggplot() +
  geom_step(mapping=aes(x=time, y=estimate, color=DG)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("#0072B2", "#D55E00", "#009E73"), name="")     +
  #  Optional Grey Scales for the paper
  # scale_colour_manual(values = c("#000000", "grey50", "grey75"), name="")     +
  facet_grid(.~sexo) +
  theme_bw()
km.1 +  theme(legend.position = c(0.85, 0.80)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold")) +
  scale_shape_discrete(guide=FALSE)