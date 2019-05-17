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




# see event distribution - looks like accelerated decline leads to much higher number of deaths
round(prop.table(table(link.may50$event[link.may50$DIS_GRP=="accelerated"])),3)
round(prop.table(table(link.may50$event[link.may50$DIS_GRP=="catastrophic"])),3)

# Differences in distance between first and second limitation
summary(link.may_F$DIFF_2[link.may_F$DIS_GRP=="mild-gradual"]) # NA - mostly only one disability - doublecheck!
table(link.may_F$NumDiscas13[link.may_F$DIS_GRP=="mild-gradual"])
summary(link.may_F$DIFF_2[link.may_F$DIS_GRP=="accelerated"])  # median 3