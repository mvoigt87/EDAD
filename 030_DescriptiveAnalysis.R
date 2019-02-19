########################################################
##### Analysis of mortality differences by pathway #####
########################################################

### Code file for the descriptive analysis in preparation of the Main Models
### ------------------------------------------------------------------------

# 0.1 packages

library(tidyverse)
library(data.table)
library(foreign)
library(survival)
library(broom)


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



##### 1. Ages at death (by sex and cluster)
##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## -------------------------------------------
## 1.2 Mean and median age at death by cluster
## -------------------------------------------

# A) DISCA 44

# males 2 clusters
link.may_M %>% group_by(cluster2) %>% summarize(mean=mean(age.ex[event==1]), median=median(age.ex[event==1]))

# males 3 clusters
link.may_M %>% group_by(cluster4) %>% summarize(mean=mean(age.ex[event==1]), median=median(age.ex[event==1]))


# A) A 12 INCAPACITY
summary(link.may_M2$age.ex[link.may_M2$event==1])


##### 2. Distribution of explanatory variables by sex and cluster
##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




##### 3. Bi-variate Survival Analysis (KMEs)
##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

####### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ########
####### !!! Change the number after "clustering$cluster" depending on the optimal group/cluster size ########
####### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ########

# Males first cluster option
# --------------------------

mfit.1a <- survfit(coxph(Surv(time=EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data = subset(link.may_M, cluster2=="early onset")), data = subset(link.may_M, cluster2=="early onset"),
                   type = "kaplan-meier")
mfit.1b <- survfit(coxph(Surv(time=EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data = subset(link.may_M, cluster2=="late onset")), data = subset(link.may_M, cluster2=="late onset"),
                   type = "kaplan-meier")


KME.Clusta <- tidy(mfit.1a) %>% select(estimate, time) %>% mutate(sex = "early")
KME.Clustb <- tidy(mfit.1b) %>% select(estimate, time) %>% mutate(sex = "late")

KME.CLustM <- union(KME.Clusta, KME.Clustb)
KME.CLustM %>% ggplot() +
  geom_step(aes(x=time, y=estimate, color=sex)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
  theme_bw()

# Males second cluster option
# ---------------------------


#   !!!!!  To account for left truncation, a cox ph approximation is used to estimate the KME


# KME for sex differences #
# %%%%%%%%%%%%%%%%%%%%%%% #

mfit.2a <- survfit(coxph(Surv(time=EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data = subset(link.may, SEXO=="Varón")), data = subset(link.may, SEXO=="Varón"),
                   type = "kaplan-meier")


mfit.2b <- survfit(coxph(Surv(time=EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data = subset(link.may, SEXO=="Mujer")), data = subset(link.may, SEXO=="Mujer"),
                   type = "kaplan-meier")


KME.SEXOa <- tidy(mfit.2a) %>% select(estimate, time) %>% mutate(sex = "male")
KME.SEXOb <- tidy(mfit.2b) %>% select(estimate, time) %>% mutate(sex = "female")

KME.SEXO <- union(KME.SEXOa, KME.SEXOb)
KME.SEXO %>% ggplot() +
  geom_step(aes(x=time, y=estimate, color=sex)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
  theme_bw()

# Very dramatic drop in survival probability over time (again women seem to have better survival)



#  KME by time in disability  #
# %%%%%%%%%%%%%%%%%%%%%%%%%%% #


## Therefore: Creating a for now arbitrary variable to group different transitions by time
# acute: less than 1 years between onset of disability and onset of dependency
# moderate: between 1 and 10 years (mean)
# gradual : more than 10 years

link.may <- link.may %>% mutate(diff.d.d= Edadinicio_cuidado - Edadinicio_disca44) %>%
  # clean up
  mutate(diff.d.d=ifelse(Edadinicio_cuidado - Edadinicio_disca44<0,diff.d.d+0.5,diff.d.d)) %>% 
  mutate(group.d.d = ifelse(Edadinicio_cuidado - Edadinicio_disca44>10,"gradual",
                            ifelse(Edadinicio_cuidado - Edadinicio_disca44<=1,"acute", "moderate")))

summary(link.may$diff.d.d)
link.may %>% mutate(event = as.factor(event)) %>% 
  ggplot(aes(x=diff.d.d, fill=event)) +
  geom_histogram(bins = 44) +
  scale_x_continuous(name = "Age") +
  scale_fill_discrete(name = "") +
  theme_bw() 
# for a large number of individuals the onset of disability was also the onset of dependency
# We can assume that these individuals were exposed to an acute health event like a stroke or heavy accident

table(link.may$group.d.d)


# Lets extract the survival probabilities for the three groups

mfit.1a <- survfit(coxph(Surv(time=EDAD,
                              time2=age.ex,
                              event=event) ~ 1, data=subset(link.may,group.d.d =="gradual")), 
                   data=subset(link.may,group.d.d =="gradual"),type="kaplan-meier")

mfit.1b <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may,group.d.d =="moderate")), 
                   data=subset(link.may,group.d.d =="moderate"),type="kaplan-meier")

mfit.1c <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may,group.d.d =="acute")), 
                   data=subset(link.may,group.d.d =="acute"),type="kaplan-meier")

KM.LIM.a <- tidy(mfit.1a) %>% select(estimate, time) %>% mutate(Limit = "Gradual")
help.KM1 <- data.frame(1,65,"Gradual")
names(help.KM1) <- c("estimate", "time", "Limit")
KM.LIM.a <- union(KM.LIM.a, help.KM1)

KM.LIM.b <- tidy(mfit.1b) %>% select(estimate, time) %>% mutate(Limit = "Moderate")
help.KM2 <- data.frame(1,65,"Moderate")
names(help.KM2) <- c("estimate", "time", "Limit")
KM.LIM.b <- union(KM.LIM.b, help.KM2)

KM.LIM.c <- tidy(mfit.1c) %>% select(estimate, time) %>% mutate(Limit = "Acute")
help.KM3 <- data.frame(1,65,"Acute")
names(help.KM3) <- c("estimate", "time", "Limit")
KM.LIM.c <- union(KM.LIM.c, help.KM3)

### ADD a starting value!!!

KM.LIM <- union(KM.LIM.a, KM.LIM.b) %>% union(KM.LIM.c)


km.1 <- KM.LIM %>% dplyr::filter(time >= 65) %>% 
  ggplot() +
  geom_step(mapping=aes(x=time, y=estimate, color=Limit)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9"), name="")     +
  theme_bw()
# change the legend postion
km.1 <- km.1 + theme(legend.position = c(0.85, 0.85)) + 
  scale_shape_discrete(guide=FALSE)

# Believable and expected survival probabilities - gradient from gradual to acute
# Just need the get the noise in the beginning under control

rm(KM.LIM.a, KM.LIM.b, KM.LIM.c, help.KM1, help.KM2, help.KM3)