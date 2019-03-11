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
library(stargazer)


# 0.3 Set right working directory
dir()
setwd("C:/Users/y4956294S/Documents/LONGPOP/Subproject 2 - SE differences in transition to dependency/R code/EDAD")

# 0.2 data
# --------------------------------------------
# load(file='010_mayor.link.RData')

# prepared datasets (see R file 025_CleanCluster)

# A 12 data
# load(file = 'datasets/030_linkmay_M_12A.RData')
# load(file = 'datasets/030_linkmay_F_12A.RData')
# 
# link.may_M2 <- link.may_M
# link.may_F2 <- link.may_F

# Disca 44 file
# load(file = 'datasets/030_linkmay_M.RData')
# load(file = 'datasets/030_linkmay_F.RData')

# Disca 44 - 50+
# load(file= 'datasets/030_linkmay_M_50.RData')
# load(file= 'datasets/030_linkmay_F_50.RData')

# link.may_M3 <- link.may_M
# link.may_F3 <- link.may_F

# D 13 Data
load(file = 'datasets/030_linkmay_M_50ADL.RData')
load(file = 'datasets/030_linkmay_F_50ADL.RData')




##### 1. Ages at death (by sex and cluster)
##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## ---------------------------
## 1.2 Age at death by cluster
## ---------------------------

# C) DISCA 44 (50+)

# males 2 clusters
link.may_M %>% group_by(cluster2) %>% summarize(mean=mean(age.ex[event==1]), median=median(age.ex[event==1]))

# males 4 clusters
link.may_M %>% group_by(cluster5) %>% summarize(mean=mean(age.ex[event==1]), median=median(age.ex[event==1]))

# females 2 cluster
link.may_F %>% group_by(cluster2) %>% summarize(mean=mean(age.ex[event==1]), median=median(age.ex[event==1]))

# females 3 clusters
link.may_F %>% group_by(cluster4) %>% summarize(mean=mean(age.ex[event==1]), median=median(age.ex[event==1]))

# Check the NAs (exclude them!)

# Output table for paper - summary stats on the age variables   !!! (Make all of them numeric arguments)
stargazer(link.may_M[,c(409,424,443,463,487,502, 513)])

## ---------------------------------
## 1.2 Time between onset and death
## ---------------------------------


###### !!! Change accordingly if you want to use a different data source

# look at the distribution of onset of ADL

# Males
link.may_M <- link.may_M %>% mutate(dur_dis = round(age.ex,0)-round(EdadInicioDisca13,0))
hist(link.may_M$dur_dis)
summary(link.may_M$dur_dis)
# event distribution by duration in disability
# --------------------------------------------
DUR_Plot_M <- link.may_M %>% mutate(event = as.factor(event)) %>% 
  ggplot(aes(x=dur_dis, fill=event)) +
  geom_histogram(aes(y=0.5*..density..), binwidth=0.5) +
  scale_x_continuous(name = "Duration in Disability (Years)") +
  scale_y_continuous(name = "Relative Frequency",labels = scales::percent) +
  scale_fill_manual(name = "", values=c("#000000", "#A9A9A9"), labels = c("Survivors", "Deceased")) +
  theme_bw()
DUR_Plot_M <- DUR_Plot_M + theme(legend.position = c(0.85, 0.80)) + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))
   
                                                                          
link.may_F <- link.may_F %>% mutate(dur_dis = round(age.ex,0)-round(EdadInicioDisca13,0))
hist(link.may_F$dur_dis)
summary(link.may_F$dur_dis)

# event distribution by duration in disability
# --------------------------------------------
DUR_Plot_F <- link.may_F %>% mutate(event = as.factor(event)) %>% 
  ggplot(aes(x=dur_dis, fill=event)) +
  geom_histogram(aes(y=0.5*..density..), binwidth=0.5) +
  scale_x_continuous(name = "Duration in Disability (Years)") +
  scale_y_continuous(name = "Relative Frequency",labels = scales::percent) +
  scale_fill_manual(name = "", values=c("#000000", "#A9A9A9"), labels = c("Survivors", "Deceased")) +
  theme_bw()
DUR_Plot_F <- DUR_Plot_F + theme(legend.position = c(0.85, 0.80)) + theme(axis.text=element_text(size=12),
                                                                          axis.title=element_text(size=12,face="bold"))  




# Dependency and Death


# Duration in states
# ------------------

      # # For now under the assumption (can be verified in the aftermath) that the states are inconvertible
      # 
      # tra_may <- tra_may %>% 
      #   
      #   # 1. start with the duration of disability free years after 50
      #   # Calculated as age after age 50 when first disability occurs - age 50 (= disability free duration after age 50)
      #   mutate(dur_DF = ifelse(EdadInicioDisca13>=50,round(EdadInicioDisca13-50,0),0)) %>% 
      #   
      #   # 2. second state duration after onset of disability but before help or assistance is needed
      #   ### Important to remember is that independence does not necessarily mean that a diagnosed disability has occurred
      #   
      #   # Calculated as: Age at when personal assistance is needed - Age at onset of disability/impairment OR if the age when first
      #   # personal assistance is requested was first, this age minus age 50, if they age of first assistance occured first, the independent time is 0
      #   
      #   mutate(dur_ID = ifelse(Edadinicio_cuidado>=50 & Edadinicio_cuidado > EdadInicioDisca13, 
      #                          round(((Edadinicio_cuidado - EdadInicioDisca13)-(50-EdadInicioDisca13)),0),
      #                          ifelse(Edadinicio_cuidado>=50 & EdadInicioDisca13>=50 & Edadinicio_cuidado <= EdadInicioDisca13,round(Edadinicio_cuidado,0) - 50,0))) %>% 
      #   
      #   # Little trick to avoid problems with cases where care taking occured before the diagnosis of disability
      #   mutate(dur_DF = ifelse(dur_DF>=dur_ID, 0, dur_DF)) %>%
      #   # A further change necessary to obtain the difference between onset of disability and dependency as time independent
      #   mutate(dur_ID = dur_ID-dur_DF) %>% 
      #   
      #   # 3. Duration in Dependency until right censoring
      #   mutate(dur_DC = EDAD-round(Edadinicio_cuidado,0)) %>% 
      #   # 4. State 4 is just for programming reasons - Censorship (duration between age at interview and 100)
      #   mutate(dur_C = ifelse(EDAD>100, 0, 100-EDAD))


##### 2. Distribution of explanatory variables by sex and cluster
##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




##### 3. Bi-variate Survival Analysis (KMEs)
##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Create additional entry and exit variables

### Since we do not observe deaths before the EDAD survey, it doesn´t make sense to 

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


####### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ########
####### !!! Change the number after "clustering$cluster" depending on the optimal group/cluster size ########
####### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ########

# Males and female first cluster option
# --------------------------

mfit.1a <- survfit(coxph(Surv(time=EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data = subset(link.may_M, cluster2=="early onset")), data = subset(link.may_M, cluster2=="early onset"),
                   type = "kaplan-meier")
mfit.1b <- survfit(coxph(Surv(time=EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data = subset(link.may_M, cluster2=="late onset")), data = subset(link.may_M, cluster2=="late onset"),
                   type = "kaplan-meier")

mfit.1c <- survfit(coxph(Surv(time=EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data = subset(link.may_F, cluster2=="early onset long")), data = subset(link.may_F, cluster2=="early onset long"),
                   type = "kaplan-meier")

mfit.1d <- survfit(coxph(Surv(time=EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data = subset(link.may_F, cluster2=="late onset short")), data = subset(link.may_F, cluster2=="late onset short"),
                   type = "kaplan-meier")

KME.Clusta <- tidy(mfit.1a) %>% select(estimate, time) %>% mutate(disab = "early") %>% mutate(sex="male")
KME.Clustb <- tidy(mfit.1b) %>% select(estimate, time) %>% mutate(disab = "late") %>% mutate(sex="male")
KME.Clustc <- tidy(mfit.1c) %>% select(estimate, time) %>% mutate(disab = "early") %>% mutate(sex="female")
KME.Clustd <- tidy(mfit.1d) %>% select(estimate, time) %>% mutate(disab = "late") %>% mutate(sex="female")

KME.CLustM <- union(KME.Clusta, KME.Clustb) %>% union(KME.Clustc) %>% union(KME.Clustd)
KME1 <- KME.CLustM %>% ggplot() +
  geom_step(aes(x=time, y=estimate, color=disab)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
  theme_bw()
KME1 + facet_grid(. ~ sex) + theme(legend.position = c(0.85, 0.80)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))


# Males first cluster option (time between onset and death)
# ----------------------------------------------------------



mfit.2a <- survfit(coxph(Surv(time = t.salida,
                              event = event) ~ 1, data = subset(link.may_M, cluster2=="early onset")), data = subset(link.may_M, cluster2=="early onset"),
                   type = "kaplan-meier")
mfit.2b <- survfit(coxph(Surv(time=t.salida,
                              event = event) ~ 1, data = subset(link.may_M, cluster2=="late onset")), data = subset(link.may_M, cluster2=="late onset"),
                   type = "kaplan-meier")

mfit.2c <- survfit(coxph(Surv(time= t.salida,
                              event = event) ~ 1, data = subset(link.may_F, cluster2=="early onset long")), data = subset(link.may_F, cluster2=="early onset long"),
                   type = "kaplan-meier")

mfit.2d <- survfit(coxph(Surv(time= t.salida,
                              event = event) ~ 1, data = subset(link.may_F, cluster2=="late onset short")), data = subset(link.may_F, cluster2=="late onset short"),
                   type = "kaplan-meier")

KME.Clusta <- tidy(mfit.2a) %>% select(estimate, time) %>% mutate(disab = "early") %>% mutate(sex="male")
KME.Clustb <- tidy(mfit.2b) %>% select(estimate, time) %>% mutate(disab = "late") %>% mutate(sex="male")
KME.Clustc <- tidy(mfit.2c) %>% select(estimate, time) %>% mutate(disab = "early") %>% mutate(sex="female")
KME.Clustd <- tidy(mfit.2d) %>% select(estimate, time) %>% mutate(disab = "late") %>% mutate(sex="female")

KME.CLustM <- union(KME.Clusta, KME.Clustb) %>% union(KME.Clustc) %>% union(KME.Clustd)
KME1 <- KME.CLustM %>% ggplot() +
  geom_step(aes(x=time, y=estimate, color=disab)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Years since survey") +
  scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
  theme_bw()
KME1 + facet_grid(. ~ sex) + theme(legend.position = c(0.85, 0.80)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))


# Males and female first cluster option - left-truncated at the age of onset of there care dependency
# ---------------------------------------------------------------------------------------------------

mfit.3a <- survfit(coxph(Surv(time=Edadinicio_cuidado,
                              time2 = age.ex,
                              event = event) ~ 1, data = subset(link.may_M, cluster2=="early onset")), data = subset(link.may_M, cluster2=="early onset"),
                   type = "kaplan-meier")
mfit.3b <- survfit(coxph(Surv(time=Edadinicio_cuidado,
                              time2 = age.ex,
                              event = event) ~ 1, data = subset(link.may_M, cluster2=="late onset")), data = subset(link.may_M, cluster2=="late onset"),
                   type = "kaplan-meier")

mfit.3c <- survfit(coxph(Surv(time=Edadinicio_cuidado,
                              time2 = age.ex,
                              event = event) ~ 1, data = subset(link.may_F, cluster2=="early onset long")), data = subset(link.may_F, cluster2=="early onset long"),
                   type = "kaplan-meier")

mfit.3d <- survfit(coxph(Surv(time=Edadinicio_cuidado,
                              time2 = age.ex,
                              event = event) ~ 1, data = subset(link.may_F, cluster2=="late onset short")), data = subset(link.may_F, cluster2=="late onset short"),
                   type = "kaplan-meier")

KME.Clusta <- tidy(mfit.3a) %>% select(estimate, time) %>% mutate(disab = "early") %>% mutate(sex="male")
KME.Clustb <- tidy(mfit.3b) %>% select(estimate, time) %>% mutate(disab = "late") %>% mutate(sex="male")
KME.Clustc <- tidy(mfit.3c) %>% select(estimate, time) %>% mutate(disab = "early") %>% mutate(sex="female")
KME.Clustd <- tidy(mfit.3d) %>% select(estimate, time) %>% mutate(disab = "late") %>% mutate(sex="female")

KME.CLustM <- union(KME.Clusta, KME.Clustb) %>% union(KME.Clustc) %>% union(KME.Clustd)
KME1 <- KME.CLustM %>% ggplot() +
  geom_step(aes(x=time, y=estimate, color=disab)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
  theme_bw()
KME1 + facet_grid(. ~ sex) + theme(legend.position = c(0.85, 0.80)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))







#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####


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