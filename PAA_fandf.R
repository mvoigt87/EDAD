# ------------------------------------ #
# IDEA flus and famines with EDAD data #
# ------------------------------------ #

library(tidyverse)
library(data.table)
library(foreign)
library(survival)
library(broom)

# load linked 65+ data
# --------------------------------------------
load(file='010_mayor.link.RData')
# --------------------------------------------

class(link.may)
link.may <- data.table(link.may)

# Check for different birth cohort sizes

  # Born 1916 - age in 2008: 91-92
link.may[, .N, .(EDAD<=92 & EDAD>90)] # 395

  # Born 1918 - age in 2008: 89-90
link.may[, .N, .(EDAD<=90 & EDAD>88)] # 572

# Born 1920 - age in 2008: 87-88
link.may[, .N, .(EDAD<=88 & EDAD>86)] # 920

# Assign individuals to cohort


# Compare there performance descriptively at age 85

ff.1918 <- link.may %>% filter(EDAD<=92 & EDAD>=86) %>%
  # create a variable to differentiate the cohorts (age values approximated)
  mutate(cohort = ifelse(2008.5-EDAD<=1918.6, "pre flu", ifelse(2008.5-EDAD>1920.8, "post flu","flu")))

table(ff.1918$cohort)

# check discriptively the distribution of different degrees of severity
round(prop.table(table(ff.1918$cohort, ff.1918$LIMIT),1),2)
 # this could just be an age effect => look at the time spent disability

ff.1918 <- ff.1918 %>% mutate(diff.onset44 = EDAD - Edadinicio_disca44) %>%
                        mutate(diff.onset13 = EDAD - as.numeric(EdadInicioDisca13))

# simple comparison
summary(ff.1918$diff.onset44[ff.1918$cohort=="post flu"])
summary(ff.1918$diff.onset44[ff.1918$cohort=="flu"])
summary(ff.1918$diff.onset44[ff.1918$cohort=="pre flu"])


ff.1918 %>% mutate(event = as.factor(event)) %>% 
  ggplot(aes(x=diff.onset44, fill=event)) +
  geom_histogram(bins = 44) +
  scale_x_continuous(name = "Age") +
  scale_fill_discrete(name = "") +
  theme_bw() 


# Exploratory Survival Analysis
# -----------------------------

ff.1918 <- within(ff.1918, Sex <- relevel(Sex, ref = "Female"))  
ff.1918 <- within(ff.1918, Education <- relevel(Education, ref = "High"))  
ff.1918 <- within(ff.1918, cohort <- relevel(as.factor(cohort), ref = "pre flu"))  

cox.flu <- coxph(Surv(time=EDAD,
                      time2=age.ex,
                      event=event) ~ cohort + Sex + Education + Residence + Household, data=ff.1918)
summary(cox.flu)


# --------------------
# --------------------
# onset of disability
# --------------------
# --------------------


ff.1918 <- ff.1918 %>% 
  # event is onset of first disability
  mutate(age.ex.2 = ifelse(!is.na(Edadinicio_disca6GS),Edadinicio_disca6GS, age.ex)) %>% 
  # event is onset of dependency
  mutate(age.ex.3 = ifelse(!is.na(Edadinicio_cuidado),Edadinicio_cuidado, age.ex)) %>% 
  # same for events
  mutate(event.2 = ifelse(!is.na(Edadinicio_disca44),1,0)) %>% 
  mutate(event.3 = ifelse(!is.na(Edadinicio_cuidado),1,0))
  
mfit.1a <- survfit(coxph(Surv(time=age.ex.2,
                              event=event.2) ~ 1, data=subset(ff.1918,cohort =="pre flu")), 
                   data=subset(ff.1918,cohort =="pre flu"),type="kaplan-meier")

mfit.1b <-  survfit(coxph(Surv(time=age.ex.2,
                               event=event.2) ~ 1, data=subset(ff.1918,cohort =="flu")), 
                    data=subset(ff.1918,cohort =="flu"),type="kaplan-meier")

mfit.1c <-  survfit(coxph(Surv(time=age.ex.2,
                               event=event.2) ~ 1, data=subset(ff.1918,cohort =="post flu")), 
                    data=subset(ff.1918,cohort =="post flu"),type="kaplan-meier")


KM.LIM.a <- tidy(mfit.1a) %>% select(estimate, time) %>% mutate(Limit = "Pre Flu")
KM.LIM.b <- tidy(mfit.1b) %>% select(estimate, time) %>% mutate(Limit = "Flu")
KM.LIM.c <- tidy(mfit.1c) %>% select(estimate, time) %>% mutate(Limit = "Post Flu")

### ADD a starting value!!!

KM.LIM <- union(KM.LIM.a, KM.LIM.b) %>% union(KM.LIM.c)


km.1 <- KM.LIM %>% dplyr::filter(time >= 50) %>% 
  ggplot() +
  geom_step(mapping=aes(x=time, y=estimate, color=Limit)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9"), name="")     +
  theme_bw()
# change the legend postion
km.1 <- km.1 + theme(legend.position = c(0.85, 0.85)) + 
  scale_shape_discrete(guide=FALSE)



# Cox model

cox.flu.2 <- coxph(Surv(time=age.ex.2,
                        event=event.2) ~ cohort + Sex + Education + Residence, data=ff.1918)
summary(cox.flu.2)


cox.flu.3 <- coxph(Surv(time=age.ex.3,
                        event=event.3) ~ cohort + Sex + Education + Residence, data=ff.1918)
summary(cox.flu.3)
