# Follows file PAA2019CFR.R

library(tidyverse)
library(data.table)
library(foreign)
library(survival)
library(broom)
library(stargazer)
library(TraMineR)

# --------------------------------------------
load(file='010_mayor.link.RData')
# --------------------------------------------


# Select Population - Drop the "NC" cases for now and only extract the dependent ones
# ------------------------------------------------------------------------------------

link.may <- link.may %>% filter(LIMIT!="NC") %>% 
  ####
  ####
  #### AND at this point only extract the ones who are dependent in 2008
  ####
  filter(!is.na(Edadinicio_cuidado))   

link.may <- data.table(link.may)
# 4761 cases - working material

# --------------------------------------------
# 1. Few discriptive plots on different states
# --------------------------------------------
# --------------------------------------------


# Age distribution (disability)
###############################

# Exit age
# ---------
with(link.may, tapply(age.ex, list(event,LIMIT), mean)) # Just means but still the No cases are puzzeling

table(link.may$estado)

link.may[,.N,.(enlazado,estado)] 

# By disability status
# --------------------
round(prop.table(table(link.may$event, link.may$Gravity),2),3)

with(link.may, tapply(age.ex, list(event,Gravity), mean))




# check distribution
hist(link.may$age.ex, nclass = 50, main = "", xlab = "Age")

# event-age distribution
link.may %>% mutate(event = as.factor(event)) %>% 
  ggplot(aes(x=age.ex, fill=event)) +
  geom_histogram(bins = 44) +
  scale_x_continuous(name = "Age") +
  scale_fill_discrete(name = "") +
  theme_bw()    
# looks believable - most of the members of this very frail group die during the observation period

