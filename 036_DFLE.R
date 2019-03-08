### ------------------------------- ###
### Disability-Free Life Expectancy ###
### ------------------------------- ###

# load packages
library(tidyverse)
library(data.table)
library(foreign)
library(ggplot2)
library(gcookbook)
library(HMDHFDplus)
library(plyr)
library(reshape2)
library(grid)
library(gridExtra)
library(tidyr)
library(ggplot2)
library(readxl)
library(dplyr)
library(scales)


# load data
load(file="datasets/MLT_HMD.Rdata")
load(file="datasets/FLT_HMD.Rdata")
load(file="datasets/TLT_HMD.Rdata")

# Disability Data
load(file='010_mayor50_LT.RData')

link.may_M <- link.may50 %>% filter(SEXO=="Varón") %>% mutate(Sex="Male")
link.may_F <- link.may50 %>% filter(SEXO=="Mujer") %>% mutate(SEXO="Female")

# Calculate age-specific disability rates #
# --------------------------------------- #

table(link.may_M$Disca13)
class(link.may_M$EDAD)

# Little helper to have a count
link.may_M <- link.may_M %>% mutate(EINS=1)

X <- aggregate(link.may_M$EINS[link.may_M$Disca13=="Si"], by=list(link.may_M$EDAD[link.may_M$Disca13=="Si"]), FUN = "sum")
Y <- aggregate(link.may_M$EINS, by=list(link.may_M$EDAD), FUN = "sum")

disrate_M <- left_join(X,Y, by="Group.1") %>% mutate(age_disrate = x.x/x.y) %>% select(-x.x, -x.y)
colnames(disrate_M)[1] <- "Age"

## Females

# Little helper to have a count
link.may_F <- link.may_F %>% mutate(EINS=1)

X <- aggregate(link.may_F$EINS[link.may_F$Disca13=="Si"], by=list(link.may_F$EDAD[link.may_F$Disca13=="Si"]), FUN = "sum")
Y <- aggregate(link.may_F$EINS, by=list(link.may_F$EDAD), FUN = "sum")

disrate_F <- left_join(X,Y, by="Group.1") %>% mutate(age_disrate = x.x/x.y) %>% select(-x.x, -x.y)
colnames(disrate_F)[1] <- "Age"


### Now cut the life tables to ages and years (2008 +)

## Females
FLT <- fem.smooth %>% filter(Year==2008) %>% filter(Age>=50) %>% filter(Age<=100)
disrate_F <- disrate_F %>% mutate(ifelse(is.na(age_disrate),1,age_disrate))

# Multiply the Lx values with the average fraction persons of the age group are free of disability

FLT <- FLT %>% left_join(disrate_F, by="Age") %>% mutate(Lx_i = Lx*(1-age_disrate)) %>% 
  # calculate the Tx_i (disability free)
  mutate(Tx_i = rev(cumsum(rev(Lx_i)))) %>% 
  ## Finally obtain the DFLE from the Tx and lx
  mutate(ex_i = Tx_i / lx)


## Males
MLT <- mal.smooth %>% filter(Year==2008) %>% filter(Age>=50) %>% filter(Age<=100)

# Multiply the Lx values with the average fraction persons of the age group are free of disability

MLT <- MLT %>% left_join(disrate_M, by="Age") %>% mutate(Lx_i = Lx*(1-age_disrate)) %>% 
  # calculate the Tx_i (disability free)
  mutate(Tx_i = rev(cumsum(rev(Lx_i)))) %>% 
  ## Finally obtain the DFLE from the Tx and lx
  mutate(ex_i = Tx_i / lx)




#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#### Das gleiche für dependency-free life expectancy


# Calculate age-specific dependency rates #
# --------------------------------------- #

table(link.may_M$Dependientes44)
table(link.may_M$DependientesReales44)    # Use dependientesReales44
class(link.may_M$EDAD)

# Little helper to have a count
link.may_M <- link.may_M %>% mutate(EINS=1)

X <- aggregate(link.may_M$EINS[link.may_M$DependientesReales44=="Dependiente real"], by=list(link.may_M$EDAD[link.may_M$DependientesReales44=="Dependiente real"]), FUN = "sum")
Y <- aggregate(link.may_M$EINS, by=list(link.may_M$EDAD), FUN = "sum")

deprate_M <- left_join(X,Y, by="Group.1") %>% mutate(age_deprate = x.x/x.y) %>% select(-x.x, -x.y)
colnames(deprate_M)[1] <- "Age"

## Females

# Little helper to have a count
link.may_F <- link.may_F %>% mutate(EINS=1)

X <- aggregate(link.may_F$EINS[link.may_F$DependientesReales44=="Dependiente real"], by=list(link.may_F$EDAD[link.may_F$DependientesReales44=="Dependiente real"]), FUN = "sum")
Y <- aggregate(link.may_F$EINS, by=list(link.may_F$EDAD), FUN = "sum")

deprate_F <- left_join(X,Y, by="Group.1") %>% mutate(age_deprate = x.x/x.y) %>% select(-x.x, -x.y)
colnames(deprate_F)[1] <- "Age"


### Now cut the life tables to ages and years (2008 +)

## Females

deprate_F <- deprate_F %>% mutate(ifelse(is.na(age_deprate),1,age_deprate))

# Multiply the Lx values with the average fraction persons of the age group are free of disability

FLT_DEP <- FLT %>% left_join(deprate_F, by="Age") %>% mutate(Lx_i = Lx*(1-age_deprate)) %>% 
  # calculate the Tx_i (disability free)
  mutate(Tx_i = rev(cumsum(rev(Lx_i)))) %>% 
  ## Finally obtain the DFLE from the Tx and lx
  mutate(ex_i = Tx_i / lx)


## Males

# Multiply the Lx values with the average fraction persons of the age group are free of disability

MLT_DEP <- MLT %>% left_join(deprate_M, by="Age") %>% mutate(Lx_i = Lx*(1-age_deprate)) %>% 
  # calculate the Tx_i (disability free)
  mutate(Tx_i = rev(cumsum(rev(Lx_i)))) %>% 
  ## Finally obtain the DFLE from the Tx and lx
  mutate(ex_i = Tx_i / lx)


