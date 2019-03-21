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
library(stargazer)

# load data
load(file="datasets/MLT_HMD.Rdata")
load(file="datasets/FLT_HMD.Rdata")
load(file="datasets/TLT_HMD.Rdata")

# Disability Data
load(file='datasets/010_mayor50_LT.RData')

link.may_M <- link.may50 %>% filter(SEXO=="Varón") %>% mutate(Sex="Male")
link.may_F <- link.may50 %>% filter(SEXO=="Mujer") %>% mutate(SEXO="Female")

# Calculate age-specific disability rates #
# --------------------------------------- #

table(link.may_M$Disca13)
table(link.may_M$Disca44)
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

FLT.DIS <- FLT %>% left_join(disrate_F, by="Age") %>% mutate(Lx_i = Lx*(1-age_disrate)) %>% 
  # calculate the Tx_i (disability free)
  mutate(Tx_i = rev(cumsum(rev(Lx_i)))) %>% 
  ## Finally obtain the DFLE from the Tx and lx
  mutate(ex_i = Tx_i / lx)


## Males
MLT <- mal.smooth %>% filter(Year==2008) %>% filter(Age>=50) %>% filter(Age<=100)

# Multiply the Lx values with the average fraction persons of the age group are free of disability

MLT.DIS <- MLT %>% left_join(disrate_M, by="Age") %>% mutate(Lx_i = Lx*(1-age_disrate)) %>% 
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

table(link.may_M$Dependientes44, useNA = "always")
table(link.may_M$DependientesReales44, useNA = "always")
class(link.may_M$EDAD, useNA = "always")
table(link.may_M$L_1, useNA = "always")
table(link.may_M$Dependientes13, useNA = "always") #### This will be what we use for now - it is according to our definition of disability

# Little helper to have a count

X <- aggregate(link.may_M$EINS[link.may_M$Dependientes13=="Dependiente"], by=list(link.may_M$EDAD[link.may_M$Dependientes13=="Dependiente"]), FUN = "sum")
Y <- aggregate(link.may_M$EINS, by=list(link.may_M$EDAD), FUN = "sum")

deprate_M <- left_join(X,Y, by="Group.1") %>% mutate(age_deprate = x.x/x.y) %>% select(-x.x, -x.y)
colnames(deprate_M)[1] <- "Age"

## Females

# Little helper to have a count
X <- aggregate(link.may_F$EINS[link.may_F$Dependientes13=="Dependiente"], by=list(link.may_F$EDAD[link.may_F$Dependientes13=="Dependiente"]), FUN = "sum")
Y <- aggregate(link.may_F$EINS, by=list(link.may_F$EDAD), FUN = "sum")

deprate_F <- left_join(X,Y, by="Group.1") %>% mutate(age_deprate = x.x/x.y) %>% select(-x.x, -x.y)
colnames(deprate_F)[1] <- "Age"


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

### Now cut the life tables to ages and years (2008 +)

## Females

deprate_F <- deprate_F %>% mutate(ifelse(is.na(age_deprate),1,age_deprate))

# Multiply the Lx values with the average fraction persons of the age group are free of disability

FLT_DEP <- FLT %>% left_join(deprate_F, by="Age") %>% mutate(Lx_i = Lx*(1-age_deprate)) %>% 
  # calculate the Tx_i (disability free)
  mutate(Tx_i = rev(cumsum(rev(Lx_i)))) %>% 
  ## Finally obtain the DFLE from the Tx and lx
  mutate(ex_dep = Tx_i / lx)


## Males

# Multiply the Lx values with the average fraction persons of the age group are free of disability

MLT_DEP <- MLT %>% left_join(deprate_M, by="Age") %>% mutate(Lx_i = Lx*(1-age_deprate)) %>% 
  # calculate the Tx_i (disability free)
  mutate(Tx_i = rev(cumsum(rev(Lx_i)))) %>% 
  ## Finally obtain the DFLE from the Tx and lx
  mutate(ex_dep = Tx_i / lx)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################


#### Make a bigger table for plot and output

Male_DFLE <- MLT_DEP %>% select(Age,Year, age_deprate,ex_dep) %>% left_join(MLT.DIS, by=c("Age","Year"))
Female_DFLE <- FLT_DEP %>% select(Age,Year, age_deprate,ex_dep) %>% left_join(FLT.DIS, by=c("Age","Year"))


#### Table
DEFLE_TABLE <- cbind(Female_DFLE[c(1,16,26,31,41),c(1,13,17,4)], Male_DFLE[c(1,16,26,31,41),c(13,17,4)])
names(DEFLE_TABLE) <- c("Age","LE","DFLE","DepFLE", "LE", "DFLE","DepFLE")


stargazer(round(DEFLE_TABLE,3), summary=FALSE, rownames=FALSE)

# stargazer(round(Female_DFLE[c(1,16,31,41),c(1,4,13,17)],3), summary=FALSE, rownames=FALSE)

#### Plot

Male_DFLE <- Male_DFLE %>% select(Age, Year, ex,ex_i,ex_dep) %>% mutate(sex="Male")
names(Male_DFLE) <- c("Age","Year", "ex","ex_i","ex_dep", "sex")
Female_DFLE <- Female_DFLE %>% select(Age, Year, ex,ex_i,ex_dep) %>% mutate(sex="Female")
names(Female_DFLE) <- c("Age","Year", "ex","ex_i","ex_dep", "sex")

DFLE <- union(Male_DFLE,Female_DFLE)

# Females
DFLE_Plot <- DFLE %>% ggplot(aes(y=ex, x=Age, color="LE")) + geom_line() +
  geom_line(aes(y=ex_i, x=Age, color="DFLE")) +
  geom_line(aes(y=ex_dep, x=Age, color="DepFLE")) +
  scale_y_continuous(name = "Life Expectancy in Years") +
  scale_colour_manual(values = c("red", "orange", "black"), name="")     +
  facet_grid(.~sex) +
  theme_bw()
DFLE_Plot <- DFLE_Plot + theme(legend.position = c(0.85, 0.80)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))
DFLE_Plot
