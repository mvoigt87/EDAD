# Prepare a few descriptive plots and statistics for the PAA proposal

library(tidyverse)
library(data.table)
library(foreign)
library(survival)

# 0.1 load data
# --------------

# EDAD households and follow up data

load('follow.up.RData')

load('EDAD2008Hogares.RData')

# make datatable functions available
as.data.table(ed.hog) -> ed.hog
as.data.table(follow.up) -> follow.up
# use the labels
ed.hog2 <-   as.data.table(lapply(ed.hog, function(e) {if (class(e)=='labelled') {as_factor(e)} else {as.integer(e) } }) )
tolower(names(ed.hog2)) ->  names(ed.hog2)

# 1. Extract variables and make a working dataset
# -----------------------------------------------

# see the two data sets
glimpse(ed.hog2)

glimpse(follow.up)
follow.up[,nseccion:= as.integer(as.character(nseccion))]

# extract variables for
names(ed.hog2)

merge(ed.hog2[,c(4:10,17:22,27),with=F],follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link
class(link)

link[,enlazado:=!is.na(estado)]

# 2. Stats on disability in the linked data
table(link$limit)
table(link$certmi)
table(link$dislim)

link %>% dplyr::mutate(tt = ifelse(limit!="No",TRUE,FALSE)) %>% dplyr::count(tt)
  # 1 FALSE 232551
  # 2 TRUE   25636
link <- link %>% mutate(limbo = ifelse(limit!="No",1,0))

################################################
# Data of interest is hidden in another source #
# ---------------------------------------------#
################################################

EDAD.mayor = read.spss("TOTAL-EDAD-2008-Mayores.sav", to.data.frame=TRUE)

summary(EDAD.mayor$NumPersonas65) # people 65 plus

str(EDAD.mayor$EdadInicioDisca44)

str(EDAD.mayor$Edadinicio_cuidado)

# see differences between different entry ages
par(mfrow=c(1,3))
hist(as.numeric(EDAD.mayor$EdadInicioDisca44))
hist(as.numeric(EDAD.mayor$EdadInicioDisca19))
hist(as.numeric(EDAD.mayor$EdadInicioDisca13))
par(mfrow=c(1,1))

# A few checks with variables which could be right
EDAD.mayor <- data.table(EDAD.mayor)
EDAD.mayor[, .N, .(!is.na(EdadInicioDisca13))]
EDAD.mayor[, .N, .(!is.na(Edadinicio_cuidado))]
EDAD.mayor[, .N, .(as.numeric(EdadInicioDisca13)<as.numeric(Edadinicio_cuidado))]

    # Dependientes13, Dependientes13 Dependientes14 AsistenciaPersonal14
    # DemandaCuidados14 EdadInicioDisca44 EdadUltimaDisca44


head(names(EDAD.mayor), 20)
names(EDAD.mayor)[6:10]<- c("nseccion", "dc", "viv", "hog", "nord")

merge(EDAD.mayor[,c(1,4:12,17:23,29,885:898, 960:963, 1326:1329, 1371:1380),with=F],follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link.may
class(link.may)
link.may[,enlazado:=!is.na(estado)]
link.may[,.N,.(enlazado,estado)]

# now reduce to the ones which are in the original data (65+)
table(link.may$enlazado)

link.may <- link.may %>% filter(!is.na(EDAD)) %>% filter(enlazado==T) %>%  ### 38985 individuals (65 +, followed up)
# event variable
mutate(event=ifelse(estado=="B", 1,0))

# time to death or censor var                                        mutate(age.d = EDAD + (2008.5 - (a_salida)))
link.may <- data.table(link.may)
link.may[estado=='B',.N ,keyby=.(Con.ANODEF=(!is.na(anodef)),  con.CAUSA = (!is.na(causa)))] ## 1783 (11.74%) def sin causa y fecha

# Impute a death date for the ones without assigned date but defuncion
summary(link.may$a_salida)

link.may %>% count(estado=='B' & is.na(anodef) & is.na(a_salida)) # 1689 bajas don´t have a year of death or end of the survey
link.may %>% count(estado=='B' & !is.na(a_salida) & is.na(causa)) # 94

### all possible ways I can think of over the top the head to impute the date from one of the two situations

# impute middle of the observation period
link.may[estado=='B' & is.na(anodef) & is.na(a_salida), ':='(anodef=2013, mesdef=1, a_salida=2013, m_salida=1)]

link.may[estado=='B' & !is.na(anodef) & !is.na(causa), ':='(a_salida=anodef, m_salida=mesdef)]

link.may[estado=='B' & !is.na(a_salida), ':='(anodef=a_salida, mesdef=m_salida)] #  ??? - !is.na(causa)

# Assign a censoring date for the ones who were censored
# for now: end of 2017
link.may %>% count(estado=='B')
link.may %>% count(estado=='B' & !is.na(a_salida)) # ok!
## censored individuals
link.may[estado=='A', ':='(a_salida=2017, m_salida=1)]

summary(link.may$a_salida) # 1 special case

head(link.may[is.na(link.may$a_salida)]) # Estado N
link.may <- link.may %>% filter(!is.na(a_salida))

# age at states
link.may <- link.may %>% mutate(age.d = EDAD + (2008.5 - (a_salida)))
hist(link.may$age.d) # looks ok!
# check the other ages
str(link.may$Edadinicio_cuidado)

# entry in disability
str(link.may$EdadInicioDisca13)
link.may$EdadInicioDisca13 <- as.numeric(link.may$EdadInicioDisca13)
summary(link.may$EdadInicioDisca13)

  # # possible end of disability
  # link.may$EdadFinDiscal13 <- as.numeric(link.may$EdadFinDiscal13)
  # summary(link.may$EdadFinDiscal13) # No hay nadie!

# 3. Few discriptive plots on different states
# --------------------------------------------

data1 <- myeloid
 data1$crstat <- factor(with(data1, ifelse(is.na(crtime), death, 2)),
                         labels=c("censor", "death", "CR"))
 data1$crtime <- with(data1, ifelse(crstat=="CR", crtime, futime))
 data1$txstat <- factor(with(data1, ifelse(is.na(txtime), death, 2)),
                         labels=c("censor", "death", "transplant"))
 data1$txtime <- with(data1, ifelse(txstat=="transplant", txtime, futime))
 for (i in c("futime", "crtime", "txtime", "rltime"))
  data1[[i]] <- data1[[i]] * 12/365.25 #rescale to months

# Define different states
temp <- link.may
class(temp)
paa2 <- tmerge(link.may[, c('Id')], temp,
                id=Id, death=event(age.d, event),
                disability = event(EdadInicioDisca13),
                dependency = event(Edadinicio_cuidado),
                recover = event(EdadFinDiscal13),
                priordis = tdc(EdadInicioDisca13),
                priordep = tdc(Edadinicio_cuidado))

