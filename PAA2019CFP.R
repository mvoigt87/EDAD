# Prepare a few descriptive plots and statistics for the PAA proposal

library(tidyverse)
library(data.table)
library(foreign)
library(survival)
library(broom)

# 0.1 load data
# --------------

# EDAD households and follow up data

load('follow.up.RData')
# 
# load('EDAD2008Hogares.RData')
# 
# # make datatable functions available
# as.data.table(ed.hog) -> ed.hog
as.data.table(follow.up) -> follow.up
# # use the labels
# ed.hog2 <-   as.data.table(lapply(ed.hog, function(e) {if (class(e)=='labelled') {as_factor(e)} else {as.integer(e) } }) )
# tolower(names(ed.hog2)) ->  names(ed.hog2)
# 
# # 1. Extract variables and make a working dataset
# # -----------------------------------------------
# 
# # see the two data sets
# glimpse(ed.hog2)
# 
glimpse(follow.up)
follow.up[,nseccion:= as.integer(as.character(nseccion))]

# extract variables for
# names(ed.hog2)
# 
# merge(ed.hog2[,c(4:10,17:22,27),with=F],follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link
# class(link)
# 
# link[,enlazado:=!is.na(estado)]
# 
# # 2. Stats on disability in the linked data
# table(link$limit)
# table(link$certmi)
# table(link$dislim)
# 
# link %>% dplyr::mutate(tt = ifelse(limit!="No",TRUE,FALSE)) %>% dplyr::count(tt)
#   # 1 FALSE 232551
#   # 2 TRUE   25636
# link <- link %>% mutate(limbo = ifelse(limit!="No",1,0))

################################################
# Data of interest is hidden in another source #
# ---------------------------------------------#
################################################

EDAD.mayor = read.spss("TOTAL-EDAD-2008-Mayores.sav", to.data.frame=TRUE)

summary(EDAD.mayor$NumPersonas65)

summary(EDAD.mayor$EDAD) # people 65 plus

# the ones with death information (!BAJA does not mean death!)
summary(link.may$anodef)

# There is information on 44 possible disabilities
str(EDAD.mayor$EdadInicioDisca13)
str(EDAD.mayor$EdadInicioDisca44)
# And the year of onset of dependency
str(EDAD.mayor$Edadinicio_cuidado)

# see differences between different entry ages (disabilities 13-19-44)
par(mfrow=c(1,3))
hist(as.numeric(EDAD.mayor$EdadInicioDisca44))
hist(as.numeric(EDAD.mayor$EdadInicioDisca19))
hist(as.numeric(EDAD.mayor$EdadInicioDisca13))
par(mfrow=c(1,1))

# A few checks with variables - to get a feeling for the number of transitions
EDAD.mayor <- data.table(EDAD.mayor)
EDAD.mayor[, .N, .(!is.na(EdadInicioDisca44))]
EDAD.mayor[, .N, .(!is.na(Edadinicio_cuidado))]
EDAD.mayor[, .N, .(as.numeric(EdadInicioDisca44)<as.numeric(Edadinicio_cuidado))] 
  # ! about 6000 cases we can work with of which 4500 experience a transition from one state to other

# compare to last age
summary(as.numeric(EDAD.mayor$EdadInicioDisca44))
summary(as.numeric(EDAD.mayor$EdadUltimaDisca44)) # could be


    # Dependientes13, Dependientes13 Dependientes14 AsistenciaPersonal14
    # DemandaCuidados14 EdadInicioDisca44 EdadUltimaDisca44

# select variables
head(names(EDAD.mayor), 1000)
tail(names(EDAD.mayor), 404)
# change variable names for merge process
names(EDAD.mayor)[6:10]<- c("nseccion", "dc", "viv", "hog", "nord")

merge(EDAD.mayor[,c(1,4:12,17:29, 53:76, 89:92, 885:898, 960:963, 1321:1329, 1381:1404),with=F],follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link.may
class(link.may)
link.may[,enlazado:=!is.na(estado)]
link.may[,.N,.(enlazado,estado)]    
# from the 6568 who are not linked, 2.127 are deaths which occurred in 2017 
    # "El resto son, bien emigraciones, bien limpiezas del padrón o bien otras causas. 
    # Los otros 469 (=6.963-6.494) son registros para los que tendríamos que analizar la casuística 
    # (a veces han podido modificarse los identificadores por distintas razones a lo largo de los años

# now reduce to the ones which are in the original data (65+)
table(link.may$enlazado)


# Choose the linked ones and the ones with age information
link.may <- link.may %>% filter(!is.na(EDAD)) %>% filter(enlazado==T) %>%  ### 38985 individuals (65 +, followed up)
# and make an event variable with the sure information we have (death year and month)
mutate(event=ifelse(!is.na(anodef),1,0))
# !This will censor everybody after 31.12.2016 - even if some information is available

# time to death or censor var                                        mutate(age.d = EDAD + (2008.5 - (a_salida)))
link.may <- data.table(link.may)
link.may[estado=='B',.N ,keyby=.(Con.ANODEF=(!is.na(anodef)),  con.CAUSA = (!is.na(causa)))]
  ## 1783 (11.74%) def sin causa y fecha
# Impute a death date for the ones without assigned date but information on state at exit ("B")
summary(link.may$a_salida) # muy pocos! only the ones who left?

# Check the "bajas"
link.may %>% count(estado=='B' & is.na(anodef) & is.na(a_salida)) # 1689 bajas don´t have a year of death or exit from the survey
link.may %>% count(estado=='B' & !is.na(a_salida))
link.may %>% count(estado=='B' & !is.na(a_salida) & is.na(causa)) # 94 have a year of exit but no cause (death, censor)

# See if that is in line with the INE email
link.may %>% count(event==1 & is.na(a_salida)) # Año de salida does not coincide with death year
link.may %>% count(event==1 & is.na(causa)) # 0 cases = which encourages one to censor at 12/2016


#########################################################################################################################
#########################################################################################################################
### For now this is out commented as have no further information on the bajas in 2017
#########################################################################################################################
#########################################################################################################################
      ### all possible ways I can think of over the top the head to impute the date from one of the two situations
      ### ---------------------------------------------------------------------------------------------------------
      # 
      # # If there is neither a death date nor an exit date but the "baja" information, we approximate by the medium time
      # link.may[estado=='B' & is.na(anodef) & is.na(a_salida), ':='(anodef=2013, mesdef=1, a_salida=2013, m_salida=1)]
      # # If there is a death date this will become the exit date (absorbing state)
      # link.may[estado=='B' & !is.na(anodef) & !is.na(causa), ':='(a_salida=anodef, m_salida=mesdef)]
      # # If there is a age at death information but no exit year
      # link.may[estado=='B' & !is.na(a_salida), ':='(anodef=a_salida, mesdef=m_salida)] #  ??? - !is.na(causa)
      
      #  link.may %>% count(estado=='B')
      # link.may %>% count(estado=='B' & !is.na(a_salida)) # ok! Same number (assuming everyone has an exit year)
      # # Assign a censoring date for censored cases
      # # for now: last month of 2017 where event happened
      # max(link.may$m_salida[link.may$estado=='B' & link.may$a_salida==2017]) # Last month = May

#########################################################################################################################
#########################################################################################################################

# For now the analysis is censored at 31.12.2016

# Censored are all individuals without event before 31.12.2016

link.may[event==0, ':='(a_salida=2016, m_salida=12)]
# for the rest it is the year/month of death information
link.may[event==1, ':='(a_salida=anodef, m_salida=mesdef)]

# Quick check! Looks okay!
summary(link.may$a_salida)

# For earlier problems
  # head(link.may[is.na(link.may$a_salida)]) # this person is censored at the end but does not any other time information
  # link.may[is.na(a_salida), ':='(a_salida=2017, m_salida=5)]  # impute censoring date

# double-check age at different states
######################################

# Entry age in 2008 (EDAD)
# ------------------------
summary(link.may$EDAD)
hist(link.may$EDAD, nclass = 42, main = "", xlab = "Age")  # age 65 years seem to be overrepresented (also more 66)
# --- assumption: ??

# creating an exit age variable based on a_salida (2007 as first year for data collection - 
# some deaths are early in 2007 and lead to entry age = exit age problem)

### ------------
### AGE AT EXIT
### ------------

link.may <- link.may %>% 
  # first making the exit age smoother by adding the month and than substracting the first interview date (! find variable)
  mutate(age.ex = EDAD + (a_salida+(m_salida/12) - 2006.99))

hist(link.may$age.ex) # looks ok!

# Look at the entry to exit age relationship - no cases with lower
link.may %>% count(EDAD < age.ex)   


# entry in disability (What does the 13 mean?)
# --------------------------------------------
str(link.may$Edadinicio_disca44)
link.may$Edadinicio_disca44 <- as.numeric(link.may$Edadinicio_disca44)
summary(link.may$Edadinicio_disca44)

link.may %>% count(Edadinicio_disca44>=65)


# check the other ages - entry to dependency
# ------------------------------------------
str(link.may$Edadinicio_cuidado)
link.may$Edadinicio_cuidado <- as.numeric(link.may$Edadinicio_cuidado)
summary(link.may$Edadinicio_cuidado)

link.may %>% count(Edadinicio_cuidado>=65)

  # # possible end of disability
  # link.may$EdadFinDiscal13 <- as.numeric(link.may$EdadFinDiscal13)
  # summary(link.may$EdadFinDiscal13) # No hay nadie!

# --------------------------------------------
# save(link.may, file='010_mayor.link.RData')
# --------------------------------------------

# rm(ed.hog, ed.hog2, EDAD.mayor, follow.up)
