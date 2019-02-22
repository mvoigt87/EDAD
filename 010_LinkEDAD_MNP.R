#### Linking Data Sources ####

rm(list = ls())
require(tidyverse)
require(data.table)
library(foreign)
source('https://raw.githubusercontent.com/bjornerstedt/assist/master/R/describe.R')

load('follow.up.RData')

load('EDAD2008Hogares.RData')
as.data.table(ed.hog) -> ed.hog
as.data.table(follow.up) -> follow.up

ed.hog2 <-   as.data.table(lapply(ed.hog, function(e) {if (class(e)=='labelled') {as_factor(e)} else {as.integer(e) } }) )

tolower(names(ed.hog2)) ->  names(ed.hog2)

glimpse(ed.hog2)
glimpse(follow.up)

follow.up[,nseccion:= as.integer(as.character(nseccion))]


# setkey(
setkey(ed.hog2,nseccion,dc,viv,hog,nord)
setkey(follow.up,nseccion,dc,viv,hog,nord)  

follow.up[,.N,estado]
#    estado      N
# 1:      A 183792
# 2:      B  23700
# 3:      N     37

names(ed.hog2)

merge(ed.hog2[,c(4:10),with=F],follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link
class(link)

link[,enlazado:=!is.na(estado)]


link[,.N,.(enlazado,estado)]
#    enlazado estado      N
# 1:    FALSE     NA  50658
# 2:     TRUE      A 183792
# 3:     TRUE      B  23700
# 4:     TRUE      N     37


link[,.N,.(enlazado,estado, gedad=cut(edad, c(0,10,20,30,50,65,110) ) )] -> pp
pp

dcast(pp, gedad+estado~enlazado, fun.aggregate = sum)

dcast(pp, gedad~enlazado, fun.aggregate = sum)

################################################
# Data of interest is hidden in another source #
# ---------------------------------------------#
################################################

EDAD.mayor = read.spss("TOTAL-EDAD-2008-Mayores.sav", to.data.frame=TRUE)

summary(EDAD.mayor$NumPersonas65)

summary(EDAD.mayor$EDAD) # people 65 plus


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

# select variables
head(names(EDAD.mayor), 1000)
tail(names(EDAD.mayor), 404)

names(EDAD.mayor)[6:10]<- c("nseccion", "dc", "viv", "hog", "nord")


# Select variables from the SPSS data set (working file Antonio and Julio)
merge(EDAD.mayor[,c(1,4:29, 53:76, 89:92, 94:104, 108, 114, 115, 120, 123, 125, 128, 133, 136, 141, 142, 147, 148, 153,
                    154, 156, 166, 171, 172, 174, 176, 178, 180, 185, 186, 191, 192, 197, 198, 203, 204, 209, 210, 215,
                    216, 221, 222, 227, 228, 233, 234, 239, 240, 245, 246, 251, 252, 257, 258, 263, 264, 269, 270, 275,
                    276, 281, 282, 287, 288, 293, 294, 299, 300, 305, 306, 311, 312, 317, 318, 320, 322, 324, 326, 328,
                    330, 332, 334, 336, 338, 340, 342:399, 407, 408, 410, 414, 455, 465, 470, 480, 485, 490, 495, 500,
                    505, 510, 515, 517, 522, 527:534, 552, 553:588, 601, 616:646, 648:656, 661, 672, 677:706, 711, 716,
                    717:738, 740, 741, 754, 758:763, 771, 772, 774, 778:780, 796, 801:836, 892:898, 900:918, 977:990,
                    1085, 1086, 1093, 1094, 1097:1118, 1250, 1277:1321, 1326, 1381:1404), with=F],
      follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link.may

class(link.may)
link.may[,enlazado:=!is.na(estado)]
link.may[,.N,.(enlazado,estado)]  

# Dependientes13, Dependientes13 Dependientes14 AsistenciaPersonal14
# DemandaCuidados14 EdadInicioDisca44 EdadUltimaDisca44


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


# Censored are all individuals without event before 31.12.2016

link.may[event==0, ':='(a_salida=2016, m_salida=12)]
# for the rest it is the year/month of death information
link.may[event==1, ':='(a_salida=anodef, m_salida=mesdef)]

# Quick check! Looks okay!
summary(link.may$a_salida)

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

# Little FIX
link.may <- link.may %>% mutate(EdadInicioDisca44 = as.numeric(EdadInicioDisca44))
link.may %>% count(EDAD < EdadInicioDisca44)

link.may %>% count(EDAD < Edadinicio_cuidado)

link.may <- link.may %>% filter(round(EdadInicioDisca44,0) < round(EDAD,0))  # 215 cases


# entry in disability (What does the 13 mean?)
# --------------------------------------------
str(link.may$EdadInicioDisca44)
summary(link.may$EdadInicioDisca44)


# --------------------------------------------
# save(link.may, file='010_mayor.link.RData')
# --------------------------------------------