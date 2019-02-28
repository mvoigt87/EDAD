## --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##  Linking the the INE Follow-Up with the EDAD 2008 base file
## --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


rm(list = ls())
require(tidyverse)
require(data.table)
library(foreign)
source('https://raw.githubusercontent.com/bjornerstedt/assist/master/R/describe.R')


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
### Prepare follow-up Data from INE
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

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

  # merge(ed.hog2[,c(4:10),with=F],follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link
  # class(link)
  # 
  # 
  # link[,enlazado:=!is.na(estado)]
  # link[,.N,.(enlazado,estado)]
  #    enlazado estado      N
  # 1:    FALSE     NA  50658
  # 2:     TRUE      A 183792
  # 3:     TRUE      B  23700
  # 4:     TRUE      N     37



#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
### Second Data Set to Link - EDAD 2008
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################



### Link the follow-up to another data source = EDAD (full population) to be able to extract the population 50 +


EDAD.mayor50 = read.spss("TOTAL-EDAD-2008.sav", to.data.frame=TRUE)
# A few checks with variables - to get a feeling for the number of transitions
EDAD.mayor50 <- data.table(EDAD.mayor50)
# select variables
head(names(EDAD.mayor50), 1000)
tail(names(EDAD.mayor50), 291)
# change variable names for merge process
names(EDAD.mayor50)[5:9]<- c("nseccion", "dc", "viv", "hog", "nord")

# Select variables from the SPSS data set (full population file working file Antonio and Julio)
merge(EDAD.mayor50[,c(1,4:29, 53:76, 89:92, 94:104, 108, 114, 115, 120, 123, 125, 128, 133, 136, 141, 142, 147, 148, 153,
                    154, 156, 166, 171, 172, 174, 176, 178, 180, 185, 186, 191:195, 197, 204, 205, 209, 210, 211, 215,
                    216, 217, 221, 246, 247, 251, 252, 253, 257, 258, 259, 263, 264, 265, 269, 276, 277, 281, 282, 283, 287, 
                    300, 301, 305, 306, 307, 311, 312, 313, 317, 342:399, 407, 408, 410, 414, 455, 465, 470, 480, 485, 490, 
                    495, 500, 505, 510, 515, 517, 522, 527:534, 552, 553:588, 601, 616:646, 648:656, 661, 672, 677:706, 711, 716,
                    717:738, 740, 741, 754, 758:763, 771, 772, 774, 778:780, 796, 801:836, 892:898, 900:918, 952:964, 977:990,
                    1085, 1086, 1093, 1094, 1097:1118, 1231,1250, 1277:1291), with=F],
      follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link.may50

### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###
### ------------------------------------------
### Prepare the data set for later use
    # 1. Set age limit to 50 +
    # 2. Extract the ones that were not linked
    # 3. Create an exit age variable
### ------------------------------------------

link.may50 <- link.may50 %>% filter(EDAD>=50)
class(link.may50)
link.may50 <- data.table(link.may50)
link.may50[,enlazado:=!is.na(estado)]
# Choose the linked ones and the ones with age information
link.may50 <- link.may50 %>% filter(!is.na(EDAD)) %>% filter(enlazado==T) %>%  ### 38985 individuals (65 +, followed up)
  # and make an event variable with the sure information we have (death year and month)
  mutate(event=ifelse(!is.na(anodef),1,0))

# Age at exit
# -----------
link.may50 <- data.table(link.may50)
link.may50[event==0, ':='(a_salida=2016, m_salida=12)]
# for the rest it is the year/month of death information
link.may50[event==1, ':='(a_salida=anodef, m_salida=mesdef)]

link.may50 <- link.may50 %>% 
  # first making the exit age smoother by adding the month and than substracting the first interview date (! find variable)
  mutate(age.ex = EDAD + (a_salida+(m_salida/12) - 2006.99))

hist(link.may50$age.ex, breaks = 50) # looks ok! (high increase at age 60 is caused by large numbers of censored 50 year old)

# Look at the entry to exit age relationship 
link.may50 %>% count(EDAD < age.ex)         # bien!

# Missing-ness on central variables (sex, onset age disability, onset age dependency)
# -----------------------------------------------------------------------------------

# Select the individuals which have experienced care episodes
link.may50 %>% count(!is.na(Edadinicio_cuidado))
link.may50 <- link.may50 %>% filter(!is.na(Edadinicio_cuidado))        ### Here we select the ones with care need!
# (5674 individuals)

# disability
# DISCA 44 - mix between functional limitations (de piel a dentro) and (I)ADLs (de piel a fuera)
link.may50 %>% count(!is.na(EdadInicioDisca44))
# DISCA 13 - selection of 8 ADLs and 5 IADLs
link.may50 %>% count(!is.na(EdadInicioDisca13))

# combinations
link.may50 <- data.table(link.may50)
link.may50[, .N, .(!is.na(EdadInicioDisca44))] # everybody 
link.may50[, .N, .(!is.na(EdadInicioDisca13))] # 133 cases
link.may50[!is.na(EdadInicioDisca44), .N, .(as.numeric(EdadInicioDisca44)>=50)] # 1466 cases before age 50
link.may50[!is.na(EdadInicioDisca13), .N, .(as.numeric(EdadInicioDisca13)>=50)] # 915 cases
link.may50[is.na(EdadInicioDisca13), .N, .(as.numeric(EdadInicioDisca44)>=50)] #  88 cases   (we might be able to recover)


link.may50[, .N, .(as.numeric(!is.na(EdadInicioDisca13))<=as.numeric(!is.na(Edadinicio_cuidado)))]  # todos

# Exclude the 133 cases that haven´t experienced a DISCA13 ADL limitations
# link.may50 <- link.may50 %>% filter(!is.na(EdadInicioDisca13))

class(link.may50$EdadInicioDisca13)
link.may50$EdadInicioDisca13 <- as.numeric(as.factor(link.may50$EdadInicioDisca13))

table(link.may50$EdadInicioDisca13>=50)

## Create a new DISCA 13 Entry AGE

table(link.may50$Disca13) # only 97 cases! (less than when using the age variable)

link.may50 <- link.may50 %>% 
  mutate(DIS_1_A = ifelse(!is.na(MOV_18_5), MOV_18_5, 999)) %>% 
  mutate(DIS_2_A = ifelse(!is.na(MOV_20_5), MOV_20_5, 999)) %>%
  mutate(DIS_3_A = ifelse(!is.na(MOV_21_5), MOV_21_5, 999)) %>% 
  mutate(DIS_4_A = ifelse(!is.na(MOV_22_5), MOV_22_5, 999)) %>% 
  mutate(DIS_5_A = ifelse(!is.na(AUT_27_5), AUT_27_5, 999)) %>% 
  mutate(DIS_6_A = ifelse(!is.na(AUT_28_5), AUT_28_5, 999)) %>% 
  mutate(DIS_7_A = ifelse(!is.na(AUT_29_5), AUT_29_5, 999)) %>% 
  mutate(DIS_8_A = ifelse(!is.na(AUT_30_5), AUT_30_5, 999)) %>% 
  mutate(DIS_9_A = ifelse(!is.na(AUT_32_5), AUT_32_5, 999)) %>% 
  mutate(DIS_10_A = ifelse(!is.na(AUT_33_5), AUT_33_5, 999)) %>% 
  mutate(DIS_11_A = ifelse(!is.na(VDOM_36_5), VDOM_36_5, 999)) %>% 
  mutate(DIS_12_A = ifelse(!is.na(VDOM_37_5), VDOM_37_5, 999)) %>% 
  mutate(DIS_13_A = ifelse(!is.na(VDOM_38_5), VDOM_38_5, 999)) %>% 
  
  ## Now compute the column minimum (gives the entry age to severity)
  mutate(DISCA13_AGE = pmin(DIS_1_A, DIS_2_A, DIS_3_A, DIS_4_A, DIS_5_A, DIS_6_A,
                            DIS_7_A, DIS_8_A, DIS_9_A, DIS_10_A, DIS_11_A, DIS_12_A,
                            DIS_13_A)) 
  

link.may50 <- link.may50 %>% filter(DISCA13_AGE>=50) %>% filter(DISCA13_AGE<999)

# -----------------------------------------------
# save(link.may50, file='010_mayor50.link.RData')
# -----------------------------------------------



################################################
################################################
################################################
################################################
################################################
################################################   From here on are only the cases 65 + !!!
################################################
################################################
################################################
################################################
################################################




################################################
# Data of interest is hidden in another source #
# ---------------------------------------------#
################################################

EDAD.mayor = read.spss("TOTAL-EDAD-2008-Mayores.sav", to.data.frame=TRUE)


# There is information on 44 possible disabilities - DISCA13 contains 13 "activities" (5 IADL + 8 ADL)
str(EDAD.mayor$EdadInicioDisca13)
# str(EDAD.mayor$EdadInicioDisca44)
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
EDAD.mayor[, .N, .(!is.na(EdadInicioDisca13))]
EDAD.mayor[, .N, .(!is.na(Edadinicio_cuidado))]
EDAD.mayor[, .N, .(as.numeric(EdadInicioDisca13)<as.numeric(Edadinicio_cuidado))] 

# Dependientes13, Dependientes13 Dependientes14 AsistenciaPersonal14
# DemandaCuidados14 EdadInicioDisca44 EdadUltimaDisca44

# select variables
head(names(EDAD.mayor), 1000)
tail(names(EDAD.mayor), 404)
# change variable names for merge process
names(EDAD.mayor)[6:10]<- c("nseccion", "dc", "viv", "hog", "nord")

# Select variables from the SPSS data set (working file Antonio and Julio)
merge(EDAD.mayor[,c(1,4:29, 53:76, 89:92, 94:104, 108, 114, 115, 120, 123, 125, 128, 133, 136, 141, 142, 147, 148, 153,
                    154, 156, 166, 171, 172, 174, 176, 178, 180, 185, 186, 191, 192, 197, 198, 203, 204, 209, 210, 215,
                    216, 221, 222, 227, 228, 233, 234, 239, 240, 245, 246, 251, 252, 257, 258, 263, 264, 269, 270, 275,
                    276, 281, 282, 287, 288, 293, 294, 299, 300, 305, 306, 311, 312, 317, 318, 320, 322, 324, 326, 328,
                    330, 332, 334, 336, 338, 340, 342:399, 407, 408, 410, 414, 455, 465, 470, 480, 485, 490, 495, 500,
                    505, 510, 515, 517, 522, 527:534, 552, 553:588, 601, 616:646, 648:656, 661, 672, 677:706, 711, 716,
                    717:738, 740, 741, 754, 758:763, 771, 772, 774, 778:780, 796, 801:836, 892:898, 900:918, 952:964, 977:990,
                    1085, 1086, 1093, 1094, 1097:1118, 1231,1250, 1277:1321, 1326, 1381:1404), with=F],
      follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link.may

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


# double-check age at different states #
# ------------------------------------ #

# ------------------------ #
# Entry age in 2008 (EDAD) #
# ------------------------ #

summary(link.may$EDAD)
hist(link.may$EDAD, nclass = 42, main = "", xlab = "Age")  # age 65 years seem to be overrepresented (also more 66)


### ------------
### AGE AT EXIT
### ------------
# some deaths are early in 2007 and lead to entry age = exit age problem)

link.may <- link.may %>% 
  # first making the exit age smoother by adding the month and than substracting the first interview date (! find variable)
  mutate(age.ex = EDAD + (a_salida+(m_salida/12) - 2006.99))

hist(link.may$age.ex) # looks ok!

# Look at the entry to exit age relationship - no cases with lower
link.may %>% count(EDAD < age.ex)   


# check the other ages - entry to dependency
# ------------------------------------------
str(link.may$Edadinicio_cuidado)
link.may$Edadinicio_cuidado <- as.numeric(link.may$Edadinicio_cuidado)
summary(link.may$Edadinicio_cuidado)

link.may %>% count(Edadinicio_cuidado>=50)


# --------------------------------------------
# save(link.may, file='010_mayor.link.RData')
# --------------------------------------------

