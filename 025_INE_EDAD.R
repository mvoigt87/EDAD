### EDAD compare

# 1. Run 020_Link_EDAD08... code

library(Epi)

## Some necessary checks

# age
hist(link$edad)

# cause of exit
link %>% count(estado)

#     A      B      N   NA
# 183792  23700     37  50658

# age
link %>% ggplot(aes(x=edad,fill=estado)) + geom_histogram() + theme_bw()  # interesting structure of the missings!

# leaving
link %>% ggplot(aes(x=a_salida)) + geom_histogram() + theme_bw()

# death/bajas
link %>% ggplot(aes(x=anodef)) + geom_histogram() + theme_bw()

link[estado=='B',.N ,keyby=.(Con.ANODEF=(!is.na(anodef)),  con.CAUSA = (!is.na(causa)))] 
## 6963 (41%) bajas sin causa y fecha # deaths in 2017

# Impute a death date for the ones without assigned date but defuncion
summary(link$a_salida)

link %>% count(estado=='B' & !is.na(anodef))  # 16738 bajas with confirmed death dates (1 more than in the other count)
link %>% count(estado=='B' & !is.na(a_salida)) # 3070 bajas with exit age = emigration?
link %>% count(estado=='B' & is.na(anodef) & is.na(a_salida)) # 3893 bajas don´t have a year of death or end of the survey

# link %>% count(estado=='B' & !is.na(a_salida) & is.na(causa))

### all possible ways I can think of over the top the head to impute the date from one of the two situations

# impute middle of the observation period
link[estado=='B' & is.na(anodef) & is.na(a_salida), ':='(anodef=2012, mesdef=6, a_salida=2012, m_salida=6)]

link[estado=='B' & !is.na(anodef) & !is.na(causa), ':='(a_salida=anodef, m_salida=mesdef)]

# link[estado=='B' & !is.na(a_salida), ':='(anodef=a_salida, mesdef=m_salida)] #  ??? - !is.na(causa)

# Assign a censoring date for the ones who were censored
  # for now: end of 2017
link %>% count(estado=='B')                    # 23700
link %>% count(estado=='B' & !is.na(a_salida)) # ok!

# Get rid of the one without follow up (about 50.000 cases)
# link[estado!='N'] -> link

## censored individuals
link[estado=='A', ':='(a_salida=2016, m_salida=12)]


summary(link$a_salida)



# create a date of birth
link.fw <- subset(link,enlazado==TRUE) %>% filter(estado!="N")
link.fw %>% ggplot(aes(x=edad,fill=estado)) + geom_histogram() + theme_bw() # deaths amounted at the last years

class(link.fw)
link.fw <- data.table(link.fw)

# create a birth data with random month (sample)
# ----------------------------------------------
n11 <- length(link.fw$nseccion)
set.seed(101)
# dob
link.fw[,dob:=as.IDate(paste0(2008-edad-1,'-',sample(1:12,n11,replace = T),'-15'))]

# exit date
# ---------
link.fw[,dout:= as.IDate(ifelse(estado=='B',ifelse(!is.na(anodef),
                                                  paste0(anodef,'-',mesdef,'-15'),
                                                  paste0(a_salida,'-',m_salida,'-','-15')),'2016-12-31'))]

# cause count
link.fw %>% count(causa)

### Lexis package - create data with and without

Lcoh.sin.B <- Lexis( entry = list( per=rep(2008,n11), age= 2008-cal.yr(dob)),
               exit = list(per=cal.yr(dout)),
               entry.status = rep(0,n11),
               exit.status = ifelse(!is.na(causa),1,0),
               data = link.fw )



Lcoh.con.B <- Lexis( entry = list( per=rep(2008,n11), age= 2008-cal.yr(dob)),
                     exit = list(per=cal.yr(dout)),
                     entry.status = rep(0,n11),
                     exit.status = ifelse(estado=="B",1,0),
                     data = link.fw )


###  Calculate person years and events by lexis triangle

# Events are only "bajas" with cause of death
splitLexis(Lcoh.sin.B,breaks = seq(20,110,by=5), time.scale = 'age') -> edad.1
edad.1$age.g <- timeBand(edad.1, time.scale='age', type="middle") 
as.data.table(edad.1) -> edad.1
edad.1 <- data.table(edad.1)

# Events are all "bajas"
splitLexis(Lcoh.con.B,breaks = seq(20,110,by=5), time.scale = 'age') -> edad.2
edad.2$age.g <- timeBand(edad.2, time.scale='age', type="middle") 
as.data.table(edad.2) -> edad.2
edad.2 <- data.table(edad.2)

# Extract mortality rates

# sin
edad.1[,.(event=sum(lex.Xst), PY=sum(lex.dur)),keyby=.(sexo,age.g)] -> resu.1
resu.1[ ,mortality.rate:=event/PY]
resu.1 <- resu.1 %>% mutate(dat = as.factor("EDAD"))
# con
edad.2[,.(event=sum(lex.Xst), PY=sum(lex.dur)),keyby=.(sexo,age.g)] -> resu.2
resu.2[ ,mortality.rate:=event/PY]
resu.2 <- resu.2 %>% mutate(dat = as.factor("EDAD"))


# INE death rates
# ---------------
INE2014 <- read.csv("INE2014_tasasdemortalidad.csv", sep=",", header = T)

# Notas:
# Las tasas de mortalidad y riesgos de muerte están expresados en tanto  por mil.     
# La tasa de mortalidad, el promedio de años vividos el último año de  vida , el riesgo de muerte, las defunciones teóricas y  la población  estacionaria de edad 100 están referidas al grupo de edad de 100 o  más años.               
# El promedio de años vividos el último año de vida en el grupo de edad  de 100 y más años corresponde al promedio de años vividos con 100  años cumplidos.
# Fuente: Instituto Nacional de Estadística

# Quick changes for obtaining the graphs

INE2014.h <- INE2014[1:101,] %>% mutate(sexo="Varón") %>% mutate(mortality.rate=Tasa.de.mortalidad/1000) %>% 
  select(-X, -Tasa.de.mortalidad) %>% 
  # taking advantage of the data structure
  mutate(age.g = seq(0,100,1)) %>% select(-age)
INE2014.m <- INE2014[102:202,] %>% mutate(sexo="Mujer") %>% mutate(mortality.rate=Tasa.de.mortalidad/1000) %>% 
  select(-X, -Tasa.de.mortalidad)%>% 
  # taking advantage of the data structure
  mutate(age.g = seq(0,100,1)) %>% select(-age)

INE2014 <- union(INE2014.h,INE2014.m) %>% select(-year) %>% mutate(dat = as.factor("INE 2014"))
rm(INE2014.h, INE2014.m)

head(INE2014[order(INE2014[,1], INE2014[,2]), ])

### 
RESU.UNITE.A <-resu.1 %>% select(-event, - PY) %>% union(INE2014)

RESU.UNITE.B <-resu.2 %>% select(-event, - PY) %>% union(INE2014)

### The graph

INE.C <- RESU.UNITE.A %>% filter(age.g < 100 & age.g > 20 ) %>% 
  ggplot(aes(x=age.g, y=mortality.rate, color=sexo, linetype=dat)) +
  geom_line() +
  # geom_line(aes(age, tdm, colour=sex), INE2011) +
  scale_y_log10(name="Tasas de mortalidad en una escala logarítmica")  +
  scale_linetype_discrete(name=" ") +
  scale_x_continuous(name = "Edad") +
  ggtitle('Mortalidad de la cohorte de EDAD e INE 2014, por sexo', 
          subtitle = 'Tasas de mortalidad 2014') +
  theme_bw() +
  theme(legend.position="none")

INE.D <- RESU.UNITE.B %>% filter(age.g < 100 & age.g > 20 ) %>% 
  ggplot(aes(x=age.g, y=mortality.rate, color=sexo, linetype=dat)) +
  geom_line() +
  # geom_line(aes(age, tdm, colour=sex), INE2011) +
  scale_y_log10(name=" ")  + # name="Tasas de mortalidad en una escala logarítmica"
  scale_linetype_discrete(name="Fuente") +
  scale_x_continuous(name = "Edad") +
  scale_color_discrete(name="Sexo") +
  ggtitle(' ', 
          subtitle = 'Tasas de mortalidad 2014 - con personas sin causa') +
  theme_bw()
INE.D <- INE.D + theme(legend.position = c(0.85, 0.25))

multiplot(INE.C,INE.D)


############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

### 

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

# Little FIX
link.may %>% count(EDAD < EdadInicioDisca44)
link.may %>% count(EDAD < Edadinicio_cuidado)

link.may <- link.may %>% filter(round(EdadInicioDisca44,0) < round(EDAD,0))  # 215 cases


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

