### Compare age-specific death rates from the linked data with the official death rates from INE

# 0.1. packages
require(data.table)
library(tidyverse)
require(Epi)
rm(list = ls())

# INE death rates
# ---------------
INE2011 <- read.csv("tasas2011_INE.csv", sep=",", header = T)

# Notas:
# Las tasas de mortalidad y riesgos de muerte están expresados en tanto  por mil.     
# La tasa de mortalidad, el promedio de años vividos el último año de  vida , el riesgo de muerte, las defunciones teóricas y  la población  estacionaria de edad 100 están referidas al grupo de edad de 100 o  más años.               
# El promedio de años vividos el último año de vida en el grupo de edad  de 100 y más años corresponde al promedio de años vividos con 100  años cumplidos.
# Fuente: Instituto Nacional de Estadística

# Quick changes for obtaining the graphs

INE2011.t <- INE2011[1:101,] %>% mutate(Sexo="ambos") %>% mutate(mortality.rate=Tasa.de.mortalidad/1000) %>% select(-X, -Tasa.de.mortalidad)
INE2011.h <- INE2011[102:202,] %>% mutate(Sexo="Hombre") %>% mutate(mortality.rate=Tasa.de.mortalidad/1000) %>% select(-X, -Tasa.de.mortalidad)
INE2011.m <- INE2011[203:303,] %>% mutate(Sexo="Mujer") %>% mutate(mortality.rate=Tasa.de.mortalidad/1000) %>% select(-X, -Tasa.de.mortalidad)

INE2011 <- union(INE2011.h,INE2011.m)
rm(INE2011.t, INE2011.h, INE2011.m)

head(INE2011[order(INE2011[,1], INE2011[,2]), ])

# for comparison, death rates should be at the same scale

INE2011 <- INE2011 %>% filter(age<100 & age>20)

     
# link data
# ---------

load(file='010_data.exploratory.RData')

n11<- length(ens11b$idi)
set.seed(100)
# create a date of birth
ens11b[,dob:=as.IDate(paste0(2011-Edad-1,'-',sample(1:12,n11,replace = T),'-15'))]

# estados (B=Baja (por fallecimiento), A=Asignar fecha de censura (31.12.2016), N=missing date (padronal))
table(ens11b$ESTADO)
# A     B     N 
# 17624  1787  1596 

ens11b[ESTADO=='B',.N ,keyby=.(Con.ANODEF=(!is.na(ANODEF)),  con.CAUSA = (!is.na(CAUSA)))] ## 533 def sin causa y fecha
# suspected unobserved deaths in the group lost in the process (N)
hist(ens11b$Edad[ens11b$ESTADO=='N']) # the age structure seems to be a little too old
ens11b[ESTADO=='N', c(1:2,5:6,8,11,12,13)]

##
ens11b[ESTADO!='N'] -> ens11b # Elimina los sin seguimiento
ens11b[,.N,ESTADO]

### Imputación grosera de fecha de censura: mitad del periodo de seguimiento
ens11b[ESTADO=='B' & is.na(ANODEF) & is.na(A_SALIDA), ':='(A_SALIDA=2014, M_SALIDA=1)]

cbind(names(ens11b))

nn<-length(ens11b$idh)

# middle of the year in case the month is missing
ens11b[ESTADO=='B',.N,M_SALIDA]
ens11b[,dout:= as.IDate(ifelse(ESTADO=='B',ifelse(!is.na(ANODEF),
                                                  paste0(ANODEF,'-',MESDEF,'-15'),
                                                  paste0(A_SALIDA,'-','07','-15')),'2016-12-31'))]
ens11b[is.na(dout)]  # 0 ok

# use Lexis command to generate age specific death rates
Lexis(entry = list(per= rep(2011,nn), age= 2011-cal.yr(dob)),
      exit = list(per= cal.yr(dout)),
      entry.status = rep(0,nn),
      exit.status = ifelse(!is.na(CAUSA),1,0),
      data=ens11b[,c(1:6,9,10,13,14)])   -> s11
s11$ENS<- 'ENS 2011'

###  Split PT y eventos 
splitLexis(s11,breaks = seq(20,110,by=5), time.scale = 'age') -> ens.s
ens.s$age.g <- timeBand(ens.s, time.scale='age', type="middle") 
as.data.table(ens.s) -> ens.s

###------------------------------------------------
### Estimar tasas: por sexo y año de la encuesta 
###------------------------------------------------
ens.s[,.(event=sum(lex.Xst), PY=sum(lex.dur)),keyby=.(ENS,Sexo,age.g)] -> resu
resu[ ,mortality.rate:=event/PY]


### Plot Mortality rates from the linked data
names(INE2011)[1] <- "age.g"
INE2011$age.g <- as.numeric(INE2011$age.g)
INE2011 <- INE2011 %>% mutate(ENS="INE 2011")

resu.g <- resu %>% select(-event, - PY) %>% union(INE2011)

INE.A <- resu.g %>% filter(age.g < 100 & age.g > 20 ) %>% 
  ggplot(aes(x=age.g, y=mortality.rate, color=Sexo, linetype=ENS)) +
  geom_line() +
 # geom_line(aes(age, tdm, colour=sex), INE2011) +
  scale_y_log10(name="Tasas de mortalidad en una escala logarítmica")  +
  scale_linetype_discrete(name=" ") +
  scale_x_continuous(name = "Edad") +
  ggtitle('Mortalidad de la cohorte de la ENS de 2011 e INE 2011, por sexo', 
          subtitle = 'Segumiento 2011: [2011-2017).; INE Tasas de mortalidad 2011 ') +
  theme_bw() +
  theme(legend.position="none")


#####################################################################################################
#####################################################################################################
#####################################################################################################

### Second plot - assuming all bajas are deaths

# use Lexis command to generate age specific death rates

# Change event condition to "= b"
Lexis(entry = list(per= rep(2011,nn), age= 2011-cal.yr(dob)),
      exit = list(per= cal.yr(dout)),
      entry.status = rep(0,nn),
      exit.status = ifelse(ESTADO=="B",1,0),
      data=ens11b[,c(1:6,9,10,13,14)])   -> s11.b
s11.b$ENS<- 'ENS 2011'

###  Split PT y eventos 
splitLexis(s11.b,breaks = seq(20,110,by=5), time.scale = 'age') -> ens.s.b
ens.s.b$age.g <- timeBand(ens.s.b, time.scale='age', type="middle") 
as.data.table(ens.s.b) -> ens.s.b


ens.s.b[,.(event=sum(lex.Xst), PY=sum(lex.dur)),keyby=.(ENS,Sexo,age.g)] -> resu.2
resu.2[ ,mortality.rate:=event/PY]


### Plot Mortality rates from the linked data again - THIS time they should match in old ages
###  and theoretically be overrepresenting deaths at younger (migration) ages


resu.g.2 <- resu.2 %>% select(-event, - PY) %>% union(INE2011)

INE.B <- resu.g.2 %>% filter(age.g < 100 & age.g > 20 ) %>% 
  ggplot(aes(x=age.g, y=mortality.rate, color=Sexo, linetype=ENS)) +
  geom_line() +
  # geom_line(aes(age, tdm, colour=sex), INE2011) +
  scale_y_log10(name=" ")  + # name="Tasas de mortalidad en una escala logarítmica"
  scale_linetype_discrete(name="Fuente") +
  scale_x_continuous(name = "Edad") +
  ggtitle(' ', 
          subtitle = 'Segumiento 2011 (más casos sin fecha): [2011-2017).; INE Tasas de mortalidad 2011 ') +
  theme_bw()
  INE.B <- INE.B + theme(legend.position = c(0.85, 0.25))

multiplot(INE.A,INE.B)



#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

#########
# 2014
#########

# INE death rates
# ---------------
INE2014 <- read.csv("INE2014_tasasdemortalidad.csv", sep=",", header = T)

# Notas:
# Las tasas de mortalidad y riesgos de muerte están expresados en tanto  por mil.     
# La tasa de mortalidad, el promedio de años vividos el último año de  vida , el riesgo de muerte, las defunciones teóricas y  la población  estacionaria de edad 100 están referidas al grupo de edad de 100 o  más años.               
# El promedio de años vividos el último año de vida en el grupo de edad  de 100 y más años corresponde al promedio de años vividos con 100  años cumplidos.
# Fuente: Instituto Nacional de Estadística

# Quick changes for obtaining the graphs

INE2014.h <- INE2014[1:101,] %>% mutate(Sexo="Hombre") %>% mutate(mortality.rate=Tasa.de.mortalidad/1000) %>% 
  select(-X, -Tasa.de.mortalidad) %>% 
  # taking advantage of the data structure
  mutate(age.g = seq(0,100,1)) %>% select(-age)
INE2014.m <- INE2014[102:202,] %>% mutate(Sexo="Mujer") %>% mutate(mortality.rate=Tasa.de.mortalidad/1000) %>% 
  select(-X, -Tasa.de.mortalidad)%>% 
  # taking advantage of the data structure
  mutate(age.g = seq(0,100,1)) %>% select(-age)

INE2014 <- union(INE2014.h,INE2014.m)
rm(INE2014.h, INE2014.m)

head(INE2014[order(INE2014[,1], INE2014[,2]), ])



### Prepare ENS data
### ----------------

ens14b[ESTADO=='B',.N ,keyby=.(Con.ANODEF=(!is.na(ANODEF)),  con.CAUSA = (!is.na(CAUSA)))] ## 
n14<- length(ens14b$idi)  
ens14b[,dob:=as.IDate(paste0(2014-Edad-1,'-',sample(1:12,n14,replace = T),'-15'))] ## a 1-1-2014 

ens14b[ESTADO!='N'] -> ens14b  # Elimina los sin seguimiento
### Imputación grosera de fecha de censura: mitad del periodo de seguimiento
ens14b[ESTADO=='B' & is.na(ANODEF) & is.na(A_SALIDA), ':='(A_SALIDA=2015, M_SALIDA=7)]
ens14b[ESTADO=='B',.N,M_SALIDA]

ens14b[,dout:= as.IDate(ifelse(ESTADO=='B',ifelse(!is.na(ANODEF),
                                                  paste0(ANODEF,'-',MESDEF,'-15'),
                                                  paste0(A_SALIDA,'-','07','-15')),'2016-12-31'))]
ens14b[is.na(dout)]

### ----------------------------------------------------------------- ###

nn<-length(ens14b$idh)
Lexis(entry = list(per= rep(2014,nn), age= 2014-cal.yr(dob)),
      exit = list(per= cal.yr(dout)),
      entry.status = rep(0,nn),
      exit.status = ifelse(!is.na(CAUSA),1,0),
      data=ens14b[,c(1:6,9,10,13,14)])   -> s14

s14$ENS<- 'ENS 2014'

###  Split PT y eventos 
splitLexis(s14,breaks = seq(20,110,by=5), time.scale = 'age') -> ens.s14
ens.s14$age.g <- timeBand(ens.s14, time.scale='age', type="middle") 
as.data.table(ens.s14) -> ens.s14

ens.s14[,.(event=sum(lex.Xst), PY=sum(lex.dur)),keyby=.(ENS,Sexo,age.g)] -> resu.14
resu.14[ ,mortality.rate:=event/PY]


### Plot Mortality rates from the linked data
# for comparison, death rates should be at the same scale
INE2014 <- INE2014 %>% filter(age.g<100 & age.g>20)
INE2014 <- INE2014 %>% mutate(ENS="INE 2014") %>% select(-year)
resu.g.3 <- resu.14 %>% select(-event, - PY) %>% union(INE2014)

### ----------------------------------------------------------------- ###
### ----------------------------------------------------------------- ###

### add cases without date of exit but death padron

Lexis(entry = list(per= rep(2014,nn), age= 2014-cal.yr(dob)),
      exit = list(per= cal.yr(dout)),
      entry.status = rep(0,nn),
      exit.status = ifelse(ESTADO=="B",1,0),
      data=ens14b[,c(1:6,9,10,13,14)])   -> s14.b

s14.b$ENS<- 'ENS 2014'

###  Split PT y eventos 
splitLexis(s14.b,breaks = seq(20,110,by=5), time.scale = 'age') -> ens.s.b14
ens.s.b14$age.g <- timeBand(ens.s.b14, time.scale='age', type="middle") 
as.data.table(ens.s.b14) -> ens.s.b14

ens.s.b14[,.(event=sum(lex.Xst), PY=sum(lex.dur)),keyby=.(ENS,Sexo,age.g)] -> resu.b14
resu.b14[ ,mortality.rate:=event/PY]


### Plot Mortality rates from the linked data

resu.g.4 <- resu.b14 %>% select(-event, - PY) %>% union(INE2014)

######################################################################################################## 

########## Adding the missing dates cases

##### Graph!
##### -----

INE.C <- resu.g.3 %>% filter(age.g < 100 & age.g > 20 ) %>% 
  ggplot(aes(x=age.g, y=mortality.rate, color=Sexo, linetype=ENS)) +
  geom_line() +
  # geom_line(aes(age, tdm, colour=sex), INE2011) +
  scale_y_log10(name="Tasas de mortalidad en una escala logarítmica")  +
  scale_linetype_discrete(name=" ") +
  scale_x_continuous(name = "Edad") +
  ggtitle('Mortalidad de la cohorte de la ENS de 2011 e INE 2011, por sexo', 
          subtitle = 'Segumiento 2014: [2014-2017).; INE Tasas de mortalidad 2014') +
  theme_bw() +
  theme(legend.position="none")

INE.D <- resu.g.4 %>% filter(age.g < 100 & age.g > 20 ) %>% 
  ggplot(aes(x=age.g, y=mortality.rate, color=Sexo, linetype=ENS)) +
  geom_line() +
  # geom_line(aes(age, tdm, colour=sex), INE2011) +
  scale_y_log10(name=" ")  + # name="Tasas de mortalidad en una escala logarítmica"
  scale_linetype_discrete(name="Fuente") +
  scale_x_continuous(name = "Edad") +
  ggtitle(' ', 
          subtitle = 'Segumiento 2014 (más casos sin fecha): [2014-2017).; INE Tasas de mortalidad 2014 ') +
  theme_bw()
INE.D <- INE.D + theme(legend.position = c(0.85, 0.25))

multiplot(INE.C,INE.D)



