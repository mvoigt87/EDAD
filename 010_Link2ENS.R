##-- Lee fichero de enlaces

library(foreign)
require(readr)
require(data.table)
rm(list = ls())
# require(Hmisc)
# require(tidyverse)
# source('https://raw.githubusercontent.com/bjornerstedt/assist/master/R/describe.R')

load (file='005_follow.up_ens.RData')

ens11 = read.spss('microdatos/Adultos_2011.sav')  
warnings()
ens14 = read.spss('microdatos/Adultos_2014.sav')  
warnings()


ens11 = as.data.frame(ens11)  ;
ens14 = as.data.frame(ens14)  ;



### Comprueba enlace .. borro variables que de momento no uso
ens11b <-  as.data.table(ens11[,c(1:2,4:6)])
ens14b <-  as.data.table(ens14[,c(1:5)])


setnames(ens11b,c(2,3), c('idh','idi'))
setnames(ens14b,c(2,3), c('idh','idi'))
setnames(ens14b,c("SEXOa","EDADa"), c("Sexo","Edad"))

cbind(names(ens11b), names(ens14b))



setkey(ens11b, idh,idi)
setkey(ens14b, idh,idi)

# merge follow ups with base data
merge(ens11b,fu.ens11, by =  c('idh','idi'), all=T) -> ens11b
merge(ens14b,fu.ens14, by =  c('idh','idi'), all=T) -> ens14b

cbind(names(ens11b),names(ens14b))


### ---

ens11b [,.N,keyby=.(ESTADO)]
#    ESTADO     N
# 1:      A 17624
# 2:      B  1787
# 3:      N  1596

ens14b [,.N,keyby=.(ESTADO)]
#    ESTADO     N
# 1:      A 20240
# 2:      B  1084
# 3:      N  1518


##  FECHAS de SALIDA factantes  en BAJAS  (presupongo que son censura no defunciones ¿?)
ens14b[ESTADO=='B' & is.na(ANODEF) & is.na(A_SALIDA), .N] ##  355
ens11b[ESTADO=='B' & is.na(ANODEF) & is.na(A_SALIDA), .N] ##  377

save(ens11b,ens14b, file='010_data.exploratory.RData')
