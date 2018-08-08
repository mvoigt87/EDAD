##-----------------------
## Exploratorio rapido
##-----------------------
require(data.table)
rm(list = ls())
load(file='010_data.exploratory.RData')

### alguna comprobaciones FDEF y CAUSA : OK
ens14b[ESTADO=='B',.N ,keyby=.(Con.ANODEF=(!is.na(ANODEF)),  con.CAUSA = (!is.na(CAUSA)))] ## 
ens11b[ESTADO=='B',.N ,keyby=.(Con.ANODEF=(!is.na(ANODEF)),  con.CAUSA = (!is.na(CAUSA)))] ## 
ens11b[is.na(CAUSA) & ! is.na(ANODEF)]  ## Defunciones sin CAUSA pero con fecha ...

#### Ventana de seguimiento temporal: Feb-2011 a Dic-2016 y Feb-2014 a Dic-2016
range(ens11b$ANODEF*100+ens11b$MESDEF, na.rm = T)
range(ens14b$ANODEF*100+ens14b$MESDEF, na.rm = T)


## Impute date of birth: uniformentente en torno al año de nacimiento estimado con edad
n11<- length(ens11b$idi)  
n14<- length(ens14b$idi)  
set.seed(100)
ens11b[,dob:=as.IDate(paste0(2011-Edad-1,'-',sample(1:12,n11,replace = T),'-15'))]  ## a 1-1-2011 (I think..¿?)
ens14b[,dob:=as.IDate(paste0(2014-Edad-1,'-',sample(1:12,n14,replace = T),'-15'))] ## a 1-1-2014 

# Muestra algunos datos
ens11b[ESTADO=='B', c(1:2,5:6,8,11,12,13)]  ## fallecidos o emigrados (EN BAJA)
ens11b[ESTADO=='N', c(1:2,5:6,8,11,12,13)]  ## Hay que quitar los sin seguimiento padronal (aunque hay bajas por defunción ??)
ens11b[ESTADO=='A', c(1:2,5:6,8,11,12,13)]  ## Hay que asignar fecha de censura por fin de seguimiento: 31-12-2016

## Quitar los no enlazados_  En teoria si informacion de eventos, aunque en algunso casos si la hay ¿?
## si hay fecha de defuncion
# ens14[ESTADO=='N' &  ! is.na(ANODEF), c(1:2,5:6,8,10,11,12)]  ## 31 No enlazados con padron pero fallecidos en ens14
# ens11[ESTADO=='N' &  ! is.na(ANODEF), c(1:2,5:6,8,10,11,12)]  ## 47 No enlazados con padron pero fallecidos en ens11

ens14b[ESTADO!='N'] -> ens14b  # Elimina los sin seguimiento
ens11b[ESTADO!='N'] -> ens11b # Elimina los sin seguimiento
ens11b[,.N,ESTADO]


### IMPUTO la fecha de perdia (CENSURA) las BAJAS sin fecha de defunción.
##  En mitad del periodo de seguimiento de la encuesta: 
##  + (1-1-2014) para ens2011 
##  + (1-7-2014) para ens2011

### Imputación grosera de fecha de censura: mitad del periodo de seguimiento
ens11b[ESTADO=='B' & is.na(ANODEF) & is.na(A_SALIDA), ':='(A_SALIDA=2014, M_SALIDA=1)]
ens14b[ESTADO=='B' & is.na(ANODEF) & is.na(A_SALIDA), ':='(A_SALIDA=2015, M_SALIDA=7)]


## Fecha de Defunción o de Censura 
##  M_SALIDA no tiene informacion imputo a '07' mes central del año
ens11b[ESTADO=='B',.N,M_SALIDA]
ens14b[ESTADO=='B',.N,M_SALIDA]

ens11b[,dout:= as.IDate(ifelse(ESTADO=='B',ifelse(!is.na(ANODEF),
                                                 paste0(ANODEF,'-',MESDEF,'-15'),
                                                 paste0(A_SALIDA,'-','07','-15')
                                                ),'2016-12-31'))]

ens14b[,dout:= as.IDate(ifelse(ESTADO=='B',ifelse(!is.na(ANODEF),
                                                 paste0(ANODEF,'-',MESDEF,'-15'),
                                                 paste0(A_SALIDA,'-','07','-15')
                                                 ),'2016-12-31'))]
## No debe de haber NA en dout
ens11b[is.na(dout)]  # 0 ok
ens14b[is.na(dout)]  # 0 ok
ens11b[is.na(CAUSA) & ! is.na(ANODEF)]  # 0 ok

ens11b[,.N,ESTADO]

# Muestra algunos datos
ens11b[ESTADO=='B', c(1:2,5:6,7,11,10,13,14)] ##
ens14b[ESTADO=='B', c(1:2,5:6,7,11,13,14)] ##
ens11b[ESTADO=='A', c(1:2,5:6,7,11,13,14)] ##
ens14b[ESTADO=='A', c(1:2,5:6,7,11,13,14)] ##


##----------------------------------------------------
## Crea episodios para estimar tasas de mortalidad ...
##----------------------------------------------------
require(Epi)
cbind(names(ens11b),names(ens14b))

nn<-length(ens11b$idh)
Lexis(entry = list(per= rep(2011,nn), age= 2011-cal.yr(dob)),
      exit = list(per= cal.yr(dout)),
      entry.status = rep(0,nn),
      exit.status = ifelse(!is.na(CAUSA),1,0),
      data=ens11b[,c(1:6,9,10,13,14)])   -> s11


nn<-length(ens14b$idh)
Lexis(entry = list(per= rep(2014,nn), age= 2014-cal.yr(dob)),
      exit = list(per= cal.yr(dout)),
      entry.status = rep(0,nn),
      exit.status = ifelse(!is.na(CAUSA),1,0),
      data=ens14b[,c(1:6,9,10,13,14)])   -> s14

s11$ENS<- '2011'
s14$ENS<- '2014'
cbind(names(s11),names(s14))
ens <- rbind(s11,s14)

###  Split PT y eventos
splitLexis(ens,breaks = seq(15,110,by=5), time.scale = 'age') -> ens.s
ens.s$age.g <- timeBand(ens.s, time.scale='age', type="middle") 
as.data.table(ens.s) -> ens.s
### Algunas comprobaciones ...
as.data.table(ens)  -> ens
as.data.table(ens.s) -> ens.s

###------------------------------------------------
### Estimar tasas: por sexo y año de la encuesta 
###------------------------------------------------
ens.s[,.(event=sum(lex.Xst), PY=sum(lex.dur)),keyby=.(ENS,Sexo,age.g)] -> resu
resu[ ,mortality.rate:=event/PY]

####----------------
pdf('MortRate_ENS.pdf', height = 8, width = 9)
require(ggplot2)
qplot(x=age.g,y=mortality.rate, geom = c('line'), 
      color=Sexo, linetype=ENS, 
      data=resu[age.g < 90 & age.g > 20 ]) + scale_y_log10()  +
  ggtitle('Mortalidad de las chortes de las ENS de 2011 y 2014, por sexo', 
          subtitle = 'Segumiento 2011: [2011-2017). 2014: [2014-2017). ') +
      theme_bw()
dev.off()
##---

###------------------------------------------------
### Estimar tasas: andalucia y resto de España 
###------------------------------------------------
ens.s[,.(event=sum(lex.Xst), PY=sum(lex.dur)),
      keyby=.(Region= ifelse(CCAA=='Andalucía','Andalucía','Resto España'),Sexo,age.g)] -> resu
resu[ ,mortality.rate:=event/PY]
####----------------
pdf('MortRate_ENS_And.pdf', height = 8, width = 9)
qplot(x=age.g,y=mortality.rate, geom = c('line'), 
      color=Sexo, linetype=Region, 
      data=resu[age.g < 90 & age.g > 20 ]) + scale_y_log10()  +
  ggtitle('Mortalidad de las chortes de las ENS por sexo y region', 
          subtitle = 'Andalucia en comparacion con resto de España')+
  theme_bw()
dev.off()
##---



###------------------------------------------------
### Estimar intesidad de la censura: posible migracion exerior no informada
###------------------------------------------------

## objetio lexis estudio modelos por edad de la censura
nn<-length(ens11b$idh)
Lexis(entry = list(per= rep(2011,nn), age= 2011-cal.yr(dob)),
      exit = list(per= cal.yr(dout)),
      entry.status = rep(0,nn),
      exit.status = ifelse(!is.na(A_SALIDA),1,0),
      data=ens11b[,c(1:6,10,11,13,14)])   -> c11


nn<-length(ens14b$idh)
Lexis(entry = list(per= rep(2014,nn), age= 2014-cal.yr(dob)),
      exit = list(per= cal.yr(dout)),
      entry.status = rep(0,nn),
      exit.status = ifelse(!is.na(A_SALIDA),1,0),
      data=ens14b[,c(1:6,10,11,13,14)])   -> c14

c11$ENS<- '2011'
c14$ENS<- '2014'
enc <- rbind(c11,c14)

###  Split PT y eventos
splitLexis(enc,breaks = seq(15,110,by=5), time.scale = 'age') -> enc.s
enc.s$age.g <- timeBand(enc.s, time.scale='age', type="middle") 
as.data.table(enc.s) -> enc.s
### Algunas comprobaciones ...
as.data.table(enc)  -> enc
as.data.table(enc.s) -> enc.s





#####------
enc.s[,.(event=sum(lex.Xst), PY=sum(lex.dur)),keyby=.(ENS,Sexo,age.g)] -> resu
resu[ ,mortality.rate:=event/PY]

####----------------
pdf('Perfil_de_Censuras.pdf', height = 8, width = 9)
require(ggplot2)
qplot(x=age.g,y=mortality.rate, geom = c('line'), 
      color=Sexo, linetype=ENS, 
      data=resu[age.g < 90 & age.g > 20 ]) + scale_y_log10()  +
  ggtitle('Censura por edad en las cohortes de las ENS de 2011 y 2014, por sexo', 
          subtitle = 'Segumiento 2011: [2011-2017). 2014: [2014-2017). ') +
  theme_bw()
dev.off()
##---
