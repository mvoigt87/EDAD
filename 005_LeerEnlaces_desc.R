### ----------------------------------------------------------
###  Lectura de datos de enlace con la ENS de 2011 y 2014 
##   muestrar los problemas con los datos
##-----------------------------------------------------------
rm(list = ls())
require(readr)
ww  <- c(1, 9, 11, 13, 17, 19, 23, 24, 28, 29) ; ww <-diff(ww)
nms <- c('IDENTHOGAR', 'A7_2a', 'Renta', 'ANODEF', 'MESDEF', 'CAUSA', 'ESTADO', 'A_SALIDA', 'M_SALIDA')

## follow.up ENS 2011 y 2014

fu.ens11 <- read_fwf(file = 'enlaces/Final2011_salud.txt',
                     fwf_widths(ww, col_names = nms),
                     col_types =  cols(
                       IDENTHOGAR = col_integer(),
                       A7_2a = col_integer(),
                       Renta= col_integer(),
                       ANODEF= col_integer(),
                       MESDEF= col_integer(),
                       CAUSA= col_character(),
                       ESTADO= col_character(),
                       A_SALIDA= col_integer(),
                       M_SALIDA= col_integer()))

fu.ens14 <- read_fwf(file = 'enlaces/Final2014_salud.txt',
                     fwf_widths(ww, col_names = nms),
                     col_types =  cols(
                       IDENTHOGAR = col_integer(),
                       A7_2a = col_integer(),
                       Renta= col_integer(),
                       ANODEF= col_integer(),
                       MESDEF= col_integer(),
                       CAUSA= col_character(),
                       ESTADO= col_character(),
                       A_SALIDA= col_integer(),
                       M_SALIDA= col_integer()))

require(data.table)
fu.ens11 <-  as.data.table(fu.ens11)
fu.ens14 <-  as.data.table(fu.ens14)
setnames(fu.ens11,c(1,2), c('idh','idi'))
setnames(fu.ens14,c(1,2), c('idh','idi'))


# save (fu.ens11, fu.ens14,file='005_follow.up_ens.RData')


##------
fu.ens11 [,.N,keyby=.(ESTADO)] # 
#    ESTADO     N
# 1:      A 17624
# 2:      B  1787
# 3:      N  1596
fu.ens14 [,.N,keyby=.(ESTADO)] # 
#    ESTADO     N
# 1:      A 20240
# 2:      B  1084
# 3:      N  1518

## Proporciones 
fu.ens11 [, .(.SD[,.N,ESTADO]$ESTADO,.SD[,.N,ESTADO]$N/.N)]
fu.ens14 [, .(.SD[,.N,ESTADO]$ESTADO,.SD[,.N,ESTADO]$N/.N)]


## BAJAS
fu.ens11[ESTADO=='B' & is.na(ANODEF) & is.na(A_SALIDA)] ## 377

fu.ens11[ESTADO=='B',.N, keyby=.(FALLECIDO=!is.na(ANODEF), MIGR.EXT= !is.na(A_SALIDA))]  -> pp 
pp[,.(N,N/sum(pp$N)),.(FALLECIDO,MIGR.EXT)]
fu.ens14[ESTADO=='B',.N, keyby=.(FALLECIDO=!is.na(ANODEF), MIGR.EXT= !is.na(A_SALIDA))]  -> pp 
pp[,.(N,N/sum(pp$N)),.(FALLECIDO,MIGR.EXT)]

#  FALLECIDO MIGR.EXT  ens-11   ens-14     
# 1:     FALSE    FALSE    377      355  
# 2:     FALSE     TRUE    156       70
# 3:      TRUE    FALSE   1254      659




## DEDUZCO que esos 355 y 377 registros EN BAJA sin fecha de fin de segumiento,
## son bajas que no estan en las estadistica de migraciones (?¿)
## Necesitariamos proporcionaran la fecha de la ultima baja en padron ...
## para asirnarle una fecha de censura ...


#### ----------------------------------
#### REVISA COHERENCIA ENS2014
#### ----------------------------------
## FALTAN FECHAS (de SALIDA o de DEFUNCION) en muchas en BAJAS 
## no podemo fijar claramente fecha de censura e incluso tipo de baja
fu.ens11[ESTADO=='B' & is.na(ANODEF) & is.na(A_SALIDA)] ## 377

## Fecha y causa de defuncion en bajas
fu.ens11[ESTADO=='B',.N ,keyby=.(Con.ANODEF=(!is.na(ANODEF)),  con.CAUSA = (!is.na(CAUSA)))] ## 
#   Con.ANODEF con.CAUSA    N
# 1:      FALSE     FALSE  533 
# 2:       TRUE      TRUE 1254     OK Todos los que tiene fecha de defunción tienen causa de muerte.

## Fecha y causa en no localizadas
fu.ens11[ESTADO=='N',.N ,keyby=.(ESTADO,Con.ANODEF=(!is.na(ANODEF)),  con.CAUSA = (!is.na(CAUSA)))] ## ç
#     ESTADO Con.ANODEF con.CAUSA    N
# 1:      N      FALSE     FALSE 1549
# 2:      N       TRUE      TRUE   47  <- 47 no tendria que tener causa


## Año de Baja y cuando las bajas no son defuciones 
fu.ens11[ESTADO=='B' &  is.na(ANODEF),.N ,keyby=.(Con.A_SALIDA=(!is.na(A_SALIDA)))] ## 
#    Con.A_SALIDA   N   
# 1:        FALSE 377   <- no tiene fecha de baja
# 2:         TRUE 156

## El mes de salida no esta informado
fu.ens11[ESTADO=='B' &  is.na(ANODEF),.N ,keyby=.(M_SALIDA)] ## 
#    M_SALIDA   N
# 1:       NA 377
# 2:        0 118
# 3:        1  38

## Esta informado el mes de defunción y el AÑO 
class(fu.ens11)
fu.ens11[,.N,.(Hay.AñoD=!is.na(ANODEF),Hay.MesD=!is.na(MESDEF))]
#      Hay.AñoD Hay.MesD     N
# 1:    FALSE    FALSE 19706
# 2:     TRUE     TRUE  1301  OK correcto

##-----------


dcast(fu.ens11[,.N,keyby=.(ESTADO,Sin.ANODEF=is.na(ANODEF), 
                           Sin.A_SALIDA= is.na(A_SALIDA))], 
      Sin.ANODEF + Sin.A_SALIDA ~ ESTADO,
      value.var = 'N') -> pp ; pp
#    Sin.ANODEF Sin.A_SALIDA     A    B    N
# 1:      FALSE         TRUE    NA 1254   47
# 2:       TRUE        FALSE    NA  156   NA
# 3:       TRUE         TRUE 17624  377 1549
# -------------------------- ----- ---- ----
#   TOTAL                    17624 1787  1596     
# colSums(pp,na.rm = T)


## en ENS11 del total del bajas 1787: en 377 (21%) casos no hay fecha de 
## defuncion ni año de salida de España. 
## No se esta seguro si es una censura o una defunción. Se presupone una CENSURA ...
## pero se necesitaria fecha en ese caso ...


#### ----------------------------------
#### REVISA COHERENCIA ENS2014
#### ----------------------------------

## FALTAN FECHAS (de SALIDA o de DEFUNCION) en muchas en BAJAS 
## no podemo fijar claramente fecha de censura e incluso tipo de baja
fu.ens11[ESTADO=='B' & is.na(ANODEF) & is.na(A_SALIDA)] ## 377

## Fecha y causa de defuncion en bajas
fu.ens14[ESTADO=='B',.N ,keyby=.(Con.ANODEF=(!is.na(ANODEF)),  con.CAUSA = (!is.na(CAUSA)))] ## 
#   Con.ANODEF con.CAUSA    N
# 1:      FALSE     FALSE  425 
# 2:       TRUE      TRUE  659     OK Todos los que tiene fecha de defunción tienen causa de muerte.

## Fecha y causa en no localizadas
fu.ens14[ESTADO=='N',.N ,keyby=.(ESTADO,Con.ANODEF=(!is.na(ANODEF)),  con.CAUSA = (!is.na(CAUSA)))] ## ç
#     ESTADO Con.ANODEF con.CAUSA    N
# 1:      N      FALSE     FALSE 1487
# 2:      N       TRUE      TRUE   31  <- 31 no tendria que tener causa


## Año de Baja y cuando las bajas no son defuciones 
fu.ens14[ESTADO=='B' &  is.na(ANODEF),.N ,keyby=.(Con.A_SALIDA=(!is.na(A_SALIDA)))] ## 
#    Con.A_SALIDA   N   
# 1:        FALSE 355   <- no tiene fecha de baja
# 2:         TRUE  70

## El mes de salida no esta informado
fu.ens14[ESTADO=='B' &  is.na(ANODEF),.N ,keyby=.(M_SALIDA)] ## 
#    M_SALIDA   N
# 1:       NA 355
# 2:        0  54
# 3:        1  16

##-----------
##-----------


dcast(fu.ens14[,.N,keyby=.(ESTADO,Sin.ANODEF=is.na(ANODEF), 
                           Sin.A_SALIDA= is.na(A_SALIDA))], 
      Sin.ANODEF + Sin.A_SALIDA ~ ESTADO,
      value.var = 'N') -> pp ; pp



#    Sin.ANODEF Sin.A_SALIDA     A    B    N
# 1:      FALSE         TRUE    NA  659   31  <=  
# 2:       TRUE        FALSE    NA   70   NA  <=  
# 3:       TRUE         TRUE 20240  355 1487  <= ¿? 355
# -------------------------- ----- ---- ----
#   TOTAL                    20240 1084 1518 
# colSums(pp,na.rm = T)


## en ENS14 del total del bajas 1084: en 355 (32%) casos no hay fecha de 
## defuncion ni año de salida de España. 
## No se esta seguro si es una censura o una defunción. Se presupone una CENSURA ...
## pero se necesitaria fecha en ese caso ...

fu.ens14[,.N,.(Hay.AñoD=!is.na(ANODEF),Hay.MesD=!is.na(MESDEF))]
#      Hay.AñoD Hay.MesD     N
# 1:     TRUE     TRUE   690
# 2:    FALSE    FALSE 22152   Ok correcto.

