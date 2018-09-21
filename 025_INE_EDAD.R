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

