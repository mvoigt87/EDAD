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

str(EDAD.mayor$EdadInicioDisca44)

str(EDAD.mayor$Edadinicio_cuidado)

# see differences between different entry ages
par(mfrow=c(1,3))
hist(as.numeric(EDAD.mayor$EdadInicioDisca44))
hist(as.numeric(EDAD.mayor$EdadInicioDisca19))
hist(as.numeric(EDAD.mayor$EdadInicioDisca13))
par(mfrow=c(1,1))

# A few checks with variables - to get a feeling for the number of transitions
EDAD.mayor <- data.table(EDAD.mayor)
EDAD.mayor[, .N, .(!is.na(EdadInicioDisca13))]
EDAD.mayor[, .N, .(!is.na(Edadinicio_cuidado))]
EDAD.mayor[, .N, .(as.numeric(EdadInicioDisca13)<as.numeric(Edadinicio_cuidado))] # !

    # Dependientes13, Dependientes13 Dependientes14 AsistenciaPersonal14
    # DemandaCuidados14 EdadInicioDisca44 EdadUltimaDisca44


head(names(EDAD.mayor), 20)
names(EDAD.mayor)[6:10]<- c("nseccion", "dc", "viv", "hog", "nord")

merge(EDAD.mayor[,c(1,4:12,17:23,29,885:898, 960:963, 1326:1329, 1371:1380),with=F],follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link.may
class(link.may)
link.may[,enlazado:=!is.na(estado)]
link.may[,.N,.(enlazado,estado)]    # 6568 are not linked

# now reduce to the ones which are in the original data (65+)
table(link.may$enlazado)

# Choose the linked ones and the ones with age information
link.may <- link.may %>% filter(!is.na(EDAD)) %>% filter(enlazado==T) %>%  ### 38985 individuals (65 +, followed up)
# and make an event variable
mutate(event=ifelse(estado=="B", 1,0))

# time to death or censor var                                        mutate(age.d = EDAD + (2008.5 - (a_salida)))
link.may <- data.table(link.may)
link.may[estado=='B',.N ,keyby=.(Con.ANODEF=(!is.na(anodef)),  con.CAUSA = (!is.na(causa)))] ## 1783 (11.74%) def sin causa y fecha
# Impute a death date for the ones without assigned date but information on state at exit ("B")
summary(link.may$a_salida)

# Check the "bajas"
link.may %>% count(estado=='B' & is.na(anodef) & is.na(a_salida)) # 1689 bajas don´t have a year of death or exit from the survey
link.may %>% count(estado=='B' & !is.na(a_salida) & is.na(causa)) # 94 have a year of exit but no cause (death, censor)

### all possible ways I can think of over the top the head to impute the date from one of the two situations
### ---------------------------------------------------------------------------------------------------------

# If there is neither a death date nor an exit date but the "baja" information, we approximate by the medium time
link.may[estado=='B' & is.na(anodef) & is.na(a_salida), ':='(anodef=2013, mesdef=1, a_salida=2013, m_salida=1)]
# If there is a death date this will become the exit date (absorbing state)
link.may[estado=='B' & !is.na(anodef) & !is.na(causa), ':='(a_salida=anodef, m_salida=mesdef)]
# If there is a age at death information but no exit year
link.may[estado=='B' & !is.na(a_salida), ':='(anodef=a_salida, mesdef=m_salida)] #  ??? - !is.na(causa)

link.may %>% count(estado=='B')
link.may %>% count(estado=='B' & !is.na(a_salida)) # ok! Same number (assuming everyone has an exit year)

# Assign a censoring date for censored cases
  # for now: last month of 2017 where event happened
max(link.may$m_salida[link.may$estado=='B' & link.may$a_salida==2017]) # Last month = May

link.may[estado=='A', ':='(a_salida=2017, m_salida=5)]

summary(link.may$a_salida) # 1 individual fell through somehow

head(link.may[is.na(link.may$a_salida)]) # this person is censored at the end but does not any other time information
link.may[is.na(a_salida), ':='(a_salida=2017, m_salida=5)]  # impute censoring date

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
str(link.may$EdadInicioDisca13)
link.may$EdadInicioDisca13 <- as.numeric(link.may$EdadInicioDisca13)
summary(link.may$EdadInicioDisca13)

link.may %>% count(EdadInicioDisca13>=65)


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


# --------------------------------------------
# 3. Few discriptive plots on different states
# --------------------------------------------
# --------------------------------------------


# Age distribution (disability)
###############################

# Exit age
# ---------
with(link.may, tapply(age.ex, list(event,LIMIT), mean)) # Just means but still the No cases are puzzeling

    # Sí, gravemente limitado Sí, limitado pero no gravemente       No       NC
    # 0                83.80638                        84.09716 82.52330 87.09333
    # 1                85.88888                        85.75008 83.75016 85.46238
# get rid of the "NC" cases for now

link.may <- link.may %>% filter(LIMIT!="NC")

# check distribution
hist(link.may$age.ex, nclass = 50, main = "", xlab = "Age")

# event-age distribution
link.may %>% mutate(event = as.factor(event)) %>% 
  ggplot(aes(x=age.ex, fill=event)) +
  geom_histogram(bins = 44) +
  scale_x_continuous(name = "Age") +
  scale_fill_discrete(name = "") +
  theme_bw()    
  # looks believable - everybody before age 75 exits because of an event


# Survival 
##########
#     To account for left truncation, a cox ph approximation is used to estimate the KME



# KME by disability
#%%%%%%%%%%%%%%%%%%#

mfit.1a <- survfit(coxph(Surv(time=EDAD,
                              time2=age.ex,
                              event=event)~1, data=subset(link.may,LIMIT=="Sí, gravemente limitado")), 
                      data=subset(link.may,LIMIT=="Sí, gravemente limitado"),type="kaplan-meier")

mfit.1b <- survfit(coxph(Surv(time = EDAD,
                             time2 = age.ex,
                             event = event) ~ 1, data=subset(link.may,LIMIT=="Sí, limitado pero no gravemente")), 
                        data=subset(link.may,LIMIT=="Sí, limitado pero no gravemente"),type="kaplan-meier")

mfit.1c <- survfit(coxph(Surv(time = EDAD,
                             time2 = age.ex,
                             event = event) ~ 1, data=subset(link.may,LIMIT=="No")), 
                          data=subset(link.may,LIMIT=="No"),type="kaplan-meier")

KM.LIM.a <- tidy(mfit.1a) %>% select(estimate, time) %>% mutate(Limit = "Severely limited")
KM.LIM.b <- tidy(mfit.1b) %>% select(estimate, time) %>% mutate(Limit = "Mildly limited")
KM.LIM.c <- tidy(mfit.1c) %>% select(estimate, time) %>% mutate(Limit = "No limitation")

KM.LIM <- union(KM.LIM.a, KM.LIM.b) %>% union(KM.LIM.c)

KM.LIM %>% dplyr::filter(time > 65) %>% 
  ggplot() +
  geom_step(mapping=aes(x=time, y=estimate, color=Limit)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("orange", "darkgrey", "red"), name="")     +
  theme_bw()

# Believable and expected (will need to find a way to enter the month information)

rm(KM.LIM.a, KM.LIM.b, KM.LIM.c)

# sex differences
#%%%%%%%%%%%%%%%%%#

mfit.2a <- survfit(coxph(Surv(time=EDAD,
                       time2 = age.ex,
                       event = event) ~ 1, data = subset(link.may, SEXO=="Varón")), data = subset(link.may, SEXO=="Varón"),
                        type = "kaplan-meier")


mfit.2b <- survfit(coxph(Surv(time=EDAD,
                             time2 = age.ex,
                             event = event) ~ 1, data = subset(link.may, SEXO=="Mujer")), data = subset(link.may, SEXO=="Mujer"),
                   type = "kaplan-meier")


KME.SEXOa <- tidy(mfit.2a) %>% select(estimate, time) %>% mutate(sex = "male")
KME.SEXOb <- tidy(mfit.2b) %>% select(estimate, time) %>% mutate(sex = "female")

KME.SEXO <- union(KME.SEXOa, KME.SEXOb)
KME.SEXO %>% ggplot() +
             geom_step(aes(x=time, y=estimate, color=sex)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
  theme_bw()

  # Very similar to former sex comparisons with Spanish contemporary data


# time variables for onset of disability and dependency
#######################################################

### A) the state graph

states <- function(what, horizontal=T, ...){
  st.names <- c("Entry", "Recovery", "Disability", "Dependency", "Death")
  connect <- matrix(0,5,5, dimnames = list(st.names, st.names))
  connect[1,-1] <- c(0,1,1, 1.4)
  connect[2,3:5] <- c(1, 1.4, 1)
  connect[3, c(2,4,5)] <- 1
  connect[4, c(3,5)] <- 1
  statefig(matrix(c(1,3,1)), connect, cex = .8, ...)
}

states()





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

