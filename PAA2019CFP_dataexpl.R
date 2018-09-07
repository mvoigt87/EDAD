# Follows file PAA2019CFR.R

# --------------------------------------------
load(file='010_mayor.link.RData')
# --------------------------------------------


# --------------------------------------------
# 1. Few discriptive plots on different states
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


# Drop the "NC" cases for now and only extract the dependent ones
# ----------------------------------------------------------------

link.may <- link.may %>% filter(LIMIT!="NC") %>% 
  ####
  ####
  #### AND at this point only extract the ones who are dependent in 2008
  ####
  filter(!is.na(Edadinicio_cuidado))    

# 4761 cases - working material

# check distribution
hist(link.may$age.ex, nclass = 50, main = "", xlab = "Age")

# event-age distribution
link.may %>% mutate(event = as.factor(event)) %>% 
  ggplot(aes(x=age.ex, fill=event)) +
  geom_histogram(bins = 44) +
  scale_x_continuous(name = "Age") +
  scale_fill_discrete(name = "") +
  theme_bw()    
# looks believable - most of the members of this very frail group die during the observation period


################
### Survival ### 
################

#     To account for left truncation, a cox ph approximation is used to estimate the KME

# First a KME for sex differences
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

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

# Very dramatic drop in survival probability over time (again women seem to have better survival)



# KME by time in disability
#%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## Therefore: Creating a for now arbitrary variable to group different transitions by time
  # acute: less than 1 years between onset of disability and onset of dependency
  # moderate: between 1 and 10 years (mean)
  # gradual : more than 10 years

link.may <- link.may %>% mutate(diff.d.d= Edadinicio_cuidado - Edadinicio_disca44) %>%
  # clean up
  mutate(diff.d.d=ifelse(Edadinicio_cuidado - Edadinicio_disca44<0,diff.d.d+0.5,diff.d.d)) %>% 
  mutate(group.d.d = ifelse(Edadinicio_cuidado - Edadinicio_disca44>10,"gradual",
                                                  ifelse(Edadinicio_cuidado - Edadinicio_disca44<=1,"acute", "moderate")))

summary(link.may$diff.d.d)
link.may %>% mutate(event = as.factor(event)) %>% 
  ggplot(aes(x=diff.d.d, fill=event)) +
  geom_histogram(bins = 44) +
  scale_x_continuous(name = "Age") +
  scale_fill_discrete(name = "") +
  theme_bw() 
# for a large number of individuals the onset of disability was also the onset of dependency
# We can assume that these individuals were exposed to an acute health event like a stroke or heavy accident

table(link.may$group.d.d)


# Lets extract the survival probabilities for the three groups

mfit.1a <- survfit(coxph(Surv(time=EDAD,
                              time2=age.ex,
                              event=event)~1, data=subset(link.may,group.d.d =="gradual")), 
                   data=subset(link.may,group.d.d =="gradual"),type="kaplan-meier")

mfit.1b <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may,group.d.d =="moderate")), 
                   data=subset(link.may,group.d.d =="moderate"),type="kaplan-meier")

mfit.1c <- survfit(coxph(Surv(time = EDAD,
                              time2 = age.ex,
                              event = event) ~ 1, data=subset(link.may,group.d.d =="acute")), 
                   data=subset(link.may,group.d.d =="acute"),type="kaplan-meier")

KM.LIM.a <- tidy(mfit.1a) %>% select(estimate, time) %>% mutate(Limit = "Gradual")
KM.LIM.b <- tidy(mfit.1b) %>% select(estimate, time) %>% mutate(Limit = "Moderate")
KM.LIM.c <- tidy(mfit.1c) %>% select(estimate, time) %>% mutate(Limit = "Acute")

### ADD a starting value!!!

KM.LIM <- union(KM.LIM.a, KM.LIM.b) %>% union(KM.LIM.c)


KM.LIM %>% dplyr::filter(time > 65) %>% 
  ggplot() +
  geom_step(mapping=aes(x=time, y=estimate, color=Limit)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("orange", "darkgrey", "red"), name="")     +
  theme_bw()

  # 

# Believable and expected survival probabilities - gradient from gradual to acute
# Just need the get the noise in the beginning under control

rm(KM.LIM.a, KM.LIM.b, KM.LIM.c)



### Exploratory Cox Regression

# Preliminary step: Change Reference Category and Category names

Cox.1 <- coxph(Surv(time=EDAD,
                   time2=age.ex,
                    event=event)~ group.d.d + Sex + Education + Income + AIREAC, data=subset(link.may))

summary(Cox.1)


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

