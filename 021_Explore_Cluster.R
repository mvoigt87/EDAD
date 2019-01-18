# Need to run 020_Link_EDAD08_Followup.R at least once
# --------------------------------------------------

# 0.1. Packages
# -------------

library(tidyverse)
library(data.table)
library(foreign)
library(survival)
library(broom)
library(stargazer)
library(TraMineR)
library(reshape)
library("RColorBrewer")


# --------------------------------------------
load(file='010_mayor.link.RData')
# --------------------------------------------



# 1.0. Select Population - Get rid of the "NC" cases for now and only extract the dependent ones
# -----------------------------------------------------------------------------------------------

link.may <- link.may %>% filter(LIMIT!="NC") %>% 
  ####
  ####
  #### AND at this point only extract the ones who are dependent in 2008
  ####
  filter(!is.na(Edadinicio_cuidado))   

link.may <- data.table(link.may)
# 4761 cases - working material

# ---------------------------------------------
# 1.1 Few discriptive plots on different states
# ---------------------------------------------
# ---------------------------------------------


# 1.1.2 Age distribution (disability)
#####################################

# Exit age
# ---------
with(link.may, tapply(age.ex, list(event,LIMIT), mean)) # Just mean ages but still the No cases are puzzeling 
# (What's NC?)

table(link.may$estado)
  # A    B(ajas = refers to either events (death) or outmigration) 
  # 1171 3590 
link.may <- data.table(link.may)
link.may[,.N,.(enlazado,estado)] 

# By disability status
# --------------------
round(prop.table(table(link.may$event, link.may$Gravity),2),3)

with(link.may, tapply(age.ex, list(event,Gravity), mean))


# check distribution of exit ages
# -------------------------------
hist(link.may$age.ex, nclass = 50, main = "", xlab = "Age")

# event-age distribution
# ----------------------
link.may %>% mutate(event = as.factor(event)) %>% 
  ggplot(aes(x=age.ex, fill=event)) +
  geom_histogram(bins = 44) +
  scale_x_continuous(name = "Age") +
  scale_fill_discrete(name = "") +
  theme_bw()    
# looks believable - most of the members of this very frail group die during the observation period


#### ----------------------------------------------------- ####
#### 2. Assigning time information for crucial time points ####
#### ----------------------------------------------------- ####

## 2.1. Onset of disability by kind of disability
## ----------------------------------------------


## 2.2. Onset of incapacity of an activity of daily living
## -------------------------------------------------------

# 2.2.1 ABC Scheme
# ----------------
# Group A : disabled individuals which are able to live by themselves
# Group B : individuals are incapable of doing housework
# Group C : individuals have problems (need personal assistance) with mobility

# A
summary(link.may$edadiniciodisca12A) # 415 NA´s
# B
summary(link.may$edadiniciodisca12B) # 934 NA´s
# C
summary(link.may$edadiniciodisca12C) # 1811 NA´s

### See if the entry ages follow the theory: A < Age B < Age C
abc <- link.may[,.(a=edadiniciodisca12A, b=edadiniciodisca12B, c=edadiniciodisca12C)]
summary(abc)

  # A < B  Individuals with where disability didn´t cause loss of the ability to manage the household
  link.may[,.N,.(ab=edadiniciodisca12A<=edadiniciodisca12B, ba=edadiniciodisca12B<edadiniciodisca12A)]
  # A < C  Individuals with where disability didn´t cause loss of the ability to manage the household
  link.may[,.N,.(ac=edadiniciodisca12A<=edadiniciodisca12C, ca=edadiniciodisca12C<edadiniciodisca12A)]
  # B < C  Individuals with where disability didn´t cause loss of the ability to manage the household
  link.may[,.N,.(bc=edadiniciodisca12B<=edadiniciodisca12C, cb=edadiniciodisca12C<edadiniciodisca12B)]
  # A < B < C
  link.may[,.N,.(abc=edadiniciodisca12A<=edadiniciodisca12B & edadiniciodisca12C)]
  # abc    N
  # 1:    NA 2079
  # 2:  TRUE 2391
  # 3: FALSE  291
  
  # NA´s
  # ----
  link.may[,.N,.(NAa=is.na(edadiniciodisca12A), NAb=is.na(edadiniciodisca12B), NAc=is.na(edadiniciodisca12C))]
  #      NAa   NAb   NAc    N
  # 1: FALSE  TRUE FALSE  159
  # 2: FALSE FALSE FALSE 2609
  # 3: FALSE FALSE  TRUE  913
  # 4: FALSE  TRUE  TRUE  665
  # 5:  TRUE FALSE  TRUE  150
  # 6:  TRUE  TRUE  TRUE   83
  # 7:  TRUE FALSE FALSE  146
  # 8:  TRUE  TRUE FALSE   36
  
  ##############################################################################################################
  
### 3. Cluster analysis
  
  ## 3.1. Subdata set with information on Age, Entry to disability, A, B, C, and dependency
  
  tra_may <- link.may %>% select(Id, EDAD, EdadInicioDisca44, edadiniciodisca12A, edadiniciodisca12B, edadiniciodisca12C, Edadinicio_cuidado)
  
  # 3.2. Define an alphabet for the different states an individual can obtain
    # DF = Disability Free
    # FD = First Disability
    # NA = Needs Assistance
    # HH = Help with Housework
    # IM = Impaired Mobility
    # DC = Dependent on Caretaker
  Seq.alphab <- c("DF","FD", "NA", "HH", "IM", "DC")
  
  # 3.3. Define color scheme
  display.brewer.all()
  Brewer <- brewer.pal(6, "Dark2")
  
  # Give single year disability states
  # ----------------------------------
  
  # Fun with strings
  
  # Preparation: round the age of first disability and dependency
  tra_may <- tra_may %>% mutate(age_f_dis = round(EdadInicioDisca44,0)) %>% 
                         mutate(age_dep = round(Edadinicio_cuidado,0))
  
  # Approach 1
  
  #
  #
  #
  #
  #
  #  This needs a fix!
  #
  #
  #
  #
  
  tra_may <- tra_may %>% mutate(disstring = ifelse(EdadInicioDisca44<50, paste(rep("FD", 50), collapse = "-"),
                                                   ifelse(EdadInicioDisca44>=50, paste0(paste(strrep("DF", age_f_dis-50), collapse = "-"),
                                                          paste(strrep("FD",100-age_f_dis), collapse = "-"), sep = " "),
                                                   ifelse(
                                                            
                                                          ))))
  
  
  paste0(strrep(0, 10 - nchar(test$x)), test$x)
  
  
  # Approach 2
  
  tra_may$disstring <- ifelse(tra_may$EdadInicioDisca44<50,strrep("FD",50),0)
  tra_may$disstring <- ifelse(tra_may$EdadInicioDisca44>50, paste(rep("DF", 50:round(tra_may$EdadInicioDisca44,0))))
  
  # Prepare Matrix for the seqdef() command
  cbind(names(tra_may))
  
  seqmat <- tra_may[,c(13:25)]
  