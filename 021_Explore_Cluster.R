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


# check distribution of exit ages            (looks normal)
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
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
  
  
  # Give single year disability states
  # ----------------------------------
  
  ### Single year values with focus on the ages 50 to 100 
  ### Information is censored below and abov
  ### entry state can be any
  ### assumption of irreversibility meaning someone who needs personal assistance cannot go back to be only slightly disabled)
  ### This assumption makes live easy for now and can be confirmed albeit exceptions (However, ideally it is possible to move in both directions)
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
  
  
  
  ## 3.1. Subdata set with information on Age, Entry to disability, A, B, C, and dependency
  ## ---------------------------------------------------------------------------------------
  tra_may <- link.may %>% select(Id, EDAD, EdadInicioDisca44, edadiniciodisca12A, edadiniciodisca12B, edadiniciodisca12C, Edadinicio_cuidado)
  
  
  ## 3.2. Define an alphabet for the different states an individual can obtain
  ## -------------------------------------------------------------------------
  
      # Alphabet 1 (4-5 states)
        # DF = Disability Free
        # ID = Idenpendent albeit first disability
        # HH = Help with Housework combined with "needs assistance" (NA) 
        # IM = Impaired Mobility
        # DC = Dependent on Caretaker
      
        # Think about a censorship state for younger individuals (C)
  
  SeqAlphab_1 <- c("DF","ID", "HH", "DC", "IM","C")
  
  # 3.3. Define color scheme
  display.brewer.all()
  Brewer_1 <- brewer.pal(6, "Dark2")
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
  
  # Alphabet 2 (4-5 states)
  # DF = Disability Free
  # ID = Idenpendent albeit first disability
  # DC = Dependent on Caretaker
  # RC = Right Censored
  
  SeqAlphab_2 <- c("DF","ID", "DC", "RC")
  
  # 3.3. Define color scheme
  display.brewer.all()
  Brewer_2 <- brewer.pal(4, "Dark2")
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
  
  
  
  # -----------------  
  # Fun with strings
  # -----------------
  
  # Preparation: round the age of first disability and dependency
  tra_may <- tra_may %>% mutate(age_f_dis = round(EdadInicioDisca44,0)) %>% 
                         mutate(age_dep = round(Edadinicio_cuidado,0))
  
  # Approach 1
  #
  #  This needs a fix!
  #
  tra_may <- tra_may %>% mutate(disstring = ifelse(EdadInicioDisca44<50, paste(rep("FD", 50), collapse = "-"),
                                                   ifelse(EdadInicioDisca44>=50, paste0(paste(strrep("DF", age_f_dis-50), collapse = "-"),
                                                          paste(strrep("FD",100-age_f_dis), collapse = "-"), sep = " "),
                                                   ifelse(
                                                            
                                                          ))))
  
  tra_may <- tra_may %>% mutate(time_df = ifelse(EdadInicioDisca44>=50, round(age_f_dis-49.9,0), 50)) %>% mutate(time_fd = 101-age_f_dis) %>% 
                         mutate(disstring = ifelse(EdadInicioDisca44<50, paste(rep("FD", 50), collapse = "-"),
                                                   ifelse(EdadInicioDisca44>=50, paste0(paste(strrep("DF", time_df), collapse = "-"),
                                                                                        paste(strrep("FD",time_fd), collapse = "-"), sep = " "),"A"
                                                          )))
  
  # # With a vector
  # d <- c("fig", "grapefruit", "honeydew")
  # 
  # # If the input is a vector, use collapse to put the elements together:
  # paste(d, collapse=", ")
  # #> [1] "fig, grapefruit, honeydew"
  
  paste0(strrep(0, 10 - nchar(test$x)), test$x)
  
  
  # Approach 2
  
  tra_may$disstring <- ifelse(tra_may$EdadInicioDisca44<50,strrep("FD",50),0)
  tra_may$disstring <- ifelse(tra_may$EdadInicioDisca44>50, paste(rep("DF", 50:round(tra_may$EdadInicioDisca44,0))))
  
  
  
  #### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
  #### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
  
    # Approach 3 - Try it with durations which a person spent in a particular state and combine everything in the end with 3 1/2 states for now
  
  # For now under the assumption (can be verified in the aftermath) that the states are inconvertible
  
  tra_may <- tra_may %>% 
    
    # 1. start with the duration of disability free years after 50
      # Calculated as age after age 50 when first disability occurs - age 50 (= disability free duration after age 50)
    mutate(dur_DF = ifelse(EdadInicioDisca44>=50,round(EdadInicioDisca44-50,0),0)) %>% 
    
    # 2. second state duration after onset of disability but before help or assistance is needed
    ### Important to remember is that independence does not necessarily mean that a diagnosed disability has occurred
      
    # Calculated as: Age at when personal assistance is needed - Age at onset of disability/impairment OR if the age when first
    # personal assistance is requested was first, this age minus age 50, if they age of first assistance occured first, the independent time is 0

    mutate(dur_ID = ifelse(Edadinicio_cuidado>=50 & Edadinicio_cuidado > EdadInicioDisca44, 
                           round(((Edadinicio_cuidado - EdadInicioDisca44)-(50-EdadInicioDisca44)),0),
                           ifelse(Edadinicio_cuidado>=50 & EdadInicioDisca44>=50 & Edadinicio_cuidado <= EdadInicioDisca44,round(Edadinicio_cuidado,0) - 50,0))) %>% 
    
    # Little trick to avoid problems with cases where care taking occured before the diagnosis of disability
    mutate(dur_DF = ifelse(dur_DF>=dur_ID, 0, dur_DF)) %>%
    # A further change necessary to obtain the difference between onset of disability and dependency as time independent
    mutate(dur_ID = dur_ID-dur_DF) %>% 
    
    # 3. Duration in Dependency until right censoring
    mutate(dur_DC = EDAD-round(Edadinicio_cuidado,0)) %>% 
    # 4. State 4 is just for programming reasons - Censorship (duration between age at interview and 100)
    mutate(dur_C = ifelse(EDAD>100, 0, 100-EDAD))

  
  ### Now prepare the compressed sequences from here:
  
  tra_may <- tra_may %>% mutate(disstring = paste(rep("DF", dur_DF), rep("ID", dur_ID), rep("DC", dur_DC), rep("C", dur_C), collapse = "-"))
  
  ### Toy version with for loop
# 
#   for(i in min(tra_may$Id):max(tra_may$Id)){
#     tra_may$distring[tra_may$Id==i] <- paste(rep("DF", dur_DF[tra_may$Id==i]), rep("ID", dur_ID[tra_may$Id==i]), rep("DC", dur_DC[tra_may$Id==i]), 
#                                              rep("C", dur_C[tra_may$Id==i]), collapse = "-")
#   }
#   
  
  
  ### Another toy version
  
  tra <- matrix(nrow = length(tra_may$Id), ncol = 51)
  tra[,1] <- tra_may$Id
  
  
  tra_test <- tra_may[1:20,]
  tra_test <- tra_test %>% select(Id,dur_DF,dur_ID, dur_DC, dur_C)
  
  
  Test_X <- as.data.frame(c(tra_test$Id))
  
  
  Test_X <- matrix(c(tra_test$Id), nrow = length(tra_test$Id), ncol = 1)                          
  Test_X <- cbind(Test_X, matrix(0, nrow = length(tra_test$Id), ncol = 50))
  
  
  
  
  for(i in min(tra_test$Id):max(tra_test$Id)){
  
  Test_X[,] <-  c(rep("DF", tra_test$dur_DF[tra_test$Id==i]),rep("ID", tra_test$dur_ID[tra_test$Id==i]),
                  rep("DC", tra_test$dur_DC[tra_test$Id==i]), rep("C",tra_test$dur_C[tra_test$Id==i]))
  }
  
  
  
  
  
  
  
  
  for(i in min(tra_test$Id):max(tra_test$Id)){
    
    B = matrix( 
     c(rep("DF", tra_test$dur_DF[tra_test$Id==i]),rep("ID", tra_test$dur_ID[tra_test$Id==i]),
        rep("DC", tra_test$dur_DC[tra_test$Id==i]), rep("C",tra_test$dur_C[tra_test$Id==i])), 
         nrow=length(tra_test$Id), 
         ncol=50, byrow = T) 
  }
    
    
    
  test_vec <- as.vector(c(rep("DF", tra_may$dur_DF[tra_may$Id==min(tra_may$Id)]),rep("ID", tra_may$dur_ID[tra_may$Id==min(tra_may$Id)]),
                          rep("DC", tra_may$dur_DC[tra_may$Id==min(tra_may$Id)]), rep("C",tra_may$dur_C[tra_may$Id==min(tra_may$Id)])))
  
  tra <- as.data.frame(tra)
  
  
  
  
  seq_b <- tra_may[,12]
                                
  tra_mat <- seqdecomp(seq_b)
    
  #### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
  #### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####

  
  
  
  # Prepare Matrix for the seqdef() command
  cbind(names(tra_may))
  
  seqmat <- tra_may[,c(13:25)]
  