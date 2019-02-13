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
library(cluster)


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
  tra_may <- link.may %>% select(Id, SEXO, EDAD, EdadInicioDisca44, edadiniciodisca12A, edadiniciodisca12B, edadiniciodisca12C, Edadinicio_cuidado, age.ex)
  
  
  ## 3.2. Define an alphabet for the different states an individual can obtain
  ## -------------------------------------------------------------------------
  
      # Alphabet 1 (5 states)
        # DF = Disability Free
        # ID = Idenpendent albeit first disability
        # ADL = Problems ADL - Help with Housework combined with "needs assistance" (NA) 
        # DC = Dependent on Caretaker
        # C = Censored
      
        # Think about a censorship state for younger individuals (C)
  
  SeqAlphab_1 <- c("DF","ID", "ADL", "DC", "C")
  
  # 3.3. Define color scheme
  display.brewer.all()
  Brewer_1 <- brewer.pal(5, "Dark2")
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
  
  # Alphabet 2 (4-5 states)
  # DF = Disability Free
  # ID = Idenpendent albeit first disability
  # DC = Dependent on Caretaker
  # RC = Right Censored
  
  SeqAlphab_2 <- c("DF","ID", "DC", "C")
  
  # 3.3. Define color scheme
  display.brewer.all()
  Brewer_2 <- brewer.pal(4, "Dark2")
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
  
  
  
  # # -----------------  
  # # Fun with strings
  # # -----------------
  # 
  # # Preparation: round the age of first disability and dependency
  # tra_may <- tra_may %>% mutate(age_f_dis = round(EdadInicioDisca44,0)) %>% 
  #                        mutate(age_dep = round(Edadinicio_cuidado,0))
  # 
  # # Approach 1
  # #
  # #  This needs a fix!
  # #
  # tra_may <- tra_may %>% mutate(disstring = ifelse(EdadInicioDisca44<50, paste(rep("FD", 50), collapse = "-"),
  #                                                  ifelse(EdadInicioDisca44>=50, paste0(paste(strrep("DF", age_f_dis-50), collapse = "-"),
  #                                                         paste(strrep("FD",100-age_f_dis), collapse = "-"), sep = " "),
  #                                                  ifelse(
  #                                                           
  #                                                         ))))
  # 
  # tra_may <- tra_may %>% mutate(time_df = ifelse(EdadInicioDisca44>=50, round(age_f_dis-49.9,0), 50)) %>% mutate(time_fd = 101-age_f_dis) %>% 
  #                        mutate(disstring = ifelse(EdadInicioDisca44<50, paste(rep("FD", 50), collapse = "-"),
  #                                                  ifelse(EdadInicioDisca44>=50, paste0(paste(strrep("DF", time_df), collapse = "-"),
  #                                                                                       paste(strrep("FD",time_fd), collapse = "-"), sep = " "),"A"
  #                                                         )))
  # 
  # # # With a vector
  # # d <- c("fig", "grapefruit", "honeydew")
  # # 
  # # # If the input is a vector, use collapse to put the elements together:
  # # paste(d, collapse=", ")
  # # #> [1] "fig, grapefruit, honeydew"
  # 
  # paste0(strrep(0, 10 - nchar(test$x)), test$x)
  # 
  # 
  # # Approach 2
  # 
  # tra_may$disstring <- ifelse(tra_may$EdadInicioDisca44<50,strrep("FD",50),0)
  # tra_may$disstring <- ifelse(tra_may$EdadInicioDisca44>50, paste(rep("DF", 50:round(tra_may$EdadInicioDisca44,0))))
  
  
  
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
  
  # tra_may <- tra_may %>% mutate(disstring = paste(rep("DF", dur_DF), rep("ID", dur_ID), rep("DC", dur_DC), rep("C", dur_C), collapse = "-"))
  # Doesn´t work because of invalid time argument
  
### Kindergarten dataframe approach (One Variable for EACH time point) - copy and paste work
tra_may <- tra_may %>% mutate("50" = ifelse(EdadInicioDisca44>=50, "DF", ifelse(Edadinicio_cuidado>=50, "ID", ifelse(EDAD>=50,"DC", "C")))) %>% 
  mutate("51" = ifelse(EdadInicioDisca44>=51, "DF", ifelse(Edadinicio_cuidado>=51, "ID", ifelse(EDAD>=51,"DC", "C")))) %>% 
  mutate("52" = ifelse(EdadInicioDisca44>=52, "DF", ifelse(Edadinicio_cuidado>=52, "ID", ifelse(EDAD>=52,"DC", "C")))) %>% 
  mutate("53" = ifelse(EdadInicioDisca44>=53, "DF", ifelse(Edadinicio_cuidado>=53, "ID", ifelse(EDAD>=53,"DC", "C")))) %>% 
  mutate("54" = ifelse(EdadInicioDisca44>=54, "DF", ifelse(Edadinicio_cuidado>=54, "ID", ifelse(EDAD>=54,"DC", "C")))) %>% 
  mutate("55" = ifelse(EdadInicioDisca44>=55, "DF", ifelse(Edadinicio_cuidado>=55, "ID", ifelse(EDAD>=55,"DC", "C")))) %>% 
  mutate("56" = ifelse(EdadInicioDisca44>=56, "DF", ifelse(Edadinicio_cuidado>=56, "ID", ifelse(EDAD>=56,"DC", "C")))) %>% 
  mutate("57" = ifelse(EdadInicioDisca44>=57, "DF", ifelse(Edadinicio_cuidado>=57, "ID", ifelse(EDAD>=57,"DC", "C")))) %>% 
  mutate("58" = ifelse(EdadInicioDisca44>=58, "DF", ifelse(Edadinicio_cuidado>=58, "ID", ifelse(EDAD>=58,"DC", "C")))) %>% 
  mutate("59" = ifelse(EdadInicioDisca44>=59, "DF", ifelse(Edadinicio_cuidado>=59, "ID", ifelse(EDAD>=59,"DC", "C")))) %>% 
  mutate("60" = ifelse(EdadInicioDisca44>=60, "DF", ifelse(Edadinicio_cuidado>=60, "ID", ifelse(EDAD>=60,"DC", "C")))) %>% 
  mutate("61" = ifelse(EdadInicioDisca44>=61, "DF", ifelse(Edadinicio_cuidado>=61, "ID", ifelse(EDAD>=61,"DC", "C")))) %>% 
  mutate("62" = ifelse(EdadInicioDisca44>=62, "DF", ifelse(Edadinicio_cuidado>=62, "ID", ifelse(EDAD>=62,"DC", "C")))) %>% 
  mutate("63" = ifelse(EdadInicioDisca44>=63, "DF", ifelse(Edadinicio_cuidado>=63, "ID", ifelse(EDAD>=63,"DC", "C")))) %>% 
  mutate("64" = ifelse(EdadInicioDisca44>=64, "DF", ifelse(Edadinicio_cuidado>=64, "ID", ifelse(EDAD>=64,"DC", "C")))) %>% 
  mutate("65" = ifelse(EdadInicioDisca44>=65, "DF", ifelse(Edadinicio_cuidado>=65, "ID", ifelse(EDAD>=65,"DC", "C")))) %>% 
  mutate("66" = ifelse(EdadInicioDisca44>=66, "DF", ifelse(Edadinicio_cuidado>=66, "ID", ifelse(EDAD>=66,"DC", "C")))) %>% 
  mutate("67" = ifelse(EdadInicioDisca44>=67, "DF", ifelse(Edadinicio_cuidado>=67, "ID", ifelse(EDAD>=67,"DC", "C")))) %>% 
  mutate("68" = ifelse(EdadInicioDisca44>=68, "DF", ifelse(Edadinicio_cuidado>=68, "ID", ifelse(EDAD>=68,"DC", "C")))) %>% 
  mutate("69" = ifelse(EdadInicioDisca44>=69, "DF", ifelse(Edadinicio_cuidado>=69, "ID", ifelse(EDAD>=69,"DC", "C")))) %>% 
  mutate("70" = ifelse(EdadInicioDisca44>=70, "DF", ifelse(Edadinicio_cuidado>=70, "ID", ifelse(EDAD>=70,"DC", "C")))) %>% 
  mutate("71" = ifelse(EdadInicioDisca44>=71, "DF", ifelse(Edadinicio_cuidado>=71, "ID", ifelse(EDAD>=71,"DC", "C")))) %>% 
  mutate("72" = ifelse(EdadInicioDisca44>=72, "DF", ifelse(Edadinicio_cuidado>=72, "ID", ifelse(EDAD>=72,"DC", "C")))) %>% 
  mutate("73" = ifelse(EdadInicioDisca44>=73, "DF", ifelse(Edadinicio_cuidado>=73, "ID", ifelse(EDAD>=73,"DC", "C")))) %>% 
  mutate("74" = ifelse(EdadInicioDisca44>=74, "DF", ifelse(Edadinicio_cuidado>=74, "ID", ifelse(EDAD>=74,"DC", "C")))) %>% 
  mutate("75" = ifelse(EdadInicioDisca44>=75, "DF", ifelse(Edadinicio_cuidado>=75, "ID", ifelse(EDAD>=75,"DC", "C")))) %>% 
  mutate("76" = ifelse(EdadInicioDisca44>=76, "DF", ifelse(Edadinicio_cuidado>=76, "ID", ifelse(EDAD>=76,"DC", "C")))) %>% 
  mutate("77" = ifelse(EdadInicioDisca44>=77, "DF", ifelse(Edadinicio_cuidado>=77, "ID", ifelse(EDAD>=77,"DC", "C")))) %>% 
  mutate("78" = ifelse(EdadInicioDisca44>=78, "DF", ifelse(Edadinicio_cuidado>=78, "ID", ifelse(EDAD>=78,"DC", "C")))) %>% 
  mutate("79" = ifelse(EdadInicioDisca44>=79, "DF", ifelse(Edadinicio_cuidado>=79, "ID", ifelse(EDAD>=79,"DC", "C")))) %>% 
  mutate("80" = ifelse(EdadInicioDisca44>=80, "DF", ifelse(Edadinicio_cuidado>=80, "ID", ifelse(EDAD>=80,"DC", "C")))) %>% 
  mutate("81" = ifelse(EdadInicioDisca44>=81, "DF", ifelse(Edadinicio_cuidado>=81, "ID", ifelse(EDAD>=81,"DC", "C")))) %>% 
  mutate("82" = ifelse(EdadInicioDisca44>=82, "DF", ifelse(Edadinicio_cuidado>=82, "ID", ifelse(EDAD>=82,"DC", "C")))) %>% 
  mutate("83" = ifelse(EdadInicioDisca44>=83, "DF", ifelse(Edadinicio_cuidado>=83, "ID", ifelse(EDAD>=83,"DC", "C")))) %>% 
  mutate("84" = ifelse(EdadInicioDisca44>=84, "DF", ifelse(Edadinicio_cuidado>=84, "ID", ifelse(EDAD>=84,"DC", "C")))) %>% 
  mutate("85" = ifelse(EdadInicioDisca44>=85, "DF", ifelse(Edadinicio_cuidado>=85, "ID", ifelse(EDAD>=85,"DC", "C")))) %>% 
  mutate("86" = ifelse(EdadInicioDisca44>=86, "DF", ifelse(Edadinicio_cuidado>=86, "ID", ifelse(EDAD>=86,"DC", "C")))) %>% 
  mutate("87" = ifelse(EdadInicioDisca44>=87, "DF", ifelse(Edadinicio_cuidado>=87, "ID", ifelse(EDAD>=87,"DC", "C")))) %>% 
  mutate("88" = ifelse(EdadInicioDisca44>=88, "DF", ifelse(Edadinicio_cuidado>=88, "ID", ifelse(EDAD>=88,"DC", "C")))) %>% 
  mutate("89" = ifelse(EdadInicioDisca44>=89, "DF", ifelse(Edadinicio_cuidado>=89, "ID", ifelse(EDAD>=89,"DC", "C")))) %>% 
  mutate("90" = ifelse(EdadInicioDisca44>=90, "DF", ifelse(Edadinicio_cuidado>=90, "ID", ifelse(EDAD>=90,"DC", "C")))) %>% 
  mutate("91" = ifelse(EdadInicioDisca44>=91, "DF", ifelse(Edadinicio_cuidado>=91, "ID", ifelse(EDAD>=91,"DC", "C")))) %>% 
  mutate("92" = ifelse(EdadInicioDisca44>=92, "DF", ifelse(Edadinicio_cuidado>=92, "ID", ifelse(EDAD>=92,"DC", "C")))) %>% 
  mutate("93" = ifelse(EdadInicioDisca44>=93, "DF", ifelse(Edadinicio_cuidado>=93, "ID", ifelse(EDAD>=93,"DC", "C")))) %>% 
  mutate("94" = ifelse(EdadInicioDisca44>=94, "DF", ifelse(Edadinicio_cuidado>=94, "ID", ifelse(EDAD>=94,"DC", "C")))) %>% 
  mutate("95" = ifelse(EdadInicioDisca44>=95, "DF", ifelse(Edadinicio_cuidado>=95, "ID", ifelse(EDAD>=95,"DC", "C")))) %>% 
  mutate("96" = ifelse(EdadInicioDisca44>=96, "DF", ifelse(Edadinicio_cuidado>=96, "ID", ifelse(EDAD>=96,"DC", "C")))) %>% 
  mutate("97" = ifelse(EdadInicioDisca44>=97, "DF", ifelse(Edadinicio_cuidado>=97, "ID", ifelse(EDAD>=97,"DC", "C")))) %>% 
  mutate("98" = ifelse(EdadInicioDisca44>=98, "DF", ifelse(Edadinicio_cuidado>=98, "ID", ifelse(EDAD>=98,"DC", "C")))) %>% 
  mutate("99" = ifelse(EdadInicioDisca44>=99, "DF", ifelse(Edadinicio_cuidado>=99, "ID", ifelse(EDAD>=99,"DC", "C")))) %>% 
  mutate("100" = ifelse(EdadInicioDisca44>=100, "DF", ifelse(Edadinicio_cuidado>=100, "ID", ifelse(EDAD>=100,"DC", "C"))))


### Kindergarten dataframe approach (One Variable for EACH time point) - copy and paste work
tra_may_C <- tra_may %>% mutate("50" = ifelse(EdadInicioDisca44>=50, "DF", ifelse(Edadinicio_cuidado>=50, "ID", ifelse(EDAD>=50,"DC", NA)))) %>% 
  mutate("51" = ifelse(EdadInicioDisca44>=51, "DF", ifelse(Edadinicio_cuidado>=51, "ID", ifelse(EDAD>=51,"DC", NA)))) %>% 
  mutate("52" = ifelse(EdadInicioDisca44>=52, "DF", ifelse(Edadinicio_cuidado>=52, "ID", ifelse(EDAD>=52,"DC", NA)))) %>% 
  mutate("53" = ifelse(EdadInicioDisca44>=53, "DF", ifelse(Edadinicio_cuidado>=53, "ID", ifelse(EDAD>=53,"DC", NA)))) %>% 
  mutate("54" = ifelse(EdadInicioDisca44>=54, "DF", ifelse(Edadinicio_cuidado>=54, "ID", ifelse(EDAD>=54,"DC", NA)))) %>% 
  mutate("55" = ifelse(EdadInicioDisca44>=55, "DF", ifelse(Edadinicio_cuidado>=55, "ID", ifelse(EDAD>=55,"DC", NA)))) %>% 
  mutate("56" = ifelse(EdadInicioDisca44>=56, "DF", ifelse(Edadinicio_cuidado>=56, "ID", ifelse(EDAD>=56,"DC", NA)))) %>% 
  mutate("57" = ifelse(EdadInicioDisca44>=57, "DF", ifelse(Edadinicio_cuidado>=57, "ID", ifelse(EDAD>=57,"DC", NA)))) %>% 
  mutate("58" = ifelse(EdadInicioDisca44>=58, "DF", ifelse(Edadinicio_cuidado>=58, "ID", ifelse(EDAD>=58,"DC", NA)))) %>% 
  mutate("59" = ifelse(EdadInicioDisca44>=59, "DF", ifelse(Edadinicio_cuidado>=59, "ID", ifelse(EDAD>=59,"DC", NA)))) %>% 
  mutate("60" = ifelse(EdadInicioDisca44>=60, "DF", ifelse(Edadinicio_cuidado>=60, "ID", ifelse(EDAD>=60,"DC", NA)))) %>% 
  mutate("61" = ifelse(EdadInicioDisca44>=61, "DF", ifelse(Edadinicio_cuidado>=61, "ID", ifelse(EDAD>=61,"DC", NA)))) %>% 
  mutate("62" = ifelse(EdadInicioDisca44>=62, "DF", ifelse(Edadinicio_cuidado>=62, "ID", ifelse(EDAD>=62,"DC", NA)))) %>% 
  mutate("63" = ifelse(EdadInicioDisca44>=63, "DF", ifelse(Edadinicio_cuidado>=63, "ID", ifelse(EDAD>=63,"DC", NA)))) %>% 
  mutate("64" = ifelse(EdadInicioDisca44>=64, "DF", ifelse(Edadinicio_cuidado>=64, "ID", ifelse(EDAD>=64,"DC", NA)))) %>% 
  mutate("65" = ifelse(EdadInicioDisca44>=65, "DF", ifelse(Edadinicio_cuidado>=65, "ID", ifelse(EDAD>=65,"DC", NA)))) %>% 
  mutate("66" = ifelse(EdadInicioDisca44>=66, "DF", ifelse(Edadinicio_cuidado>=66, "ID", ifelse(EDAD>=66,"DC", NA)))) %>% 
  mutate("67" = ifelse(EdadInicioDisca44>=67, "DF", ifelse(Edadinicio_cuidado>=67, "ID", ifelse(EDAD>=67,"DC", NA)))) %>% 
  mutate("68" = ifelse(EdadInicioDisca44>=68, "DF", ifelse(Edadinicio_cuidado>=68, "ID", ifelse(EDAD>=68,"DC", NA)))) %>% 
  mutate("69" = ifelse(EdadInicioDisca44>=69, "DF", ifelse(Edadinicio_cuidado>=69, "ID", ifelse(EDAD>=69,"DC", NA)))) %>% 
  mutate("70" = ifelse(EdadInicioDisca44>=70, "DF", ifelse(Edadinicio_cuidado>=70, "ID", ifelse(EDAD>=70,"DC", NA)))) %>% 
  mutate("71" = ifelse(EdadInicioDisca44>=71, "DF", ifelse(Edadinicio_cuidado>=71, "ID", ifelse(EDAD>=71,"DC", NA)))) %>% 
  mutate("72" = ifelse(EdadInicioDisca44>=72, "DF", ifelse(Edadinicio_cuidado>=72, "ID", ifelse(EDAD>=72,"DC", NA)))) %>% 
  mutate("73" = ifelse(EdadInicioDisca44>=73, "DF", ifelse(Edadinicio_cuidado>=73, "ID", ifelse(EDAD>=73,"DC", NA)))) %>% 
  mutate("74" = ifelse(EdadInicioDisca44>=74, "DF", ifelse(Edadinicio_cuidado>=74, "ID", ifelse(EDAD>=74,"DC", NA)))) %>% 
  mutate("75" = ifelse(EdadInicioDisca44>=75, "DF", ifelse(Edadinicio_cuidado>=75, "ID", ifelse(EDAD>=75,"DC", NA)))) %>% 
  mutate("76" = ifelse(EdadInicioDisca44>=76, "DF", ifelse(Edadinicio_cuidado>=76, "ID", ifelse(EDAD>=76,"DC", NA)))) %>% 
  mutate("77" = ifelse(EdadInicioDisca44>=77, "DF", ifelse(Edadinicio_cuidado>=77, "ID", ifelse(EDAD>=77,"DC", NA)))) %>% 
  mutate("78" = ifelse(EdadInicioDisca44>=78, "DF", ifelse(Edadinicio_cuidado>=78, "ID", ifelse(EDAD>=78,"DC", NA)))) %>% 
  mutate("79" = ifelse(EdadInicioDisca44>=79, "DF", ifelse(Edadinicio_cuidado>=79, "ID", ifelse(EDAD>=79,"DC", NA)))) %>% 
  mutate("80" = ifelse(EdadInicioDisca44>=80, "DF", ifelse(Edadinicio_cuidado>=80, "ID", ifelse(EDAD>=80,"DC", NA)))) %>% 
  mutate("81" = ifelse(EdadInicioDisca44>=81, "DF", ifelse(Edadinicio_cuidado>=81, "ID", ifelse(EDAD>=81,"DC", NA)))) %>% 
  mutate("82" = ifelse(EdadInicioDisca44>=82, "DF", ifelse(Edadinicio_cuidado>=82, "ID", ifelse(EDAD>=82,"DC", NA)))) %>% 
  mutate("83" = ifelse(EdadInicioDisca44>=83, "DF", ifelse(Edadinicio_cuidado>=83, "ID", ifelse(EDAD>=83,"DC", NA)))) %>% 
  mutate("84" = ifelse(EdadInicioDisca44>=84, "DF", ifelse(Edadinicio_cuidado>=84, "ID", ifelse(EDAD>=84,"DC", NA)))) %>% 
  mutate("85" = ifelse(EdadInicioDisca44>=85, "DF", ifelse(Edadinicio_cuidado>=85, "ID", ifelse(EDAD>=85,"DC", NA)))) %>% 
  mutate("86" = ifelse(EdadInicioDisca44>=86, "DF", ifelse(Edadinicio_cuidado>=86, "ID", ifelse(EDAD>=86,"DC", NA)))) %>% 
  mutate("87" = ifelse(EdadInicioDisca44>=87, "DF", ifelse(Edadinicio_cuidado>=87, "ID", ifelse(EDAD>=87,"DC", NA)))) %>% 
  mutate("88" = ifelse(EdadInicioDisca44>=88, "DF", ifelse(Edadinicio_cuidado>=88, "ID", ifelse(EDAD>=88,"DC", NA)))) %>% 
  mutate("89" = ifelse(EdadInicioDisca44>=89, "DF", ifelse(Edadinicio_cuidado>=89, "ID", ifelse(EDAD>=89,"DC", NA)))) %>% 
  mutate("90" = ifelse(EdadInicioDisca44>=90, "DF", ifelse(Edadinicio_cuidado>=90, "ID", ifelse(EDAD>=90,"DC", NA)))) %>% 
  mutate("91" = ifelse(EdadInicioDisca44>=91, "DF", ifelse(Edadinicio_cuidado>=91, "ID", ifelse(EDAD>=91,"DC", NA)))) %>% 
  mutate("92" = ifelse(EdadInicioDisca44>=92, "DF", ifelse(Edadinicio_cuidado>=92, "ID", ifelse(EDAD>=92,"DC", NA)))) %>% 
  mutate("93" = ifelse(EdadInicioDisca44>=93, "DF", ifelse(Edadinicio_cuidado>=93, "ID", ifelse(EDAD>=93,"DC", NA)))) %>% 
  mutate("94" = ifelse(EdadInicioDisca44>=94, "DF", ifelse(Edadinicio_cuidado>=94, "ID", ifelse(EDAD>=94,"DC", NA)))) %>% 
  mutate("95" = ifelse(EdadInicioDisca44>=95, "DF", ifelse(Edadinicio_cuidado>=95, "ID", ifelse(EDAD>=95,"DC", NA)))) %>% 
  mutate("96" = ifelse(EdadInicioDisca44>=96, "DF", ifelse(Edadinicio_cuidado>=96, "ID", ifelse(EDAD>=96,"DC", NA)))) %>% 
  mutate("97" = ifelse(EdadInicioDisca44>=97, "DF", ifelse(Edadinicio_cuidado>=97, "ID", ifelse(EDAD>=97,"DC", NA)))) %>% 
  mutate("98" = ifelse(EdadInicioDisca44>=98, "DF", ifelse(Edadinicio_cuidado>=98, "ID", ifelse(EDAD>=98,"DC", NA)))) %>% 
  mutate("99" = ifelse(EdadInicioDisca44>=99, "DF", ifelse(Edadinicio_cuidado>=99, "ID", ifelse(EDAD>=99,"DC", NA)))) %>% 
  mutate("100" = ifelse(EdadInicioDisca44>=100, "DF", ifelse(Edadinicio_cuidado>=100, "ID", ifelse(EDAD>=100,"DC", NA))))
  


### Kindergarten dataframe approach (One Variable for EACH time point) - copy and paste work
### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###

### NEW: Disability Variable
### ------------------------
tra_may_12A <- tra_may %>% 
  # first taking care of those without A12 disability onset (make it the general)
  mutate(edadiniciodisca12A = ifelse(is.na(edadiniciodisca12A),EdadInicioDisca44,edadiniciodisca12A)) %>% 
  # now the hand work
  mutate("50" = ifelse(edadiniciodisca12A>=50, "DF", ifelse(Edadinicio_cuidado>=50, "ID", ifelse(EDAD>=50,"DC", NA)))) %>% 
  mutate("51" = ifelse(edadiniciodisca12A>=51, "DF", ifelse(Edadinicio_cuidado>=51, "ID", ifelse(EDAD>=51,"DC", NA)))) %>% 
  mutate("52" = ifelse(edadiniciodisca12A>=52, "DF", ifelse(Edadinicio_cuidado>=52, "ID", ifelse(EDAD>=52,"DC", NA)))) %>% 
  mutate("53" = ifelse(edadiniciodisca12A>=53, "DF", ifelse(Edadinicio_cuidado>=53, "ID", ifelse(EDAD>=53,"DC", NA)))) %>% 
  mutate("54" = ifelse(edadiniciodisca12A>=54, "DF", ifelse(Edadinicio_cuidado>=54, "ID", ifelse(EDAD>=54,"DC", NA)))) %>% 
  mutate("55" = ifelse(edadiniciodisca12A>=55, "DF", ifelse(Edadinicio_cuidado>=55, "ID", ifelse(EDAD>=55,"DC", NA)))) %>% 
  mutate("56" = ifelse(edadiniciodisca12A>=56, "DF", ifelse(Edadinicio_cuidado>=56, "ID", ifelse(EDAD>=56,"DC", NA)))) %>% 
  mutate("57" = ifelse(edadiniciodisca12A>=57, "DF", ifelse(Edadinicio_cuidado>=57, "ID", ifelse(EDAD>=57,"DC", NA)))) %>% 
  mutate("58" = ifelse(edadiniciodisca12A>=58, "DF", ifelse(Edadinicio_cuidado>=58, "ID", ifelse(EDAD>=58,"DC", NA)))) %>% 
  mutate("59" = ifelse(edadiniciodisca12A>=59, "DF", ifelse(Edadinicio_cuidado>=59, "ID", ifelse(EDAD>=59,"DC", NA)))) %>% 
  mutate("60" = ifelse(edadiniciodisca12A>=60, "DF", ifelse(Edadinicio_cuidado>=60, "ID", ifelse(EDAD>=60,"DC", NA)))) %>% 
  mutate("61" = ifelse(edadiniciodisca12A>=61, "DF", ifelse(Edadinicio_cuidado>=61, "ID", ifelse(EDAD>=61,"DC", NA)))) %>% 
  mutate("62" = ifelse(edadiniciodisca12A>=62, "DF", ifelse(Edadinicio_cuidado>=62, "ID", ifelse(EDAD>=62,"DC", NA)))) %>% 
  mutate("63" = ifelse(edadiniciodisca12A>=63, "DF", ifelse(Edadinicio_cuidado>=63, "ID", ifelse(EDAD>=63,"DC", NA)))) %>% 
  mutate("64" = ifelse(edadiniciodisca12A>=64, "DF", ifelse(Edadinicio_cuidado>=64, "ID", ifelse(EDAD>=64,"DC", NA)))) %>% 
  mutate("65" = ifelse(edadiniciodisca12A>=65, "DF", ifelse(Edadinicio_cuidado>=65, "ID", ifelse(EDAD>=65,"DC", NA)))) %>% 
  mutate("66" = ifelse(edadiniciodisca12A>=66, "DF", ifelse(Edadinicio_cuidado>=66, "ID", ifelse(EDAD>=66,"DC", NA)))) %>% 
  mutate("67" = ifelse(edadiniciodisca12A>=67, "DF", ifelse(Edadinicio_cuidado>=67, "ID", ifelse(EDAD>=67,"DC", NA)))) %>% 
  mutate("68" = ifelse(edadiniciodisca12A>=68, "DF", ifelse(Edadinicio_cuidado>=68, "ID", ifelse(EDAD>=68,"DC", NA)))) %>% 
  mutate("69" = ifelse(edadiniciodisca12A>=69, "DF", ifelse(Edadinicio_cuidado>=69, "ID", ifelse(EDAD>=69,"DC", NA)))) %>% 
  mutate("70" = ifelse(edadiniciodisca12A>=70, "DF", ifelse(Edadinicio_cuidado>=70, "ID", ifelse(EDAD>=70,"DC", NA)))) %>% 
  mutate("71" = ifelse(edadiniciodisca12A>=71, "DF", ifelse(Edadinicio_cuidado>=71, "ID", ifelse(EDAD>=71,"DC", NA)))) %>% 
  mutate("72" = ifelse(edadiniciodisca12A>=72, "DF", ifelse(Edadinicio_cuidado>=72, "ID", ifelse(EDAD>=72,"DC", NA)))) %>% 
  mutate("73" = ifelse(edadiniciodisca12A>=73, "DF", ifelse(Edadinicio_cuidado>=73, "ID", ifelse(EDAD>=73,"DC", NA)))) %>% 
  mutate("74" = ifelse(edadiniciodisca12A>=74, "DF", ifelse(Edadinicio_cuidado>=74, "ID", ifelse(EDAD>=74,"DC", NA)))) %>% 
  mutate("75" = ifelse(edadiniciodisca12A>=75, "DF", ifelse(Edadinicio_cuidado>=75, "ID", ifelse(EDAD>=75,"DC", NA)))) %>% 
  mutate("76" = ifelse(edadiniciodisca12A>=76, "DF", ifelse(Edadinicio_cuidado>=76, "ID", ifelse(EDAD>=76,"DC", NA)))) %>% 
  mutate("77" = ifelse(edadiniciodisca12A>=77, "DF", ifelse(Edadinicio_cuidado>=77, "ID", ifelse(EDAD>=77,"DC", NA)))) %>% 
  mutate("78" = ifelse(edadiniciodisca12A>=78, "DF", ifelse(Edadinicio_cuidado>=78, "ID", ifelse(EDAD>=78,"DC", NA)))) %>% 
  mutate("79" = ifelse(edadiniciodisca12A>=79, "DF", ifelse(Edadinicio_cuidado>=79, "ID", ifelse(EDAD>=79,"DC", NA)))) %>% 
  mutate("80" = ifelse(edadiniciodisca12A>=80, "DF", ifelse(Edadinicio_cuidado>=80, "ID", ifelse(EDAD>=80,"DC", NA)))) %>% 
  mutate("81" = ifelse(edadiniciodisca12A>=81, "DF", ifelse(Edadinicio_cuidado>=81, "ID", ifelse(EDAD>=81,"DC", NA)))) %>% 
  mutate("82" = ifelse(edadiniciodisca12A>=82, "DF", ifelse(Edadinicio_cuidado>=82, "ID", ifelse(EDAD>=82,"DC", NA)))) %>% 
  mutate("83" = ifelse(edadiniciodisca12A>=83, "DF", ifelse(Edadinicio_cuidado>=83, "ID", ifelse(EDAD>=83,"DC", NA)))) %>% 
  mutate("84" = ifelse(edadiniciodisca12A>=84, "DF", ifelse(Edadinicio_cuidado>=84, "ID", ifelse(EDAD>=84,"DC", NA)))) %>% 
  mutate("85" = ifelse(edadiniciodisca12A>=85, "DF", ifelse(Edadinicio_cuidado>=85, "ID", ifelse(EDAD>=85,"DC", NA)))) %>% 
  mutate("86" = ifelse(edadiniciodisca12A>=86, "DF", ifelse(Edadinicio_cuidado>=86, "ID", ifelse(EDAD>=86,"DC", NA)))) %>% 
  mutate("87" = ifelse(edadiniciodisca12A>=87, "DF", ifelse(Edadinicio_cuidado>=87, "ID", ifelse(EDAD>=87,"DC", NA)))) %>% 
  mutate("88" = ifelse(edadiniciodisca12A>=88, "DF", ifelse(Edadinicio_cuidado>=88, "ID", ifelse(EDAD>=88,"DC", NA)))) %>% 
  mutate("89" = ifelse(edadiniciodisca12A>=89, "DF", ifelse(Edadinicio_cuidado>=89, "ID", ifelse(EDAD>=89,"DC", NA)))) %>% 
  mutate("90" = ifelse(edadiniciodisca12A>=90, "DF", ifelse(Edadinicio_cuidado>=90, "ID", ifelse(EDAD>=90,"DC", NA)))) %>% 
  mutate("91" = ifelse(edadiniciodisca12A>=91, "DF", ifelse(Edadinicio_cuidado>=91, "ID", ifelse(EDAD>=91,"DC", NA)))) %>% 
  mutate("92" = ifelse(edadiniciodisca12A>=92, "DF", ifelse(Edadinicio_cuidado>=92, "ID", ifelse(EDAD>=92,"DC", NA)))) %>% 
  mutate("93" = ifelse(edadiniciodisca12A>=93, "DF", ifelse(Edadinicio_cuidado>=93, "ID", ifelse(EDAD>=93,"DC", NA)))) %>% 
  mutate("94" = ifelse(edadiniciodisca12A>=94, "DF", ifelse(Edadinicio_cuidado>=94, "ID", ifelse(EDAD>=94,"DC", NA)))) %>% 
  mutate("95" = ifelse(edadiniciodisca12A>=95, "DF", ifelse(Edadinicio_cuidado>=95, "ID", ifelse(EDAD>=95,"DC", NA)))) %>% 
  mutate("96" = ifelse(edadiniciodisca12A>=96, "DF", ifelse(Edadinicio_cuidado>=96, "ID", ifelse(EDAD>=96,"DC", NA)))) %>% 
  mutate("97" = ifelse(edadiniciodisca12A>=97, "DF", ifelse(Edadinicio_cuidado>=97, "ID", ifelse(EDAD>=97,"DC", NA)))) %>% 
  mutate("98" = ifelse(edadiniciodisca12A>=98, "DF", ifelse(Edadinicio_cuidado>=98, "ID", ifelse(EDAD>=98,"DC", NA)))) %>% 
  mutate("99" = ifelse(edadiniciodisca12A>=99, "DF", ifelse(Edadinicio_cuidado>=99, "ID", ifelse(EDAD>=99,"DC", NA)))) %>% 
  mutate("100" = ifelse(edadiniciodisca12A>=100, "DF", ifelse(Edadinicio_cuidado>=100, "ID", ifelse(EDAD>=100,"DC", NA))))

### save data

# save(tra_may, file='020_traMay.RData')
# save(tra_may_C, file='020_traMay_C.RData')
# save(tra_may_12A, file='020_traMay_12A.RData')

#### ---------------------------------------------------------------------------------------------------------------- ####
#### ---------------------------------------------------------------------------------------------------------------- ####
#### ---------------------------------------------------------------------------------------------------------------- ####


# ### Kindergarten dataframe approach (One Variable for EACH time point) - copy and paste work
# 
# 
# tra_may_ADL <- tra_may %>% 
#   
#   # Take care of the NAs in the edadiniciodisca12A/B = Change with "replace" function
#   
#   mutate(edadiniciodisca12A = ifelse(is.na(edadiniciodisca12A), 0, edadiniciodisca12A)) %>% 
#   
#   mutate("50" = ifelse(EdadInicioDisca44>=50, "DF", ifelse(Edadinicio_cuidado>=50, "ID",  
#                                                                                 ifelse(edadiniciodisca12A>=51, "ADL", ifelse(EDAD>=50,"DC", "C"))))) %>% 
#   mutate("51" = ifelse(EdadInicioDisca44>=51, "DF", ifelse(Edadinicio_cuidado>=51, "ID",  
#                                                            ifelse(edadiniciodisca12A>=51, "ADL", ifelse(EDAD>=51,"DC", "C"))))) %>% 
#   mutate("52" = ifelse(EdadInicioDisca44>=52, "DF", ifelse(Edadinicio_cuidado>=52, "ID", 
#                                                            ifelse(edadiniciodisca12A>=52, "ADL", ifelse(EDAD>=52,"DC", "C"))))) %>% 
#   mutate("53" = ifelse(EdadInicioDisca44>=53, "DF", ifelse(Edadinicio_cuidado>=53, "ID", 
#                                                            ifelse(edadiniciodisca12A>=53, "ADL", ifelse(EDAD>=53,"DC", "C"))))) %>% 
#   mutate("54" = ifelse(EdadInicioDisca44>=54, "DF", ifelse(Edadinicio_cuidado>=54, "ID",
#                                                            ifelse(edadiniciodisca12A>=54, "ADL",ifelse(EDAD>=54,"DC", "C"))))) %>% 
#   mutate("55" = ifelse(EdadInicioDisca44>=55, "DF", ifelse(Edadinicio_cuidado>=55, "ID",
#                                                            ifelse(edadiniciodisca12A>=55, "ADL",ifelse(EDAD>=55,"DC", "C"))))) %>% 
#   mutate("56" = ifelse(EdadInicioDisca44>=56, "DF", ifelse(Edadinicio_cuidado>=56, "ID",
#                                                            ifelse(edadiniciodisca12A>=56, "ADL", ifelse(EDAD>=56,"DC", "C"))))) %>% 
#   mutate("57" = ifelse(EdadInicioDisca44>=57, "DF", ifelse(Edadinicio_cuidado>=57, "ID", 
#                                                            ifelse(edadiniciodisca12A>=57, "ADL", ifelse(EDAD>=57,"DC", "C"))))) %>% 
#   mutate("58" = ifelse(EdadInicioDisca44>=58, "DF", ifelse(Edadinicio_cuidado>=58, "ID", 
#                                                            ifelse(edadiniciodisca12A>=58, "ADL", ifelse(EDAD>=58,"DC", "C"))))) %>% 
#   mutate("59" = ifelse(EdadInicioDisca44>=59, "DF", ifelse(Edadinicio_cuidado>=59, "ID", 
#                                                            ifelse(edadiniciodisca12A>=59, "ADL", ifelse(EDAD>=59,"DC", "C"))))) %>% 
#   mutate("60" = ifelse(EdadInicioDisca44>=60, "DF", ifelse(Edadinicio_cuidado>=60, "ID", 
#                                                            ifelse(edadiniciodisca12A>=60, "ADL", ifelse(EDAD>=60,"DC", "C"))))) %>% 
#   mutate("61" = ifelse(EdadInicioDisca44>=61, "DF", ifelse(Edadinicio_cuidado>=61, "ID", 
#                                                            ifelse(edadiniciodisca12A>=61, "ADL", ifelse(EDAD>=61,"DC", "C"))))) %>% 
#   mutate("62" = ifelse(EdadInicioDisca44>=62, "DF", ifelse(Edadinicio_cuidado>=62, "ID", 
#                                                            ifelse(edadiniciodisca12A>=62, "ADL", ifelse(EDAD>=62,"DC", "C"))))) %>% 
#   mutate("63" = ifelse(EdadInicioDisca44>=63, "DF", ifelse(Edadinicio_cuidado>=63, "ID", 
#                                                            ifelse(edadiniciodisca12A>=63, "ADL", ifelse(EDAD>=63,"DC", "C"))))) %>% 
#   mutate("64" = ifelse(EdadInicioDisca44>=64, "DF", ifelse(Edadinicio_cuidado>=64, "ID", 
#                                                            ifelse(edadiniciodisca12A>=64, "ADL", ifelse(EDAD>=64,"DC", "C"))))) %>% 
#   mutate("65" = ifelse(EdadInicioDisca44>=65, "DF", ifelse(Edadinicio_cuidado>=65, "ID", 
#                                                            ifelse(edadiniciodisca12A>=65, "ADL", ifelse(EDAD>=65,"DC", "C"))))) %>% 
#   mutate("66" = ifelse(EdadInicioDisca44>=66, "DF", ifelse(Edadinicio_cuidado>=66, "ID", 
#                                                            ifelse(edadiniciodisca12A>=66, "ADL", ifelse(EDAD>=66,"DC", "C"))))) %>% 
#   mutate("67" = ifelse(EdadInicioDisca44>=67, "DF", ifelse(Edadinicio_cuidado>=67, "ID", 
#                                                            ifelse(edadiniciodisca12A>=67, "ADL", ifelse(EDAD>=67,"DC", "C"))))) %>% 
#   mutate("68" = ifelse(EdadInicioDisca44>=68, "DF", ifelse(Edadinicio_cuidado>=68, "ID", 
#                                                            ifelse(edadiniciodisca12A>=68, "ADL", ifelse(EDAD>=68,"DC", "C"))))) %>% 
#   mutate("69" = ifelse(EdadInicioDisca44>=69, "DF", ifelse(Edadinicio_cuidado>=69, "ID", 
#                                                            ifelse(edadiniciodisca12A>=69, "ADL", ifelse(EDAD>=69,"DC", "C"))))) %>% 
#   mutate("70" = ifelse(EdadInicioDisca44>=70, "DF", ifelse(Edadinicio_cuidado>=70, "ID", 
#                                                            ifelse(edadiniciodisca12A>=70, "ADL", ifelse(EDAD>=70,"DC", "C"))))) %>% 
#   mutate("71" = ifelse(EdadInicioDisca44>=71, "DF", ifelse(Edadinicio_cuidado>=71, "ID", 
#                                                            ifelse(edadiniciodisca12A>=71, "ADL", ifelse(EDAD>=71,"DC", "C"))))) %>% 
#   mutate("72" = ifelse(EdadInicioDisca44>=72, "DF", ifelse(Edadinicio_cuidado>=72, "ID", 
#                                                            ifelse(edadiniciodisca12A>=72, "ADL", ifelse(EDAD>=72,"DC", "C"))))) %>% 
#   mutate("73" = ifelse(EdadInicioDisca44>=73, "DF", ifelse(Edadinicio_cuidado>=73, "ID", 
#                                                            ifelse(edadiniciodisca12A>=73, "ADL", ifelse(EDAD>=73,"DC", "C"))))) %>% 
#   mutate("74" = ifelse(EdadInicioDisca44>=74, "DF", ifelse(Edadinicio_cuidado>=74, "ID", 
#                                                            ifelse(edadiniciodisca12A>=74, "ADL", ifelse(EDAD>=74,"DC", "C"))))) %>% 
#   mutate("75" = ifelse(EdadInicioDisca44>=75, "DF", ifelse(Edadinicio_cuidado>=75, "ID", 
#                                                            ifelse(edadiniciodisca12A>=75, "ADL", ifelse(EDAD>=75,"DC", "C"))))) %>% 
#   mutate("76" = ifelse(EdadInicioDisca44>=76, "DF", ifelse(Edadinicio_cuidado>=76, "ID", 
#                                                            ifelse(edadiniciodisca12A>=76, "ADL", ifelse(EDAD>=76,"DC", "C"))))) %>% 
#   mutate("77" = ifelse(EdadInicioDisca44>=77, "DF", ifelse(Edadinicio_cuidado>=77, "ID", 
#                                                            ifelse(edadiniciodisca12A>=77, "ADL", ifelse(EDAD>=77,"DC", "C"))))) %>% 
#   mutate("78" = ifelse(EdadInicioDisca44>=78, "DF", ifelse(Edadinicio_cuidado>=78, "ID", 
#                                                            ifelse(edadiniciodisca12A>=78, "ADL", ifelse(EDAD>=78,"DC", "C"))))) %>% 
#   mutate("79" = ifelse(EdadInicioDisca44>=79, "DF", ifelse(Edadinicio_cuidado>=79, "ID", 
#                                                            ifelse(edadiniciodisca12A>=79, "ADL", ifelse(EDAD>=79,"DC", "C"))))) %>% 
#   mutate("80" = ifelse(EdadInicioDisca44>=80, "DF", ifelse(Edadinicio_cuidado>=80, "ID", 
#                                                            ifelse(edadiniciodisca12A>=80, "ADL", ifelse(EDAD>=80,"DC", "C"))))) %>% 
#   mutate("81" = ifelse(EdadInicioDisca44>=81, "DF", ifelse(Edadinicio_cuidado>=81, "ID", 
#                                                            ifelse(edadiniciodisca12A>=81, "ADL", ifelse(EDAD>=81,"DC", "C"))))) %>% 
#   mutate("82" = ifelse(EdadInicioDisca44>=82, "DF", ifelse(Edadinicio_cuidado>=82, "ID", 
#                                                            ifelse(edadiniciodisca12A>=82, "ADL", ifelse(EDAD>=82,"DC", "C"))))) %>% 
#   mutate("83" = ifelse(EdadInicioDisca44>=83, "DF", ifelse(Edadinicio_cuidado>=83, "ID", 
#                                                            ifelse(edadiniciodisca12A>=83, "ADL", ifelse(EDAD>=83,"DC", "C"))))) %>% 
#   mutate("84" = ifelse(EdadInicioDisca44>=84, "DF", ifelse(Edadinicio_cuidado>=84, "ID", 
#                                                            ifelse(edadiniciodisca12A>=84, "ADL", ifelse(EDAD>=84,"DC", "C"))))) %>% 
#   mutate("85" = ifelse(EdadInicioDisca44>=85, "DF", ifelse(Edadinicio_cuidado>=85, "ID", 
#                                                            ifelse(edadiniciodisca12A>=85, "ADL", ifelse(EDAD>=85,"DC", "C"))))) %>% 
#   mutate("86" = ifelse(EdadInicioDisca44>=86, "DF", ifelse(Edadinicio_cuidado>=86, "ID", 
#                                                            ifelse(edadiniciodisca12A>=86, "ADL", ifelse(EDAD>=86,"DC", "C"))))) %>% 
#   mutate("87" = ifelse(EdadInicioDisca44>=87, "DF", ifelse(Edadinicio_cuidado>=87, "ID", 
#                                                            ifelse(edadiniciodisca12A>=87, "ADL", ifelse(EDAD>=87,"DC", "C"))))) %>% 
#   mutate("88" = ifelse(EdadInicioDisca44>=88, "DF", ifelse(Edadinicio_cuidado>=88, "ID", 
#                                                            ifelse(edadiniciodisca12A>=88, "ADL", ifelse(EDAD>=88,"DC", "C"))))) %>% 
#   mutate("89" = ifelse(EdadInicioDisca44>=89, "DF", ifelse(Edadinicio_cuidado>=89, "ID", 
#                                                            ifelse(edadiniciodisca12A>=89, "ADL", ifelse(EDAD>=89,"DC", "C"))))) %>% 
#   mutate("90" = ifelse(EdadInicioDisca44>=90, "DF", ifelse(Edadinicio_cuidado>=90, "ID", 
#                                                            ifelse(edadiniciodisca12A>=90, "ADL", ifelse(EDAD>=90,"DC", "C"))))) %>% 
#   mutate("91" = ifelse(EdadInicioDisca44>=91, "DF", ifelse(Edadinicio_cuidado>=91, "ID", 
#                                                            ifelse(edadiniciodisca12A>=91, "ADL", ifelse(EDAD>=91,"DC", "C"))))) %>% 
#   mutate("92" = ifelse(EdadInicioDisca44>=92, "DF", ifelse(Edadinicio_cuidado>=92, "ID", 
#                                                            ifelse(edadiniciodisca12A>=92, "ADL", ifelse(EDAD>=92,"DC", "C"))))) %>% 
#   mutate("93" = ifelse(EdadInicioDisca44>=93, "DF", ifelse(Edadinicio_cuidado>=93, "ID", 
#                                                            ifelse(edadiniciodisca12A>=93, "ADL", ifelse(EDAD>=93,"DC", "C"))))) %>% 
#   mutate("94" = ifelse(EdadInicioDisca44>=94, "DF", ifelse(Edadinicio_cuidado>=94, "ID", 
#                                                            ifelse(edadiniciodisca12A>=94, "ADL", ifelse(EDAD>=94,"DC", "C"))))) %>% 
#   mutate("95" = ifelse(EdadInicioDisca44>=95, "DF", ifelse(Edadinicio_cuidado>=95, "ID", 
#                                                            ifelse(edadiniciodisca12A>=95, "ADL", ifelse(EDAD>=95,"DC", "C"))))) %>% 
#   mutate("96" = ifelse(EdadInicioDisca44>=96, "DF", ifelse(Edadinicio_cuidado>=96, "ID", 
#                                                            ifelse(edadiniciodisca12A>=96, "ADL", ifelse(EDAD>=96,"DC", "C"))))) %>% 
#   mutate("97" = ifelse(EdadInicioDisca44>=97, "DF", ifelse(Edadinicio_cuidado>=97, "ID", 
#                                                            ifelse(edadiniciodisca12A>=97, "ADL", ifelse(EDAD>=97,"DC", "C"))))) %>% 
#   mutate("98" = ifelse(EdadInicioDisca44>=98, "DF", ifelse(Edadinicio_cuidado>=98, "ID", 
#                                                            ifelse(edadiniciodisca12A>=98, "ADL", ifelse(EDAD>=98,"DC", "C"))))) %>% 
#   mutate("99" = ifelse(EdadInicioDisca44>=99, "DF", ifelse(Edadinicio_cuidado>=99, "ID", 
#                                                            ifelse(edadiniciodisca12A>=99, "ADL", ifelse(EDAD>=99,"DC", "C"))))) %>% 
#   mutate("100" = ifelse(EdadInicioDisca44>=100, "DF", ifelse(Edadinicio_cuidado>=100, "ID", 
#                                                              ifelse(edadiniciodisca12A>=100, "ADL", ifelse(EDAD>=100,"DC", "C")))))

#### ---------------------------------------------------------------------------------------------------------------- ####
#### ---------------------------------------------------------------------------------------------------------------- ####
#### ---------------------------------------------------------------------------------------------------------------- ####


##########################################################################################################################
##########################################################################################################################
##################################### Explore State Sequences by Sex
##########################################################################################################################
##########################################################################################################################


### Step 1 - Create separate datasets for men and women (literature: different timings and disability occurrences)

tra_may_M <- tra_may %>% filter(SEXO=="Varón") %>% mutate(SEXO="Male")

tra_may_F <- tra_may %>% filter(SEXO=="Mujer") %>% mutate(SEXO="Female")


### Create a matrix for the sequence analysis
### -----------------------------------------

seqmat_M <- tra_may_M[,c(14:64)]

seqmat_F <- tra_may_F[,c(14:64)]

 ### seqdef to create an object for TraMineR
 ### ---------------------------------------

?seqdef

# Male
DisSeq_M <- seqdef(seqmat_M,informat = "STS", alphabet = SeqAlphab_2, id="auto", cpal =  Brewer_2 ,start = 55, 
                 labels = c("Disability Free","Idependent","Care","Censored"))

summary(DisSeq_M) # 1495 cases

# Females
DisSeq_F <- seqdef(seqmat_F,informat = "STS", alphabet = SeqAlphab_2, id="auto", cpal =  Brewer_2 ,start = 55, 
                   labels = c("Disability Free","Idependent","Care","Censored"))

summary(DisSeq_F) # 3266 cases


###################################################################################################

### Plotting the sequences
?seqplot

# Sequence Examples
par(mfrow=c(1,2))
seqplot(DisSeq_M,type = "i", with.legend = FALSE)
seqplot(DisSeq_F,type = "i", with.legend = FALSE)
# seqlegend(DisSeq_M)

# # Ordered Sequences by sex (?) for analysis with both sexes
# par(mfrow=c(1,2))
# seqplot(DisSeq, type = "I",group = tra_may$SEXO, with.legend = FALSE, sort="from.end")

# sorted sequences for both sexes
par(mfrow=c(1,2))
seqplot(DisSeq_M,type = "I", with.legend = FALSE, sort="from.end")
seqplot(DisSeq_F,type = "I", with.legend = FALSE, sort="from.end")



### d- plot - cummulated state plot
par(mfrow=c(1,3))
seqdplot(DisSeq_M, with.legend = FALSE)
seqdplot(DisSeq_F, with.legend = FALSE)
seqlegend(DisSeq_M)

# Without legend
# par(mfrow=c(1,2))
# seqplot(DisSeq,type = "i", with.legend = FALSE)
# seqdplot(DisSeq, with.legend = FALSE)
# par(mfrow=c(1,1))

##################################################################################
# Without legend groups (sexes)
# par(mfrow=c(2,2))
# seqplot(DisSeq,type = "i", group = tra_may$SEXO, with.legend = FALSE)
# seqdplot(DisSeq,group = tra_may$SEXO, with.legend = FALSE)
# par(mfrow=c(1,1))
##################################################################################

### f- plot - cummulated frequencies of sequences
seqfplot(DisSeq_M)
seqfplot(DisSeq_F)

### ----------------------------------------------------------------
### state distribution table as numerical counterpart for the d-plot

seqstatd(DisSeq_M)
seqstatd(DisSeq_F)

### Total numbers ####################
#      50   51   52   53     54   55    56    57    58    59    60    61    62   63   64   65    66    67    68    69    70
# DF 0.83 0.82 0.82 0.81 0.7847 0.78 0.768 0.754 0.745 0.742 0.680 0.672 0.653 0.64 0.62 0.58 0.562 0.543 0.520 0.501 0.498
# ID 0.17 0.18 0.18 0.19 0.2094 0.21 0.216 0.221 0.223 0.215 0.266 0.260 0.264 0.26 0.26 0.28 0.267 0.261 0.255 0.246 0.221
# DC 0.00 0.00 0.00 0.00 0.0059 0.01 0.016 0.024 0.032 0.043 0.054 0.068 0.083 0.10 0.12 0.14 0.151 0.163 0.171 0.184 0.195
# C  0.00 0.00 0.00 0.00 0.0000 0.00 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.00 0.00 0.00 0.019 0.034 0.054 0.069 0.086
######################################

### --------------------
### Sequence Frequencies

seqtab(DisSeq)

#############################################
# Just the most common frequencies

                  # Freq Percent
# DF/51              87    1.83
# ID/9-DC/12-C/30    31    0.65
# ID/4-DC/12-C/35    28    0.59
# ID/10-DC/12-C/29   25    0.53
# ID/18-DC/12-C/21   23    0.48
# ID/7-DC/12-C/32    23    0.48
# ID/11-DC/12-C/28   22    0.46
# ID/12-DC/12-C/27   22    0.46
# ID/14-DC/12-C/25   22    0.46
# ID/15-DC/12-C/24   22    0.46
#############################################

# male
seqtab(DisSeq_M)

# female
seqtab(DisSeq_F)


### --------------------
### Distinct state - DSS
seqdss(head(DisSeq))

# male
seqdss(head(DisSeq_M))


### -----------------------------
### Distinct state Duration - DSS
seqdur(head(DisSeq))

# female
seqdur(head(DisSeq_F))


### ------------------------
### Sequence duration state 
seqistatd(DisSeq[1:6,])

### ----------------
### Transition Rate !
### ----------------

seqtrate(DisSeq)
round(seqtrate(DisSeq),2)

# males
round(seqtrate(DisSeq_M),2)

# females
round(seqtrate(DisSeq_F),2)


############ COMPLEXITY MEASURES ##############
###############################################

### ----------------
### Shannon Entropy
seqient(head(DisSeq))

# males
seqient(head(DisSeq_M))

# females
seqient(head(DisSeq_F))  # lower values for femals (due to higher case number?)

### ----------
### Turbulence
par(mfrow=c(1,1))
hist(seqST(DisSeq))

par(mfrow=c(1,2))
# males
hist(seqST(DisSeq_M))
# females
hist(seqST(DisSeq_F))



###################################################################################################################
############################################## Clustering #########################################################

# For the analysis of clusters, the optimal distance matrix is required

# 1. Generating a substitutions cost matrix (read!) from the sequence object (in our case "DisSeq")
# -------------------------------------------------------------------------------------------------

submat <- seqsubm(DisSeq, method = "CONSTANT", cval = 2)

  #       DF-> ID-> DC-> C->
  # DF->    0    2    2   2
  # ID->    2    0    2   2
  # DC->    2    2    0   2
  # C->     2    2    2   0

# males
submat_M <- seqsubm(DisSeq_M, method = "CONSTANT", cval = 2)

# females
submat_F <- seqsubm(DisSeq_F, method = "CONSTANT", cval = 2)

# 1b. Computing distances using the matrix and the default indel cost of 1 ("seqdist" command)
# --------------------------------------------------------------------------------------------

      # matrix will be filled in under option "sm"
      # This large matrix gives us the calculated optimal distance between two individuals

      # The mathematical process is called "optimal matching" (read Halpin 2010!)

dismat <- seqdist(DisSeq, method = "OM", sm = submat)
dismat[1:10,1:10]

# example - optimal distance between individual 4 and 5 (result: it´d take 48 operations to make both sequences the same)
dismat[4,5]


# males (OM)
dismat_M <- seqdist(DisSeq_M, method = "OM", sm = submat_M)
dismat_M[1:10,1:10]
# males (OMslen - spell-length sensitive)
dismat_M2 <- seqdist(DisSeq_M, method = "OMslen", sm = submat_M)
dismat_M2[1:10,1:10]

# females
dismat_F <- seqdist(DisSeq_F, method = "OM", sm = submat_F)
dismat_F[1:10,1:10]

# 2. Build the clusters with the agnes function (cluster package)
# ---------------------------------------------------------------

?agnes
# command provides computes a agglomorative hieracrchical clustering
# option "ward" refers to the Ward´s method! - Other option "weigthed" which uses weighted averages

clusterw <- agnes(dismat, diss = TRUE, method = "ward")

# Plotting a dendrogram (if possible)
plot(clusterw, which.plots = 2)


# 3. Try to cluster sequences in 3 groups (Adjustable)
# ----------------------------------------------------

# cutree command (3 groups)
cluster3 <- cutree(clusterw, k = 4)

# Create three factors for grouping (for now without descriptive name)
cluster3 <- factor(cluster3, labels = c("Type 1", "Type 2", "Type 3"))
table(cluster3)

# Output
  # Type 1 Type 2 Type 3 
  # 1582   1815   1364 
## ---------------------------------------------------------- ##
## for males only
clusterw_M <- agnes(dismat_M, diss = TRUE, method = "ward")
clusterw_M2 <- agnes(dismat_M2, diss = TRUE, method = "ward")
plot(clusterw_M2, which.plots = 2)

cluster3_M <- cutree(clusterw_M, k = 3)
cluster3_M2 <- cutree(clusterw_M2, k = 2)

cluster4_M <- cutree(clusterw_M, k = 4)
cluster4_M2 <- cutree(clusterw_M2, k = 4)
cluster5_M2 <- cutree(clusterw_M2, k = 5)


cluster3_M <- factor(cluster3_M, labels = c("Type 1", "Type 2", "Type 3"))
cluster3_M2 <- factor(cluster3_M2, labels = c("Type 1", "Type 2", "Type 3"))
table(cluster3_M)

cluster4_M <- factor(cluster4_M, labels = c("Type 1", "Type 2", "Type 3", "Type 4"))
cluster4_M2 <- factor(cluster4_M2, labels = c("Type 1", "Type 2", "Type 3", "Type 4"))
cluster5_M2 <- factor(cluster5_M2, labels = c("Type 1", "Type 2", "Type 3", "Type 4","Type 5"))

table(cluster4_M)
table(cluster4_M2)
table(cluster5_M2)

## ---------------------------------------------------------- ##
## for females only
clusterw_F <- agnes(dismat_F, diss = TRUE, method = "ward")
cluster3_F <- cutree(clusterw_F, k = 3)
cluster4_F <- cutree(clusterw_F, k = 5)

cluster3_F <- factor(cluster3_F, labels = c("Type 1", "Type 2", "Type 3"))
table(cluster3_F)

# 5 groups seems to be the ideal cluster
cluster5_F <- factor(cluster4_F, labels = c("Type 1", "Type 2", "Type 3", "Type 4","Type 5"))
table(cluster5_F)

## ---------------------------------------------------------- ##
# Now plotting for visual examination

seqfplot(DisSeq, group = cluster3, pbarw = T)

# males 3 cats
seqfplot(DisSeq_M, group = cluster3_M, pbarw = T)
seqfplot(DisSeq_M, group = cluster3_M2, pbarw = T)

# males 4/5 cats
seqfplot(DisSeq_M, group = cluster4_M, pbarw = T)
seqfplot(DisSeq_M, group = cluster4_M2, pbarw = T)
seqfplot(DisSeq_M, group = cluster5_M2, pbarw = T)

# females 3 cats
seqfplot(DisSeq_F, group = cluster3_F, pbarw = T)

# females 4 cats
seqfplot(DisSeq_F, group = cluster4_F, pbarw = T)

# Second plot - mean time spent in each state by cluster

seqmtplot(DisSeq, group = cluster3)

# 4. Try to cluster sequences in 2 groups (Adjustable)
# ----------------------------------------------------

# cutree command (2 groups)
cluster2 <- cutree(clusterw, k = 2)

# Create three factors for grouping (for now without descriptive name)
cluster2 <- factor(cluster2, labels = c("Type 1", "Type 2"))
table(cluster2)

# Output
# Type 1 Type 2 Type 3 
# 1582   1815   1364 

# Now plotting for visual examination
seqdplot(DisSeq, group = cluster2, border=NA)

seqfplot(DisSeq, group = cluster2, border=NA)

# Second plot - mean time spent in each state by cluster

seqmtplot(DisSeq, group = cluster2)













######### Group Differences = Gender differences
################################################

## vector (T/F) for the being male or not in the length of the dataframe (row number)

filter.men <- which(rs2$sex=="male")
## we use this vector to extract the males from the data set
men.sts <- RetSeq[filter.men,]


## and do the same for the females
filter.women <- which(rs2$sex=="female")
women.sts <- RetSeq[filter.women,]

### See difference in turbulence

turb.men <- seqST(men.sts)

turb.women <- seqST(women.sts)

boxplot(turb.men,ylim=c(0,15))
boxplot(turb.women,ylim=c(0,15)) 



#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################


### -----------------------------------------
### Create a matrix for the sequence analysis
### -----------------------------------------

seqmat <- tra_may_ADL[,c(14:64)]


### seqdef to create an object for TraMineR
### ---------------------------------------

?seqdef

DisSeq <- seqdef(seqmat,informat = "STS", alphabet = SeqAlphab_1, id="auto", cpal =  Brewer_1 ,start = 55, 
                 labels = c("Disability Free","Idependent","ADL","Care","Censored"))

summary(DisSeq)








      # ### Matrix Approach (Problem: Operation of binding the vectors and )
      #   
      #   for(i in min(tra_test$Id):max(tra_test$Id)){
      #     
      #     B = matrix( 
      #       c(rep("DF", tra_test$dur_DF[tra_test$Id==i]),rep("ID", tra_test$dur_ID[tra_test$Id==i]),
      #         rep("DC", tra_test$dur_DC[tra_test$Id==i]), rep("C",tra_test$dur_C[tra_test$Id==i])), 
      #       nrow=length(tra_test$Id), 
      #       ncol=50, byrow = T) 
      #   }
      #  
      #   ### Another toy version
      #   
      #   tra <- matrix(nrow = length(tra_may$Id), ncol = 51)
      #   tra[,1] <- tra_may$Id
      #   
      #   
      #   tra_test <- tra_may[1:20,]
      #   tra_test <- tra_test %>% select(Id,dur_DF,dur_ID, dur_DC, dur_C)
      #   
      #   
      #   ### For this step, delete all the objects as it will be challenging for the memory
      #   # rm(abc,B, link.may, tra_may, Test_X)
      #     
      #   #########################  
      #   test_vec <- as.vector(c(rep("DF", tra_may$dur_DF[tra_may$Id==min(tra_may$Id)]),rep("ID", tra_may$dur_ID[tra_may$Id==min(tra_may$Id)]),
      #                           rep("DC", tra_may$dur_DC[tra_may$Id==min(tra_may$Id)]), rep("C",tra_may$dur_C[tra_may$Id==min(tra_may$Id)])))
      #   class(test_vec)
      #   length(test_vec)
      # 
      #   
      #   for (i in min(tra_test$Id):max(tra_test$Id)){
      #    x <- list("")
      #    x[i] <- list(as.vector(c(tra_test$Id[i], rep("DF", tra_test$dur_DF[tra_test$Id==i]),rep("ID", tra_test$dur_ID[tra_test$Id==i]),
      #                             rep("DC", tra_test$dur_DC[tra_test$Id==i]), rep("C",tra_test$dur_C[tra_test$Id==i]))))
      #   }
      #   
      #   tra <- matrix(nrow = length(tra_may$Id), ncol = 51)
      #   # Add Id and sort from low to high
      #   tra[,1] <- sort(tra_may$Id, decreasing = FALSE)
      #   
      #   
      #   
      #   for (i in min(tra_test$Id):max(tra_test$Id)){
      #     tra[i,1] <-  as.vector(c(tra_may$Id[i], rep("DF", tra_may$dur_DF[tra_may$Id==i]),rep("ID", tra_may$dur_ID[tra_may$Id==i]),
      #                             rep("DC", tra_may$dur_DC[tra_may$Id==i]), rep("C",tra_may$dur_C[tra_may$Id==i])))
      #   }
      #   
      #   
      # 
      #   #########################
      #   
      #   
      #   
      #   seq_b <- tra_may[,12]
      #                                 
      #   tra_mat <- seqdecomp(seq_b)
      #     
      #   #### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
      #   #### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
      # 
      #   
      #   
      #   
      #   # Prepare Matrix for the seqdef() command
      #   cbind(names(tra_may))
      #   
      #   seqmat <- tra_may[,c(13:25)]
  