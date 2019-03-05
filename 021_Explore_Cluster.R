
# ----------------------------------------
# Need to run 020_Link_EDAD08_Followup.R !
# ----------------------------------------

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
# load(file='010_mayor.link.RData')
load(file='010_mayor50.link.RData')           ### !!! Change when different data set is used !!!
# --------------------------------------------



# 1.0. Select Population - Get rid of the "NC" cases for now and only extract the dependent ones
# -----------------------------------------------------------------------------------------------

# Enter name of the dataset right of the equation sign (in case different data is used)
link.may <- link.may50 
link.may <- data.table(link.may)



# ---------------------------------------------
# 1.1 Few discriptive plots on different states
# ---------------------------------------------
# ---------------------------------------------

# 1.1.1. Correctness of age information
########################################

# Checks for Disability and Death
# link.may %>% count(EDAD < age.ex)
# link.may$Edadinicio_disca44 <- as.numeric(link.may$Edadinicio_disca44)
# link.may$age.ex <- as.numeric(link.may$age.ex)

# link.may[,.N ,keyby=.(Good=(!is.na(EdadInicioDisca44) <= age.ex),  Bad= (!is.na(EdadInicioDisca44) > age.ex))]
# 
# link.may %>% count(EdadInicioDisca44 < age.ex)
# link.may %>% count(round(EdadInicioDisca44,0) < round(EDAD,0))   ### THIS SHOULD NOT BE POSSIBLE
# 
# ## FIX (Listwise deletion)
# 
# link.may <- link.may %>% filter(round(EdadInicioDisca44,0) < round(EDAD,0))  # 215 cases


# DISCA 13

link.may$EdadInicioDisca13 <- as.numeric(link.may$EdadInicioDisca13)

link.may %>% count(EdadInicioDisca13 < age.ex)
link.may %>% count(EdadInicioDisca13 > EDAD)              ### THIS SHOULD NOT BE POSSIBLE (91 cases)

## FIX (Listwise deletion)

link.may <- link.may %>% filter(EdadInicioDisca13 <= EDAD)  


# Estado - A = censored , B = Bajas (death and migration)
# -------------------------------------------------------
link.may <- data.table(link.may)
link.may[,.N,.(enlazado,estado)] 
# A    B(ajas = refers to either events (death) or outmigration) 
# 1174 3353 


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

# Be aware of age heaping!

#### ----------------------------------------------------- ####
#### 2. Assigning time information for crucial time points ####
#### ----------------------------------------------------- ####


## 2.1. Onset of incapacity of an activity of daily living (by severity)
## ---------------------------------------------------------------------

# For first onset age use "EdadInicioDisca13"

# Minimum age for onset of one of the 13 (I)ADLs (Using the age information for single disabilities)

table(link.may$MOV_18_2)

# recode all severity variables (DISCA 13) in two groups
link.may <- link.may %>%
  # change posture/move
  mutate(DIS_1_S = ifelse(MOV_18_2=="Con dificultad moderada", "mild", "severe")) %>% 
  # walking and moving inside
  mutate(DIS_2_S = ifelse(MOV_20_2=="Con dificultad moderada", "mild", "severe")) %>%
  # walking and moving outside
  mutate(DIS_3_S = ifelse(MOV_21_2=="Con dificultad moderada", "mild", "severe")) %>% 
  # Sitting down & using public transport
  mutate(DIS_4_S = ifelse(MOV_22_2=="Con dificultad moderada", "mild", "severe")) %>% 
  # washing and drying
  mutate(DIS_5_S = ifelse(AUT_27_2=="Con dificultad moderada", "mild", "severe")) %>% 
  # basic hygene
  mutate(DIS_6_S = ifelse(AUT_28_2=="Con dificultad moderada", "mild", "severe")) %>% 
  # urinating
  mutate(DIS_7_S = ifelse(AUT_29_2=="Con dificultad moderada", "mild", "severe")) %>% 
  # bathroom
  mutate(DIS_8_S = ifelse(AUT_30_2=="Con dificultad moderada", "mild", "severe")) %>% 
  # dressing and undressing
  mutate(DIS_9_S = ifelse(AUT_32_2=="Con dificultad moderada", "mild", "severe")) %>% 
  # eating and drinking
  mutate(DIS_10_S = ifelse(AUT_33_2=="Con dificultad moderada", "mild", "severe")) %>% 
  # organising shopping for groceries
  mutate(DIS_11_S = ifelse(VDOM_36_2=="Con dificultad moderada", "mild", "severe")) %>% 
  # preparing food
  mutate(DIS_12_S = ifelse(VDOM_37_2=="Con dificultad moderada", "mild", "severe")) %>% 
  # household tasks
  mutate(DIS_13_S = ifelse(AUT_28_2=="Con dificultad moderada", "mild", "severe"))
  
  
# recode entry age into first severe limitation (DISCA 13)
link.may <- link.may %>% 
  mutate(DIS_1_SA = ifelse(DIS_1_S=="severe", MOV_18_5, 999)) %>% mutate(DIS_1_SA = ifelse(is.na(DIS_1_SA),999, DIS_1_SA)) %>% 
  mutate(DIS_2_SA = ifelse(DIS_2_S=="severe", MOV_20_5, 999)) %>% mutate(DIS_2_SA = ifelse(is.na(DIS_2_SA),999, DIS_2_SA)) %>%
  mutate(DIS_3_SA = ifelse(DIS_3_S=="severe", MOV_21_5, 999)) %>% mutate(DIS_3_SA = ifelse(is.na(DIS_3_SA),999, DIS_3_SA)) %>%
  mutate(DIS_4_SA = ifelse(DIS_4_S=="severe", MOV_22_5, 999)) %>% mutate(DIS_4_SA = ifelse(is.na(DIS_4_SA),999, DIS_4_SA)) %>%
  mutate(DIS_5_SA = ifelse(DIS_5_S=="severe", AUT_27_5, 999)) %>% mutate(DIS_5_SA = ifelse(is.na(DIS_5_SA),999, DIS_5_SA)) %>%
  mutate(DIS_6_SA = ifelse(DIS_6_S=="severe", AUT_28_5, 999)) %>% mutate(DIS_6_SA = ifelse(is.na(DIS_6_SA),999, DIS_6_SA)) %>%
  mutate(DIS_7_SA = ifelse(DIS_7_S=="severe", AUT_29_5, 999)) %>% mutate(DIS_7_SA = ifelse(is.na(DIS_7_SA),999, DIS_7_SA)) %>%
  mutate(DIS_8_SA = ifelse(DIS_8_S=="severe", AUT_30_5, 999)) %>% mutate(DIS_8_SA = ifelse(is.na(DIS_8_SA),999, DIS_8_SA)) %>%
  mutate(DIS_9_SA = ifelse(DIS_9_S=="severe", AUT_32_5, 999)) %>% mutate(DIS_9_SA = ifelse(is.na(DIS_9_SA),999, DIS_9_SA)) %>%
  mutate(DIS_10_SA = ifelse(DIS_10_S=="severe", AUT_33_5, 999)) %>% mutate(DIS_10_SA = ifelse(is.na(DIS_10_SA),999, DIS_10_SA)) %>%
  mutate(DIS_11_SA = ifelse(DIS_11_S=="severe", VDOM_36_5, 999)) %>% mutate(DIS_11_SA = ifelse(is.na(DIS_11_SA),999, DIS_11_SA)) %>%
  mutate(DIS_12_SA = ifelse(DIS_12_S=="severe", VDOM_37_5, 999)) %>% mutate(DIS_12_SA = ifelse(is.na(DIS_12_SA),999, DIS_12_SA)) %>%
  mutate(DIS_13_SA = ifelse(DIS_13_S=="severe", VDOM_38_5, 999)) %>% mutate(DIS_13_SA = ifelse(is.na(DIS_13_SA),999, DIS_13_SA)) %>% 

## Now compute the column minimum (gives the entry age to severity)
  mutate(EntryGrave13 =pmin(DIS_1_SA, DIS_2_SA, DIS_3_SA, DIS_4_SA, DIS_5_SA, DIS_6_SA,
                                  DIS_7_SA, DIS_8_SA, DIS_9_SA, DIS_10_SA, DIS_11_SA, DIS_12_SA,
                                  DIS_13_SA))


summary(link.may$EntryGrave13)
hist(link.may$EntryGrave13[link.may$EntryGrave13<999])

# For later purposes the 999 have to be set to NAs
link.may <- link.may %>% mutate(EntryGrave13=ifelse(EntryGrave13==999, NA, EntryGrave13))



# 2.2 ABC Scheme
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
  tra_may <- link.may %>% select(Id, SEXO, EDAD, EdadInicioDisca44, EdadInicioDisca13, Edadinicio_cuidado, age.ex, EntryGrave13, DISCA13_AGE) # edadiniciodisca12A,
  
  

  # # -----------------  
  # # Fun with strings
  # # -----------------
  # 
  # tra_may$disstring <- ifelse(tra_may$EdadInicioDisca44<50,strrep("FD",50),0)
  # tra_may$disstring <- ifelse(tra_may$EdadInicioDisca44>50, paste(rep("DF", 50:round(tra_may$EdadInicioDisca44,0))))
  
  
  
  #### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
  #### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
  
    # Approach 3 - Try it with durations which a person spent in a particular state and combine everything in the end with 3 1/2 states for now
  #### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #####
  
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
  # first taking care of those without A12 disability onset (make it the general)
  
  tra_may_12A <- subset(tra_may, !is.na(DISCA13_AGE))
  
  tra_may_D13 <- tra_may %>% 
    # now the hand work
    mutate("50" = ifelse(DISCA13_AGE>=50, "DF", ifelse(Edadinicio_cuidado>=50, "ID", ifelse(EDAD>=50,"DC", NA)))) %>% 
    mutate("51" = ifelse(DISCA13_AGE>=51, "DF", ifelse(Edadinicio_cuidado>=51, "ID", ifelse(EDAD>=51,"DC", NA)))) %>% 
    mutate("52" = ifelse(DISCA13_AGE>=52, "DF", ifelse(Edadinicio_cuidado>=52, "ID", ifelse(EDAD>=52,"DC", NA)))) %>% 
    mutate("53" = ifelse(DISCA13_AGE>=53, "DF", ifelse(Edadinicio_cuidado>=53, "ID", ifelse(EDAD>=53,"DC", NA)))) %>% 
    mutate("54" = ifelse(DISCA13_AGE>=54, "DF", ifelse(Edadinicio_cuidado>=54, "ID", ifelse(EDAD>=54,"DC", NA)))) %>% 
    mutate("55" = ifelse(DISCA13_AGE>=55, "DF", ifelse(Edadinicio_cuidado>=55, "ID", ifelse(EDAD>=55,"DC", NA)))) %>% 
    mutate("56" = ifelse(DISCA13_AGE>=56, "DF", ifelse(Edadinicio_cuidado>=56, "ID", ifelse(EDAD>=56,"DC", NA)))) %>% 
    mutate("57" = ifelse(DISCA13_AGE>=57, "DF", ifelse(Edadinicio_cuidado>=57, "ID", ifelse(EDAD>=57,"DC", NA)))) %>% 
    mutate("58" = ifelse(DISCA13_AGE>=58, "DF", ifelse(Edadinicio_cuidado>=58, "ID", ifelse(EDAD>=58,"DC", NA)))) %>% 
    mutate("59" = ifelse(DISCA13_AGE>=59, "DF", ifelse(Edadinicio_cuidado>=59, "ID", ifelse(EDAD>=59,"DC", NA)))) %>% 
    mutate("60" = ifelse(DISCA13_AGE>=60, "DF", ifelse(Edadinicio_cuidado>=60, "ID", ifelse(EDAD>=60,"DC", NA)))) %>% 
    mutate("61" = ifelse(DISCA13_AGE>=61, "DF", ifelse(Edadinicio_cuidado>=61, "ID", ifelse(EDAD>=61,"DC", NA)))) %>% 
    mutate("62" = ifelse(DISCA13_AGE>=62, "DF", ifelse(Edadinicio_cuidado>=62, "ID", ifelse(EDAD>=62,"DC", NA)))) %>% 
    mutate("63" = ifelse(DISCA13_AGE>=63, "DF", ifelse(Edadinicio_cuidado>=63, "ID", ifelse(EDAD>=63,"DC", NA)))) %>% 
    mutate("64" = ifelse(DISCA13_AGE>=64, "DF", ifelse(Edadinicio_cuidado>=64, "ID", ifelse(EDAD>=64,"DC", NA)))) %>% 
    mutate("65" = ifelse(DISCA13_AGE>=65, "DF", ifelse(Edadinicio_cuidado>=65, "ID", ifelse(EDAD>=65,"DC", NA)))) %>% 
    mutate("66" = ifelse(DISCA13_AGE>=66, "DF", ifelse(Edadinicio_cuidado>=66, "ID", ifelse(EDAD>=66,"DC", NA)))) %>% 
    mutate("67" = ifelse(DISCA13_AGE>=67, "DF", ifelse(Edadinicio_cuidado>=67, "ID", ifelse(EDAD>=67,"DC", NA)))) %>% 
    mutate("68" = ifelse(DISCA13_AGE>=68, "DF", ifelse(Edadinicio_cuidado>=68, "ID", ifelse(EDAD>=68,"DC", NA)))) %>% 
    mutate("69" = ifelse(DISCA13_AGE>=69, "DF", ifelse(Edadinicio_cuidado>=69, "ID", ifelse(EDAD>=69,"DC", NA)))) %>% 
    mutate("70" = ifelse(DISCA13_AGE>=70, "DF", ifelse(Edadinicio_cuidado>=70, "ID", ifelse(EDAD>=70,"DC", NA)))) %>% 
    mutate("71" = ifelse(DISCA13_AGE>=71, "DF", ifelse(Edadinicio_cuidado>=71, "ID", ifelse(EDAD>=71,"DC", NA)))) %>% 
    mutate("72" = ifelse(DISCA13_AGE>=72, "DF", ifelse(Edadinicio_cuidado>=72, "ID", ifelse(EDAD>=72,"DC", NA)))) %>% 
    mutate("73" = ifelse(DISCA13_AGE>=73, "DF", ifelse(Edadinicio_cuidado>=73, "ID", ifelse(EDAD>=73,"DC", NA)))) %>% 
    mutate("74" = ifelse(DISCA13_AGE>=74, "DF", ifelse(Edadinicio_cuidado>=74, "ID", ifelse(EDAD>=74,"DC", NA)))) %>% 
    mutate("75" = ifelse(DISCA13_AGE>=75, "DF", ifelse(Edadinicio_cuidado>=75, "ID", ifelse(EDAD>=75,"DC", NA)))) %>% 
    mutate("76" = ifelse(DISCA13_AGE>=76, "DF", ifelse(Edadinicio_cuidado>=76, "ID", ifelse(EDAD>=76,"DC", NA)))) %>% 
    mutate("77" = ifelse(DISCA13_AGE>=77, "DF", ifelse(Edadinicio_cuidado>=77, "ID", ifelse(EDAD>=77,"DC", NA)))) %>% 
    mutate("78" = ifelse(DISCA13_AGE>=78, "DF", ifelse(Edadinicio_cuidado>=78, "ID", ifelse(EDAD>=78,"DC", NA)))) %>% 
    mutate("79" = ifelse(DISCA13_AGE>=79, "DF", ifelse(Edadinicio_cuidado>=79, "ID", ifelse(EDAD>=79,"DC", NA)))) %>% 
    mutate("80" = ifelse(DISCA13_AGE>=80, "DF", ifelse(Edadinicio_cuidado>=80, "ID", ifelse(EDAD>=80,"DC", NA)))) %>% 
    mutate("81" = ifelse(DISCA13_AGE>=81, "DF", ifelse(Edadinicio_cuidado>=81, "ID", ifelse(EDAD>=81,"DC", NA)))) %>% 
    mutate("82" = ifelse(DISCA13_AGE>=82, "DF", ifelse(Edadinicio_cuidado>=82, "ID", ifelse(EDAD>=82,"DC", NA)))) %>% 
    mutate("83" = ifelse(DISCA13_AGE>=83, "DF", ifelse(Edadinicio_cuidado>=83, "ID", ifelse(EDAD>=83,"DC", NA)))) %>% 
    mutate("84" = ifelse(DISCA13_AGE>=84, "DF", ifelse(Edadinicio_cuidado>=84, "ID", ifelse(EDAD>=84,"DC", NA)))) %>% 
    mutate("85" = ifelse(DISCA13_AGE>=85, "DF", ifelse(Edadinicio_cuidado>=85, "ID", ifelse(EDAD>=85,"DC", NA)))) %>% 
    mutate("86" = ifelse(DISCA13_AGE>=86, "DF", ifelse(Edadinicio_cuidado>=86, "ID", ifelse(EDAD>=86,"DC", NA)))) %>% 
    mutate("87" = ifelse(DISCA13_AGE>=87, "DF", ifelse(Edadinicio_cuidado>=87, "ID", ifelse(EDAD>=87,"DC", NA)))) %>% 
    mutate("88" = ifelse(DISCA13_AGE>=88, "DF", ifelse(Edadinicio_cuidado>=88, "ID", ifelse(EDAD>=88,"DC", NA)))) %>% 
    mutate("89" = ifelse(DISCA13_AGE>=89, "DF", ifelse(Edadinicio_cuidado>=89, "ID", ifelse(EDAD>=89,"DC", NA)))) %>% 
    mutate("90" = ifelse(DISCA13_AGE>=90, "DF", ifelse(Edadinicio_cuidado>=90, "ID", ifelse(EDAD>=90,"DC", NA)))) %>% 
    mutate("91" = ifelse(DISCA13_AGE>=91, "DF", ifelse(Edadinicio_cuidado>=91, "ID", ifelse(EDAD>=91,"DC", NA)))) %>% 
    mutate("92" = ifelse(DISCA13_AGE>=92, "DF", ifelse(Edadinicio_cuidado>=92, "ID", ifelse(EDAD>=92,"DC", NA)))) %>% 
    mutate("93" = ifelse(DISCA13_AGE>=93, "DF", ifelse(Edadinicio_cuidado>=93, "ID", ifelse(EDAD>=93,"DC", NA)))) %>% 
    mutate("94" = ifelse(DISCA13_AGE>=94, "DF", ifelse(Edadinicio_cuidado>=94, "ID", ifelse(EDAD>=94,"DC", NA)))) %>% 
    mutate("95" = ifelse(DISCA13_AGE>=95, "DF", ifelse(Edadinicio_cuidado>=95, "ID", ifelse(EDAD>=95,"DC", NA)))) %>% 
    mutate("96" = ifelse(DISCA13_AGE>=96, "DF", ifelse(Edadinicio_cuidado>=96, "ID", ifelse(EDAD>=96,"DC", NA)))) %>% 
    mutate("97" = ifelse(DISCA13_AGE>=97, "DF", ifelse(Edadinicio_cuidado>=97, "ID", ifelse(EDAD>=97,"DC", NA)))) %>% 
    mutate("98" = ifelse(DISCA13_AGE>=98, "DF", ifelse(Edadinicio_cuidado>=98, "ID", ifelse(EDAD>=98,"DC", NA)))) %>% 
    mutate("99" = ifelse(DISCA13_AGE>=99, "DF", ifelse(Edadinicio_cuidado>=99, "ID", ifelse(EDAD>=99,"DC", NA)))) %>% 
    mutate("100" = ifelse(DISCA13_AGE>=100, "DF", ifelse(Edadinicio_cuidado>=100, "ID", ifelse(EDAD>=100,"DC", NA))))
  

### Kindergarten dataframe approach (One Variable for EACH time point)

### DISCA13
tra_may_13 <- tra_may %>% mutate("50" = ifelse(DISCA13_AGE>=50, "DF", ifelse(EntryGrave13>=50, "MD", ifelse(Edadinicio_cuidado>=50, "ID", ifelse(EDAD>=50,"DC", NA))))) %>% 
  mutate("51" = ifelse(DISCA13_AGE>=51, "DF", ifelse(EntryGrave13>=51, "MD", ifelse(Edadinicio_cuidado>=51, "ID", ifelse(EDAD>=51,"DC", NA))))) %>% 
  mutate("52" = ifelse(DISCA13_AGE>=52, "DF", ifelse(EntryGrave13>=52, "MD", ifelse(Edadinicio_cuidado>=52, "ID", ifelse(EDAD>=52,"DC", NA))))) %>% 
  mutate("53" = ifelse(DISCA13_AGE>=53, "DF", ifelse(EntryGrave13>=53, "MD", ifelse(Edadinicio_cuidado>=53, "ID", ifelse(EDAD>=53,"DC", NA))))) %>% 
  mutate("54" = ifelse(DISCA13_AGE>=54, "DF", ifelse(EntryGrave13>=54, "MD", ifelse(Edadinicio_cuidado>=54, "ID", ifelse(EDAD>=54,"DC", NA))))) %>% 
  mutate("55" = ifelse(DISCA13_AGE>=55, "DF", ifelse(EntryGrave13>=55, "MD", ifelse(Edadinicio_cuidado>=55, "ID", ifelse(EDAD>=55,"DC", NA))))) %>% 
  mutate("56" = ifelse(DISCA13_AGE>=56, "DF", ifelse(EntryGrave13>=56, "MD", ifelse(Edadinicio_cuidado>=56, "ID", ifelse(EDAD>=56,"DC", NA))))) %>% 
  mutate("57" = ifelse(DISCA13_AGE>=57, "DF", ifelse(EntryGrave13>=57, "MD", ifelse(Edadinicio_cuidado>=57, "ID", ifelse(EDAD>=57,"DC", NA))))) %>% 
  mutate("58" = ifelse(DISCA13_AGE>=58, "DF", ifelse(EntryGrave13>=58, "MD", ifelse(Edadinicio_cuidado>=58, "ID", ifelse(EDAD>=58,"DC", NA))))) %>% 
  mutate("59" = ifelse(DISCA13_AGE>=59, "DF", ifelse(EntryGrave13>=59, "MD", ifelse(Edadinicio_cuidado>=59, "ID", ifelse(EDAD>=59,"DC", NA))))) %>% 
  mutate("60" = ifelse(DISCA13_AGE>=60, "DF", ifelse(EntryGrave13>=60, "MD", ifelse(Edadinicio_cuidado>=60, "ID", ifelse(EDAD>=60,"DC", NA))))) %>% 
  mutate("61" = ifelse(DISCA13_AGE>=61, "DF", ifelse(EntryGrave13>=61, "MD", ifelse(Edadinicio_cuidado>=61, "ID", ifelse(EDAD>=61,"DC", NA))))) %>% 
  mutate("62" = ifelse(DISCA13_AGE>=62, "DF", ifelse(EntryGrave13>=62, "MD", ifelse(Edadinicio_cuidado>=62, "ID", ifelse(EDAD>=62,"DC", NA))))) %>% 
  mutate("63" = ifelse(DISCA13_AGE>=63, "DF", ifelse(EntryGrave13>=63, "MD", ifelse(Edadinicio_cuidado>=63, "ID", ifelse(EDAD>=63,"DC", NA))))) %>% 
  mutate("64" = ifelse(DISCA13_AGE>=64, "DF", ifelse(EntryGrave13>=64, "MD", ifelse(Edadinicio_cuidado>=64, "ID", ifelse(EDAD>=64,"DC", NA))))) %>% 
  mutate("65" = ifelse(DISCA13_AGE>=65, "DF", ifelse(EntryGrave13>=65, "MD", ifelse(Edadinicio_cuidado>=65, "ID", ifelse(EDAD>=65,"DC", NA))))) %>% 
  mutate("66" = ifelse(DISCA13_AGE>=66, "DF", ifelse(EntryGrave13>=66, "MD", ifelse(Edadinicio_cuidado>=66, "ID", ifelse(EDAD>=66,"DC", NA))))) %>% 
  mutate("67" = ifelse(DISCA13_AGE>=67, "DF", ifelse(EntryGrave13>=67, "MD", ifelse(Edadinicio_cuidado>=67, "ID", ifelse(EDAD>=67,"DC", NA))))) %>% 
  mutate("68" = ifelse(DISCA13_AGE>=68, "DF", ifelse(EntryGrave13>=68, "MD", ifelse(Edadinicio_cuidado>=68, "ID", ifelse(EDAD>=68,"DC", NA))))) %>% 
  mutate("69" = ifelse(DISCA13_AGE>=69, "DF", ifelse(EntryGrave13>=69, "MD", ifelse(Edadinicio_cuidado>=69, "ID", ifelse(EDAD>=69,"DC", NA))))) %>% 
  mutate("70" = ifelse(DISCA13_AGE>=70, "DF", ifelse(EntryGrave13>=70, "MD", ifelse(Edadinicio_cuidado>=70, "ID", ifelse(EDAD>=70,"DC", NA))))) %>% 
  mutate("71" = ifelse(DISCA13_AGE>=71, "DF", ifelse(EntryGrave13>=71, "MD", ifelse(Edadinicio_cuidado>=71, "ID", ifelse(EDAD>=71,"DC", NA))))) %>% 
  mutate("72" = ifelse(DISCA13_AGE>=72, "DF", ifelse(EntryGrave13>=72, "MD", ifelse(Edadinicio_cuidado>=72, "ID", ifelse(EDAD>=72,"DC", NA))))) %>% 
  mutate("73" = ifelse(DISCA13_AGE>=73, "DF", ifelse(EntryGrave13>=73, "MD", ifelse(Edadinicio_cuidado>=73, "ID", ifelse(EDAD>=73,"DC", NA))))) %>% 
  mutate("74" = ifelse(DISCA13_AGE>=74, "DF", ifelse(EntryGrave13>=74, "MD", ifelse(Edadinicio_cuidado>=74, "ID", ifelse(EDAD>=74,"DC", NA))))) %>% 
  mutate("75" = ifelse(DISCA13_AGE>=75, "DF", ifelse(EntryGrave13>=75, "MD", ifelse(Edadinicio_cuidado>=75, "ID", ifelse(EDAD>=75,"DC", NA))))) %>% 
  mutate("76" = ifelse(DISCA13_AGE>=76, "DF", ifelse(EntryGrave13>=76, "MD", ifelse(Edadinicio_cuidado>=76, "ID", ifelse(EDAD>=76,"DC", NA))))) %>% 
  mutate("77" = ifelse(DISCA13_AGE>=77, "DF", ifelse(EntryGrave13>=77, "MD", ifelse(Edadinicio_cuidado>=77, "ID", ifelse(EDAD>=77,"DC", NA))))) %>% 
  mutate("78" = ifelse(DISCA13_AGE>=78, "DF", ifelse(EntryGrave13>=78, "MD", ifelse(Edadinicio_cuidado>=78, "ID", ifelse(EDAD>=78,"DC", NA))))) %>% 
  mutate("79" = ifelse(DISCA13_AGE>=79, "DF", ifelse(EntryGrave13>=79, "MD", ifelse(Edadinicio_cuidado>=79, "ID", ifelse(EDAD>=79,"DC", NA))))) %>% 
  mutate("80" = ifelse(DISCA13_AGE>=80, "DF", ifelse(EntryGrave13>=80, "MD", ifelse(Edadinicio_cuidado>=80, "ID", ifelse(EDAD>=80,"DC", NA))))) %>% 
  mutate("81" = ifelse(DISCA13_AGE>=81, "DF", ifelse(EntryGrave13>=81, "MD", ifelse(Edadinicio_cuidado>=81, "ID", ifelse(EDAD>=81,"DC", NA))))) %>% 
  mutate("82" = ifelse(DISCA13_AGE>=82, "DF", ifelse(EntryGrave13>=82, "MD", ifelse(Edadinicio_cuidado>=82, "ID", ifelse(EDAD>=82,"DC", NA))))) %>% 
  mutate("83" = ifelse(DISCA13_AGE>=83, "DF", ifelse(EntryGrave13>=83, "MD", ifelse(Edadinicio_cuidado>=83, "ID", ifelse(EDAD>=83,"DC", NA))))) %>% 
  mutate("84" = ifelse(DISCA13_AGE>=84, "DF", ifelse(EntryGrave13>=84, "MD", ifelse(Edadinicio_cuidado>=84, "ID", ifelse(EDAD>=84,"DC", NA))))) %>% 
  mutate("85" = ifelse(DISCA13_AGE>=85, "DF", ifelse(EntryGrave13>=85, "MD", ifelse(Edadinicio_cuidado>=85, "ID", ifelse(EDAD>=85,"DC", NA))))) %>% 
  mutate("86" = ifelse(DISCA13_AGE>=86, "DF", ifelse(EntryGrave13>=86, "MD", ifelse(Edadinicio_cuidado>=86, "ID", ifelse(EDAD>=86,"DC", NA))))) %>% 
  mutate("87" = ifelse(DISCA13_AGE>=87, "DF", ifelse(EntryGrave13>=87, "MD", ifelse(Edadinicio_cuidado>=87, "ID", ifelse(EDAD>=87,"DC", NA))))) %>% 
  mutate("88" = ifelse(DISCA13_AGE>=88, "DF", ifelse(EntryGrave13>=88, "MD", ifelse(Edadinicio_cuidado>=88, "ID", ifelse(EDAD>=88,"DC", NA))))) %>% 
  mutate("89" = ifelse(DISCA13_AGE>=89, "DF", ifelse(EntryGrave13>=89, "MD", ifelse(Edadinicio_cuidado>=89, "ID", ifelse(EDAD>=89,"DC", NA))))) %>% 
  mutate("90" = ifelse(DISCA13_AGE>=90, "DF", ifelse(EntryGrave13>=90, "MD", ifelse(Edadinicio_cuidado>=90, "ID", ifelse(EDAD>=90,"DC", NA))))) %>% 
  mutate("91" = ifelse(DISCA13_AGE>=91, "DF", ifelse(EntryGrave13>=91, "MD", ifelse(Edadinicio_cuidado>=91, "ID", ifelse(EDAD>=91,"DC", NA))))) %>% 
  mutate("92" = ifelse(DISCA13_AGE>=92, "DF", ifelse(EntryGrave13>=92, "MD", ifelse(Edadinicio_cuidado>=92, "ID", ifelse(EDAD>=92,"DC", NA))))) %>% 
  mutate("93" = ifelse(DISCA13_AGE>=93, "DF", ifelse(EntryGrave13>=93, "MD", ifelse(Edadinicio_cuidado>=93, "ID", ifelse(EDAD>=93,"DC", NA))))) %>% 
  mutate("94" = ifelse(DISCA13_AGE>=94, "DF", ifelse(EntryGrave13>=94, "MD", ifelse(Edadinicio_cuidado>=94, "ID", ifelse(EDAD>=94,"DC", NA))))) %>% 
  mutate("95" = ifelse(DISCA13_AGE>=95, "DF", ifelse(EntryGrave13>=95, "MD", ifelse(Edadinicio_cuidado>=95, "ID", ifelse(EDAD>=95,"DC", NA))))) %>% 
  mutate("96" = ifelse(DISCA13_AGE>=96, "DF", ifelse(EntryGrave13>=96, "MD", ifelse(Edadinicio_cuidado>=96, "ID", ifelse(EDAD>=96,"DC", NA))))) %>% 
  mutate("97" = ifelse(DISCA13_AGE>=97, "DF", ifelse(EntryGrave13>=97, "MD", ifelse(Edadinicio_cuidado>=97, "ID", ifelse(EDAD>=97,"DC", NA))))) %>% 
  mutate("98" = ifelse(DISCA13_AGE>=98, "DF", ifelse(EntryGrave13>=98, "MD", ifelse(Edadinicio_cuidado>=98, "ID", ifelse(EDAD>=98,"DC", NA))))) %>% 
  mutate("99" = ifelse(DISCA13_AGE>=99, "DF", ifelse(EntryGrave13>=99, "MD", ifelse(Edadinicio_cuidado>=99, "ID", ifelse(EDAD>=99,"DC", NA))))) %>% 
  mutate("100" = ifelse(DISCA13_AGE>=100, "DF", ifelse(EntryGrave13>=100, "MD", ifelse(Edadinicio_cuidado>=100, "ID", ifelse(EDAD>=100,"DC", NA)))))


### save data

# save(tra_may, file='datasets/020_traMay.RData')
# save(tra_may_C, file='datasets/020_traMay_C.RData')
# save(tra_may_13, file='datasets/020_traMay_13.RData')
# save(tra_may_D13, file='datasets/020_traMay_D13.RData')

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
