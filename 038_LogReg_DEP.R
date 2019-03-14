### ------------------------------- ###
### LOGREG - Dependency             ###
### ------------------------------- ###

# load packages
library(tidyverse)
library(data.table)
library(foreign)
library(ggplot2)
library(gcookbook)
library(HMDHFDplus)
library(plyr)
library(reshape2)
library(grid)
library(gridExtra)
library(tidyr)
library(ggplot2)
library(readxl)
library(dplyr)
library(scales)
library(stargazer)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 1. load data - Disability Data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

load(file='datasets/010_mayor50_LT.RData')


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. prepare variables (change references and categories)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# 2.1. Age at first DISCA 13 disability
link.may50 <- link.may50 %>% 
  mutate(DIS_1_A = ifelse(!is.na(MOV_18_5), MOV_18_5, 999)) %>% 
  mutate(DIS_2_A = ifelse(!is.na(MOV_20_5), MOV_20_5, 999)) %>%
  mutate(DIS_3_A = ifelse(!is.na(MOV_21_5), MOV_21_5, 999)) %>% 
  mutate(DIS_4_A = ifelse(!is.na(MOV_22_5), MOV_22_5, 999)) %>% 
  mutate(DIS_5_A = ifelse(!is.na(AUT_27_5), AUT_27_5, 999)) %>% 
  mutate(DIS_6_A = ifelse(!is.na(AUT_28_5), AUT_28_5, 999)) %>% 
  mutate(DIS_7_A = ifelse(!is.na(AUT_29_5), AUT_29_5, 999)) %>% 
  mutate(DIS_8_A = ifelse(!is.na(AUT_30_5), AUT_30_5, 999)) %>% 
  mutate(DIS_9_A = ifelse(!is.na(AUT_32_5), AUT_32_5, 999)) %>% 
  mutate(DIS_10_A = ifelse(!is.na(AUT_33_5), AUT_33_5, 999)) %>% 
  mutate(DIS_11_A = ifelse(!is.na(VDOM_36_5), VDOM_36_5, 999)) %>% 
  mutate(DIS_12_A = ifelse(!is.na(VDOM_37_5), VDOM_37_5, 999)) %>% 
  mutate(DIS_13_A = ifelse(!is.na(VDOM_38_5), VDOM_38_5, 999)) %>% 
  
  ## Now compute the column minimum (gives the entry age to severity)
  mutate(DISCA13_AGE = pmin(DIS_1_A, DIS_2_A, DIS_3_A, DIS_4_A, DIS_5_A, DIS_6_A,
                            DIS_7_A, DIS_8_A, DIS_9_A, DIS_10_A, DIS_11_A, DIS_12_A,
                            DIS_13_A))

# link.may50 <- link.may50 %>% filter(DISCA13_AGE>=50) %>% filter(DISCA13_AGE<999)
# link.may50 <- link.may50 %>% filter(Edadinicio_disca44>=50)

# 2.2 Find the minimum age from all (I)ADLs

link.may50 <- link.may50 %>% mutate(FIRST_DIS = ifelse(DISCA13_AGE==DIS_1_A,"Changing the body posture", 
                                                       ifelse(DISCA13_AGE==DIS_2_A, "Walking and moving inside the house",
                                                              ifelse(DISCA13_AGE==DIS_3_A, "Walking and moving outside",
                                                                     ifelse(DISCA13_AGE==DIS_4_A, "Sitting down and using public transport",
                                                                            ifelse(DISCA13_AGE==DIS_5_A, "Wash and dry different body parts",
                                                                                   ifelse(DISCA13_AGE==DIS_6_A, "Basic hygene",
                                                                                          ifelse(DISCA13_AGE==DIS_7_A, "Urination",
                                                                                                 ifelse(DISCA13_AGE==DIS_8_A, "Going to the toilet",
                                                                                                        ifelse(DISCA13_AGE==DIS_9_A, "To dress and undress",
                                                                                                               ifelse(DISCA13_AGE==DIS_10_A, "Eating and drinking",
                                                                                                                      ifelse(DISCA13_AGE==DIS_11_A, "Shopping (groceries)",
                                                                                                                             ifelse(DISCA13_AGE==DIS_12_A, "Preparing food/cooking",
                                                                                                                                    ifelse(DISCA13_AGE==DIS_13_A, "Household task (cleaning the house)", "NONE"))))))))))))))     

table(link.may50$FIRST_DIS, useNA="always")

# 2.2.2 Make dummy variables

link.may50 <- link.may50 %>% mutate(FIRST_DIS_1 = ifelse(FIRST_DIS=="Changing the body posture", 1, 0)) %>% 
  mutate(FIRST_DIS_2 = ifelse(FIRST_DIS=="Walking and moving inside the house", 1, 0)) %>% 
  mutate(FIRST_DIS_3 = ifelse(FIRST_DIS=="Walking and moving outside", 1, 0)) %>% 
  mutate(FIRST_DIS_4 = ifelse(FIRST_DIS=="Sitting down and using public transport", 1, 0)) %>% 
  mutate(FIRST_DIS_5 = ifelse(FIRST_DIS=="Wash and dry different body parts", 1, 0)) %>% 
  mutate(FIRST_DIS_6 = ifelse(FIRST_DIS=="Basic hygene", 1, 0)) %>% 
  mutate(FIRST_DIS_7 = ifelse(FIRST_DIS=="Urination", 1, 0)) %>% 
  mutate(FIRST_DIS_8 = ifelse(FIRST_DIS=="Going to the toilet", 1, 0)) %>% 
  mutate(FIRST_DIS_9 = ifelse(FIRST_DIS=="To dress and undress", 1, 0)) %>% 
  mutate(FIRST_DIS_10 = ifelse(FIRST_DIS=="Eating and drinking", 1, 0)) %>% 
  mutate(FIRST_DIS_11 = ifelse(FIRST_DIS=="Shopping (groceries)", 1, 0)) %>% 
  mutate(FIRST_DIS_12 = ifelse(FIRST_DIS=="Preparing food/cooking", 1, 0)) %>% 
  mutate(FIRST_DIS_13 = ifelse(FIRST_DIS=="Household task (cleaning the house)", 1, 0))


  # 2.3 Accident in the last 12 months #
# ---------------------------------- #

class(link.may50$K_4)   
table(link.may50$K_4, useNA = "always")

link.may50 <- link.may50 %>% mutate(Accident12 = ifelse(K_4=="Sí", "Accident 12 mo", ifelse(K_4=="NC", NA, "No Accident")))

# 2.4 Body and attitude #
# ---------------------- #
table(link.may50$K_7, useNA = "always")

link.may50 <- link.may50 %>% mutate(DailyAct = ifelse(K_7=="Sí", "Daily activity", ifelse(K_7=="NC", NA, "No daily act.")))


###### 2.45 Co-morbidity

# Cardiovascular diseases (includes stroke)
link.may50 <- link.may50 %>% mutate(D1_CVD = ifelse(K_3_2=="Sí" | K_3_3=="Sí" | K_3_5=="Sí","CVD", "No CVD")) %>% 
  # Now bring the missings back
  mutate(D1_CVD = ifelse(K_3_2=="NC" & K_3_3=="NC" & K_3_5=="NC", NA, D1_CVD))

# Cancer
table(link.may50$K_3_12)
link.may50 <- link.may50 %>% mutate(D2_C = ifelse(K_3_12!="Sí","Cancer", ifelse(K_3_12=="NC", NA, "No Cancer")))

# Mental diseases
table(link.may50$K_3_15)
table(link.may50$K_3_16)
table(link.may50$K_3_17)
link.may50 <- link.may50 %>% mutate(D3_MD = ifelse(K_3_15=="Sí"| K_3_16=="Sí" | K_3_17=="Sí","Mental disease", "No mental disease")) %>% 
  # Now bring the missings back
  mutate(D3_MD = ifelse(K_3_15=="NC" & K_3_16=="NC" & K_3_17=="NC", NA, D3_MD))                    



# 2.5 Separate analysis by sex
link.may_M <- link.may50 %>% filter(SEXO=="Varón") %>% mutate(Sex="Male")
link.may_F <- link.may50 %>% filter(SEXO=="Mujer") %>% mutate(SEXO="Female")

# 2.6  Education
# --------------
table(link.may_M$Estudios4)
table(link.may_F$Estudios4)

# delete the missings
link.may_M <- link.may_M %>% mutate(Estudios4=ifelse(Estudios4=="NC",NA,Estudios4)) %>% filter(!is.na(Estudios4))
link.may_F <- link.may_F %>% mutate(Estudios4=ifelse(Estudios4=="NC",NA,Estudios4)) %>% filter(!is.na(Estudios4))

# Create English equivalent with less categories
link.may_M <- link.may_M %>% mutate(education = as.factor(ifelse(Estudios4==2, "Incomplete Educ.",
                                                                 ifelse(Estudios4==3, "Primary Educ.", "Secondary Educ or higher"))))

link.may_F <- link.may_F %>% mutate(education = as.factor(ifelse(Estudios4==2, "Incomplete Educ.",
                                                                 ifelse(Estudios4==3, "Primary Educ.", "Secondary Educ or higher"))))

# change the reference category
link.may_M <- within(link.may_M, education <- relevel(education, ref = "Secondary Educ or higher")) 
link.may_F <- within(link.may_F, education <- relevel(education, ref = "Secondary Educ or higher")) 


# 2.7 Civil Status
# ----------------
table(link.may_M$Ecivil4)
table(link.may_F$Ecivil4)

# Create English equivalent with less categories
link.may_M <- link.may_M %>% mutate(civil = as.factor(ifelse(Ecivil4=="Casado", "Married",
                                                             ifelse(Ecivil4=="viudo", "Widowed", "Others"))))

link.may_F <- link.may_F %>% mutate(civil = as.factor(ifelse(Ecivil4=="Casado", "Married",
                                                             ifelse(Ecivil4=="viudo", "Widowed", "Others"))))
link.may_M <- within(link.may_M, civil <- relevel(civil, ref = "Married")) 
link.may_F <- within(link.may_F, civil <- relevel(civil, ref = "Married")) 

# 2.8 Cohabitation with the partner
# ---------------------------------
table(link.may_M$PAREJA)
table(link.may_F$PAREJA)

link.may_M <- link.may_M %>% mutate(PAREJA=ifelse(PAREJA=="NC",NA,PAREJA)) %>% filter(!is.na(PAREJA))
link.may_F <- link.may_F %>% mutate(PAREJA=ifelse(PAREJA=="NC",NA,PAREJA)) %>% filter(!is.na(PAREJA))

# Create English equivalent with less categories
link.may_M <- link.may_M %>% mutate(CP = as.factor(ifelse(PAREJA==1, "Lives with Partner", "Doesn´t live with partner")))
link.may_F <- link.may_F %>% mutate(CP = as.factor(ifelse(PAREJA==1, "Lives with Partner", "Doesn´t live with partner")))

# change the reference category
link.may_M <- within(link.may_M, CP <- relevel(CP, ref = "Lives with Partner")) 
link.may_F <- within(link.may_F, CP <- relevel(CP, ref = "Lives with Partner")) 

# 2.9 Accident variable
# ----------------------
link.may_M <- link.may_M %>% mutate(Accident12 = as.factor(Accident12))
link.may_F <- link.may_F %>% mutate(Accident12 = as.factor(Accident12))

link.may_M <- within(link.may_M, Accident12 <- relevel(Accident12, ref = "No Accident")) 
link.may_F <- within(link.may_F, Accident12 <- relevel(Accident12, ref = "No Accident")) 

# 2.10 Co-morbidity variables
# --------------------------
# CVD
link.may_M <- link.may_M %>% mutate(D1_CVD = as.factor(D1_CVD))
link.may_F <- link.may_F %>% mutate(D1_CVD = as.factor(D1_CVD))

link.may_M <- within(link.may_M, D1_CVD <- relevel(D1_CVD, ref = "No CVD")) 
link.may_F <- within(link.may_F, D1_CVD <- relevel(D1_CVD, ref = "No CVD")) 

# Mental diseases
link.may_M <- link.may_M %>% mutate(D3_MD = as.factor(D3_MD))
link.may_F <- link.may_F %>% mutate(D3_MD = as.factor(D3_MD))

link.may_M <- within(link.may_M, D3_MD <- relevel(D3_MD, ref = "No mental disease")) 
link.may_F <- within(link.may_F, D3_MD <- relevel(D3_MD, ref = "No mental disease"))

# 2.11 Missing values  !!! Still needs to be fixed !!!!!!!!!!!!!!!!!!!!!!!!!!
# -------------------
library(Amelia)

# Make data set with variables for the analysis
training.data_M <- link.may_M %>% select(EDAD, Disca13, DISCA13_AGE, FIRST_DIS_1, FIRST_DIS_2, FIRST_DIS_3, FIRST_DIS_4,
                                           FIRST_DIS_5, FIRST_DIS_6, FIRST_DIS_7, FIRST_DIS_8, FIRST_DIS_9, FIRST_DIS_10, 
                                           FIRST_DIS_11, FIRST_DIS_12, civil, education, Accident12, DailyAct, D1_CVD, 
                                           D2_C, D3_MD)

missmap(training.data_M, main = "Missing values vs observed")




### 3. Logistic Regression (dependientes)
### -------------------------------------



# 3.1 dependent variable (based on L_1 = dependent on personal care)

table(link.may_F$DependientesReales44, useNA="always")
table(link.may_M$DependientesReales44, useNA="always")

# Make the 1-0 variable
link.may_F <- link.may_F %>% mutate(DEP = ifelse(is.na(L_1), 0, ifelse(L_1=="Sí", 1, 0)))
table(link.may_F$DEP, useNA = "always")
# Dep var distribution in 2008 - females
# --------------------------------------
DEP_Plot_F <- link.may_F %>% mutate(DEP = as.factor(DEP)) %>% 
  ggplot(aes(x=EDAD, fill=DEP)) +
  geom_histogram(aes(y=0.5*..density..), binwidth=0.5) +
  scale_x_continuous(name = "Age in 2008") +
  scale_y_continuous(name = "Relative Frequency",labels = scales::percent) +
  scale_fill_manual(name = "", values=c("#000000", "#A9A9A9"), labels = c("No care", "Care")) +
  theme_bw()
DEP_Plot_F <- DEP_Plot_F + theme(legend.position = c(0.85, 0.80)) + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))

link.may_M <- link.may_M %>% mutate(DEP = ifelse(is.na(L_1), 0, ifelse(L_1=="Sí", 1, 0)))
table(link.may_M$DEP, useNA = "always")

# Dep var distribution in 2008 - males
# ------------------------------------
DEP_Plot_M <- link.may_M %>% mutate(DEP = as.factor(DEP)) %>% 
  ggplot(aes(x=EDAD, fill=DEP)) +
  geom_histogram(aes(y=0.5*..density..), binwidth=0.5) +
  scale_x_continuous(name = "Age in 2008") +
  scale_y_continuous(name = "Relative Frequency",labels = scales::percent) +
  scale_fill_manual(name = "", values=c("#000000", "#A9A9A9"), labels = c("No care", "Care")) +
  theme_bw()
DEP_Plot_M <- DEP_Plot_M + theme(legend.position = c(0.85, 0.80)) + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))


# 3.2 Base Model

# females
model_F1 <- glm(DEP ~ EDAD,family=binomial(link='logit'),data=link.may_F)

summary(model_F1)

# males
model_M1 <- glm(DEP ~ EDAD,family=binomial(link='logit'),data=link.may_M)

summary(model_M1)

# 3.3 Model Playing Range

# females 

  # Household tasks excluded as reference dummy
model_F2 <- glm(DEP ~ EDAD + Disca13 + DISCA13_AGE + FIRST_DIS_1 + FIRST_DIS_2 +  FIRST_DIS_3  +  FIRST_DIS_4 +
                  FIRST_DIS_5 + FIRST_DIS_6 + FIRST_DIS_7 + FIRST_DIS_8 + FIRST_DIS_9 + FIRST_DIS_10 + FIRST_DIS_11 +
                  FIRST_DIS_12 + civil + education,
                family=binomial(link='logit'),data=link.may_F)
# + Accident12 + DailyAct + D1_CVD + D2_C + D3_MD

summary(model_F2)

# males
model_M2 <- glm(DEP ~ EDAD + Disca13 + DISCA13_AGE + FIRST_DIS_1 + FIRST_DIS_2 + FIRST_DIS_3  +  FIRST_DIS_4 +
                  FIRST_DIS_5 + FIRST_DIS_6 + FIRST_DIS_7 + FIRST_DIS_8 + FIRST_DIS_9 + FIRST_DIS_10 + FIRST_DIS_11 +
                  FIRST_DIS_12 + civil + education ,family=binomial(link='logit'),data=link.may_M)
# + Accident12 + DailyAct + D1_CVD + D2_C + D3_MD
summary(model_M2)

# Put the output together
stargazer(model_F1, model_F2, model_M1, model_M2, title="Logistic Regression Models",no.space=F,
          ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative risk of being dependent"),
          covariate.labels=c("Age","Disca 13", "Onset Disca 13", "First Dis 1 = Changing the body posture",
                             "First Dis 2 = Moving inside", "First Dis 3 = Moving outside",
                             "First Dis 4 = Sitting down", "First Dis 5 = Wash and dry",
                             "First Dis 6 = Basic hygene", "First Dis 7 = Urination",
                             "First Dis 8 = Going to the toilet", "First Dis 9 = Dress/undress",
                             "First Dis 10 = Eating and Drinking", "First Dis 11 = Shopping", "First Dis 12 = Preparing food/cooking",
                             "Single/Div","Widowed","Incomplete Ed." ,"Primary Ed."),
          single.row=FALSE, apply.coef = exp)


# Other variables
# , "Accident in last 12 mo", "No daily activity","Has CVD", "Doesn´t have cancer", "Has Mental Illness"