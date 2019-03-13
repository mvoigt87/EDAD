### --------------------------------------- ###
### Survival models for entry in Dependency ###
### --------------------------------------- ###

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
library(broom)

# Disability Data - full linked data
# ---------------------------------- #

load(file='datasets/030_mayorEntry.RData')

######### Extract just the cases needed! ############
link.may50 <- link.may
link.may50 <- link.may50 %>% filter(DISCA13_AGE>=50) %>% filter(DISCA13_AGE<999)

######### Extract just the cases needed! ############

# ------------------------------------------- #
# 2. Create variable - First DISCA13 ADL/IADL #
# ------------------------------------------- #

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

# Find the minimum age from all (I)ADLs

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

table(link.may50$FIRST_DIS)

# "New" event variable
# --------------------
link.may50 <- link.may50 %>% mutate(event_dep = ifelse(!is.na(Edadinicio_cuidado),1,0))

# "New" age variable
# ------------------

# %%%%%%%% #
# Exit age #
# %%%%%%%% #

# QUick check
link.may50 <- data.table(link.may50)
link.may50[!is.na(Edadinicio_cuidado), .N, .(Edadinicio_cuidado<DISCA13_AGE)] # 1519 problem cases
link.may50[!is.na(Edadinicio_cuidado), .N, .(Edadinicio_cuidado<Edadinicio_disca44)] # only 0 problem cases
link.may50[!is.na(Edadinicio_cuidado), .N, .(Edadinicio_cuidado==Edadinicio_disca44)] # only 0 problem cases
link.may50[Edadinicio_cuidado<DISCA13_AGE & !is.na(Edadinicio_cuidado), .N, .(Edadinicio_disca44<=DISCA13_AGE)]

summary(round(link.may50$Edadinicio_cuidado,0) - link.may50$DISCA13_AGE)
hist(link.may50$Edadinicio_cuidado - link.may50$DISCA13_AGE, breaks = 50)
summary(link.may50$Edadinicio_disca44)

link.may50 <- link.may50 %>% 
  # filter the cases lower than 50 (EDAD_disca44)
  filter(Edadinicio_disca44>=50) %>% 
  # build a new exit age variable (give room for the possibility that care comes first)
  mutate(age_ex_dep = ifelse(is.na(Edadinicio_cuidado),EDAD, 
                             ifelse(Edadinicio_disca44==Edadinicio_cuidado,Edadinicio_cuidado+0.1, Edadinicio_cuidado))) %>% 
  # filter all people with onset age 50 and older
  filter(age_ex_dep>=50)
  
hist(link.may50$age_ex_dep, breaks = 50)
summary(link.may50$age_ex_dep)


# %%%%%%%%% #
# Entry age #
# %%%%%%%%% #
link.may50[!is.na(Edadinicio_cuidado), .N, .(Edadinicio_cuidado>as.numeric(EdadInicioDisca44))] 

# take care of the possibility that it could be the same as the age of first disability
link.may50 <- link.may50 %>% mutate(age_entry = Edadinicio_disca44)

# 7688 individuals

# --------------------------- #
# 2. Split into men and women #
# --------------------------- #

# 2.1 
link.may_M <- link.may50 %>% filter(SEXO=="Varón") %>% mutate(Sex="Male")
link.may_F <- link.may50 %>% filter(SEXO=="Mujer") %>% mutate(SEXO="Female")

### Recode variables for the analysis
#####################################


# Education
# ---------
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


# Civil Status
# -------------
table(link.may_M$Ecivil4)
table(link.may_F$Ecivil4)

# Create English equivalent with less categories
link.may_M <- link.may_M %>% mutate(civil = as.factor(ifelse(Ecivil4=="Casado", "Married",
                                                   ifelse(Ecivil4=="viudo", "Widowed", "Others"))))

link.may_F <- link.may_F %>% mutate(civil = as.factor(ifelse(Ecivil4=="Casado", "Married",
                                                   ifelse(Ecivil4=="viudo", "Widowed", "Others"))))
link.may_M <- within(link.may_M, civil <- relevel(civil, ref = "Married")) 
link.may_F <- within(link.may_F, civil <- relevel(civil, ref = "Married")) 

# Co-vivienca with the partner
# ----------------------------
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

# Accident variable
link.may_M <- link.may_M %>% mutate(Accident12 = as.factor(Accident12))
link.may_F <- link.may_F %>% mutate(Accident12 = as.factor(Accident12))

link.may_M <- within(link.may_M, Accident12 <- relevel(Accident12, ref = "No Accident")) 
link.may_F <- within(link.may_F, Accident12 <- relevel(Accident12, ref = "No Accident")) 

# Co-morbidity variables

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

#### ---------------------------------------
#### 3. Explorative and descriptive analysis
#### ---------------------------------------


# Output table for paper - summary stats on the age variables   !!! (Make all of them numeric arguments)
stargazer(link.may_M[,c(409,424,443,463,487,502, 513)])
stargazer(link.may_F[,c(409,424,443,463,487,502, 513)])

# Another out put of key variables

# Males
link.may_M <- link.may_M %>% mutate(dur_dis_dep = round(age_ex_dep,0)-round(EdadInicioDisca13,0))
hist(link.may_M$dur_dis_dep, breaks = 50)
summary(link.may_M$dur_dis_dep)

# event distribution by duration in disability
# --------------------------------------------
DUR_Plot_M <- link.may_M %>% mutate(event = as.factor(event_dep)) %>% 
  ggplot(aes(x=dur_dis_dep, fill=event)) +
  geom_histogram(aes(y=0.5*..density..), binwidth=0.5) +
  scale_x_continuous(name = "Duration in Disability (Years)") +
  scale_y_continuous(name = "Relative Frequency",labels = scales::percent) +
  scale_fill_manual(name = "", values=c("#000000", "#A9A9A9"), labels = c("Care", "No care")) +
  theme_bw()
DUR_Plot_M <- DUR_Plot_M + theme(legend.position = c(0.85, 0.80)) + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))

#### 4. KME - Kaplan Meier
#### ---------------------

F_fit.1a <- survfit(coxph(Surv(time=age_entry,
                              time2 = age_ex_dep,
                              event = event_dep) ~ 1, data = link.may_F), data = link.may_F,
                   type = "kaplan-meier")

M_fit.1a <- survfit(coxph(Surv(time=age_entry,
                               time2 = age_ex_dep,
                               event = event_dep) ~ 1, data = link.may_M), data = link.may_M,
                    type = "kaplan-meier")


KME_F_1A <- tidy(F_fit.1a) %>% dplyr::select(estimate, time) %>% mutate(sex="female")
KME_M_1A <- tidy(M_fit.1a) %>% dplyr::select(estimate, time) %>% mutate(sex="male")
KME1 <- union(KME_F_1A, KME_M_1A)

KME1_plot <- KME1 %>% ggplot() +
  geom_step(aes(x=time, y=estimate, color=sex)) +
  scale_y_continuous(name = "Probability of Free of Care Need")                  +
  scale_x_continuous(name = "Age") +
  scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
  theme_bw()

KME1_plot + theme(legend.position = c(0.85, 0.80)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))


#### 5. Cox Models
#### -------------

## Males - time since EDAD

Cox.CFP.a <- coxph(Surv(time=age_entry,
                        time2 = age_ex_dep,
                        event = event_dep) ~ FIRST_DIS + civil + education + CP + DailyAct + Accident12 + D1_CVD + D2_C + D3_MD, 
                   data=link.may_M)

summary(Cox.CFP.a)

Cox.CFP.a2 <- coxph(Surv(time=rep(50,length(age_entry)),
                         time2 = age_ex_dep,
                         event = event_dep) ~ DISCA13_AGE + civil + education + CP + DailyAct + Accident12  + D1_CVD + D2_C + D3_MD, 
                    data=link.may_M)

summary(Cox.CFP.a2)

## Females - Time since EDAD

Cox.CFP.b <- coxph(Surv(time=age_entry,
                        time2 = age_ex_dep,
                        event = event_dep) ~ FIRST_DIS + civil + education + CP + DailyAct + Accident12 + D1_CVD + D2_C + D3_MD, 
                   data=link.may_F)

summary(Cox.CFP.b)


Cox.CFP.b2 <- coxph(Surv(time=age_entry,
                         time2 = age_ex_dep,
                         event = event_dep) ~ DISCA13_AGE + CP + education + DailyAct + Accident12 + D1_CVD + D2_C + D3_MD, 
                    data=link.may_F)

summary(Cox.CFP.b2)

  
