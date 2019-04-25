## --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##  Linking the the INE Follow-Up with the EDAD 2008 base file
## --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


rm(list = ls())
require(tidyverse)
require(data.table)
library(foreign)
source('https://raw.githubusercontent.com/bjornerstedt/assist/master/R/describe.R')


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
### Prepare follow-up Data from INE
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

# Old data
# load('follow.up.RData')


# New linked follow ups (based on Frans code)

ww  <- c(1, 5, 6, 8, 9, 11, 13, 14, 18, 20, 24, 26, 30) ; ww <-diff(ww)
nms <- c("nseccion", "dc", "viv", "hog", "nord", "renta", "estado", "causa", "mesdef", "anodef", "m_salida", "a_salida")
cty= rep('integer',12); cty[8] <- 'character'


# read.fwf()
#, col_types =  cty
# , colnames(nms)

follow.up2 <- read_fwf(file = './datasets/fichero_Discapacidad_2008_feb_2019.txt',
                       fwf_widths(ww, col_names = nms),
                       col_types =  cols(
                         nseccion = col_character(),
                         dc = col_integer(),
                         viv = col_integer(),
                         hog = col_integer(),
                         nord = col_integer(),
                         renta = col_integer(),
                         estado = col_character(),
                         causa = col_character(),
                         mesdef = col_integer(),
                         anodef = col_integer(),
                         m_salida = col_integer(),
                         a_salida = col_integer()
                       ))

glimpse(follow.up2)



# EDAD data #
# --------- #

load('EDAD2008Hogares.RData')
as.data.table(ed.hog) -> ed.hog

as.data.table(follow.up2) -> follow.up

ed.hog2 <-   as.data.table(lapply(ed.hog, function(e) {if (class(e)=='labelled') {as_factor(e)} else {as.integer(e) } }) )

tolower(names(ed.hog2)) ->  names(ed.hog2)

glimpse(ed.hog2)
glimpse(follow.up)

follow.up[,nseccion:= as.integer(as.character(nseccion))]


# setkey(
setkey(ed.hog2,nseccion,dc,viv,hog,nord)
setkey(follow.up,nseccion,dc,viv,hog,nord)  

follow.up[,.N,estado]
#    estado      N
# 1:      A 183792
# 2:      B  23700
# 3:      N     37

  names(ed.hog2)

  # merge(ed.hog2[,c(4:10),with=F],follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link
  # class(link)
  # 
  # 
  # link[,enlazado:=!is.na(estado)]
  # link[,.N,.(enlazado,estado)]
  #    enlazado estado      N
  # 1:    FALSE     NA  50658
  # 2:     TRUE      A 183792
  # 3:     TRUE      B  23700
  # 4:     TRUE      N     37



#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
### Second Data Set to Link - EDAD 2008
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################



### Link the follow-up to another data source = EDAD (full population) to be able to extract the population 50 +


EDAD.mayor50 = read.spss("TOTAL-EDAD-2008.sav", to.data.frame=TRUE)
# A few checks with variables - to get a feeling for the number of transitions
EDAD.mayor50 <- data.table(EDAD.mayor50)
# select variables
head(names(EDAD.mayor50), 1000)
tail(names(EDAD.mayor50), 291)
# change variable names for merge process
names(EDAD.mayor50)[5:9]<- c("nseccion", "dc", "viv", "hog", "nord")

# Select variables from the SPSS data set (full population file working file Antonio and Julio)
merge(EDAD.mayor50[,c(1,4:29, 43:47, 53:76, 89:92, 94:104, 108, 114, 115, 120, 123, 125, 128, 133, 136, 141, 142, 147, 148, 153,
                    154, 156, 166, 171, 172, 174, 176, 178, 180, 185, 186, 191:195, 197, 204, 205, 209, 210, 211, 215,
                    216, 217, 221, 246, 247, 251, 252, 253, 257, 258, 259, 263, 264, 265, 269, 276, 277, 281, 282, 283, 287, 
                    300, 301, 305, 306, 307, 311, 312, 313, 317, 342:399, 407, 408, 410, 414, 455, 465, 470, 480, 485, 490, 
                    495, 500, 505, 510, 515, 517, 522, 527:534, 552, 553:588, 601, 616:646, 648:656, 661, 672, 677:706, 711, 716,
                    717:738, 740, 741, 754, 758:763, 771, 772, 774, 778:780, 796, 801:836, 892:898, 900:918, 952:964, 977:990,
                    1085, 1086, 1093, 1094, 1097:1118, 1231,1250, 1277:1291), with=F],
      follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link.may50

### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###
### ------------------------------------------
### Prepare the data set for later use
    # 1. Set age limit to 50 +
    # 2. Extract the ones that were not linked
    # 3. Create an exit age variable
### ------------------------------------------

link.may50 <- link.may50 %>% filter(EDAD>=50)
class(link.may50)
link.may50 <- data.table(link.may50)
link.may50[,enlazado:=!is.na(estado)]
# Choose the linked ones and the ones with age information
link.may50 <- link.may50 %>% filter(!is.na(EDAD)) %>% filter(enlazado==T) %>%  
  # and make an event variable with the sure information we have (death year and month)
  mutate(event=ifelse(!is.na(anodef),1,0))

# Age at exit
# -----------
link.may50 <- data.table(link.may50)
link.may50[event==0, ':='(a_salida=2016, m_salida=12)]
# for the rest it is the year/month of death information
link.may50[event==1, ':='(a_salida=anodef, m_salida=mesdef)]

link.may50 <- link.may50 %>% 
  # first making the exit age smoother by adding the month and than substracting the first interview date (! find variable)
  mutate(age.ex = EDAD + (a_salida+(m_salida/12) - 2006.99))

hist(link.may50$age.ex, breaks = 50) # looks ok! (high increase at age 60 is caused by large numbers of censored 50 year old)

# Look at the entry to exit age relationship 
link.may50 %>% dplyr::count(EDAD < age.ex)         # bien!

# Missing-ness on central variables (sex, onset age disability, onset age dependency)
# -----------------------------------------------------------------------------------

# Select the individuals which have experienced care episodes
link.may50 %>% dplyr::count(!is.na(Edadinicio_cuidado))

# disability
# DISCA 44 - mix between functional limitations (de piel a dentro) and (I)ADLs (de piel a fuera)
link.may50 %>% dplyr::count(!is.na(EdadInicioDisca44))
# DISCA 13 - selection of 8 ADLs and 5 IADLs
link.may50 %>% dplyr::count(!is.na(EdadInicioDisca13))

# combinations
link.may50 <- data.table(link.may50)
link.may50[, .N, .(!is.na(EdadInicioDisca44))] # everybody 
link.may50[, .N, .(!is.na(EdadInicioDisca13))] # 133 cases
link.may50[!is.na(EdadInicioDisca44), .N, .(as.numeric(EdadInicioDisca44)>=50)] # 1466 cases before age 50
link.may50[!is.na(EdadInicioDisca13), .N, .(as.numeric(EdadInicioDisca13)>=50)] # 915 cases
link.may50[is.na(EdadInicioDisca13), .N, .(as.numeric(EdadInicioDisca44)>=50)] #  88 cases   (we might be able to recover)


class(link.may50$EdadInicioDisca13)
link.may50$EdadInicioDisca13 <- as.numeric(as.factor(link.may50$EdadInicioDisca13))

table(link.may50$EdadInicioDisca13>=50)

## Create a new DISCA 13 Entry AGE
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

## Make a variable to count the disabilities that occur at the onset age of disability



linkTEST <- link.may50 %>% dplyr::select(IdHogar, SEXO, EDAD, DIS_1_A, DIS_2_A, DIS_3_A, DIS_4_A, DIS_5_A, DIS_6_A,
                                         DIS_7_A, DIS_8_A, DIS_9_A, DIS_10_A, DIS_11_A, DIS_12_A,DISCA13_AGE,
                                         DIS_13_A) 

# %>% mutate(DIS_1_A = ifelse(DIS_1_A==999, NA, DIS_1_A)) %>% 
#   mutate(DIS_2_A = ifelse(DIS_2_A==999, NA, DIS_2_A)) %>%  
#   mutate(DIS_3_A = ifelse(DIS_3_A==999, NA, DIS_3_A)) %>%  
#   mutate(DIS_4_A = ifelse(DIS_4_A==999, NA, DIS_4_A)) %>%
#   mutate(DIS_5_A = ifelse(DIS_5_A==999, NA, DIS_5_A)) %>%
#   mutate(DIS_6_A = ifelse(DIS_6_A==999, NA, DIS_6_A)) %>%
#   mutate(DIS_7_A = ifelse(DIS_7_A==999, NA, DIS_7_A)) %>%
#   mutate(DIS_8_A = ifelse(DIS_8_A==999, NA, DIS_8_A)) %>%
#   mutate(DIS_9_A = ifelse(DIS_9_A==999, NA, DIS_9_A)) %>%
#   mutate(DIS_10_A = ifelse(DIS_10_A==999, NA, DIS_10_A)) %>%
#   mutate(DIS_11_A = ifelse(DIS_11_A==999, NA, DIS_11_A)) %>%
#   mutate(DIS_12_A = ifelse(DIS_12_A==999, NA, DIS_12_A)) %>%
#   mutate(DIS_13_A = ifelse(DIS_13_A==999, NA, DIS_13_A)) %>% 
#   # aaand disca 13
#   mutate(DISCA13_AGE = ifelse(DISCA13_AGE==999, NA, DISCA13_AGE))
# 
# linkTEST <- linkTEST %>% dplyr::filter(!is.na(DISCA13_AGE))
# 
# linkTESTX <- linkTEST[,c(4:15,17)]
# 
# linkTESTX$dif <- apply(linkTESTX, 1, FUN = function(x) min( x[x!=min(x)] ))

# link.may50 <- link.may50 %>% filter(DISCA13_AGE>=50) %>% filter(DISCA13_AGE<999)
# link.may50 <- link.may50 %>% filter(Edadinicio_disca44>=50)

# Find the minimum age from all (I)ADLs

link.may50 <- link.may50 %>% mutate(FIRST_DIS = ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_1_A,"Transfer or Body position Changes or Getting in or out of bed", 
                                                       ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_2_A, "Walking indoor",
                                                              ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_3_A, "Walking outdoor",
                                                                     ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_4_A, "Using public transport",
                                                                            ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_5_A, "Bathing/showering",
                                                                                   ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_6_A, "Basic hygiene",
                                                                                          ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_7_A, "Urinating (bladder control)",
                                                                                                 ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_8_A, "Toileting (bowel control)",
                                                                                                        ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_9_A, "Dressing",
                                                                                                               ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_10_A, "Eating",
                                                                                                                      ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_11_A, "Shopping",
                                                                                                                             ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_12_A, "Preparing meals",
                                                                                                                                    ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_13_A, "Housework", "NONE"))))))))))))))     

table(link.may50$FIRST_DIS, useNA = "always")


### ADL / IADL variable - was the first limitation an ADL or IADL ###
### ------------------------------------------------------------- ###

link.may50 <- link.may50 %>% mutate(ADL = ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_1_A | DISCA13_AGE<999 & DISCA13_AGE==DIS_2_A |
                                      DISCA13_AGE<999 & DISCA13_AGE==DIS_3_A | DISCA13_AGE<999 & DISCA13_AGE==DIS_4_A |
                                      DISCA13_AGE<999 & DISCA13_AGE==DIS_5_A | DISCA13_AGE<999 & DISCA13_AGE==DIS_6_A |
                                      DISCA13_AGE<999 & DISCA13_AGE==DIS_7_A | DISCA13_AGE<999 & DISCA13_AGE==DIS_8_A |
                                      DISCA13_AGE<999 & DISCA13_AGE==DIS_9_A | DISCA13_AGE<999 & DISCA13_AGE==DIS_10_A, 1, 0)) %>% 
### And IADLs
### ---------  
  mutate(IADL = ifelse(DISCA13_AGE<999 & DISCA13_AGE==DIS_1_A | DISCA13_AGE<999 & DISCA13_AGE==DIS_2_A |
           DISCA13_AGE<999 & DISCA13_AGE==DIS_3_A, 1, 0))

table(link.may50$ADL, useNA = "always")

table(link.may50$IADL, useNA = "always")

### Catastrophic vs. progressive ###
### ---------------------------- ###

# By number of problems (quantity)

# 1. Make a helper to count the ADLs
link.may50 <- link.may50 %>% dplyr::mutate(Dis_1 = ifelse(!is.na(MOV_18_1) & MOV_18_1=="Sí", 1, 0)) %>% 
  dplyr::mutate(Dis_2 = ifelse(!is.na(MOV_20_1) & MOV_20_1=="Sí", 1, 0)) %>% 
  dplyr::mutate(Dis_3 = ifelse(!is.na(MOV_21_1) & MOV_21_1=="Sí", 1, 0)) %>% 
  dplyr::mutate(Dis_4 = ifelse(!is.na(MOV_22_1) & MOV_22_1=="Sí", 1, 0)) %>% 
  dplyr::mutate(Dis_5 = ifelse(!is.na(AUT_27_1) & AUT_27_1=="Sí", 1, 0)) %>% 
  dplyr::mutate(Dis_7 = ifelse(!is.na(AUT_29_1) & AUT_29_1=="Sí", 1, 0)) %>% 
  dplyr::mutate(Dis_8 = ifelse(!is.na(AUT_30_1) & AUT_30_1=="Sí", 1, 0)) %>% 
  dplyr::mutate(Dis_9 = ifelse(!is.na(AUT_32_1) & AUT_32_1=="Sí", 1, 0)) %>% 
  dplyr::mutate(Dis_10 = ifelse(!is.na(AUT_33_1) & AUT_33_1=="Sí", 1, 0)) %>%
  dplyr::mutate(DIS_6 = ifelse(!is.na(AUT_28_1) & AUT_28_1=="Sí", 1, 0)) %>% 
# 2. Count ADLs
mutate(DIS_count = rowSums(.[531:540])) %>% 
  
# 3. Define catastrophic disability as 2+ ADLs (factor variable)
mutate(CatPro = as.factor(ifelse(DIS_count>=3,"catastrophic", "progressive")))

## Quick check with our definition of disablity  
table(link.may50$CatPro[link.may50$Disca13=="Si"], useNA = "always")



## 2.1. Onset of incapacity of an activity of daily living (by severity)
## ---------------------------------------------------------------------

# For first onset age use "EdadInicioDisca13"

# Minimum age for onset of one of the 13 (I)ADLs (Using the age information for single disabilities)

table(link.may50$MOV_18_2)

# recode all severity variables (DISCA 13) in two groups
link.may50 <- link.may50 %>%
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
  mutate(DIS_13_S = ifelse(VDOM_38_2=="Con dificultad moderada", "mild", "severe"))



# recode entry age into first severe limitation (DISCA 13)
link.may50 <- link.may50 %>% 
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
##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ######
##### Alternative Disability Assessment
table(link.may50$MOV_18_2, useNA = "always")
# recode all severity variables (DISCA 13) in two groups
link.may50 <- link.may50 %>%
  # change posture/move
  mutate(Sev1 = ifelse(is.na(MOV_18_2), 0, ifelse(MOV_18_2=="Con dificultad moderada", 1 , ifelse(MOV_18_2=="Con dificultad severa", 2,3)))) %>% 
  # walking and moving inside
  mutate(Sev2 = ifelse(is.na(MOV_20_2), 0, ifelse(MOV_20_2=="Con dificultad moderada", 1 , ifelse(MOV_20_2=="Con dificultad severa", 2,3)))) %>% 
  # walking and moving outside
  mutate(Sev3 = ifelse(is.na(MOV_21_2), 0, ifelse(MOV_21_2=="Con dificultad moderada", 1 , ifelse(MOV_21_2=="Con dificultad severa", 2,3)))) %>% 
  # Sitting down & using public transport
  mutate(Sev4 = ifelse(is.na(MOV_22_1), 0, ifelse(MOV_22_2=="Con dificultad moderada", 1 , ifelse(MOV_22_2=="Con dificultad severa", 2,3)))) %>% 
  # washing and drying
  mutate(Sev5 = ifelse(is.na(AUT_27_1), 0, ifelse(AUT_27_2=="Con dificultad moderada", 1 , ifelse(AUT_27_2=="Con dificultad severa", 2,3)))) %>% 
  # basic hygene
  mutate(Sev6 = ifelse(is.na(AUT_28_1), 0, ifelse(AUT_28_2=="Con dificultad moderada", 1 , ifelse(AUT_28_2=="Con dificultad severa", 2,3)))) %>% 
  # urinating
  mutate(Sev7 = ifelse(is.na(AUT_29_1), 0, ifelse(AUT_29_2=="Con dificultad moderada", 1 , ifelse(AUT_29_2=="Con dificultad severa", 2,3)))) %>% 
  # bathroom
  mutate(Sev8 = ifelse(is.na(AUT_30_1), 0, ifelse(AUT_30_2=="Con dificultad moderada", 1 , ifelse(AUT_30_2=="Con dificultad severa", 2,3)))) %>% 
  # dressing and undressing
  mutate(Sev9 = ifelse(is.na(AUT_32_1), 0, ifelse(AUT_32_2=="Con dificultad moderada", 1 , ifelse(AUT_32_2=="Con dificultad severa", 2,3)))) %>% 
  # eating and drinking
  mutate(Sev10 = ifelse(is.na(AUT_33_1), 0, ifelse(AUT_33_2=="Con dificultad moderada", 1 , ifelse(AUT_33_2=="Con dificultad severa", 2,3)))) %>% 
  # organising shopping for groceries
  mutate(Sev11 = ifelse(is.na(VDOM_36_1), 0, ifelse(VDOM_36_2=="Con dificultad moderada", 1 , ifelse(VDOM_36_2=="Con dificultad severa", 2,3)))) %>% 
  # preparing food
  mutate(Sev12 = ifelse(is.na(VDOM_37_1), 0, ifelse(VDOM_37_2=="Con dificultad moderada", 1 , ifelse(VDOM_37_2=="Con dificultad severa", 2,3)))) %>%  
  # household tasks
  mutate(Sev13 = ifelse(is.na(VDOM_38_1), 0, ifelse(VDOM_38_2=="Con dificultad moderada", 1 , ifelse(VDOM_38_2=="Con dificultad severa", 2,3)))) %>% 

  # sum them up to create a point system
  mutate(SEV_COUNT = Sev1+Sev2+Sev3+Sev4+Sev5+Sev6+Sev7+Sev8+Sev9+Sev10+Sev11+Sev12+Sev13)

summary(link.may50$SEV_COUNT[link.may50$SEV_COUNT>0])
hist(link.may50$SEV_COUNT[link.may50$SEV_COUNT>0]) # Median is 9, 4 is the first quartile

link.may50 <- link.may50 %>% mutate(SEVEREDIS = ifelse(SEV_COUNT<4, "mild", ifelse(SEV_COUNT<9,"moderate", "severe")))


##############################################################################################################################
#### PLOT for the paper (remember to select the individuals with disability)

SEV_Plot <- link.may50 %>% dplyr::filter(SEV_COUNT>0) %>% mutate(event = as.factor(event)) %>% 
  ggplot(aes(x=SEV_COUNT, fill=event)) +
  geom_histogram(binwidth=0.5) +                                                     # position="identity"
  scale_x_continuous(name = "Severity Score 2008", breaks = c(1,10,20,30,39)) +
  scale_y_continuous(name = "Frequency") +                                  # ,labels = scales::percent
  scale_fill_manual(name = "", values=c("#000000", "#A9A9A9"), labels = c("Survivors", "Deceased")) +
  theme_bw()
SEV_Plot <- SEV_Plot + theme(legend.position = c(0.85, 0.80)) + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))

##############################################################################################################################

# For later purposes the 999 have to be set to NAs
link.may50 <- link.may50 %>% mutate(EntryGrave13=ifelse(EntryGrave13==999, NA, EntryGrave13))

# For later purposes categorize
link.may50 <- link.may50 %>% mutate(EntryGrave13_cat= ifelse(is.na(EntryGrave13),"no severe disability",
                                                         ifelse(EntryGrave13 <=65, "$<$ 65", ifelse(EntryGrave13<=75,"65-75",
                                                                                                    ifelse(EntryGrave13 <=85, "75-85", "85+")))))


# Multi- and co- morbidity variables
# ---------------------------------- #

table(link.may50$K_2, useNA="always")     # - chronic disease
class(link.may50$K_3_1)   # - chronic bronchitis
table(link.may50$K_3_1)
# other chronic diseases
table(link.may50$K_3_19)

## Co-Morbidity counts

# 1. Make a helper to count the ADLs
link.may50 <- link.may50 %>% mutate(CoMo_1 = ifelse(!is.na(K_3_19) & K_3_19=="Sí", 1, 0)) %>% 
  mutate(CoMo_2 = ifelse(!is.na(K_3_1) & K_3_1=="Sí", 1, 0)) %>% 
  mutate(CoMo_3 = ifelse(!is.na(K_3_2) & K_3_2=="Sí", 1, 0)) %>% 
  mutate(CoMo_4 = ifelse(!is.na(K_3_3) & K_3_3=="Sí", 1, 0)) %>% 
  mutate(CoMo_5 = ifelse(!is.na(K_3_4) & K_3_4=="Sí", 1, 0)) %>% 
  mutate(CoMo_6 = ifelse(!is.na(K_3_5) & K_3_5=="Sí", 1, 0)) %>% 
  mutate(CoMo_7 = ifelse(!is.na(K_3_6) & K_3_6=="Sí", 1, 0)) %>% 
  mutate(CoMo_8 = ifelse(!is.na(K_3_7) & K_3_7=="Sí", 1, 0)) %>% 
  mutate(CoMo_9 = ifelse(!is.na(K_3_8) & K_3_8=="Sí", 1, 0)) %>% 
  mutate(CoMo_10 = ifelse(!is.na(K_3_9) & K_3_9=="Sí", 1, 0)) %>% 
  mutate(CoMo_11 = ifelse(!is.na(K_3_10) & K_3_10=="Sí", 1, 0)) %>% 
  mutate(CoMo_12 = ifelse(!is.na(K_3_11) & K_3_11=="Sí", 1, 0)) %>% 
  mutate(CoMo_13 = ifelse(!is.na(K_3_12) & K_3_12=="Sí", 1, 0)) %>% 
  mutate(CoMo_14 = ifelse(!is.na(K_3_13) & K_3_13=="Sí", 1, 0)) %>% 
  mutate(CoMo_15 = ifelse(!is.na(K_3_14) & K_3_14=="Sí", 1, 0)) %>% 
  mutate(CoMo_16 = ifelse(!is.na(K_3_15) & K_3_15=="Sí", 1, 0)) %>% 
  mutate(CoMo_17 = ifelse(!is.na(K_3_16) & K_3_16=="Sí", 1, 0)) %>% 
  mutate(CoMo_18 = ifelse(!is.na(K_3_17) & K_3_17=="Sí", 1, 0)) %>% 
  mutate(CoMo_19 = ifelse(!is.na(K_3_18) & K_3_18=="Sí", 1, 0)) %>% 
  # 2. Count ADLs
  mutate(CoMo_count = rowSums(.[586:604])) %>% 
  
  # 3. Define co-morbidity as 2+ extra diseases (factor variable)
  mutate(CoMorb = as.factor(ifelse(CoMo_count>=2,"multi morbid", "no multi morbidity")))

## Quick check with our definition of disablity  
table(link.may50$CoMorb[link.may50$Disca13=="Si"], useNA = "always")





# Try to make groups
table(link.may50$K_3_2)
table(link.may50$K_3_3)
table(link.may50$K_3_2[link.may50$K_3_3=="NC"]) # seem to be more or less the same people

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

                         
                         
# Accident in the last 12 months #
# ------------------------------ #

class(link.may50$K_4)   
table(link.may50$K_4)

link.may50 <- link.may50 %>% mutate(Accident12 = ifelse(K_4=="Sí", "Accident 12 mo", ifelse(K_4=="NC", NA, "No Accident"))) %>% 
  # listwise delete the NAs (annoying)
     dplyr::filter(!is.na(Accident12))                        
# Body and attitude #
# ----------------- #
table(link.may50$K_7)

link.may50 <- link.may50 %>% mutate(DailyAct = ifelse(K_7=="Sí", "Daily activity", ifelse(K_7=="NC", NA, "No daily act."))) %>% 
# listwise delete the NAs (annoying)
dplyr::filter(!is.na(DailyAct))


######### Extract just the cases needed! ############
link.may50 %>% dplyr::count(DISCA13_AGE>=50)
link.may50 %>% dplyr::count(DISCA13_AGE<999)
link.may50 %>% dplyr::count(DISCA13_AGE>=50 & DISCA13_AGE<999)

link.may50 <- link.may50 %>% dplyr::filter(DISCA13_AGE>=50) %>% dplyr::filter(DISCA13_AGE<110)

######### Extract just the cases needed! ############

# -----------------------------------------------
# save(link.may50, file='010_mayor50.link.RData')
# -----------------------------------------------


################################################
################################################
################################################
################################################
################################################
################################################   From here on are only the cases 65 + !!!
################################################
################################################
################################################
################################################
################################################




################################################
# Data of interest is hidden in another source #
# ---------------------------------------------#
################################################

EDAD.mayor = read.spss("TOTAL-EDAD-2008-Mayores.sav", to.data.frame=TRUE)


# There is information on 44 possible disabilities - DISCA13 contains 13 "activities" (5 IADL + 8 ADL)
str(EDAD.mayor$EdadInicioDisca13)
# str(EDAD.mayor$EdadInicioDisca44)
# And the year of onset of dependency
str(EDAD.mayor$Edadinicio_cuidado)

# see differences between different entry ages (disabilities 13-19-44)
par(mfrow=c(1,3))
hist(as.numeric(EDAD.mayor$EdadInicioDisca44))
hist(as.numeric(EDAD.mayor$EdadInicioDisca19))
hist(as.numeric(EDAD.mayor$EdadInicioDisca13))
par(mfrow=c(1,1))

# A few checks with variables - to get a feeling for the number of transitions
EDAD.mayor <- data.table(EDAD.mayor)
EDAD.mayor[, .N, .(!is.na(EdadInicioDisca13))]
EDAD.mayor[, .N, .(!is.na(Edadinicio_cuidado))]
EDAD.mayor[, .N, .(as.numeric(EdadInicioDisca13)<as.numeric(Edadinicio_cuidado))] 

# Dependientes13, Dependientes13 Dependientes14 AsistenciaPersonal14
# DemandaCuidados14 EdadInicioDisca44 EdadUltimaDisca44

# select variables
head(names(EDAD.mayor), 1000)
tail(names(EDAD.mayor), 404)
# change variable names for merge process
names(EDAD.mayor)[6:10]<- c("nseccion", "dc", "viv", "hog", "nord")

# Select variables from the SPSS data set (working file Antonio and Julio)
merge(EDAD.mayor[,c(1,4:29, 53:76, 89:92, 94:104, 108, 114, 115, 120, 123, 125, 128, 133, 136, 141, 142, 147, 148, 153,
                    154, 156, 166, 171, 172, 174, 176, 178, 180, 185, 186, 191, 192, 197, 198, 203, 204, 209, 210, 215,
                    216, 221, 222, 227, 228, 233, 234, 239, 240, 245, 246, 251, 252, 257, 258, 263, 264, 269, 270, 275,
                    276, 281, 282, 287, 288, 293, 294, 299, 300, 305, 306, 311, 312, 317, 318, 320, 322, 324, 326, 328,
                    330, 332, 334, 336, 338, 340, 342:399, 407, 408, 410, 414, 455, 465, 470, 480, 485, 490, 495, 500,
                    505, 510, 515, 517, 522, 527:534, 552, 553:588, 601, 616:646, 648:656, 661, 672, 677:706, 711, 716,
                    717:738, 740, 741, 754, 758:763, 771, 772, 774, 778:780, 796, 801:836, 892:898, 900:918, 952:964, 977:990,
                    1085, 1086, 1093, 1094, 1097:1118, 1231,1250, 1277:1321, 1326, 1381:1404), with=F],
      follow.up, by=c('nseccion','dc','viv','hog','nord'), all=T) -> link.may

class(link.may)
link.may[,enlazado:=!is.na(estado)]
link.may[,.N,.(enlazado,estado)]    
# from the 6568 who are not linked, 2.127 are deaths which occurred in 2017 
# "El resto son, bien emigraciones, bien limpiezas del padrón o bien otras causas. 
# Los otros 469 (=6.963-6.494) son registros para los que tendríamos que analizar la casuística 
# (a veces han podido modificarse los identificadores por distintas razones a lo largo de los años

# now reduce to the ones which are in the original data (65+)
table(link.may$enlazado)


# Choose the linked ones and the ones with age information
link.may <- link.may %>% filter(!is.na(EDAD)) %>% filter(enlazado==T) %>%  ### 38985 individuals (65 +, followed up)
  # and make an event variable with the sure information we have (death year and month)
  mutate(event=ifelse(!is.na(anodef),1,0))
# !This will censor everybody after 31.12.2016 - even if some information is available

# time to death or censor var                                        mutate(age.d = EDAD + (2008.5 - (a_salida)))
link.may <- data.table(link.may)
link.may[estado=='B',.N ,keyby=.(Con.ANODEF=(!is.na(anodef)),  con.CAUSA = (!is.na(causa)))]
## 1783 (11.74%) def sin causa y fecha
# Impute a death date for the ones without assigned date but information on state at exit ("B")
summary(link.may$a_salida) # muy pocos! only the ones who left?

# Check the "bajas"
link.may %>% count(estado=='B' & is.na(anodef) & is.na(a_salida)) # 1689 bajas don´t have a year of death or exit from the survey
link.may %>% count(estado=='B' & !is.na(a_salida))
link.may %>% count(estado=='B' & !is.na(a_salida) & is.na(causa)) # 94 have a year of exit but no cause (death, censor)

# See if that is in line with the INE email
link.may %>% count(event==1 & is.na(a_salida)) # Año de salida does not coincide with death year
link.may %>% count(event==1 & is.na(causa)) # 0 cases = which encourages one to censor at 12/2016


#########################################################################################################################
#########################################################################################################################
### For now this is out commented as have no further information on the bajas in 2017
#########################################################################################################################
#########################################################################################################################
### all possible ways I can think of over the top the head to impute the date from one of the two situations
### ---------------------------------------------------------------------------------------------------------
# 
# # If there is neither a death date nor an exit date but the "baja" information, we approximate by the medium time
# link.may[estado=='B' & is.na(anodef) & is.na(a_salida), ':='(anodef=2013, mesdef=1, a_salida=2013, m_salida=1)]
# # If there is a death date this will become the exit date (absorbing state)
# link.may[estado=='B' & !is.na(anodef) & !is.na(causa), ':='(a_salida=anodef, m_salida=mesdef)]
# # If there is a age at death information but no exit year
# link.may[estado=='B' & !is.na(a_salida), ':='(anodef=a_salida, mesdef=m_salida)] #  ??? - !is.na(causa)

#  link.may %>% count(estado=='B')
# link.may %>% count(estado=='B' & !is.na(a_salida)) # ok! Same number (assuming everyone has an exit year)
# # Assign a censoring date for censored cases
# # for now: last month of 2017 where event happened
# max(link.may$m_salida[link.may$estado=='B' & link.may$a_salida==2017]) # Last month = May

#########################################################################################################################
#########################################################################################################################

# For now the analysis is censored at 31.12.2016

# Censored are all individuals without event before 31.12.2016

link.may[event==0, ':='(a_salida=2016, m_salida=12)]
# for the rest it is the year/month of death information
link.may[event==1, ':='(a_salida=anodef, m_salida=mesdef)]

# Quick check! Looks okay!
summary(link.may$a_salida)


# double-check age at different states #
# ------------------------------------ #

# ------------------------ #
# Entry age in 2008 (EDAD) #
# ------------------------ #

summary(link.may$EDAD)
hist(link.may$EDAD, nclass = 42, main = "", xlab = "Age")  # age 65 years seem to be overrepresented (also more 66)


### ------------
### AGE AT EXIT
### ------------
# some deaths are early in 2007 and lead to entry age = exit age problem)

link.may <- link.may %>% 
  # first making the exit age smoother by adding the month and than substracting the first interview date (! find variable)
  mutate(age.ex = EDAD + (a_salida+(m_salida/12) - 2006.99))

hist(link.may$age.ex) # looks ok!

# Look at the entry to exit age relationship - no cases with lower
link.may %>% count(EDAD < age.ex)   


# check the other ages - entry to dependency
# ------------------------------------------
str(link.may$Edadinicio_cuidado)
link.may$Edadinicio_cuidado <- as.numeric(link.may$Edadinicio_cuidado)
summary(link.may$Edadinicio_cuidado)

link.may %>% count(Edadinicio_cuidado>=50)


# --------------------------------------------
# save(link.may, file='010_mayor.link.RData')
# --------------------------------------------

