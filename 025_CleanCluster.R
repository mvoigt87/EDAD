### Cluster Analysis - Discas ###

## 1. Explore sequences for males and females separately

    # ! To compare different onset variables (i.e. Disca 44 and IADL) change the dataset in line 48 and 50!

## 2. Compare different clustering techniques -  find optimal group size

## 3. Group trajectories

# set working directory
dir()
setwd("C:/Users/y4956294S/Documents/LONGPOP/Subproject 2 - SE differences in transition to dependency/R code/EDAD")

set.seed(17952)

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
library(WeightedCluster)

### load data

load("020_traMay.RData")
# censored data
load("020_traMay_C.RData")
# censored data with age of onset of a 12 A (ADL group A) disability as transition point from disability free to idependent dis
load("020_traMay_12A.RData")



### 1.1 - Create separate datasets for men and women (literature: different timings and disability occurrences)


###########################################################
### A) CHANGE DATA SET DEPENDING ON NEEDS !!!!!!!!!!!!!!!!!
###########################################################
tra_may_M <- tra_may_C %>% filter(SEXO=="Varón") %>% mutate(SEXO="Male")

tra_may_F <- tra_may_C %>% filter(SEXO=="Mujer") %>% mutate(SEXO="Female")


###########################################################
### B) CHANGE HIGHEST AGE DEPENDING ON NEEDS !!!!!!!!!!!!!! 
###########################################################

#### This next code piece excludes 2 females which potentially mess up the cluster plots

# males
tra_may_M <- tra_may_M %>% filter(Edadinicio_cuidado<100)
# females
tra_may_F <- tra_may_F %>% filter(Edadinicio_cuidado<100)

### 1.2. Create a matrix for the sequence analysis
### ---------------------------------------------

seqmat_M <- tra_may_M[,c(14:64)]

seqmat_F <- tra_may_F[,c(14:64)]

### 1.3 seqdef to create an object for TraMineR
### ---------------------------------------

?seqdef

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# 1.3.1 Requirement of TraMineR to define the alphabet and colors

SeqAlphab_C <- c("DF","ID", "DC")

# Define color scheme
Brewer_C <- brewer.pal(3, "Set1")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# Male
DisSeq_M <- seqdef(seqmat_M,informat = "STS", alphabet = SeqAlphab_C, id="auto", cpal =  Brewer_C ,start = 55, 
                   labels = c("Disability Free","Idependent","Care"))

summary(DisSeq_M) # 1495 cases

# Females
DisSeq_F <- seqdef(seqmat_F,informat = "STS", alphabet = SeqAlphab_C, id="auto", cpal =  Brewer_C ,start = 55, 
                   labels = c("Disability Free","Idependent","Care"))

summary(DisSeq_F) # 3266 cases

### 1.4 Descriptive Overview ###
### ------------------------ ###

# 1.4.1 Sorted sequences for both sexes
par(mfrow=c(1,2))
seqplot(DisSeq_M,type = "I", with.legend = FALSE, sort="from.end")
seqplot(DisSeq_F,type = "I", with.legend = FALSE, sort="from.end")

### 1.4.2 d- plot - cummulated state plot   - Very interesting relative frequency of states!

# State distribution plot (type="d") represent the sequence of the cross-sectional state frequencies by position (time point) 
# computed by the seqstatd function. Such plots are also known as chronograms

par(mfrow=c(1,3))
seqdplot(DisSeq_M, with.legend = FALSE)
seqdplot(DisSeq_F, with.legend = FALSE)
seqlegend(DisSeq_M)

### 1.4.3 state distribution table as numerical counterpart for the d-plot
seqstatd(DisSeq_M)
seqstatd(DisSeq_F)

### 1.4.4 Modal State Sequence Plots by Age

seqmsplot(DisSeq_M)
seqmsplot(DisSeq_F)

### 1.4.5 - Transition Rates

# males
round(seqtrate(DisSeq_M),2)

# females
round(seqtrate(DisSeq_F),2)

## 1.4.6 Turbulence as discrepancy measure
par(mfrow=c(1,2))
# males
hist(seqST(DisSeq_M))
# females
hist(seqST(DisSeq_F))

########################################################################################################################

#### 2. Clustering Techniques

## 2.1. Matrix of substitution costs

# males
submat_M <- seqsubm(DisSeq_M, method = "CONSTANT", cval = 2)

# females
submat_F <- seqsubm(DisSeq_F, method = "CONSTANT", cval = 2)

## 2.2 Change METHOD accordingly 
  # 1. First we applied Optimal Matching (OM) to estimate the sequence changes
  # 2. OMslen - Optimal Matching taking sequence length into account             --- currently used!

# males (OM)
dismat_M <- seqdist(DisSeq_M, method = "OMslen", sm = submat_M)
dismat_M[1:10,1:10]

# females (OM)
dismat_F <- seqdist(DisSeq_F, method = "OMslen", sm = submat_F)
dismat_F[1:10,1:10]

# 2.3 Build the clusters with the agnes function (cluster package)
# ---------------------------------------------------------------

# command provides computes a agglomorative hieracrchical clustering
#
# Option "ward" refers to the Ward´s method! = Minimizes the residual variance (weighted)
# Other option "weigthed" which uses weighted averages

clusterw_M <- agnes(dismat_M, diss = TRUE, method = "ward")
clusterw_F <- agnes(dismat_F, diss = TRUE, method = "ward")


# 2.3.2 Plotting a dendrogram
par(mfrow=c(1,1))
plot(clusterw_M, which.plots = 2)  # 2 or 4
plot(clusterw_F, which.plots = 2)  # 3 or 6

# 2.3.3 Assessing optimal number of clusters by Hierarchical Clustering (see Studer 2013)

# summary command gives values for various statistical tests (apparently preferable = ASW) - needs to be doublechecked

# males
data.clust_M <- wcKMedRange(dismat_M, kvals=2:20)
summary(data.clust_M, max.rank = 2)
plot(data.clust_M)

# females
data.clust_F <- wcKMedRange(dismat_F, kvals=2:20)
summary(data.clust_F, max.rank = 2)
plot(data.clust_F)


# 2.3.4 Group sizes
# ..................

# 2 different options based on the statistical test above

# cutree command (2/3 groups)     ------------------- !!! Here be careful to use the right files

clusterM <- cutree(clusterw_M, k = 2)
clusterM2 <- cutree(clusterw_M, k = 4)

clusterF <- cutree(clusterw_F, k = 2)
clusterF2 <- cutree(clusterw_F, k = 3)

# Create three factors for grouping (for now without descriptive name)
clusterM <- factor(clusterM, labels = c("Type 1", "Type 2"))
table(clusterM)
clusterM2 <- factor(clusterM2, labels = c("Type 1", "Type 2", "Type 3", "Type 4"))
table(clusterM2)


clusterF <- factor(clusterF, labels = c("Type 1", "Type 2"))
table(clusterF)
clusterF2 <- factor(clusterF2, labels = c("Type 1", "Type 2", "Type 3"))
table(clusterF2)


# 2.5 Plotting the group characteristics
# -------------------------------------- #

# 2.5.1 Examples for group members (Sequence Frequency Plot)

# Change F to T for proportional size of the bars to their frequencies

# males 
seqfplot(DisSeq_M, group = clusterM, pbarw = F)
seqfplot(DisSeq_M, group = clusterM2, pbarw = F)

# females
seqfplot(DisSeq_F, group = clusterF, pbarw = F)
seqfplot(DisSeq_F, group = clusterF2, pbarw = F)

# 2.5.2 - mean time spent in each state by cluster

# males
seqmtplot(DisSeq_M, group = clusterM)
seqmtplot(DisSeq_M, group = clusterM2)

# females
seqmtplot(DisSeq_F, group = clusterF)
seqmtplot(DisSeq_F, group = clusterF2)


# 2.6 Put the pieces together - matching the cluster #
# -------------------------------------------------- #

# Values are hard to interpret = actually they are „medoids“, the representative sequences for each of these groups

####### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ########
####### !!! Change the number after "clustering$cluster" depending on the optimal group/cluster size ########
####### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ########

# males 
# -----

# 2 groups (OMslen object)
tra_may_M$clusters2 <- data.clust_M$clustering$cluster2

# 4 groups (OMslen object)
tra_may_M$clusters4 <- data.clust_M$clustering$cluster4

# females 
# -------

# 2 groups (OMslen object)
tra_may_F$clusters2 <- data.clust_F$clustering$cluster2

# 4 groups (OMslen object)
tra_may_F$clusters3 <- data.clust_F$clustering$cluster3


# 2.7 Graphical check for the cluster
# -----------------------------------

####### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ########
####### !!! Change the number after "clustering$cluster" depending on the optimal group/cluster size ########
####### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ########

# males 2 and 4 groups
seqdplot(DisSeq_M, group=tra_may_M$clusters2)
seqdplot(DisSeq_M, group=tra_may_M$clusters4)

# females 2 and 3 groups
seqdplot(DisSeq_F, group=tra_may_F$clusters2)
seqdplot(DisSeq_F, group=tra_may_F$clusters3)


# 3. Calculate medium age at death per cluster
##############################################

## 3.1. link the cluster information back to the original dataset (and find meaningful descriptions)

####### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ########
####### !!! Change the number after "clustering$cluster" depending on the optimal group/cluster size ########
####### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ########

# males
tra_may_M <- tra_may_M %>% select(Id, clusters2, clusters4) %>% 
              mutate(cluster2 = ifelse(clusters2==177, "late onset", "early onset")) %>% 
              mutate(cluster4 = ifelse(clusters4==143, "late gradual", ifelse(clusters4==1472, "early gradual",
                                 ifelse(clusters4==256, "late healthy", "late abrupt")))) %>% 
              select(-clusters4, - clusters2)

# females
tra_may_F <- tra_may_F %>% select(Id, clusters2, clusters3) %>%
             mutate(cluster2 = ifelse(clusters2==1844, "late onset short", "early onset long")) %>%
             mutate(cluster3 = ifelse(clusters3==1214, "early abrupt", 
                    ifelse(clusters3==1426,"late abrupt", "early gradual"))) %>% 
             select(-clusters2, -clusters3)

### For the A12 disabilities

# males
tra_may_M_A12 <- tra_may_M %>% select(Id, clusters2, clusters3) %>% 
  mutate(cluster2 = ifelse(clusters2==177, "late onset", "early onset")) %>% 
  mutate(cluster3 = ifelse(clusters3==345, "late gradual", ifelse(clusters3==52, "late abrupt", "early gradual"))) %>% 
  select(-clusters3, - clusters2)

# females
tra_may_F_A12 <- tra_may_F %>% select(Id, clusters2, clusters5) %>%
  mutate(cluster2 = ifelse(clusters2==281, "late onset short", "early onset long")) %>%
  mutate(cluster5 = ifelse(clusters5==1852, "middle gradual",ifelse(clusters5==3001,"middle abrupt", 
                                                                    ifelse(clusters5==3018,"early gradual",
                                                                        ifelse(clusters5==56,"late abrupt", "late gradual"))))) %>% 
  select(-clusters2, -clusters5)
## 3.2 order data set for linkage to big data set

# males
tra_may_M <- tra_may_M[order(tra_may_M$Id),]

# males
tra_may_F <- tra_may_F[order(tra_may_F$Id),]

## 3.3 Link back to big data set!

# order big data + extract the two women with late onset of anything

### Depending on the dataset, change the filters
# --------------------------------------------


load(file='010_mayor.link.RData')

link.may <- link.may %>% filter(Edadinicio_cuidado<100) #%>% filter(!is.na(edadiniciodisca12A))

# make 2 data sets out of them (by sex) and order them

link.may_M <- link.may %>% filter(Sex=="Male")
link.may_F <- link.may %>% filter(Sex=="Female")


link.may_M <- link.may_M[order(link.may_M$Id),]
link.may_F <- link.may_F[order(link.may_F$Id),]

## 3.4  Create working data sets for the mortality analysis
link.may_M <- link.may_M %>% left_join(tra_may_M, by="Id")
link.may_F <- link.may_F %>% left_join(tra_may_F, by="Id")


### 3.5 save data

# save(link.may_M, file='030_linkmay_M.RData')
# save(link.may_F, file='030_linkmay_F.RData')
