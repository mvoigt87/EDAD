### Cluster Analysis - Discas ###

## 1. Explore sequences for males and females separately

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

### load data

load("020_traMay.RData")
load("020_traMay_C.RData")




### 1.1 - Create separate datasets for men and women (literature: different timings and disability occurrences)
###

tra_may_M <- tra_may_C %>% filter(SEXO=="Varón") %>% mutate(SEXO="Male")

tra_may_F <- tra_may_C %>% filter(SEXO=="Mujer") %>% mutate(SEXO="Female")


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
Brewer_C <- brewer.pal(3, "Dark2")


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
par(mfrow=c(1,3))
seqdplot(DisSeq_M, with.legend = FALSE)
seqdplot(DisSeq_F, with.legend = FALSE)
seqlegend(DisSeq_M)

### 1.4.3 state distribution table as numerical counterpart for the d-plot
seqstatd(DisSeq_M)
seqstatd(DisSeq_F)

### 1.4.4 - Transition Rates

# males
round(seqtrate(DisSeq_M),2)

# females
round(seqtrate(DisSeq_F),2)

## 1.4.5 Turbulence as discrepancy measure
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

## 2.2 Apply Optimal Matching (OM) to estimate the sequence changes

# males (OM)
dismat_M <- seqdist(DisSeq_M, method = "OM", sm = submat_M)
dismat_M[1:10,1:10]

# females (OM)
dismat_F <- seqdist(DisSeq_F, method = "OM", sm = submat_F)
dismat_F[1:10,1:10]

# 2.2.b - OMslen - Optimal Matching taking sequence length into account

# males (OMslen - spell-length sensitive)
dismat_M2 <- seqdist(DisSeq_M, method = "OMslen", sm = submat_M)
dismat_M2[1:10,1:10]

# females (OMslen - spell-length sensitive)
dismat_F2 <- seqdist(DisSeq_F, method = "OMslen", sm = submat_F)
dismat_F2[1:10,1:10]

## 2.3 Cluster matching and dendograms for optimal group size
## ----------------------------------------------------------

# 2.4 Build the clusters with the agnes function (cluster package)
# ---------------------------------------------------------------

# command provides computes a agglomorative hieracrchical clustering
# option "ward" refers to the Ward´s method! - Other option "weigthed" which uses weighted averages

clusterw_M <- agnes(dismat_M, diss = TRUE, method = "ward")
clusterw_M2 <- agnes(dismat_M2, diss = TRUE, method = "ward")

clusterw_F <- agnes(dismat_F, diss = TRUE, method = "ward")
clusterw_F2 <- agnes(dismat_F2, diss = TRUE, method = "ward")


# 2.4.2 Plotting a dendrogram
par(mfrow=c(1,1))
plot(clusterw_M, which.plots = 2)  # 2 or 4
plot(clusterw_M2, which.plots = 2) # 2 or 4

plot(clusterw_F, which.plots = 2)  # 3 or 6
plot(clusterw_F2, which.plots = 2) # 3 or 5

# 2.4.3 Group sizes

# cutree command (2/3 groups)

cluster2M <- cutree(clusterw_M, k = 2)
cluster2M2 <- cutree(clusterw_M2, k = 2)
cluster3M2 <- cutree(clusterw_M2, k = 3)

cluster3F <- cutree(clusterw_F, k = 3)
cluster3F2 <- cutree(clusterw_F2, k = 3)

# Create three factors for grouping (for now without descriptive name)
cluster2M <- factor(cluster2M, labels = c("Type 1", "Type 2"))
table(cluster2M)
cluster2M2 <- factor(cluster2M2, labels = c("Type 1", "Type 2"))
table(cluster2M2)
cluster3M2 <- factor(cluster3M2, labels = c("Type 1", "Type 2", "Type 3"))
table(cluster3M2)

cluster3F <- factor(cluster3F, labels = c("Type 1", "Type 2", "Type 3"))
table(cluster3F)
cluster3F2 <- factor(cluster3F2, labels = c("Type 1", "Type 2", "Type 3"))
table(cluster3F2)


# 2.5 Plotting the group characteristics 

# 2.5.1 Examples for group members (Sequence Frequency Plot)

# Change F to T for proportional size of the bars to their frequencies

# males 
seqfplot(DisSeq_M, group = cluster2M, pbarw = F)
seqfplot(DisSeq_M, group = cluster2M2, pbarw = F)
seqfplot(DisSeq_M, group = cluster3M2, pbarw = F)

# females
seqfplot(DisSeq_F, group = cluster3F, pbarw = F)
seqfplot(DisSeq_F, group = cluster3F2, pbarw = F)

# 2.5.2 - mean time spent in each state by cluster

seqmtplot(DisSeq_M, group = cluster2M)
seqmtplot(DisSeq_M, group = cluster3M2)

seqmtplot(DisSeq_F, group = cluster3F)
seqmtplot(DisSeq_F, group = cluster3F2)
