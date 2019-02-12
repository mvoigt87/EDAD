
### Example code for sequence analysis

# packages
library(TraMineR)
library(reshape)
library("RColorBrewer")
library(cluster)

## Step 0 - Bring the data in the right shape (dataframe/matrix for every time point/sting/ other things that I couldn´t figure out how to do)


## 1. Define an alphabet for all states any individual can obtain
## --------------------------------------------------------------

# 1 a) Alphabet 1 (5 states) - an example from the disability paper

# DF = Disability Free
# ID = Idenpendent albeit first disability
# ADL = Problems ADL - Help with Housework combined with "needs assistance" (NA) 
# DC = Dependent on Caretaker
# C = Censored

SeqAlphab_1 <- c("DF","ID", "ADL", "DC", "C")

# 1 b) Define color scheme (if you want)
display.brewer.all()
Brewer_1 <- brewer.pal(5, "Dark2")


### 2. Create a matrix only containing the sequence information
### ------------------------------------------------------------

  # tra_may_M is my dataframe

seqmat_M <- tra_may_M[,c(14:64)]


### 2 b) From there you create a sequence object (?seqdef)

DisSeq_M <- seqdef(seqmat_M,informat = "STS", alphabet = SeqAlphab_1, id="auto", cpal =  Brewer_1 ,
                   labels = c("Disability Free","Idependent","ADL","Care","Censored"))

### 2 c) Exploratory Plots (examples)

    # sorted sequences
    par(mfrow=c(1,2))
    seqplot(DisSeq_M,type = "I", with.legend = FALSE, sort="from.end")
    seqlegend(DisSeq_M) # the way they handle the legend is annoying
    
    ### d- plot - cummulated state plot
    par(mfrow=c(1,2))
    seqdplot(DisSeq_M, with.legend = FALSE)
    seqlegend(DisSeq_M)
    
    ### f- plot - cummulated frequencies of sequences
    seqfplot(DisSeq_M)

### 2 d)  Now the numbers
    
    ### state distribution table
    seqstatd(DisSeq_M)
    
    ### Sequence Frequencies
    seqtab(DisSeq_M)
    
    ### Transition Rate !
    round(seqtrate(DisSeq_M),2)
    
    ### Turbulence (thats a "complexity measure - have not come around to understand it yet)
    hist(seqST(DisSeq_M))

    
    
#### 3. Now the good part: Clusters
#### ------------------------------    
    
    
### 3 a) Substitutions cost matrix for Optimal Matching Algorithm (paper attached!) 

    # use sequence object (in my case "DisSeq_M")
    
    submat <- seqsubm(DisSeq_M, method = "CONSTANT", cval = 2)

### 3 b) Compute distances (= differences between two sequences) 
    
    # original statistical process is called "optimal matching" => but there seem to be better techniques
    
    dismat_M <- seqdist(DisSeq_M, method = "OM", sm = submat)
    dismat_M[1:10,1:10]  
    
    # ! One example for a technique that takes sequence length into account is "OMslen" the spell-length sensitive
    dismat_M2 <- seqdist(DisSeq_M, method = "OMslen", sm = submat_M)
    dismat_M2[1:10,1:10]

    # Rainer recommended this one and a handful of others (see slides)

### 3 c) Building the clusters with "agnes" from the "cluster" package
    
    # command provides computes a agglomorative hieracrchical clustering
    # option "ward" refers to the Ward´s method! - Other option "weigthed" which uses weighted averages
    
    clusterw <- agnes(dismat_M, diss = TRUE, method = "ward")
    
    # Plotting a dendrogram - will show a tree diagram and gives you an idea about optimal group number (cut off at a level)
    plot(clusterw, which.plots = 2)
    
### 3 d) Try to cluster sequences in 3 groups (just as an example)

    # cutree command (3 groups)
    cluster3 <- cutree(clusterw, k = 3)
    
    # Create three factors for grouping (for now without descriptive name)
    cluster3 <- factor(cluster3, labels = c("Type 1", "Type 2", "Type 3"))
    table(cluster3)
    
### 3 e) And now the real stuff! What do the clusters look like
    
    seqfplot(DisSeq, group = cluster3, pbarw = T)
    
    # mean time spent in each state by cluster
    
    seqmtplot(DisSeq, group = cluster3)
    

### 4 - When I find out how to use the groups classifications for other models, I will let you know :)