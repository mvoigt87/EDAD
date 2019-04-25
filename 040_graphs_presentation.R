### Graphs for PAA presentation ###
### --------------------------- ###

# 0.1 packages

library(tidyverse)
library(data.table)
library(foreign)
library(survival)
library(broom)
library(stargazer)
# Use for a parametric model
library(flexsurv)
# for the cool plots
library(survminer)
# for checking for different distributions
library(fitdistrplus)

# 0.3 Set right working directory
dir()
setwd("C:/Users/y4956294S/Documents/LONGPOP/Subproject 2 - SE differences in transition to dependency/R code/EDAD")




### Prepare the input data
hazard  <- c(1.021,1.0291,0.997,0.9887,1.228,1.146)
lower <- c(1.0157,1.0255 ,0.9909,0.9844,1.0928,1.0434)
upper <- c(1.026,1.0327,1.004,0.9931,1.379, 1.2583)
# color full and simple model differently
col.dev <- c("M","F","M","F","M","F")


### Another attempt with groups - for publication
#   (https://datascienceplus.com/lattice-like-forest-plot-using-ggplot2-in-r/)

# Changing the labels
label <- c("Severity\nScore", "Severity\nScore", "Onset\nAge" , "Onset\nAge",
           "Daily\nActivity", "Daily\nActivity")
df <- data.frame(label, hazard, lower, upper, col.dev)

### ----------------------------------------------------------------------------------------------------- ###
fp = ggplot(data=df,
            aes(x = col.dev,y = hazard, ymin = lower, ymax = upper))+
  geom_pointrange(aes(col=col.dev, shape=col.dev))+
  geom_hline(aes(fill=col.dev), yintercept =1, linetype=2)+
  xlab(' ')+ ylab(" ")+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=col.dev),width=0.25,cex=0.5)+ 
  scale_color_manual(values=c("#000000", "#D55E00"), labels=c("Female", "Male"), name=" ") +
  scale_fill_discrete(name = " ",guide=FALSE) +
  facet_wrap(~label,strip.position="left",nrow=9,scales = "free_y") +
  theme_bw() +
  # theme(plot.title=element_text(size=12),
  #       axis.text.y=element_blank(),
  #       axis.ticks.y=element_blank(),
  #       axis.text.x=element_text(face="bold"),
  #       axis.title=element_text(size=12),
  #       strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
  coord_flip()
# fp <- fp + theme(legend.position = c(0.85, 0.9), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
#   scale_shape_discrete(guide=FALSE)

# without legend
fp + theme(legend.position="none") + theme(axis.text=element_text(size=12),
                                           axis.title=element_text(size=14,face="bold"), strip.text.y = element_text(size=12, face="bold"),
                                           strip.background=element_rect(fill="white"))

### ------------------------------------------------------------------------------------------------------------------ ###

### Plot with onset ages and severity score

SP <- link.may50 %>% mutate(event = as.factor(event)) %>% ggplot(aes(x=DISCA13_AGE,y=SEV_COUNT, color=event)) +
  #geom_bin2d()
   geom_density2d() +   #geom_jitter() + 
                     xlab("Age at Disability Onset")+ ylab("Severity Score")+
                     scale_color_manual(values=c("#000000", "#D55E00"), labels=c("Survivor", "Deceased"), name=" ") +
                     theme_bw()

SP + theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"), strip.text.y = element_text(size=12, face="bold"))



### Plot connection - Severity and Disca Onset

SD <- link.may50 %>% dplyr::filter(SEV_COUNT>0) %>% mutate(event = as.factor(event)) %>% 
  ggplot(aes(x=DISCA13_AGEGR, fill=SEVEREDIS)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(name = "Relative Frequency", labels = scales::percent) +
  scale_x_discrete(name = "Onset Disability (Age Groups)") +
  scale_fill_manual(name = "", values=c("#0072B2", "#D55E00", "#009E73")) +
  theme_bw()

SD + theme(axis.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold"), strip.text.y = element_text(size=12, face="bold"))
