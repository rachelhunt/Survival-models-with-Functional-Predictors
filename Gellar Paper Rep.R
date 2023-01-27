# Gellar paper replication - contd. 25/01/23
library(ggplot2)
library(dplyr)
library(mgcv)
library(refund)

load("/Users/rachelhunt/Desktop/Research/Gellar Paper/pcox/data/sofa.rdata")
load("/Users/rachelhunt/Desktop/Research/Gellar Paper/pcox/data/sofa_fu.rdata")
summary(sofa)
dim(sofa)

summary(sofa_fu)
dim(sofa_fu)
?refund

refund$sofa


# load("/Users/rachelhunt/Desktop/pcox/data/sofa_fu.rdata")



#fit_lfcm_pfr <- pfr(Time ∼ Age + BMI + Education + lf(MIMS, bs="cc", k=30), 
#                    weights=Event, data=data analysis, family=cox.ph())
