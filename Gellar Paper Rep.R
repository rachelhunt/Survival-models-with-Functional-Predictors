# Gellar paper replication - contd. 25/01/23
# Code: Rachel Hunt

# Title: Cox Regression Models with Functional Covariates for Survival Data (2015)
# Authors: Jonathan E. Gellar, Elizabeth Colantuoni, 
# Dale M. Needham, and Ciprian M. Crainiceanu
# Year: 2015

# Can't use their pcox package as 'didnt make it to CRAN, superseded by 
# developments in the refund and mgcv package. 
# Instead, will use template from Erjia Cui (Ciprian's PhD student),
# to create an Additive Functional Cox Model (AFCM), 
# similar to the penalised cox PH model.
# https://github.com/ecui1/AFCM/blob/main/vignette_fcm.pdf

library(ggplot2)
library(dplyr)
library(mgcv)
library(refund)

load("/Users/rachelhunt/Desktop/Research/Gellar Paper/pcox/data/sofa.rdata")
load("/Users/rachelhunt/Desktop/Research/Gellar Paper/pcox/data/sofa_fu.rdata")
summary(sofa)
dim(sofa) #520 x 7

summary(sofa_fu)
dim(sofa_fu) # 267 x 9 (added 'event' and 'time')
names(sofa_fu)
#?refund
#refund$sofa

##  Get information using ??sofa
# 520 subjects
## event: death / event
## time: los (length of stay) / time
## functional predictor: SOFA (520 x 173 matrix) 
# (Missing values during one's ICU stay have been imputed using LOCF.)
# raw version without imputing: SOFA_raw
# in two separate files in our two loaded datasets above
## Scalar predictors: age, male, Charlson 
# (tried initially with untransformed SOFA, received error of:
# Not enough (non-NA) data to do anything meaningful)
# ds = domain standardised
# 

# --------------------------Using Erjia's code template to replicate ---------------------

## simulate a dataset
set.seed(2023)
n <- 267 ## number of subjects
s <- 157 ## number of functional observations per subject
event <- sofa_fu$event ## 30% of subjects have events observed 
survtime <- sofa_fu$time ## observed time

## transformations on X may be necessary for identifiability in practice 
X <- sofa_fu$SOFA_ds
#X <- fpca.face(X)$Yhat ## smooth each curve using fast sandwich smoother (try without for now)
Z <- sofa_fu$Charlson ## a scalar predictor (can add in age and male)
data_analysis <- data.frame(event, survtime, Z, X = I(X)) ## the dataset 
rm(event, survtime, X, Z) # removing them from the environment,  ?rm
str(data_analysis)

## create variables related to numerical approximation
### lmat: numerical integration
data_analysis$lmat <- I(matrix(1/s, ncol=s, nrow=nrow(data_analysis)))
### tmat: time indices of functional observations, we assume an equally-spaced grid here 
data_analysis$tmat <- I(matrix(seq(0, 1, len=s), ncol=s, nrow=nrow(data_analysis), byrow=TRUE))

## fit LFCM
fit_lfcm <- gam(survtime ~ Z + s(tmat, by=lmat*X, bs="cr", k=10), weights=event,
                data=data_analysis, family=cox.ph())
# (no error with sofa_fu and SOFA_ds)

## fit AFCM
fit_afcm <- gam(survtime ~ Z + ti(tmat, X, by=lmat, bs=c("cr","cr"), k=c(10,10),
                                  mc=c(FALSE,TRUE)), weights=event, data=data_analysis,
                family=cox.ph())
# (no error with sofa_fu and SOFA_ds)

## visualize the estimates
par(mfrow = c(1,2))
vis.gam(fit_lfcm, view = c("tmat", "X"), plot.type = "contour", color = "cm",
        main = "Estimated Surface from LFCM")
vis.gam(fit_afcm, view = c("tmat", "X"), plot.type = "contour", color = "cm",
        main = "Estimated Surface from AFCM")


# -------------------------- Code Line from email from Ciprian ---------------------

## Line from email from Ciprian:
#fit_lfcm_pfr <- pfr(Time ∼ Age + BMI + Education + lf(MIMS, bs="cc", k=30), 
#                    weights=Event, data=data analysis, family=cox.ph())

# ?pfr from refund package
?pfr
?lf
?te
?s

# -------------------------- SOFA Dataset - Following nhanes appl. --------------------
# Add in subject specific transformation*

# here, using quantile transformation first from paper*

event <- sofa_fu$event ## or los (length of stay)
survtime <- sofa_fu$time ## observed time
# death
Z <- sofa_fu$Charlson ## a scalar predictor (can add in age and male)

str(sofa_fu)
table(sofa_fu$event)
dim(sofa_fu$SOFA_ds)

## obtain smoothing
sofa_fu$SOFA_ds_sm   <- I(fpca.face(unclass(sofa_fu$SOFA_ds))$Yhat)

## obtain quantile-transformed smoothed
sofa_fu$SOFA_ds_sm_q <- I(apply(sofa_fu$SOFA_ds_sm, 2, function(y) ecdf(y)(y)))

## From gellar paper: specific domain transformation s := gi(u) = u/Ui
# New sofa functions that are each defined over [0, 1], Xi(s) = Xi(sUi)
# carry out LOCF too ? 
dim(sofa_fu$SOFA_ds)

### lmat: numerical integration
sofa_fu$lmat <- I(matrix(1/157, ncol=157, nrow=nrow(sofa_fu)))
### tmat: time indices, [0, 1] in our case, but effectively arbitrary
sofa_fu$tmat <- I(matrix(1:157, ncol=157, nrow=nrow(sofa_fu), byrow=TRUE))

sofa_fu$male <- as.factor(sofa_fu$male)
## fit LFCM
fit_lfcm <- gam(time ~ age + Charlson + male +
                  s(tmat, by=lmat*SOFA_ds_sm_q, bs="cc", k=10),
                weights=event, data=sofa_fu, family=cox.ph())

# fit AFCM
fit_afcm <- gam(time ~ age + Charlson + male +
                  ti(tmat, SOFA_ds_sm_q, by=lmat, bs=c("cc","cr"), 
                     k=c(5,5), mc=c(FALSE,TRUE)),
                weights=event, data=sofa_fu, family=cox.ph())


## visualize the estimates
par(mfrow = c(1,2))
vis.gam(fit_lfcm, view = c("tmat", "SOFA_ds_sm_q"), plot.type = "contour", 
        color = "cm", main = "Estimates from LFCM")
vis.gam(fit_afcm, view = c("tmat", "SOFA_ds_sm_q"), plot.type = "contour", 
        color = "cm", main = "Estimates from AFCM")
        
# working plots!
        
#fit_lfcm_pfr <- pfr(time ~ Charlson + lf(sofa_ds, bs="cc", k=30), 
#                    weights=event, data=sofa_fu, family=cox.ph())



#gam(survtime ~ Z + s(tmat, by=lmat*X, bs="cr", k=10), weights=event,
#    data=data_analysis, family=cox.ph())

