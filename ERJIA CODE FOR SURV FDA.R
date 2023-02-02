## https://github.com/ecui1/AFCM/blob/main/vignette_fcm.pdf
# Fitting Functional Cox Models Using mgcv package

## load packages
library(mgcv)
library(refund)
library(RNHANES)

## simulate a dataset
set.seed(2021)
N <- 2000 ## number of subjects
S <- 1000 ## number of functional observations per subject
event <- rbinom(N, 1, 0.3) ## 30% of subjects have events observed 
survtime <- runif(N, 0, 10) ## observed time
## transformations on X may be necessary for identifiability in practice 
X <- matrix(rnorm(N*S), nrow = N, ncol = S)
X <- fpca.face(X)$Yhat ## smooth each curve using fast sandwich smoother 
Z <- rnorm(N, 1, 1) ## a scalar predictor
data_analysis <- data.frame(event, survtime, Z, X = I(X)) ## the simulated dataset 
rm(event, survtime, X, Z)
str(data_analysis)

## create variables related to numerical approximation
### lmat: numerical integration
data_analysis$lmat <- I(matrix(1/S, ncol=S, nrow=nrow(data_analysis)))
### tmat: time indices of functional observations, we assume an equally-spaced grid here 
data_analysis$tmat <- I(matrix(seq(0, 1, len=S), ncol=S, nrow=nrow(data_analysis), 
                               byrow=TRUE))

## fit LFCM
fit_lfcm <- gam(survtime ~ Z + s(tmat, by=lmat*X, bs="cr", k=10), weights=event,
                data=data_analysis, family=cox.ph())


## fit AFCM
fit_afcm <- gam(survtime ~ Z + ti(tmat, X, by=lmat, bs=c("cr","cr"), k=c(10,10),
                                  mc=c(FALSE,TRUE)), weights=event, data=data_analysis,
                family=cox.ph())

## visualize the estimates
par(mfrow = c(1,2))
vis.gam(fit_lfcm, view = c("tmat", "X"), plot.type = "contour", color = "cm",
        main = "Estimated Surface from LFCM")
vis.gam(fit_afcm, view = c("tmat", "X"), plot.type = "contour", color = "cm",
        main = "Estimated Surface from AFCM")

# ------------------------------------------------ NHANES Data ----------------

# https://github.com/ecui1/rnhanesdata
# https://github.com/ecui1/nhanes-tutorial/blob/main/nhanes-data-tutorial.Rmd

#install.package("devtools")
devtools::install_github("andrew-leroux/rnhanesdata")
library(rnhanesdata)

# Activity Count Data
dim(PAXINTEN_C)
head(PAXINTEN_C[,1:10], n = 10)
table(PAXINTEN_C$WEEKDAY)
dim(PAXINTEN_D)
table(PAXINTEN_D$WEEKDAY)

# Mortality Data
dim(Mortality_2015_C)
head(Mortality_2015_C)
table(Mortality_2015_C$mortstat)
dim(Mortality_2015_D)
table(Mortality_2015_D$mortstat)

# Covariate Data
dim(Covariate_C)
head(Covariate_C)
dim(Covariate_D)

# Data Cleaning 

library(tidyverse)
## change activity count value under non-wear flags to 0
PAXINTEN_C[,paste0("MIN",1:1440)] <- PAXINTEN_C[,paste0("MIN",1:1440)]*
  Flags_C[,paste0("MIN",1:1440)]
PAXINTEN_D[,paste0("MIN",1:1440)] <- PAXINTEN_D[,paste0("MIN",1:1440)]*
  Flags_D[,paste0("MIN",1:1440)]
## merge mortality and covariate data
mort_cov_C <- inner_join(Mortality_2015_C, Covariate_C, by = "SEQN")
mort_cov_D <- inner_join(Mortality_2015_D, Covariate_D, by = "SEQN")
## combine data collected from two waves
mort_cov <- bind_rows(mort_cov_C, mort_cov_D)
act_cnt <- bind_rows(PAXINTEN_C, PAXINTEN_D)
wear_flag <- bind_rows(Flags_C, Flags_D)
rm(mort_cov_C, mort_cov_D)
## create Age (in years) using the age at examination
mort_cov$Age <- mort_cov$RIDAGEEX/12

### Further Cleaning of Activity Count Data

## extract count values and flags as matrices
cnt_mat <- as.matrix(act_cnt[,paste0("MIN",1:1440)])
flag_mat <- as.matrix(wear_flag[,paste0("MIN",1:1440)])
## replace NAs with 0s
cnt_mat[is.na(cnt_mat)] <- 0
flag_mat[is.na(flag_mat)] <- 0
## calculate activity count summary measures
### total activity count (TAC)
act_cnt$TAC <- rowSums(cnt_mat)
### total log activity count (TLAC)
act_cnt$TLAC <- rowSums(log(1+cnt_mat))
### total wear time (WT)
act_cnt$WT <- rowSums(flag_mat)
### total sedentary time (ST)
act_cnt$ST <- rowSums(cnt_mat < 100) ## threshold set based on the literature
### total moderate to vigorous physical activity time (MVPA)
act_cnt$MVPA <- rowSums(cnt_mat >= 2020) ## threshold set based on the literature
## create "good day" indicator
act_cnt$goodday <- ifelse(act_cnt$PAXCAL == 1 & act_cnt$PAXSTAT == 1 & 
                            act_cnt$WT >= 600, 1, 0)
## store the minute-level activity count data as a column of the data frame
act_cnt$AC <- I(cnt_mat)
## clean the multilevel activity count data
act_cnt_ml <- act_cnt %>% filter(goodday == 1) %>%
  dplyr::select("SEQN", "SDDSRVYR", "WEEKDAY",
         "AC", "TAC", "TLAC", "ST", "MVPA", "WT",
         "PAXCAL", "PAXSTAT", "goodday")
## add number of good days for each participant
act_cnt_ml <- left_join(act_cnt_ml, act_cnt_ml %>% count(SEQN) %>% 
                          mutate(n_good_days = n) %>% dplyr::select(SEQN, n_good_days),
                        by = "SEQN")
dim(act_cnt_ml)
str(act_cnt_ml)
rm(act_cnt, wear_flag, cnt_mat, flag_mat)

# The data frame `act_cnt_ml` contains cleaned multilevel activity count data in NHANES. 
# This is the data we want to use for **multilevel** statistical modeling.

### Compression of Activity Count Data

act_cnt_ml2sl <- act_cnt_ml %>% filter(n_good_days >= 3) %>%
  dplyr::select(SEQN, AC, TAC, TLAC, ST, MVPA, WT, n_good_days)
## compress activity count data into participant level
act_cnt_sl <- aggregate(act_cnt_ml2sl[,3:ncol(act_cnt_ml2sl)], 
                        list(SEQN = act_cnt_ml2sl$SEQN), mean)
### for the count value matrix stored in "AC" column, we have to do aggregation manually
inx_row <- split(1:nrow(act_cnt_ml2sl), f = factor(act_cnt_ml2sl$SEQN))
act_cnt_sl$AC <- I(t(vapply(inx_row, function(x) 
  colMeans(act_cnt_ml2sl$AC[x,,drop=FALSE],na.rm=TRUE),
  numeric(ncol(act_cnt_ml2sl$AC)))))
dim(act_cnt_sl)
str(act_cnt_sl)
rm(act_cnt_ml2sl, inx_row)

# The data frame `act_cnt_sl` contains participant-level activity count data. 
# We next merge activity count data with mortality and covariate data.

data_analysis <- left_join(mort_cov, act_cnt_sl, by = "SEQN")
dim(data_analysis)
str(data_analysis)

## contains cleaned activity count data, mortality data, and covariate data of 
# **all study participants** in the NHANES 2003-2004 ("C") and 2005-2006 ("D") waves. 
# Each row represents one study participant. 
# The proportion of missing data varies by type. 
#For different research questions, it is recommended to set corresponding exclusion criteria. 

# Ready to use!

# -----------------------------------Back to the first Link -----------------------------

## truncate time to event at 10 years
#data_analysis$event[which(data_analysis$time_mort > 10)] <- 0
#data_analysis$time_mort[which(data_analysis$time_mort > 10)] <- 10
#table(data_analysis$event)

# Event: mortstat
# Time to event: n_good_days
# functional: data_analysis$AC
s
#data_analysis$act_log_mat_sm   <- I(fpca.face(unclass(data_analysis$AC))$Yhat)
