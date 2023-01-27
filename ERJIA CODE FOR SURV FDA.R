## https://github.com/ecui1/AFCM/blob/main/vignette_fcm.pdf
# Fitting Functional Cox Models Using mgcv package

## load packages
library(mgcv)
library(refund)

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
data_analysis$tmat <- I(matrix(seq(0, 1, len=S), ncol=S, nrow=nrow(data_analysis), byrow=TRUE))

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

