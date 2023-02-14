## From Simon Wood book - Generalised Additive Models

# -------------------------- Chapter 3 - Page 136 - Cox PH stuff

## take such a data frame and automatically produce 
## the equivalent data frame for the Poisson model.

psurv <- function(surv,time="t",censor="d",event="z") { 
  ## create data frame to fit Cox PH as Poisson model. 
  ## surv[[censor]] should be 1 for event or zero for censored. 
  if (event %in% names(surv)) warning("event name clashes") 
  surv <- as.data.frame(surv)[order(surv[[time]]),] # ascending t order 
  et <- unique(surv[[time]][surv[[censor]]==1]) # unique times 
  es <- match(et,surv[[time]]) # starts of risk sets in surv 
  n <- nrow(surv); t <- rep(et,1+n-es) # times for risk sets 
  st <- cbind(0, 
              surv[unlist(apply(matrix(es),1,function(x,n) x:n,n=n)),]) 
  st[st[[time]]==t&st[[censor]]!=0,1] <- 1 # signal events 
  st[[time]] <- t ## reset event time to risk set time 
  names(st)[1] <- event 
  st } ## psurv


require(gamair); # Package
data(bone); # Data
bone$id <- 1:nrow(bone) # unique subject ID
pb <- psurv(bone); # create data frame to fit Cox PH as Poisson model. 
pb$tf <- factor(pb$t) # event time factor variable
b <- glm(z ~ tf + trt - 1,poisson,pb) # model fit

View(pb)

# t ~ Time of death, relapse
# d ~ event variable
# trt ~ Treatment - allo or auto 
# id ~ Unique subject ID
# tf ~ event time factor variable
# z ~ event


## martingale and deviance residuals
chaz <- tapply(fitted(b),pb$id,sum) ## by subject cum. hazard
mrsd <- bone$d - chaz ## Martingale residuals

summary(b)
drop1(b,test="Chisq") ## test for effect - no evidence

## To visualize what is happening it can help to plot the survival 
## curve estimates for the two groups.
te <- sort(unique(bone$t[bone$d==1])) ## event times 
te


## predict survivor function for "allo"... 
pd <- data.frame(tf=factor(te),trt=bone$trt[1]) 
fv <- predict(b,pd) 
H <- cumsum(exp(fv)) ## cumulative hazard 
plot(stepfun(te,c(1,exp(-H))),col="blue",lwd=2,do.points=FALSE,ylim=c(0,1), 
     xlim=c(0,550),main="",ylab="S(t)",xlab="t (days)") 
## add std.e. bands... 
X <- model.matrix(~tf+trt-1,pd) 
J <- apply(exp(fv)*X,2,cumsum) 
se <- diag(J%*%vcov(b)%*%t(J))^.5 
lines(stepfun(te,c(1,exp(-H+se))),do.points=FALSE,lty=2,col="blue") 
lines(stepfun(te,c(1,exp(-H-se))),do.points=FALSE,lty=2,col="blue")

## same for "auto"...
pd <- data.frame(tf=factor(te),trt=bone$trt[23])
fv <- predict(b,pd); 
H <- cumsum(exp(fv))
lines(stepfun(te,c(1,exp(-H))),col="red",lwd=2,do.points=FALSE)
## add std.e. bands...
X <- model.matrix(~tf+trt-1,pd)
J <- apply(exp(fv)*X,2,cumsum)
se <- diag(J%*%vcov(b)%*%t(J))^.5
lines(stepfun(te,c(1,exp(-H+se))),do.points=FALSE,lty=2,col="red")
lines(stepfun(te,c(1,exp(-H-se))),do.points=FALSE,lty=2,col="red")


# -------------------------- Chapter 4 ----------------------------------------

## example of using piecewise linear basis:
require(gamair); data(engine); 
attach(engine) 
plot(size,wear,xlab="Engine capacity",ylab="Wear index")
# ?engine

# defining bj(x), linear interpolant of the data
tf <- function(x,xj,j) { 
  ## generate jth tent function from set defined by knots xj 
  dj <- xj*0;dj[j] <- 1 
  approx(xj,dj,x)$y }

tf.X <- function(x,xj) { 
  ## tent function basis matrix given data x 
  ## and knot sequence xj 
  nk <- length(xj); 
  n <- length(x) 
  X <- matrix(NA,n,nk) 
  for (j in 1:nk) 
    X[,j] <- tf(x,xj,j) 
  X }

sj <- seq(min(size),max(size),length=6) ## generate knots 
X <- tf.X(size,sj) ## get model matrix 
b <- lm(wear ~ X - 1) ## fit model 
s <- seq(min(size),max(size),length=200)## prediction data 
Xp <- tf.X(s,sj) ## prediction matrix 
plot(size,wear) ## plot data 
lines(s,Xp %*% coef(b)) ## overlay estimated f

### Penalised regression spline in action

## a simple function for fitting a penalized piecewise linear smoother.
prs.fit <- function(y,x,xj,sp) { 
  X <- tf.X(x,xj) ## model matrix 
  D <- diff(diag(length(xj)),differences=2) ## sqrt penalty 
  X <- rbind(X,sqrt(sp)*D) ## augmented model matrix 
  y <- c(y,rep(0,nrow(D))) ## augmented data 
  lm(y ~ X - 1) ## penalized least squares fit
}

sj <- seq(min(size),max(size),length=20) ## knots 
b <- prs.fit(wear,size,sj,2) ## penalized fit 
plot(size,wear) ## plot data 
Xp <- tf.X(s,sj) ## prediction matrix 
lines(s,Xp %*% coef(b)) ## plot the smooth 

### Simple direct search for the GCV optimal smoothing parameter
rho = seq(-9,11,length=90) 
n <- length(wear) 
V <- rep(NA,90)

for (i in 1:90) { 
  ## loop through smoothing params 
  b <- prs.fit(wear,size,sj,exp(rho[i])) ## fit model 
  trF <- sum(influence(b)$hat[1:n]) ## extract EDF 
  rss <- sum((wear-fitted(b)[1:n])^2) ## residual SS 
  V[i] <- n*rss/(n-trF)^2 ## GCV score 
  }

## Plots of the GCV score and the optimal model
plot(rho,V,type="l",xlab=expression(log(lambda)), 
     main="GCV score") 
sp <- exp(rho[V==min(V)]) ## extract optimal sp 
b <- prs.fit(wear,size,sj,sp) ## re-fit 
plot(size,wear,main="GCV optimal fit") 
lines(s,Xp %*% coef(b))


### Code to re-parameterize the model and estimate it using optim and llm
llm <- function(theta,X,Z,y) { 
  ## untransform parameters... 
  sigma.b <- exp(theta[1]) 
  sigma <- exp(theta[2]) ## extract dimensions... 
  n <- length(y); pr <- ncol(Z); pf <- ncol(X) 
  ## obtain \hat \beta, \hat b... 
  X1 <- cbind(X,Z) 
  ipsi <- c(rep(0,pf),rep(1/sigma.b^2,pr)) 
  b1 <- solve(crossprod(X1)/sigma^2+diag(ipsi), 
              t(X1)%*%y/sigma^2) 
  ## compute log|Z’Z/sigma^2 + I/sigma.b^2|... 
  ldet <- sum(log(diag(chol(crossprod(Z)/sigma^2 + 
                              diag(ipsi[-(1:pf)]))))) 
  ## compute log profile likelihood... 
  l <- (-sum((y-X1%*%b1)^2)/sigma^2 - sum(b1^2*ipsi) -
          n*log(sigma^2) - pr*log(sigma.b^2) - 2*ldet - n*log(2*pi))/2
  attr(l,"b") <- as.numeric(b1) ## return \hat beta and \hat b -l 
  }


  
X0 <- tf.X(size,sj) ## X in original parameterization 
D <- rbind(0,0,diff(diag(20),difference=2)) 
diag(D) <- 1 ## augmented D 
X <- t(backsolve(t(D),t(X0))) ## re-parameterized X 
Z <- X[,-c(1,2)]; X <- X[,1:2] ## mixed model matrices 
## estimate smoothing and variance parameters... 
m <- optim(c(0,0),llm,method="BFGS",X=X,Z=Z,y=wear)  # -- Error here! 
b <- attr(llm(m$par,X,Z,wear),"b") ## extract coefficients 
## plot results... 
plot(size,wear) 
Xp1 <- t(backsolve(t(D),t(Xp))) ## re-parameterized pred. mat. 
lines(s,Xp1 %*% as.numeric(b),col="grey",lwd=2)
## Not working, see book for result

## Estimation using REML via lme
library(nlme) 
g <- factor(rep(1,nrow(X))) ## dummy factor 
m <- lme(wear ~ X - 1, random=list(g = pdIdent(~ Z-1))) 
lines(s,Xp1 %*% as.numeric(coef(m))) ## and to plot 


