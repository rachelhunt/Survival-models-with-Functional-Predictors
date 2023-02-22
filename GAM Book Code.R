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
# z ~ event (expl. in doc)


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

## 'Using a basis to represent f(x), now becomes the 
## linear model y = Xβ + ǫ where Xij = bj(xi).'

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


### Penalised Piecewise Regression Representation of an Additive Model

## Here is an R function which produces constrained versions of Xj and Dj.
tf.XD <- function(x,xk,cmx=NULL,m=2) { 
  ## get X and D subject to constraint 
  nk <- length(xk) 
  X <- tf.X(x,xk)[,-nk] ## basis matrix 
  D <- diff(diag(nk),differences=m)[,-nk] ## root penalty 
  if (is.null(cmx)) 
    cmx <- colMeans(X) 
  X <- sweep(X,2,cmx) ## subtract cmx from columns 
  list(X=X,D=D,cmx=cmx) 
}

## function to set up and fit a simple two term additive model,
## assuming the same number of knots for each smooth.
am.fit <- function(y,x,v,sp,k=10) { 
  ## setup bases and penalties... 
  xk <- seq(min(x),max(x),length=k) 
  xdx <- tf.XD(x,xk) 
  vk <- seq(min(v),max(v),length=k) 
  xdv <- tf.XD(v,vk) 
  ## create augmented model matrix and response... 
  nD <- nrow(xdx$D)*2 
  sp <- sqrt(sp) 
  X <- cbind(c(rep(1,nrow(xdx$X)),rep(0,nD)), 
             rbind(xdx$X,sp[1]*xdx$D,xdv$D*0), 
             rbind(xdv$X,xdx$D*0,sp[2]*xdv$D)) 
  y1 <- c(y,rep(0,nD)) 
  ## fit model.. 
  b <- lm(y1 ~ X - 1) 
  ## compute some useful quantities... 
  n <- length(y) 
  trA <- sum(influence(b)$hat[1:n]) ## EDF 
  rsd <- y - fitted(b)[1:n] ## residuals 
  rss <- sum(rsd^2) ## residual SS 
  sig.hat <- rss/(n-trA) ## residual variance 
  gcv <- sig.hat*n/(n-trA) ## GCV score 
  Vb <- vcov(b)*sig.hat/summary(b)$sigma^2 ## coeff cov matrix 
  ## return fitted model... 
  list(b=coef(b),Vb=Vb,edf=trA,gcv=gcv,fitted=fitted(b)[1:n], 
       rsd=rsd,xk=list(xk,vk),cmx=list(xdx$cmx,xdv$cmx)) }


## Example: estimate an additive model for the data in R data frame trees.
## Volumei = α + f1(Girthi) + f2(Heighti) + ϵi

am.gcv <- function(lsp,y,x,v,k) { 
  ## function suitable for GCV optimization by optim 
  am.fit(y,x,v,exp(lsp),k)$gcv }

## Fitting the model 
## find GCV optimal smoothing parameters... 
fit <- optim(c(0,0), am.gcv, y=trees$Volume, x=trees$Girth, 
             v=trees$Height,k=10) 
sp <- exp(fit$par) ## best fit smoothing parameters 
## Get fit at GCV optimal smoothing parameters... 
fit <- am.fit(trees$Volume,trees$Girth,trees$Height,sp,k=10)

## fuction to plot the smooth effects
am.plot <- function(fit,xlab,ylab) {
  ## produces effect plots for simple 2 term #
  # additive model 
  start <- 2 
  ## where smooth coeffs start in beta 
  for (i in 1:2) { 
    ## sequence of values at which to predict... 
    x <- seq(min(fit$xk[[i]]), max(fit$xk[[i]]), length=200) 
    ## get prediction matrix for this smooth... 
    Xp <- tf.XD(x, fit$xk[[i]], fit$cmx[[i]])$X 
    ## extract coefficients and cov matrix for this smooth 
    stop <- start + ncol(Xp)-1; ind <- start:stop 
    b <- fit$b[ind];Vb <- fit$Vb[ind,ind] 
    ## values for smooth at x... 
    fv <- Xp %*% b 
    ## standard errors of smooth at x.... 
    se <- rowSums((Xp %*% Vb) * Xp)^.5 
    ## 2 s.e. limits for smooth... 
    ul <- fv + 2 * se; ll <- fv - 2 * se 
    ## plot smooth and limits... 
    plot(x, fv, type="l", ylim=range(c(ul,ll)), xlab=xlab[i], 
         ylab=ylab[i]) 
    lines(x, ul, lty=2); lines(x, ll, lty=2) 
    start <- stop + 1 
    } 
  }

## Now applying to trees data:
par(mfrow=c(1,3)) 
plot(fit$fitted,trees$Vol,xlab="fitted volume ", ylab="observed volume") 
am.plot(fit,xlab=c("Girth","Height"), ylab=c("s(Girth)","s(Height)"))

### function implements the PIRLS loop for the log-gamma model, 
### and returns the required GCV score in its return list.
gam.fit <- function(y,x,v,sp,k=10) { 
  ## gamma error log link 2 term gam fit... 
  eta <- log(y) ## get initial eta 
  not.converged <- TRUE 
  old.gcv <- -100 ## don’t converge immediately 
  while (not.converged) { 
    mu <- exp(eta) ## current mu estimate 
    z <- (y - mu)/mu + eta ## pseudodata 
    fit <- am.fit(z,x,v,sp,k) ## penalized least squares 
    if (abs(fit$gcv-old.gcv)<1e-5*fit$gcv) { 
      not.converged <- FALSE 
    } 
    old.gcv <- fit$gcv 
    eta <- fit$fitted ## updated eta 
  } 
  fit$fitted <- exp(fit$fitted) ## mu 
  fit 
}

## simple wrapper is needed in order to optimize the GCV score using optim
gam.gcv <- function(lsp,y,x,v,k=10) { 
  gam.fit(y,x,v,exp(lsp),k=k)$gcv 
  }

## Now, fitting and plotting proceeds exactly as in the simple additive case:
fit <- optim(c(0,0),gam.gcv,y=trees$Volume,x=trees$Girth, v=trees$Height,k=10) 
sp <- exp(fit$par) 
fit <- gam.fit(trees$Volume,trees$Girth,trees$Height,sp) 
par(mfrow=c(1,3)) 
plot(fit$fitted,trees$Vol,xlab="fitted volume ", ylab="observed volume") 
am.plot(fit,xlab=c("Girth","Height"), ylab=c("s(Girth)","s(Height)"))


### INTRODUCING MGCV PACKAGE
## cherry tree data example
library(mgcv)
data(trees) 
ct1 <- gam(Volume ~ s(Height) + s(Girth), 
           family=Gamma(link=log),data=trees)

ct1
plot(ct1,residuals=TRUE)

## Finer control of gam 

# penalized cubic regression splines are selected using s(...,bs="cr").
ct2 <- gam(Volume ~ s(Height,bs="cr") + s(Girth,bs="cr"),
           family=Gamma(link=log),data=trees)
ct2

# the dimension of the basis used to represent smooth terms - k
ct3 <- gam(Volume ~ s(Height) + s(Girth,bs="cr",k=20), 
           family=Gamma(link=log),data=trees)
ct3

# Gamma - - can be used to multiply the model effective degrees of freedom 
# in the GCV or UBRE scores
ct4 <- gam(Volume ~ s(Height) + s(Girth), 
           family=Gamma(link=log),data=trees,gamma=1.4)
ct4

## Smooths of several variables
ct5 <- gam(Volume ~ s(Height,Girth,k=25), 
           family=Gamma(link=log),data=trees)
ct5

ct6 <- gam(Volume ~ te(Height,Girth,k=5), 
           family=Gamma(link=log),data=trees)
ct6

## let us instead suppose that the Height is actually only measured 
## as a categorical variable:
trees$Hclass <- factor(floor(trees$Height/10)-5, 
                       labels=c("small","medium","large"))

## fit a generalized additive model to these data, 
## using the Hclass variable as a factor variable, and plot the result
ct7 <- gam(Volume ~ Hclass + s(Girth), 
           family=Gamma(link=log), data=trees) 
par(mfrow=c(1,2)); plot(ct7,all.terms=TRUE)

anova(ct7)
AIC(ct7)
summary(ct7)

# -------------------------- Chapter 5 ----------------------------------------

## evaluate single B-spline basis functions at a series of x values
bspline <- function(x,k,i,m=2) 
  # evaluate ith B-spline basis function of order m at the 
  # values in x, given knot locations in k 
{ if (m==-1) { 
    # base of recursion 
  res <- as.numeric(x<k[i+1]&x>=k[i]) 
  } else {
    # construct from call to lower order basis 
  z0 <- (x-k[i])/(k[i+m+1]-k[i]) 
  z1 <- (k[i+m+2]-x)/(k[i+m+2]-k[i+1]) 
  res <- z0*bspline(x,k,i,m-1)+ z1*bspline(x,k,i+1,m-1) 
  } 
  res 
}

## P-spline creation example
k <- 6 # example basis dimension 
P <- diff(diag(k),differences=1) # sqrt of penalty matrix 
S <- t(P)%*%P # penalty matrix

## Example - monotonic penalised spline set-up
ssp <- s(x,bs="ps",k=k)
ssp$mono <- 1 
# ssp$mono <- -1 would have set things up for monotonic decrease
sm <- smoothCon(ssp,data.frame(x))[[1]]

## The sm smooth object contains a model matrix X and penalty matrix S 
## that we can then use in a Newton loop for fitting.

## Simple Newton loop (with sp smoothing parameter)

X <- sm$X; XX <- crossprod(X); sp <- .5 
gamma <- rep(0,k); S <- sm$S[[1]] 
for (i in 1:20) { 
  gt <- c(gamma[1],exp(gamma[2:k])) 
  dg <- c(1,gt[2:k]) 
  g <- -dg*(t(X)%*%(y-X%*%gt)) + sp*S%*%gamma 
  H <- dg*t(dg*XX) 
  gamma <- gamma - solve(H+sp*S,g) 
}

# -------------------------- Chapter 7 ----------------------------------------
library(mgcv); library(MASS) ## load for mcycle data. 
## set up a smoother...
sm <- smoothCon(s(times,k=10),data=mcycle,knots=NULL)[[1]] 
## use it to fit a regression spline model... 
beta <- coef(lm(mcycle$accel~sm$X-1)) 
with(mcycle,plot(times,accel)) ## plot data 
times <- seq(0,60,length=200) ## create prediction times 
## Get matrix mapping beta to spline prediction at ’times’ 
Xp <- PredictMat(sm,data.frame(times=times)) 
lines(times,Xp%*%beta) ## add smooth to plot

## Brain Imaging Example - - No Dataset

### 

