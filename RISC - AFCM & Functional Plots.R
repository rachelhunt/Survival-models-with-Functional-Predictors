## Shortened script with work on RISC data
## See 'RISC - EDA & Formatting.R' for full version

library(ggplot2)
library(dplyr)
## Loading in the data 
basis_coef <- readRDS("~/Desktop/Research/RISC Data/basis-coef.rds")
discrete <- readRDS("~/Desktop/Research/RISC Data/discrete.rds")
library(readxl)
time_to_injury <- read_excel("~/Desktop/Research/RISC Data/RISC_Time_To_Injury.xlsx")

## Data cleaning time_to_injury
time_to_injury$DaystoRRI1[time_to_injury$DaystoRRI1 == 434] <- 555
time_to_injury$DaystoRRI1[time_to_injury$DaystoRRI1 == 555] <- 365
library(stringr)
time_to_injury$ID <- str_to_title(time_to_injury$ID)
names(time_to_injury)[1] ="subject_id"

## NB: Creating Event Variable for time_to_injury
hist(time_to_injury$DaystoRRI1)
ggplot(time_to_injury, aes(x=DaystoRRI1)) + 
  geom_histogram(binwidth=30)

time_to_injury$EventInjuryYr1 = ifelse(time_to_injury$DaystoRRI1 == 365, '0', '1')
time_to_injury$EventInjuryYr1 <- as.factor(time_to_injury$EventInjuryYr1)

# Comparing Groups: 
ggplot(time_to_injury, aes(x=DaystoRRI1)) + 
  geom_histogram(binwidth=15) + 
  facet_wrap(time_to_injury$EventInjuryYr1)

time_to_injury %>%
  group_by(EventInjuryYr1) %>% summarize(mean_days = mean(DaystoRRI1), 
                                         max_days = max(DaystoRRI1),
                                         min_days = min(DaystoRRI1))

ggplot(time_to_injury, aes(x=DaystoRRI1, fill=EventInjuryYr1)) +
  geom_boxplot()

## Extracting one row for each subject_id & 
# match to survival info based on these unique IDs (for cox models only)

indices <- which(discrete$subject_id[-1] != discrete$subject_id[-length(
  discrete$subject_id)])
indices <- append(indices, 442071, 299) # adding in last ID


subject_info <- discrete[c(indices),]  %>% 
  select(subject_id, age, sex, runner_category, bmi_kgm, 
         retrospective_injury_status, 
         prospectively_injured_12_mths,
         num_retrospective_injuries_1_yr, 
         simplified_retrospective_injury_status )
length(unique(subject_info$subject_id)) # 299 :) - to check if worked

# Creating Survival dataset with some clinical info
demographic_w_survival <- merge(subject_info, time_to_injury, 
                                by = c("subject_id"), all = TRUE)       
head(demographic_w_survival)  
length(unique(demographic_w_survival$subject_id)) 
#306 ! 7 different

## New Dataset censoring edits based on exclusion info
# Vague time-to-drop-out given: Add according to exclusion information note. 
# P_4005, P_4156, P_4187, P_4191, P_4273, P_4307, P_4314. (7 total)
demographic_w_survival$DaystoRRI1[5] <- 90 #Sub 1
demographic_w_survival$EventInjuryYr1[5] <- 0
demographic_w_survival$DaystoRRI1[152] <- 90 #Sub 2
demographic_w_survival$EventInjuryYr1[152] <- 0
demographic_w_survival$DaystoRRI1[183] <- 150 #Sub 3
demographic_w_survival$EventInjuryYr1[183] <- 0
demographic_w_survival$DaystoRRI1[187] <- 210 #Sub 4
demographic_w_survival$EventInjuryYr1[187] <- 0
demographic_w_survival$DaystoRRI1[266] <- 270 #Sub 5
demographic_w_survival$EventInjuryYr1[266] <- 0
demographic_w_survival$DaystoRRI1[299] <- 14 # Achilles Tendon 2 Weeks in ! Sub 6
demographic_w_survival$EventInjuryYr1[299] <- 0
demographic_w_survival$DaystoRRI1[305] <- 90 #Sub 7
demographic_w_survival$EventInjuryYr1[305] <- 0

## Note:  "shortly after enrollment" Subjects
# Leave out for the moment, can set as 14 days and 
# include to check for model differences.
# P_4213, P_4242, P_4255, P_4291, P_4304. (5 total)

ggplot(demographic_w_survival, aes(x=DaystoRRI1, fill=EventInjuryYr1)) +
  geom_boxplot() + scale_fill_discrete(labels=c('Injured', 'Not Injured')) +
  labs(title="Event - Injury in 12 months")

### ------------------- Functional data Extraction! -----------------------------

library(fda)
## combine survival with demo and funct. 
## Three different plane/angle/side combinations

demographic_w_survival$bmi_cat <- as.factor(demographic_w_survival$bmi_cat)
demographic_w_survival$age_cat <- as.factor(demographic_w_survival$age_cat)

######## Left, Ankle, Abduction 

## Subsetting functional curves:
# Use basis_coef
dim(basis_coef)

left_ankle_abduction <- basis_coef %>% 
  filter(side == "left" & location =="Ankle" & plane_of_motion == "abd") %>% 
  dplyr::select(1,2,4,51:132)

summary(left_ankle_abduction)
left_ankle_abduction[,3:83] <- lapply(left_ankle_abduction[,3:83] , as.numeric)
head(left_ankle_abduction)
  # OR Use discrete
#left_ankle_abduction <- discrete %>% 
#  filter(side == "left" & location =="Ankle" & plane_of_motion == "abd") %>% 
#  dplyr::select(1,2,4,52:153)

dim(left_ankle_abduction)

## Combining these curves with survival and demographic data

left_ank_abd_surv <- merge(demographic_w_survival, left_ankle_abduction, 
                           by = c("subject_id"), all = TRUE)       
head(left_ank_abd_surv) 
dim(left_ank_abd_surv)
length(unique(left_ank_abd_surv$subject_id)) 
length(unique(left_ank_abd_surv$stride_num)) 
summary(left_ank_abd_surv$stride_num)


######## Right, Hip, Flexion
# use basis_coef
dim(basis_coef)

right_hip_flexion <- basis_coef %>% 
  filter(side == "right" & location =="Hip" & plane_of_motion == "fle") %>% 
  dplyr::select(1,2,4,51:132)

# OR Use discrete
#right_hip_flexion <- discrete %>% 
#  filter(side == "right" & location =="Hip" & plane_of_motion == "fle") %>% 
#  dplyr::select(1,2,4,52:153)

dim(right_hip_flexion)
summary(right_hip_flexion)

## Combining these curves with survival and demographic data
right_hip_flexion_surv <- merge(demographic_w_survival, right_hip_flexion, 
                                by = c("subject_id"), all = TRUE)       
head(right_hip_flexion_surv) 
dim(right_hip_flexion_surv)
length(unique(right_hip_flexion_surv$subject_id)) 
length(unique(right_hip_flexion_surv$stride_num)) 
summary(right_hip_flexion_surv$stride_num)


######## Left, Knee, Rotation 

# use basis_coef
dim(basis_coef)

left_knee_rotation <- basis_coef %>% 
  filter(side == "left" & location =="Knee" & plane_of_motion == "rot") %>% 
  dplyr::select(1,2,4,51:132)

# OR Use discrete
#left_knee_rotation <- discrete %>% 
#  filter(side == "left" & location =="Knee" & plane_of_motion == "rot") %>% 
#  dplyr::select(1,2,4,52:153)

#dim(right_hip_flexion)
#summary(right_hip_flexion)

## Combining these curves with survival and demographic data
left_knee_rotation_surv <- merge(demographic_w_survival, left_knee_rotation, 
                                 by = c("subject_id"), all = TRUE)       
head(left_knee_rotation_surv) 
dim(left_knee_rotation_surv)

### ----------------------------- Functional Plotting  -----------------------------

# columns = 15:115 for discrete, 14:93 for basis_coef
# n = 101 for discrete, 80 for basis coef
stride <- 1:80
# instead of 137
n <- length(unique(left_ank_abd_surv$subject_id))

matplot(stride, t(left_ank_abd_surv[,16:95]), 
        type='l', lty=1, col=rainbow(n),
        main = "Left Ankle Angles - Abduction Plane",
        xlab="basis_coef", ylab="angle")

# try plotting overall mean
#fd1.1 <- Data2fd(stride, basisobj = left_ank_abd_surv[,16:95])
F.obj.ankle <- Data2fd(argvals = stride, y = t(left_ank_abd_surv[,16:95]))
W_mean.ankle <- mean.fd(F.obj.ankle)  
W_sd.ankle <- std.fd(F.obj.ankle)
# showing as NAs!


stride <- 1:80
n <- length(unique(right_hip_flexion_surv$subject_id))
matplot(stride, t(right_hip_flexion_surv[,16:95]), 
        type='l', lty=1, col=rainbow(n),
        main = "Right Hip Angles - Flexion Plane",
        xlab="basis_coef", ylab="angle")

stride <- 1:80
n <- length(unique(left_knee_rotation_surv$subject_id))
matplot(stride, t(left_knee_rotation_surv[,16:95]), 
        type='l', lty=1, col=rainbow(n),
        main = "Left Knee Angles - Rotation Plane",
        xlab="time", ylab="angle")

## ----------------------------- Apply Erjia Cui Method --------------------------
## Load Packages
source("/Users/rachelhunt/Desktop/FUI Code/code/lfosr3s.R")
library(ggplot2)
library(gridExtra)
library(tictoc)

## FUI - Fast Univariate Inference (Not working yet!)
# matrix within datafrome - DTI {refund}?
n <- 306 ## number of subjects  - - length(unique(left_ank_abd_surv$subject_id))
s <- 80 ## max number of functional observations per subject - - 137 for discrete
event <- left_ank_abd_surv$EventInjuryYr1 ## 30% of subjects have events observed 
survtime <- left_ank_abd_surv$DaystoRRI1 ## observed time
## transformations on X may be necessary for identifiability in practice 
curves <- as.matrix(left_ank_abd_surv[,16:95])
#X <- fpca.face(X)$Yhat ## smooth each curve using fast sandwich smoother (try without for now)
sex <- left_ank_abd_surv$sex## a scalar predictor
age_cat <- left_ank_abd_surv$age_cat
bmi_cat <- left_ank_abd_surv$bmi_cat
runner_cat <- left_ank_abd_surv$runner_category

data_analysis <- data.frame(event, survtime, sex, age_cat,
                            bmi_cat, runner_cat, curves = I(curves)) ## the dataset 

str(data_analysis)

#stride_matrix <- data.matrix(left_ank_abd_surv[,16:95])
#left_ank_abd_surv$strides <- left_ank_abd_surv[,16:95]


tic()
fit_RISC <- lfosr3s(formula = curves ~  age_cat + runner_cat +
                      sex, data = data_analysis,
                   family = "gaussian", var = TRUE, analytic = TRUE)
toc()
# running... 10:27am - 10:46 (19 mins!)
# error: solve.default(V.subj) : 'a' is 0-diml
# need to specify both fixed and random effects in model

# stack overflow: diff vars in 2 models 
# change random effect to subject_id

## ----------------------------- TEST 1 - one subject plots & Mean

fun_surv_test <- left_ank_abd_surv %>%
  subset(subject_id == "P_4001")
dim(fun_surv_test) #81 strides

basis <- 1:80
n <- 1

tic()
matplot(stride, t(fun_surv_test[,16:95]), 
        type='l', lty=1, col='blue',
        main = "Left Ankle Angles - Abduction Plane (Subject P_4001)",
        xlab="basis_coef", ylab="angle")
toc()


W.obj1 <- Data2fd(argvals = basis, y = t(fun_surv_test[,16:95]))
W_mean1 <- mean.fd(W.obj1)  
W_sd1 <- std.fd(W.obj1)

#SE_u <- fd(basisobj = basis)
#SE_l <- fd(basisobj = basis)
#SE_u$coefs <- W_mean$coefs +  1.96 * W_sd$coefs/sqrt(basis) 
#SE_l$coefs <- W_mean$coefs -  1.96 * W_sd$coefs/sqrt(n_curves)

lines(W_mean1,  lwd = 3, col='red')
#lines(SE_u, lwd = 3, lty = 3, col='green')

# https://www.r-bloggers.com/2021/05/basic-fda-descriptive-statistics-with-r/
#lines(SE_l, lwd = 3, lty = 3, col='green')


## ----------------------------- TEST 2 - two subjects plots & Mean

fun_surv_test2 <- left_ank_abd_surv %>%
  subset(subject_id == "P_4001" | subject_id == "P_4002")
dim(fun_surv_test2) # 81 strides P_4001, 48 strides P_4002 (129)

stride <- 1:80 # these actually basis coeffs
n <- length(unique(fun_surv_test2$subject_id))
matplot(stride, t(fun_surv_test2[,16:95]),
        type='l', lty=1, col = 'blue',
        main = "Left Ankle Angles - Abduction Plane (Two Subjects)",
        xlab="basis_coef", ylab="angle")

W.obj2 <- Data2fd(argvals = basis, y = t(fun_surv_test2[82:129,16:95]))
W_mean2 <- mean.fd(W.obj2)  
W_sd2 <- std.fd(W.obj2)


lines(W_mean1,  lwd = 3, col='red')

lines(W_mean2,  lwd = 3, col='yellow')

# Q - colouring by subject_id?? and not cyclic

## ----------------------------- TEST 3 - get plots of all means

indices <- which(left_ank_abd_surv$subject_id[-1] != left_ank_abd_surv$subject_id[-length(
  left_ank_abd_surv$subject_id)])
dim(left_ank_abd_surv)
indices <- append(indices, 24714, 306) # adding in last ID
  

meancurves = data.frame()

## get mean curve for each subject 
for (i in indices){
  id <- left_ank_abd_surv[as.numeric(i),1] # selecting the subject iD
  fun_surv_test3 <- left_ank_abd_surv %>%
    subset(subject_id == id) # filtering dataset to curves for one subject 
  n <- length(fun_surv_test3$subject_id) # no of strides for this subject
  basis <- 1:80 #  
  fun.obj.loop <- Data2fd(argvals = basis, y = t(fun_surv_test3[,16:95]))
  fun_mean.loop <- mean.fd(fun.obj.loop)  
  ## adding subject ID and mean curve to a new dataframe
  index <- which(indices == i)
  meancurves[index , 1] <- id # collecting subject ID
  meancurves[index,2:83] <- t(fun_mean.loop$coefs) # collecting mean curve
} 
# check correct:
dim(meancurves)
head(meancurves)
tail(meancurves)

# plot all of the mean curves
basis <- 1:82 # these actually basis coeffs
n <- length(unique(meancurves$subject_id))
matplot(stride, t(meancurves[,2:83]),
        type='l', lty=1, col = rainbow(306),
        main = "L Ankle Abd. Angles, Subject Mean Curves",
        xlab="basis_coef", ylab="angle")

meancurves.fun.obj <- Data2fd(argvals = basis , y = t(meancurves[,2:83]))
meancurves_mean <- mean.fd(meancurves.fun.obj)  # coefs NA! 84?
meancurves_sd <- std.fd(meancurves.fun.obj) # coefs NA!

### ----------------------------- AFCM - Erjia Code - ATTEMPT 2  ------------------------

library(mgcv)
library(refund)

n <- 306 ## number of subjects  - - length(unique(left_ank_abd_surv$subject_id))
s <- 80 ## max number of functional observations per subject - - 137 for discrete
event <- left_ank_abd_surv$EventInjuryYr1 ## 30% of subjects have events observed 
survtime <- left_ank_abd_surv$DaystoRRI1 ## observed time
## transformations on X may be necessary for identifiability in practice 
curves <- as.matrix(left_ank_abd_surv[,16:95])
#X <- fpca.face(X)$Yhat ## smooth each curve using fast sandwich smoother (try without for now)
sex <- left_ank_abd_surv$sex## a scalar predictor
age_cat <- left_ank_abd_surv$age_cat
bmi_cat <- left_ank_abd_surv$bmi_cat
runner_cat <- left_ank_abd_surv$runner_category

data_analysis <- data.frame(event, survtime, sex, age_cat,
                            bmi_cat, runner_cat, curves = I(curves)) ## the dataset 

str(data_analysis)

## create variables related to numerical approximation
### lmat: numerical integration
data_analysis$lmat <- I(matrix(80, ncol=80, nrow=nrow(data_analysis)))
### tmat: time indices of functional observations, we assume an equally-spaced grid here 
data_analysis$tmat <- I(matrix(seq(0, 80, len=80), ncol=80, nrow=nrow(data_analysis), byrow=TRUE))

## Checking linear GAM
fit_1 <- gam(survtime ~ sex + age_cat + bmi_cat + runner_cat,
                data=data_analysis, family=cox.ph())
summary(fit_1)

fit_2 <- gam(survtime ~ bmi_cat, data=data_analysis, family=cox.ph())
summary(fit_2)

## Checking for functional only
fit_lfcm <- gam(survtime ~ s(tmat, by=lmat*as.matrix(curves), bs="cr", k=15), weights=event,
                data=data_analysis, family=cox.ph())

fit_afcm <- gam(survtime ~ ti(tmat, as.matrix(curves), by=lmat, bs=c("cr","cr"), k=c(10,10),
                                  mc=c(FALSE,TRUE)), 
                weights=event, 
                data=data_analysis,
                family=cox.ph())

# ERROR: invalid type (list) for variable 'X'

#fit_lfcm <- gam(time_mort ~ Alcohol + Overall_health + PIR + Employed +
#                  Age + BMI_cat + SmokeCigs + Race + Education +
                  ## fit AFCM
#                  CHD + Diabetes + CHF + Stroke + MobilityProblem + Cancer +
#                  s(tmat, by=lmat*act_log_mat_sm_q, bs="cc", k=10),
#                weights=event, data=data_analysis, family=cox.ph())

# check tmat matrix - length

## fit AFCM
fit_afcm <- gam(survtime ~ Z + ti(tmat, X, by=lmat, bs=c("cr","cr"), k=c(10,10),
                                  mc=c(FALSE,TRUE)), weights=event, data=data_analysis,
                family=cox.ph())
# Same error

###### NEW

### lmat: numerical integration
left_ank_abd_surv$lmat <- I(matrix(1/80, ncol=80, nrow=nrow(left_ank_abd_surv)))

### tmat: time indices
left_ank_abd_surv$tmat <- I(matrix(1:80, ncol=80, nrow=nrow(left_ank_abd_surv), byrow=TRUE))


