# Work on RISC Data - started 02/11/2022
# R version 4.1.1 (2021-08-10) -- "Kick Things"
library(ggplot2)
library(dplyr)
## Loading in the data 
basis_coef <- readRDS("~/Desktop/Research/RISC Data/basis-coef.rds")
#View(basis_coef)
head(basis_coef)
dim(basis_coef)
summary(basis_coef)
names(basis_coef)

discrete <- readRDS("~/Desktop/Research/RISC Data/discrete.rds")
#View(discrete)
head(discrete)
dim(discrete)
summary(discrete)
names(discrete)

library(readxl)
time_to_injury <- read_excel("~/Desktop/Research/RISC Data/RISC_Time_To_Injury.xlsx")
#/Users/rachelhunt/Desktop/Research/RISC Data/RISC_Time_To_Injury.xlsx
time_to_injury$DaystoRRI1[time_to_injury$DaystoRRI1 == 434] <- 555
time_to_injury$DaystoRRI1[time_to_injury$DaystoRRI1 == 555] <- 365
#View(time_to_injury)

library(stringr)
time_to_injury$ID <- str_to_title(time_to_injury$ID)

# Data on exclusion information:
#excl_info <- read_excel("~/Desktop/Research/RISC Data/Injury_Exclusions_SD.xlsx")
#head(excl_info)

#excl_info <- na.omit(excl_info)
#dim(excl_info)

#excl_info$ID <- str_to_title(excl_info$ID)
#names(excl_info)[1] ="subject_id"
#rm(excl_info)

##################### EDA: comparing the two larger datasets
library(janitor)
compare_df_cols(discrete, basis_coef, return = "mismatch")
compare_df_cols(discrete, basis_coef, return = "match")
compare_df_cols_same(discrete, basis_coef) # are there columns that could be binded?

# From dplyr package:
all_equal(discrete, basis_coef)
all_equal(discrete[,1:50], basis_coef[,1:50]) # true!

discrete$trial_id
sum(str_detect(discrete$trial_id, 'P_4001')) # 1467 trials
# 256 time to injury subject IDs * 1467 = 375'552
# 299 levels for subject ID: as.factor(discrete$subject_id) * 1467 = 438'633

##################### NB: Creating Event Variable for time_to_injury
hist(time_to_injury$DaystoRRI1)
ggplot(time_to_injury, aes(x=DaystoRRI1)) + 
  geom_histogram(binwidth=30)

time_to_injury$EventInjuryYr1 = ifelse(time_to_injury$DaystoRRI1 == 365, '0', '1')
time_to_injury$EventInjuryYr1 <- as.factor(time_to_injury$EventInjuryYr1)
#View(time_to_injury)

# Comparing Groups: 
ggplot(time_to_injury, aes(x=DaystoRRI1)) + 
  geom_histogram(binwidth=15) + 
  facet_wrap(time_to_injury$EventInjuryYr1)


time_to_injury %>%
  group_by(EventInjuryYr1) %>% summarize(mean_days = mean(DaystoRRI1), 
                                         max_days = max(DaystoRRI1),
                                         min_days = min(DaystoRRI1))

event_group <- time_to_injury %>% filter(EventInjuryYr1 == 1) %>% select(ID, DaystoRRI1)
#subset(event_group, DaystoRRI1 > 365)

ggplot(time_to_injury, aes(x=DaystoRRI1, fill=EventInjuryYr1)) +
  geom_boxplot()
# Looks good !


## ------------------------------------- Survival - Plotting ----------------------------------

summary(discrete[,1:52])
length(unique(discrete$subject_id)) # 299
names(time_to_injury)[1] ="subject_id"
length(unique(time_to_injury$subject_id)) #256

# Attempting to extract one row for each subject_id & 
# match to survival info based on these unique IDs

indices <- which(discrete$subject_id[-1] != discrete$subject_id[-length(discrete$subject_id)])
indices <- append(indices, 442071, 299) # adding in last ID


subject_info <- discrete[c(indices),]  %>% 
  select(subject_id, age, sex, runner_category, bmi_kgm, 
         retrospective_injury_status, 
         prospectively_injured_12_mths,
         num_retrospective_injuries_1_yr, 
         simplified_retrospective_injury_status )
length(unique(subject_info$subject_id)) # 299 :)


demographic_w_survival <- merge(subject_info, time_to_injury, 
                                by = c("subject_id"), all = TRUE)       
head(demographic_w_survival)  
length(unique(demographic_w_survival$subject_id)) 
#306 ! 7 different

# survival with exclusion data
#survival_w_excl <- merge( time_to_injury,  excl_info,
#                          by = c("subject_id"), all = TRUE) 
#head(survival_w_excl)
#View(survival_w_excl)
#survival_w_excl[2]

#survival_w_excl %>% filter(is.na(DaystoRRI1))

#setdiff(excl_info$subject_id,time_to_injury$subject_id) 

#setdiff(excl_info$subject_id,discrete$subject_id)

## ------------------------------------- Survival Edits based on Exclusion Info ---------------

#View(demographic_w_survival)
# Injured prior to/ after testing: P_4125, P_4181, P_4183, P_4261, P_4271, P_4274 (6 total)
# survival and demographic N/A

# Vague time-to-drop-out given: Add according to exclusion information note. 
# P_4005, P_4156, P_4187, P_4191, P_4273, P_4307, P_4314. (7 total)
demographic_w_survival$DaystoRRI1[5] <- 90
demographic_w_survival$EventInjuryYr1[5] <- 0

demographic_w_survival$DaystoRRI1[152] <- 90
demographic_w_survival$EventInjuryYr1[152] <- 0

demographic_w_survival$DaystoRRI1[183] <- 150
demographic_w_survival$EventInjuryYr1[183] <- 0

demographic_w_survival$DaystoRRI1[187] <- 210
demographic_w_survival$EventInjuryYr1[187] <- 0

demographic_w_survival$DaystoRRI1[266] <- 270
demographic_w_survival$EventInjuryYr1[266] <- 0

demographic_w_survival$DaystoRRI1[299] <- 14 # Achilles Tendon 2 Weeks in !
demographic_w_survival$EventInjuryYr1[299] <- 0

demographic_w_survival$DaystoRRI1[305] <- 90
demographic_w_survival$EventInjuryYr1[305] <- 0

# Time given as "shortly after enrollment: Setting to 2 weeks
# P_4213, P_4242, P_4255, P_4291, P_4304. (5 total) Remove then add in ??
demographic_w_survival$DaystoRRI1[208] <- 14
demographic_w_survival$EventInjuryYr1[208] <- 0

demographic_w_survival$DaystoRRI1[236] <- 14
demographic_w_survival$EventInjuryYr1[236] <- 0

demographic_w_survival$DaystoRRI1[249] <- 14
demographic_w_survival$EventInjuryYr1[249] <- 0

demographic_w_survival$DaystoRRI1[284] <- 14
demographic_w_survival$EventInjuryYr1[284] <- 0

demographic_w_survival$DaystoRRI1[296] <- 14
demographic_w_survival$EventInjuryYr1[296] <- 0

# Keep these ^ ??
# Exclude at first - then add in if make a difference **


# Poor responders, No Participant, etc.
# Survival already NA. Demographic shows entries
# Delete entirely?

ggplot(demographic_w_survival, aes(x=DaystoRRI1, fill=EventInjuryYr1)) +
  geom_boxplot()

## ------------------------------------- Break - Finding NAs --------------------------------

# which(is.na(demographic_w_survival$DaystoRRI1), arr.ind=TRUE) == 
#which(is.na(demographic_w_survival$EventInjuryYr1), arr.ind=TRUE)
# length = 50

# which(is.na(demographic_w_survival$age), arr.ind=TRUE)
# length = 16
# same as sex, runner_category, 

# which(is.na(demographic_w_survival$bmi_kgm), arr.ind=TRUE)
# length = 17, 1 additional to previous "P_4239"
# same as retrospective_injury_status, num_retrospective_injuries_1_yr, 
# and simplified_retrospective_injury_status

# which(is.na(demographic_w_survival$prospectively_injured_12_mths)
# length = 57

# where general NAs exist:
na.indices <- which(is.na(demographic_w_survival$EventInjury), arr.ind=FALSE)
demographic_w_survival$subject_id[na.indices]

na.indices.age <- which(is.na(demographic_w_survival$age), arr.ind=FALSE)
demographic_w_survival$subject_id[na.indices.age]
na.indices.bmi <- which(is.na(demographic_w_survival$bmi_kgm), arr.ind=FALSE)
demographic_w_survival$subject_id[na.indices.bmi]
bmi.ind <- demographic_w_survival$subject_id[na.indices.bmi]
age.ind <- demographic_w_survival$subject_id[na.indices.age]
setdiff(bmi.ind,age.ind) 
# = 1!! "P_4239"

setdiff(bmi.ind, basis_coef$subject_id)
# "p_4009" "p_4012" "p_4013" "p_4022" "P_4060" "P_4153" "P_4159"

#extra_sub <- match(extra_sub,demographic_w_survival$subject_id)
#which(is.na(demographic_w_survival$EventInjuryYr1[extra_sub]), arr.ind=TRUE)

## ------------------------------------- Survival - Plotting --------------------------------

summary(demographic_w_survival)

event_group <- demographic_w_survival %>% filter(EventInjuryYr1 == 1) 
control_group <- demographic_w_survival %>% filter(EventInjuryYr1 == 0) 

summary(event_group)
summary(control_group)

# age
ggplot(data = demographic_w_survival, mapping= aes(x= age, fill = EventInjuryYr1)) + 
  geom_bar( ) 

ggplot(data = event_group, mapping= aes(x= age)) + 
  geom_bar( )
ggplot(data = control_group, mapping= aes(x= age)) + 
  geom_bar( ) 

# sex
ggplot(data = demographic_w_survival, mapping= aes(x= sex, fill = EventInjuryYr1)) + 
  geom_bar( )

ggplot(data = event_group, mapping= aes(x= sex)) + 
  geom_bar( )
ggplot(data = control_group, mapping= aes(x= sex)) + 
  geom_bar( )

# runner_category
ggplot(data = demographic_w_survival, mapping = aes(x= runner_category, 
                                                    fill = EventInjuryYr1)) + 
  geom_bar( )

ggplot(data = event_group, mapping = aes(x= runner_category)) + 
  geom_bar( )
ggplot(data = control_group, mapping = aes(x= runner_category)) + 
  geom_bar( )

# weight_kg
#ggplot(data = demographic_w_survival, mapping = aes(x= weight_kg, 
#                                                    fill = EventInjuryYr1)) + 
#  geom_histogram( )


# bmi_kgm
ggplot(data = demographic_w_survival, mapping = aes(x= bmi_kgm, 
                                                    fill = EventInjuryYr1)) + 
  geom_histogram( )

ggplot(data = event_group, mapping = aes(x= bmi_kgm)) + 
  geom_histogram( )
ggplot(data = control_group, mapping = aes(x= bmi_kgm)) + 
  geom_histogram( )

# retrospective_injury_status - -  Questions??
ggplot(data = demographic_w_survival, mapping = aes(x= retrospective_injury_status, 
                                                    fill = EventInjuryYr1)) + 
  geom_bar( ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = event_group, mapping = aes(x= retrospective_injury_status)) + 
  geom_bar( ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(data = control_group, mapping = aes(x= retrospective_injury_status)) + 
  geom_bar( ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# prospectively_injured_12_mths
ggplot(data = demographic_w_survival, mapping = aes(x= prospectively_injured_12_mths, 
                                                    fill = EventInjuryYr1)) + 
  geom_bar( )

ggplot(data = event_group, mapping = aes(x= prospectively_injured_12_mths)) + 
  geom_bar( )
ggplot(data = control_group, mapping = aes(x= prospectively_injured_12_mths)) + 
  geom_bar( )
#correct!

# num_retrospective_injuries_1_yr
ggplot(data = demographic_w_survival, mapping = aes(x= num_retrospective_injuries_1_yr, 
                                                    fill = EventInjuryYr1)) + 
  geom_bar( )

ggplot(data = event_group, mapping = aes(x= num_retrospective_injuries_1_yr)) + 
  geom_bar( )
ggplot(data = control_group, mapping = aes(x= num_retrospective_injuries_1_yr)) + 
  geom_bar( )


## ----------------------------- Survival - Simple Cox Model  -------------------------------
library(lattice)
library(survival)
library(survminer)
library(tidyverse)

# Survival Time - DaysToRRI1
# Status/ Event info - EventInjuryYr1
# All other variables - Xi

summary(demographic_w_survival)

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = as.numeric(demographic_w_survival$DaystoRRI1), 
                    event = as.numeric(as.character(demographic_w_survival$EventInjuryYr1)))
surv_object

#names(demographic_w_survival)

############ Break age into categories!! - make sure groups are even

# p = 0.29 - non-sig, improvement from 0.8
demographic_w_survival <- demographic_w_survival %>% 
  mutate(age_cat = case_when(age <= 36 ~ '18-36', 
                             37 <= age & age <= 42 ~ '37-42', 
                             43 <= age & age <= 49 ~ '43-49', 
                             age >= 50 ~ '50-64'))


#demographic_w_survival <- demographic_w_survival %>% 
#  mutate(age_cat = case_when(age <= 32 ~ '18-32', 
#                             33 <= age & age <= 36 ~ '33-36',
#                             37 <= age & age <= 39 ~ '37-39', 
#                             40 <= age & age <= 42 ~ '40-42',
#                             43 <= age & age <= 45 ~ '43-45', 
#                             46 <= age & age <= 49 ~ '46-49', 
#                             50 <= age & age <= 49 ~ '50-54',
 #                            age >= 55 ~ '55-64'))

## legend.labs = c('18-32', '33-36', '37-39', '40-42',
##'43-45', '46-49', '50-54', '55-64'),

ggplot(data = demographic_w_survival, mapping= aes(x= age_cat)) + 
  geom_bar( )

# stratify the curve depending on "age" n
fit1 <- survfit(surv_object ~ age_cat, data = demographic_w_survival)
summary(fit1)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit1, data = demographic_w_survival, pval = TRUE,
           legend.labs = c('18-36', '37-42', '43-49', '50-64'),
           title = "age_cat")

#  p = 0.29 - non-sig

# stratify the curve depending on "sex"
fit2 <- survfit(surv_object ~ sex, data = demographic_w_survival)
summary(fit2)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit2, data = demographic_w_survival, pval = TRUE,
           legend.labs = c("male","female"),
           title = "sex")
#  p = 0.99 - non-sig

# "runner_category"  
fit3 <- survfit(surv_object ~ runner_category, data = demographic_w_survival)
summary(fit3)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit3, data = demographic_w_survival, pval = TRUE,
           legend.labs = c("novice","recreational"),
           title = "runner_category")
#  p = 0.16 - non-sig

############ Break bmi into categories!! 
demographic_w_survival <- demographic_w_survival %>% 
  mutate(bmi_cat = case_when(bmi_kgm < 22.5 ~ 'Underweight - Normal', 
                             22.5 <= bmi_kgm & bmi_kgm < 25 ~ 'Normal', 
                             bmi_kgm >= 25 ~ 'Normal - Overweight'))

ggplot(data = demographic_w_survival, mapping= aes(x= bmi_cat)) + 
  geom_bar( )

#"bmi_kgm"  
fit4 <- survfit(surv_object ~ bmi_cat, data = demographic_w_survival)
summary(fit4)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit4, data = demographic_w_survival, pval = TRUE,
           legend.labs = c("Underweight - Normal","Normal",
                           "Overweight / Obese"),
           title = "bmi_cat" )
#  p = 0.043 - significant!

#"retrospective_injury_status" 
fit5 <- survfit(surv_object ~ retrospective_injury_status, data = demographic_w_survival)
summary(fit5)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit5, data = demographic_w_survival, pval = TRUE,
           legend.labs = c("never_injured","injured_greater_than_2_yr",
                           "injured_1_to_2_yr","injured_less_than_1_yr"),
           title = "retrospective_injury_status" )
#  p = 0.018 - significant!

# "prospectively_injured_12_mths" 
fit6 <- survfit(surv_object ~ prospectively_injured_12_mths, data = demographic_w_survival)
summary(fit6)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit6, data = demographic_w_survival, pval = TRUE,
           legend.labs = c("not_injured","injured"),
           title = "prospectively_injured_12_mths" )
#  p < 0.0001 - significant!

# "num_retrospective_injuries_1_yr" 
demographic_w_survival$num_retrospective_injuries_1_yr <- 
  as.factor(demographic_w_survival$num_retrospective_injuries_1_yr)
fit7 <- survfit(surv_object ~ num_retrospective_injuries_1_yr, data = demographic_w_survival)
summary(fit7)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit7, data = demographic_w_survival, pval = TRUE, 
           legend.labs = c("0","1","2","3","4"),
           title = "num_retrospective_injuries_1_yr" )
#  p < 0.033 - significant!

# "simplified_retrospective_injury_status"
demographic_w_survival$simplified_retrospective_injury_status <- 
  as.factor(demographic_w_survival$simplified_retrospective_injury_status)
fit8 <- survfit(surv_object ~ simplified_retrospective_injury_status, data = demographic_w_survival)
summary(fit8)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit8, data = demographic_w_survival, pval = TRUE,
           legend.labs = c("no","yes"),
            title = "simplified_retrospective_injury_status")
#  p < 0.036 - significant!


# Significant categories:
fit9 <- survfit(surv_object ~ retrospective_injury_status +
                  prospectively_injured_12_mths +
                  num_retrospective_injuries_1_yr +
                  simplified_retrospective_injury_status + bmi_cat, 
                data = demographic_w_survival)
summary(fit9)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit9, data = demographic_w_survival, pval = TRUE,) 
   #        facet.by = 'retrospective_injury_status')
           
# Fix legend here!

### ----------------------------- Getting left_ankle_abduction data! -------------------------------

## combine survival with demo and funct. 

# remember to make sure to remove these individuals:
# P_4213, P_4242, P_4255, P_4291, P_4304. 

######## Left, Ankle, Abduction 

## Subsetting functional curves:
# Use basis_coef
dim(basis_coef)

left_ankle_abduction <- basis_coef %>% 
  filter(side == "left" & location =="Ankle" & plane_of_motion == "abd") %>% 
  dplyr::select(1,2,4,51:132)

# OR Use discrete
left_ankle_abduction <- discrete %>% 
  filter(side == "left" & location =="Ankle" & plane_of_motion == "abd") %>% 
  dplyr::select(1,2,4,52:153)

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
right_hip_flexion <- discrete %>% 
  filter(side == "right" & location =="Hip" & plane_of_motion == "fle") %>% 
  dplyr::select(1,2,4,52:153)

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
left_knee_rotation <- discrete %>% 
  filter(side == "left" & location =="Knee" & plane_of_motion == "rot") %>% 
  dplyr::select(1,2,4,52:153)

dim(right_hip_flexion)
summary(right_hip_flexion)

## Combining these curves with survival and demographic data
left_knee_rotation_surv <- merge(demographic_w_survival, left_knee_rotation, 
                                by = c("subject_id"), all = TRUE)       
head(left_knee_rotation_surv) 
dim(left_knee_rotation_surv)

### ----------------------------- Functional Plotting  -------------------------------

# columns = 15:115 for discrete, 14:93 for basis_coef
# n = 101 for discrete, 80 for basis coef
stride <- 1:80
# instead of 137
n <- length(unique(left_ank_abd_surv$subject_id))

matplot(stride, t(left_ank_abd_surv[,14:93]), 
        type='l', lty=1, col=rainbow(n),
        main = "Left Ankle Angles - Abduction Plane",
        xlab="basis_coef", ylab="angle")


stride <- 1:80
matplot(stride, t(right_hip_flexion_surv[,14:93]), 
        type='l', lty=1, col=rainbow(n),
        main = "Right Hip Angles - Flexion Plane",
        xlab="basis_coef", ylab="angle")

stride <- 1:101
n <- length(unique(left_knee_rotation_surv$subject_id))

matplot(stride, t(left_knee_rotation_surv[,15:115]), 
        type='l', lty=1, col=rainbow(n),
        main = "Left Knee Angles - Rotation Plane",
        xlab="time", ylab="angle")

### ----------------------------- Getting Coverage - Erjia Code  -------------------------------

library(mgcv)
library(refund)

n <- 306 ## number of subjects
s <- 137 ## number of functional observations per subject
event <- left_ank_abd_surv$EventInjuryYr1 ## 30% of subjects have events observed 
survtime <- left_ank_abd_surv$DaystoRRI1 ## observed time

## transformations on X may be necessary for identifiability in practice 
X <- as.matrix(left_ank_abd_surv[,14:93])
#X <- fpca.face(X)$Yhat ## smooth each curve using fast sandwich smoother (try without for now)
Z <- left_ank_abd_surv$sex## a scalar predictor (can add in age and male)
data_analysis <- data.frame(event, survtime, Z, X = I(X)) ## the dataset 
#rm(event, survtime, X, Z) # removing them from the environment,  ?rm
str(data_analysis)

## create variables related to numerical approximation
### lmat: numerical integration
data_analysis$lmat <- I(matrix(1/s, ncol=s, nrow=nrow(data_analysis)))
### tmat: time indices of functional observations, we assume an equally-spaced grid here 
data_analysis$tmat <- I(matrix(seq(0, 1, len=s), ncol=s, nrow=nrow(data_analysis), byrow=TRUE))

## fit LFCM
fit_lfcm <- gam(survtime ~ Z + s(tmat, by=lmat*X, bs="cr", k=10), weights=event,
                data=data_analysis, family=cox.ph())
# ERROR: invalid type (list) for variable 'X'
# set X as matrix, new error: indefinite penalized likelihood in gam.fit5

## fit AFCM
fit_afcm <- gam(survtime ~ Z + ti(tmat, X, by=lmat, bs=c("cr","cr"), k=c(10,10),
                                  mc=c(FALSE,TRUE)), weights=event, data=data_analysis,
                family=cox.ph())
# Same error
