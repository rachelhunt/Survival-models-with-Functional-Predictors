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

## ------------------- Survival - Plotting, see longer doc for more ----------------

summary(demographic_w_survival)

event_group <- demographic_w_survival %>% filter(EventInjuryYr1 == 1) 
control_group <- demographic_w_survival %>% filter(EventInjuryYr1 == 0) 

summary(event_group)
summary(control_group)

# age
ggplot(data = demographic_w_survival, mapping= aes(x= age, fill = EventInjuryYr1)) + 
  geom_histogram( ) + facet_wrap(~EventInjuryYr1) + 
  scale_fill_discrete(labels=c('Injured', 'Not Injured')) 

# sex
ggplot(data = demographic_w_survival, mapping= aes(x= sex, fill = EventInjuryYr1)) + 
  geom_bar( )  + facet_wrap(~EventInjuryYr1) + 
  scale_fill_discrete(labels=c('Injured', 'Not Injured')) 

# runner_category
ggplot(data = demographic_w_survival, mapping = aes(x= runner_category, 
                                                    fill = EventInjuryYr1)) + 
  geom_bar( ) + facet_wrap(~EventInjuryYr1) + 
  scale_fill_discrete(labels=c('Injured', 'Not Injured')) +
  theme(axis.text.x = element_text(angle = 45))

# bmi_kgm
ggplot(data = demographic_w_survival, mapping = aes(x= bmi_kgm, 
                                                    fill = EventInjuryYr1)) + 
  geom_histogram( ) + facet_wrap(~EventInjuryYr1) + 
  scale_fill_discrete(labels=c('Injured', 'Not Injured'))

# retrospective_injury_status
ggplot(data = demographic_w_survival, mapping = aes(x= retrospective_injury_status, 
                                                    fill = EventInjuryYr1)) + 
  geom_bar( ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  scale_fill_discrete(labels=c('Injured', 'Not Injured'))

# prospectively_injured_12_mths
ggplot(data = demographic_w_survival, mapping = aes(x= prospectively_injured_12_mths, 
                                                    fill = EventInjuryYr1)) + 
  geom_bar( ) + facet_wrap(~EventInjuryYr1) + 
  scale_fill_discrete(labels=c('Injured', 'Not Injured')) +
  theme(axis.text.x = element_text(angle = 45)) 
# Correct!

# num_retrospective_injuries_1_yr
ggplot(data = demographic_w_survival, mapping = aes(x= num_retrospective_injuries_1_yr, 
                                                    fill = EventInjuryYr1)) + 
  geom_bar( ) +  scale_fill_discrete(labels=c('Injured', 'Not Injured'))

## ----------------------------- Survival - Simple Cox Model  --------------------------
library(lattice)
library(survival)
library(survminer)
library(tidyverse)

# Survival Time - DaysToRRI1
# Status/ Event info - EventInjuryYr1
# All other variables - Xi

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = as.numeric(demographic_w_survival$DaystoRRI1), 
                    event = as.numeric(as.character(
                      demographic_w_survival$EventInjuryYr1)))
surv_object

## Breaking age into categories - make sure groups are even in size
demographic_w_survival <- demographic_w_survival %>% 
  mutate(age_cat = case_when(age <= 36 ~ '18-36', 
                             37 <= age & age <= 42 ~ '37-42', 
                             43 <= age & age <= 49 ~ '43-49', 
                             age >= 50 ~ '50-64'))
ggplot(data = demographic_w_survival, mapping= aes(x= age_cat)) + 
  geom_bar( )

# stratify the curve depending on "age" 
fit1 <- survfit(surv_object ~ age_cat, data = demographic_w_survival)
summary(fit1)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit1, data = demographic_w_survival, pval = TRUE,
           pval.method = TRUE,
           legend.labs = c('18-36', '37-42', '43-49', '50-64'),
           title = "age_cat")
# p = 0.29 - non-sig

# stratify the curve depending on "sex"
fit2 <- survfit(surv_object ~ sex, data = demographic_w_survival)
summary(fit2)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit2, data = demographic_w_survival, pval = TRUE,
           pval.method = TRUE,
           legend.labs = c("male","female"),
           title = "sex")
#  p = 0.96 - non-sig

# "runner_category"  
fit3 <- survfit(surv_object ~ runner_category, data = demographic_w_survival)
summary(fit3)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit3, data = demographic_w_survival, pval = TRUE,
           pval.method = TRUE,
           legend.labs = c("novice","recreational"),
           title = "runner_category")
#  p = 0.16 - non-sig

## Break bmi into categories!! 
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
           pval.method = TRUE,
           legend.labs = c("Underweight - Normal","Normal",
                           "Overweight / Obese"),
           title = "bmi_cat" )
#  p = 0.042 - significant!

# "num_retrospective_injuries_1_yr" 
demographic_w_survival$num_retrospective_injuries_1_yr <- 
  as.factor(demographic_w_survival$num_retrospective_injuries_1_yr)
fit7 <- survfit(surv_object ~ num_retrospective_injuries_1_yr, 
                data = demographic_w_survival)
summary(fit7)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit7, data = demographic_w_survival, pval = TRUE, 
           pval.method = TRUE,
           legend.labs = c("0","1","2","3","4"),
           title = "num_retrospective_injuries_1_yr" )
#  p < 0.033 - significant!

# "simplified_retrospective_injury_status"
demographic_w_survival$simplified_retrospective_injury_status <- 
  as.factor(demographic_w_survival$simplified_retrospective_injury_status)
fit8 <- survfit(surv_object ~ simplified_retrospective_injury_status, 
                data = demographic_w_survival)
summary(fit8)
# Plotting Kaplan-Meier Survival Curves
ggsurvplot(fit8, data = demographic_w_survival, pval = TRUE,
           pval.method = TRUE,
           legend.labs = c("no","yes"),
           title = "simplified_retrospective_injury_status")
#  p < 0.036 - significant!

### ------------------- Functional data Extraction! -----------------------------

## combine survival with demo and funct. 
## Three different plane/angle/side combinations


######## Left, Ankle, Abduction 

## Subsetting functional curves:
# Use basis_coef
dim(basis_coef)

left_ankle_abduction <- basis_coef %>% 
  filter(side == "left" & location =="Ankle" & plane_of_motion == "abd") %>% 
  dplyr::select(1,2,4,51:132)

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

matplot(stride, t(left_ank_abd_surv[,15:94]), 
        type='l', lty=1, col=rainbow(n),
        main = "Left Ankle Angles - Abduction Plane",
        xlab="basis_coef", ylab="angle")


stride <- 1:80
n <- length(unique(right_hip_flexion_surv$subject_id))
matplot(stride, t(right_hip_flexion_surv[,15:94]), 
        type='l', lty=1, col=rainbow(n),
        main = "Right Hip Angles - Flexion Plane",
        xlab="basis_coef", ylab="angle")

stride <- 1:80
n <- length(unique(left_knee_rotation_surv$subject_id))
matplot(stride, t(left_knee_rotation_surv[,15:94]), 
        type='l', lty=1, col=rainbow(n),
        main = "Left Knee Angles - Rotation Plane",
        xlab="time", ylab="angle")

## ----------------------------- Apply Erjia Cui Method --------------------------

