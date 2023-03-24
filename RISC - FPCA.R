# Work on RISC Data - started 24/03/2023
# R version  4.2.3 (2023-03-15) -- "Shortstop Beagle"

## ---------------------------- Data Pre-Processing ------------------------------

library(ggplot2)
library(dplyr)
## Loading in the data 
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
time_to_injury$EventInjuryYr1 = ifelse(time_to_injury$DaystoRRI1 == 365, '0', '1')
time_to_injury$EventInjuryYr1 <- as.factor(time_to_injury$EventInjuryYr1)

## Extracting one row for each subject_id & 
# match to survival info based on these unique IDs
indices <- which(discrete$subject_id[-1] != discrete$subject_id[-length(
  discrete$subject_id)])
indices <- append(indices, 442071, 299) # adding in last ID

subject_info <- discrete[c(indices),]  %>% 
  select(subject_id, age, sex, runner_category, bmi_kgm, 
         retrospective_injury_status, 
         prospectively_injured_12_mths,
         num_retrospective_injuries_1_yr, 
         simplified_retrospective_injury_status )

# Creating Survival dataset with some clinical info
demographic_w_survival <- merge(subject_info, time_to_injury, 
                                by = c("subject_id"), all = TRUE)       
head(demographic_w_survival)  
length(demographic_w_survival)

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
# Leaving out 'shortly after enrolment' subjects

ggplot(demographic_w_survival, aes(x=DaystoRRI1, fill=EventInjuryYr1)) +
  geom_boxplot() + scale_fill_discrete(labels=c('Injured', 'Not Injured')) +
  labs(title="Event - Injury in 12 months")

## Creating age_cat and bmi_cat variables 
demographic_w_survival <- demographic_w_survival %>% 
  mutate(age_cat = case_when(age <= 36 ~ '18-36', 
                             37 <= age & age <= 42 ~ '37-42', 
                             43 <= age & age <= 49 ~ '43-49', 
                             age >= 50 ~ '50-64'))
demographic_w_survival <- demographic_w_survival %>% 
  mutate(bmi_cat = case_when(bmi_kgm < 22.5 ~ 'Underweight - Normal', 
                             22.5 <= bmi_kgm & bmi_kgm < 25 ~ 'Normal', 
                             bmi_kgm >= 25 ~ 'Normal - Overweight'))

### Functional Data Extraction
library(fda)
## combine demo and survival with functional
## Choose one plane/angle/side to start

######## Right, Hip, Flexion
## using basis_coef data
#dim(basis_coef)
#right_hip_flexion <- basis_coef %>% 
#  filter(side == "right" & location =="Hip" & plane_of_motion == "fle") %>% 
#  dplyr::select(1,2,4,51:132)

## OR Use discrete
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

time <- 1:101
n <- length(unique(right_hip_flexion_surv$subject_id))
matplot(time, t(right_hip_flexion_surv[,17:117]), 
        type='l', lty=1, col=rainbow(n),
        main = "Right Hip Angles - Flexion Plane",
        xlab="time point", ylab="angle")

## Functional Object (if needed later)
#F.obj.hip <- Data2fd(argvals = time, y = t(right_hip_flexion_surv[,15:115]))
#W_mean.hip <- mean.fd(F.obj.hip)  
#W_sd.hip <- std.fd(F.obj.hip)

## ------------------ Functional Principal Components Analysis -------------------

str(right_hip_flexion_surv)


