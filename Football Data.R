## Football Player Data from Andrew NUIG

football <- read.csv("/Users/rachelhunt/Documents/GitHub/Survival-models-with-Functional-Predictors/injury_football.csv")

View(football)
dim(football)
names(football)
summary(football)

# --------------------------  Data Description --------------------------------------

## Description:
# id - Player number (1-23)
# date - date of data point
# phase - baseline, camp, off-season or season 
# numberPhase - 1,2,3 or 4
# FORT - TAC Total Anti-Oxidant Capacity (oxidative stress biomarker)*             
# FORD - HPx hydroperoxides (oxidative stress biomarker)*            
# CRP - C-Reactive Protein? (Google) Check for infection
# OSI - oxidative stress index                
### flightTime 
### peakPower           
### jumpHeight
### dass                   
# TD - Total Distance (m) (external training load marker)
# HID - high intensity distance (m) (external training load marker)
### recQ                   
# coldFlag - all NA ??        
# fatigue - (key internal load marker, self reported) 
# sleep - (key internal load marker, self reported)           
# soreness - of muscles (key internal load marker, self reported)
# age - player age (19-23)                
# weight - player weight (175.5 - 322.7)           
# PL - PlayerLoad (external training load marker)               
# EWMA7 - Playerload exponentially weighted moving average (7 days)
# EWMA21 - Playerload exponentially weighted moving average (21 days)               
### time                   
# Date.of.first.symptoms - from 17th July to 24th September
# Position - position of play            
# Onset.of.Injury - Acute, Insidious, or Overuse
# Diagnosis - description of diagnosis
# Contact.Noncontact - C or NC   
# position - position of play (groups)
# ACWR - Acute Chronic Workload Ratio (Google)
# injured - binary 0 or 1            
# illness - binary 0 or 1
# game - (real) game or training 
# inj.day - time-to-event survival variable? - each phase different??    *     
# inj.cat - Healthy/ Injured (survival event variable)**


# -------------------------- Data Wrangling -------------------------------------

football$id <- as.factor(football$id)
football$date <- as.Date(football$date, format="%Y-%m-%d")
football$phase <- as.factor(football$phase)
football$numberPhase <- as.factor(football$numberPhase)
# flightTime, peakPower, jumpHeight, recQ??
football <- select(football, -"coldFlag")
football$fatigue <- as.factor(football$fatigue)
football$sleep <- as.factor(football$sleep)
football$soreness <- as.factor(football$soreness)
## time ?? 
football$Date.of.first.symptoms <- as.Date(football$Date.of.first.symptoms, 
                                           format = "%d/%m/%Y")
football$Position <- as.factor(football$Position)
football$Onset.of.Injury <- as.factor(football$Onset.of.Injury)
football$Contact.Noncontact <- as.factor(football$Contact.Noncontact)
football$position <- as.factor(football$position)
football$injured <- as.factor(football$injured)
football$illness <- as.factor(football$illness)
football$game <- as.factor(football$game)
football$inj.cat <- as.factor(football$inj.cat)

# -------------------------- Visualisations -------------------------------------

ggplot(data = football, mapping = aes(x= id)) + 
  geom_bar( ) + xlab("Player ID")

ggplot(data =football, mapping = aes(x= date)) + 
  geom_histogram( ) + xlab("Date of Data Recorded")

ggplot(data = football, mapping = aes(x= phase)) + 
  geom_bar( ) + xlab("Phase of Training")

ggplot(data = football, mapping = aes(x= numberPhase)) + 
  geom_bar( ) + xlab("Phase of Training (Number)")
# Q phase and numberPhase the same?

ggplot(data =football, mapping = aes(x= FORT)) + 
  geom_histogram( ) + xlab("FORT - oxidative stress biomarker")

ggplot(data =football, mapping = aes(x= FORD)) + 
  geom_histogram( ) + xlab("FORD - oxidative stress biomarker")

ggplot(data =football, mapping = aes(x= CRP)) + 
  geom_histogram( ) + xlab("C-Reactive Protein?")

ggplot(data =football, mapping = aes(x= OSI)) + 
  geom_histogram( ) + xlab("oxidative stress index")

# what is this?
ggplot(data =football, mapping = aes(x= flightTime)) + 
  geom_histogram( )

# what is this?
ggplot(data =football, mapping = aes(x= peakPower)) + 
  geom_histogram( )

# what is this?
ggplot(data =football, mapping = aes(x= jumpHeight)) + 
  geom_histogram( )

ggplot(data =football, mapping = aes(x= TD)) + 
  geom_histogram( ) + xlab("Total Distance (m) ")


ggplot(data =football, mapping = aes(x= recQ)) + 
  geom_histogram( ) + xlab("high intensity distance (m)")

# what is this?
ggplot(data =football, mapping = aes(x= recQ)) + 
  geom_histogram( )

ggplot(data = football, mapping = aes(x= fatigue)) + 
  geom_bar( ) + xlab("Fatigue Score")

ggplot(data = football, mapping = aes(x= sleep)) + 
  geom_bar( ) + xlab("Sleep Score")

ggplot(data = football, mapping = aes(x= soreness)) + 
  geom_bar( ) + xlab("Muscle Soreness Score")

## Map for unique player number*
ggplot(data =football, mapping = aes(x= age)) + 
  geom_histogram( ) + xlab("Player Age")

## Map for unique player number?
ggplot(data =football, mapping = aes(x= weight)) + 
  geom_histogram( ) + xlab("Player Weight")

ggplot(data =football, mapping = aes(x= PL)) + 
  geom_histogram( ) + xlab("PlayerLoad")

ggplot(data =football, mapping = aes(x= EWMA7)) + 
  geom_histogram( ) + 
  xlab("Playerload exponentially weighted moving average (7 days)")

ggplot(data =football, mapping = aes(x= EWMA21)) + 
  geom_histogram( ) + 
  xlab("Playerload exponentially weighted moving average (21 days)")

ggplot(data =football, mapping = aes(x= time)) + 
  geom_histogram( )
# in reference to?

ggplot(data =football, mapping = aes(x= Date.of.first.symptoms)) + 
  geom_histogram( ) + xlab("Date of First Symptoms")

ggplot(data = football, mapping = aes(x= Position)) + 
  geom_bar( ) + xlab("Position of Play") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = football, mapping = aes(x=Onset.of.Injury)) + 
  geom_bar( ) + xlab("Onset of Injury")

ggplot(data = football, mapping = aes(x=Contact.Noncontact)) + 
  geom_bar( ) + xlab("Contact or Non-Contact Injury")

ggplot(data = football, mapping = aes(x= position)) + 
  geom_bar( ) + xlab("Position of Play (Groups)") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data =football, mapping = aes(x= ACWR)) + 
  geom_histogram( ) + xlab("Acute Chronic Workload Ratio (?)")

ggplot(data = football, mapping = aes(x=injured)) + 
  geom_bar( ) + xlab("Injured")

ggplot(data = football, mapping = aes(x=illness)) + 
  geom_bar( ) + xlab("Illness")

ggplot(data = football, mapping = aes(x=game)) + 
  geom_bar( ) + xlab("Game")

ggplot(data =football, mapping = aes(x= inj.day)) + 
  geom_histogram( ) + xlab("Injury Day (time-to-event variable?)")

ggplot(data = football, mapping = aes(x=inj.cat)) + 
  geom_bar( ) + xlab("Injury Category (survival event variable")


# Ask Shirin
ggplot(football, aes(x=inj.day, fill=inj.cat)) +
  geom_boxplot()

# --------------------------  -------------------------------------


