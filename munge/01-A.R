###########################
# Munge data
#
# This file provides steps to clean and recode data and should automatically load
# if using ProjectTemplate load.project function.
#
# Michael Ching, MD, MPH 
###########################

names(appts)
appts <- appts2
rm(appts2)

######
# Convert Dates and Times into POSIXct class
######

head(appts$Timestamp)
appts$TimestampP <- strptime(appts$Timestamp, "%m/%d/%Y %H:%M:%S")
summary(appts$TimestampP)

head(appts$Date.of.Consult.Request)
appts$Date.of.Consult.Request.P <- strptime(appts$Date.of.Consult.Request, "%m/%d/%Y")
appts$Date.of.Consult.Request.P <- as.POSIXct(appts$Date.of.Consult.Request.P)
summary(appts$Date.of.Consult.Request.P) 

# some are past 30 Sep 2014 (Pua's last day), most likely by 1 or 2 years
wrong_dates <- which(appts$Date.of.Consult.Request.P > as.POSIXct("2014-09-30"))
appts$Date.of.Consult.Request.P[wrong_dates]

# subtract off the number of seconds in a year or two depending on how much off
appts$Date.of.Consult.Request.P[c(51, 54, 96)] <- 
  appts$Date.of.Consult.Request.P[c(51, 54, 96)] - 365*24*60*60 
appts$Date.of.Consult.Request.P[c(96)] <- 
  appts$Date.of.Consult.Request.P[c(96)] - 365*24*60*60 
summary(appts$Date.of.Consult.Request.P)
head(appts$Date.of.Consult.Request.P)

head(appts$Date.and.Time.of.Scheduling)
appts$Date.and.Time.of.Scheduling.P <- strptime(appts$Date.and.Time.of.Scheduling, "%m/%d/%Y %H:%M:%S")
appts$Date.and.Time.of.Scheduling.P <- as.POSIXct(appts$Date.and.Time.of.Scheduling.P)
summary(appts$Date.and.Time.of.Scheduling.P) # Ensure that none are past 30 Sep 2014
head(appts$Date.and.Time.of.Scheduling.P)

head(appts$Date.and.Time.of.Appointment)
appts$Date.and.Time.of.Appointment.P <- strptime(appts$Date.and.Time.of.Appointment, "%m/%d/%Y %H:%M:%S")
appts$Date.and.Time.of.Appointment.P <- as.POSIXct(appts$Date.and.Time.of.Appointment.P)
summary(appts$Date.and.Time.of.Appointment.P) # Some are past 30 Sep 2014
offdate <- which(appts$Date.and.Time.of.Appointment.P > as.POSIXct("2014-10-02"))
appts$Date.and.Time.of.Appointment.P[offdate]
appts$Date.and.Time.of.Appointment.P[offdate] <- appts$Date.and.Time.of.Appointment.P[offdate] - 365*24*60*60
summary(appts$Date.and.Time.of.Appointment.P) # Some are past 30 Sep 2014
head(appts$Date.and.Time.of.Appointment.P)

# Make variable for time from request to time of scheduling
appts$Days.Request.to.Scheduling <- difftime(appts$Date.and.Time.of.Scheduling.P, appts$Date.of.Consult.Request.P, units = "days")
# Nothing should be negative
which(appts$Days.Request.to.Scheduling < 0)
# TODO troubleshoot record 4 and 218. For now, make NA
appts$Days.Request.to.Scheduling[which(appts$Days.Request.to.Scheduling < 0)] <- NA

# Make variable for difference between date of appointment and date of consult request
appts$Days.Request.to.Appointment <- difftime(appts$Date.and.Time.of.Appointment.P, appts$Date.of.Consult.Request.P, units = "days")
# Nothing should be negative
which(appts$Days.Request.to.Appointment < 0)
appts$Days.Request.to.Appointment[which(appts$Days.Request.to.Appointment < 0)]
# TODO troubleshoot record 34 and 218. For now make NA
appts$Days.Request.to.Appointment[which(appts$Days.Request.to.Appointment < 0)] <- NA

# Make variable for time from scheduling to appointment
appts$Days.Scheduling.to.Appointment <- difftime(appts$Date.and.Time.of.Appointment.P, appts$Date.and.Time.of.Scheduling.P, units = "days")
which(appts$Days.Scheduling.to.Appointment < 0)
# TODO troubleshoot record 34 and 141 and 232. Why is it negative -18 days?
appts$Days.Scheduling.to.Appointment[which(appts$Days.Scheduling.to.Appointment < 0)]
# Make NA for now
appts$Days.Scheduling.to.Appointment[which(appts$Days.Scheduling.to.Appointment < 0)] <- NA

# Reorganize the levels for Day of the Week because they are created as alphabetical
appts$ApptWeekday <- appts$Day.of.the.Week.of.Appointment
appts$ApptWeekday <- factor(appts$ApptWeekday, levels(appts$ApptWeekday)[c(2, 4, 5, 3, 1)])
summary(appts$ApptWeekday)

######
# Perform sanity checks on other variables (in range? unusual responses?)
######

# Make patient canceled appointments into NAs and refactor to drop Patient Cancels
summary(appts$Appointment.Missed.or.Kept.)
appts$Appointment.NoShow <- appts$Appointment.Missed.or.Kept.
appts$Appointment.NoShow[which(appts$Appointment.Missed.or.Kept. == "Patient Canceled")] <- NA
appts$Appointment.NoShow <- factor(appts$Appointment.NoShow)
table(appts$Appointment.NoShow, appts$Appointment.Missed.or.Kept., useNA = "if")

## Reason for appointment

table(appts[8])
# I will have to recode most of these to fit into a few categories...
# appts$Reason <- rep("Other", times = length(appts[, 1]))
# appts$Reason[appts[8] == "Autism" | appts[8] == "Pervasive Developmental Disorder"] <- "ASD"
# appts$Reason[appts[8] == "ADHD"] <- "ADHD"
# appts$Reason[appts[8] == "Developmental Delay-Speech"] <- "Developmental Delay-Speech"
# appts$Reason[appts[8] == "Baseline Assessment" | 
#                appts[8] == "Baseline Developmental Assessment" |
#                appts[8] == "Delayed Milestones" |
#                appts[8] == "Developmental Delay-Motor" |
#                appts[8] == "Mental Retardation" |
#                appts[8] == "Other Developmental Delay"] <- "Other Delay"
# table(appts$Reason)
# prop.table(table(appts$Reason))

# Create ASD, ADHD, Speech, Academic/LD, Motor, Behavior (Not ADHD), Medical, variables
# Create ASD, ADHD, Speech, Academic/LD, Motor, Behavior (Not ADHD), Medical, variables

x <- appts[, 8]
appts$Reason_ASD <- rep(0, length(x))
appts$Reason_ASD[grep("[Aa]utis", x)] <- 1
appts$Reason_ASD[grep("Pervasive", x)] <- 1

x <- sub("ADHD)", "ADD)", x)
appts$Reason_ADHD <- rep(0, length(x))
appts$Reason_ADHD[grep("ADHD", x)] <- 1

appts$Reason_speech <- rep(0, length(x))
appts$Reason_speech[grep("Speech", x)] <- 1

appts$Reason_motor <- rep(0, length(x))
appts$Reason_motor[grep("Motor", x)] <- 1

appts$Reason_academic_ld <- rep(0, length(x))
appts$Reason_academic_ld[grep("Academic | Learning", x)] <- 1

appts$Reason_medical <- rep(0, length(x))
appts$Reason_medical[grep("Medical", x)] <- 1

appts$Reason_other_delay <- rep(0, length(x))
appts$Reason_other_delay[grep("Other Dev", x)] <- 1

appts$Reason_other_behavior <- rep(0, length(x))
appts$Reason_other_behavior[setdiff(grep("Behavior", x), 
                                    grep("ADHD", x))] <- 1

appts$Reason_other_unspec <- rep(0, length(x))
appts$Reason_other_unspec[which(rowSums(reasons) == 0)] <- 1

rm(x)

# reasons <- appts[, 26:34]
# rowSums(reasons)
# table(rowSums(reasons))

## Reminder call
str(appts$Reminder.call.made.)

# Blanks become NAs
appts$Reminded <- appts$Reminder.call.made.
appts$Reminded[appts$Reminded == "" | appts$Reminded == "Unknown"] <- NA
appts$Reminded <- factor(appts$Reminded) # drop blank factor
summary(appts$Reminded)

## Age in months
summary(appts$Age) 
# oldest is 22 years, that seems probable

## Previsit.questionnaire.completed
summary(appts$Previsit.questionnaire.completed.)
# convert N/A and "" into NAs
appts$QDone <- appts$Previsit.questionnaire.completed.
appts$QDone[appts$QDone == "" | appts$QDone == "N/A (Follow-up)"] <- NA
appts$QDone <- factor(appts$QDone)
summary(appts$QDone)

## Gender
summary(appts$Gender) # looks ok for now

## POC
table(appts$POC)
appts$POCreformat <- toupper(appts$POC)
table(appts$POCreformat)

# need to be consolidated into:

## Family vs Peds
appts$Peds.FP <- appts$POCreformat
# All that start with N into NA
appts$Peds.FP[grep("^N", appts$Peds.FP)] <- NA
# USAF to NA
appts$Peds.FP[grep("^USAF", appts$Peds.FP)] <- NA
# HP to Peds
appts$Peds.FP[grep("HP", appts$Peds.FP)] <- "peds"
# HF to Family
appts$Peds.FP[grep("HF", appts$Peds.FP)] <- "family"
# Family to Family
appts$Peds.FP[grep("FAMILY", appts$Peds.FP)] <- "family"
# Pediatrics to Pediatrics
appts$Peds.FP[grep("PED", appts$Peds.FP)] <- "peds"
## TODO Continue recoding to peds or family
appts$Peds.FP <- factor(appts$Peds.FP)
summary(appts$Peds.FP)

## Schofield, Makalapa, KBay, WOMH, TAMC
appts$clinic <- appts$POCreformat
# All that start with N into NA
appts$clinic[grep("^N", appts$clinic)] <- NA
# USAF to NA
appts$clinic[grep("^USAF", appts$clinic)] <- NA
# Grep the HC and make into Hickam
appts$clinic[grep("HC", appts$clinic)] <- "hickam"
# Grep the SBs and make into Schofield
# Start with PH goes to Makalapa
appts$clinic[grep("^PH", appts$clinic)] <- "makalapa"
# Start with KB to KBay
appts$clinic[grep("^KB", appts$clinic)] <- "kbay"
# P_2 to TAMC
appts$clinic[grep("P(.)2", appts$clinic)] <- "tripler"
# F03, F04 to TAMC
appts$clinic[grep("F03", appts$clinic)] <- "tripler"
appts$clinic[grep("F04", appts$clinic)] <- "tripler"
# F05 to Schofield
appts$clinic[grep("F05", appts$clinic)] <- "schofield"
# SB to Schofield
appts$clinic[grep("^SB", appts$clinic)] <- "schofield"
# GECKO to Schofield
appts$clinic[grep("GECKO$", appts$clinic)] <- "schofield"
# HONU to TAMC
appts$clinic[grep("HONU$", appts$clinic)] <- "tripler"
table(appts$clinic)
## TODO Continue recoding clinic names

## Branch of Service
table(appts$Branch.of.Service)
# No change needed