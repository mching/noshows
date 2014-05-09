###########################
# Exploratory data
#
# This file provides steps to clean and recode data and should automatically load
# if using ProjectTemplate load.project function.
#
# Michael Ching, MD, MPH 
###########################

names(appts)

# Day.of.the.Week.of.Appointment
plot(appts$ApptWeekday)
table <- table(appts$ApptWeekday)
binom.confint(table, n = sum(table), method = "exact")

# Reminder call
plot(appts$Reminded)
table <- table(appts$Reminded)
binom.confint(x = table[2], n = sum(table), method = "exact")

# No show rate
plot(appts$Appointment.NoShow)
table <- table(appts$Appointment.NoShow)
binom.confint(x = table[2], n = sum(table), method = "exact")

# Reason for appt
table <- table(appts$Reason)
binom.confint(table, n = sum(table), method = "exact")

# Reminded
# No show rate
plot(appts$Reminded)
table <- table(appts$Reminded)
binom.confint(x = table[2], n = sum(table), method = "exact")

# Age
boxplot(appts$Age)
hist(appts$Age)
boxplot(log(appts$Age))
hist(log(appts$Age))
summary(appts$Age)

# Q done
plot(appts$QDone)
table <- table(appts$QDone)
binom.confint(x = table[2], n = sum(table), method = "exact")

# Peds or FP
table <- table(appts$Peds.FP)
table
binom.confint(x = table[2], n = sum(table), method = "exact")

# Clinic location
table <- table(appts$clinic)
table
binom.confint(table, n = sum(table), method = "exact")

# Clinic by Peds or FP
table <- table(appts$clinic, appts$Peds.FP)
table

