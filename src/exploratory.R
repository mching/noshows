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

