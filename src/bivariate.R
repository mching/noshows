# Bivariate analyses

names(appts)

# Weekday by No-show rate
table <- table(appts$ApptWeekday, appts$Appointment.NoShow)
table
binom.confint(table[,2], margin.table(table, 1), method = "exact")
fisher.test(table)

# Reminded by No-show rate
table <- table(Reminded = appts$Reminded, NoShow = appts$Appointment.NoShow)
table
binom.confint(table[,2], margin.table(table, 1), method = "exact")
fisher.test(table)

epi.2by2(table) # need to fix the table layout
