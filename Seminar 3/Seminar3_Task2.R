# --- Preparation --- #

# Reading in necessary libraries
library(MASS)
library(rpart)
library("survival")
library("survminer")
library(dplyr)

# Setting Working Directory
setwd("...")

# ---- TASK 2 ---- #

# Reading in data
data(stagec) # stagec
attach(stagec)

# Data ploidy states
data_diploid = stagec[stagec$ploidy=="diploid",]
data_tetraploid = stagec[stagec$ploidy=="tetraploid",]
data_aneuploid = stagec[stagec$ploidy=="aneuploid",]

data_diploid_prog = data_diploid[data_diploid$pgstat==1,]
data_tetraploid_prog = data_tetraploid[data_tetraploid$pgstat==1,]
data_aneuploid_prog = data_aneuploid[data_aneuploid$pgstat==1,]

# Data grades
table(grade)
xtabs(~grade + ploidy)
xtabs(~grade + ploidy + pgstat)
table(gleason)
xtabs(~gleason + ploidy)
xtabs(~gleason + ploidy + pgstat)
xtabs(~gleason + mean(pgtime) + pgstat)

with(stagec, tapply(pgtime, list(ploidy=ploidy), median) )
with(stagec, tapply(pgtime, list(grade=grade,ploidy=ploidy), median) )
with(stagec, tapply(pgtime, list(gleason=gleason,pgstat=pgstat), median) )

# Data age
with(stagec, tapply(age, list(pgstat=pgstat), mean) )
with(stagec, tapply(age, list(pgstat=pgstat), sd) )
with(stagec, tapply(age, list(ploidy=ploidy), median) )

with(stagec, tapply(age, list(ploidy=ploidy,pgstat=pgstat), mean) )

# ---------------------------------------------------------------------------------#

# (A) Data presentation
c1 = rgb(173,216,230,max = 255, alpha = 200, names = "lt.blue")
c2 = rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

ax = 0:18

# ---- Histogram and Box plot ---- #
hist1=hist(pgtime,breaks=18,
     xlab="Time [years]",
     main="Time to disease progression")
par(mfrow=c(2,2))

# Plot 1
hist_1 = hist(pgtime, breaks=ax, plot=FALSE)
hist_2 = hist(stagec[stagec$pgstat==1,]$pgtime, breaks=ax, plot=FALSE)

plot(hist_1,
     col = c1,
     ylim = c(0, 25),
     xlab = "Time [years]",
     main = "Time to disease progression")

plot(hist_2,col="blue",add=TRUE)

legend("topright",
       legend = c("All","Patients with progression"),
       lty = c(1, 1),
       lwd = c(2.5, 2.5),
       col = c(c1,"blue"),
       title = "Legend")

# Boxplot all
boxplot(pgtime ~ ploidy,
        ylim = c(0, 18),
        ylab = "Time [years]", 
        xlab ="Ploidy state",
        main = "All patients")

# Bloxplot Progression
boxplot(pgtime[pgstat==1], 
        ylab = "Time [years]",
        xlab = "",
        main = "Time to disease progression")

# Bloxplot Ploidy All
boxplot(pgtime ~ ploidy,data = stagec[pgstat==1,], 
        ylim = c(0,18),
        ylab = "Time [years]", 
        xlab ="Ploidy State",
        main = "Patients with progression")

# Bloxplot Ploidy Progression
boxplot(stagec[stagec$pgstat==1,]$pgtime ~ stagec[stagec$pgstat==1,]$ploidy,
        ylab = "Time [years]", 
        xlab = "Ploidy State",
        main = "Time to disease progression")

# Plot 2
histDi_1 = hist(data_diploid$pgtime, breaks=ax, plot=FALSE)
histDi_2 = hist(data_diploid[data_diploid$pgstat==1,]$pgtime, breaks=ax, plot=FALSE)

plot(histDi_1,
     col = c1,
     ylim = c(0,25),
     xlab = "Time [years]",
     main = "Time to disease progression - Diploid")

plot(histDi_2, col="blue", add=TRUE)

legend("topright",
       legend = c("All","Patients with progression"),
       lty = c(1, 1),
       lwd = c(2.5, 2.5),
       col = c(c1,"blue"),
       title = "Legend")

# Plot 3
histTe_1 = hist(data_tetraploid$pgtime, breaks=ax, plot=FALSE)
histTe_2 = hist(data_tetraploid[data_tetraploid$pgstat==1,]$pgtime, breaks=ax, plot=FALSE)

plot(histTe_1,
     col = c1,
     ylim = c(0, 25),
     xlab = "Time [years]",
     main = "Time to disease progression - Tetraploid")

plot(histTe_2, col="blue", add=TRUE)

legend("topright",
       legend = c("All", "Patients with progression"),
       lty = c(1, 1),
       lwd = c(2.5, 2.5),
       col = c(c1, "blue"),
       title = "Legend")

# Plot 4
histAn_1 = hist(data_aneuploid$pgtime, breaks=ax, plot=FALSE)
histAn_2 = hist(data_aneuploid[data_aneuploid$pgstat==1,]$pgtime, breaks=ax, plot=FALSE)

plot(histAn_1,
     col = c1,
     ylim = c(0, 25),
     xlab = "Time [years]",
     main = "Time to disease progression - Aneuploid")

plot(histAn_2,col="blue",add=TRUE)

legend("topright",
       legend = c("All", "Patients with progression"),
       lty = c(1,1),
       lwd = c(2.5, 2.5),
       col = c(c1, "blue"),
       title = "Legend")

# ---------------------------------------------------------------------------------#

# (B) Analyze progression time

# ---- Survival Analysis ----
survival_data = Surv(pgtime, pgstat)

survival_fit = survfit(survival_data ~ ploidy, data=stagec)
survival_fit

# Plot survival probability
ggsurvplot(survival_fit,
           conf.int = TRUE,
           risk.table = TRUE,         # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata",       # Change line type by groups
           surv.median.line = "hv",   # Specify median survival
           ggtheme = theme_bw(),      # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF", "red"))

# Plot cumulative hazard
ggsurvplot(survival_fit,
           conf.int = TRUE,
           linetype = "strata",  # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF", "red"),
           fun="event")

# Summary Difference of groups
surv_diff = survdiff(survival_data ~ ploidy, data = stagec)
surv_diff

# Linear regression test

# ---------------------------------------------------------------------------------#

# (C) Other factors

# Repeat this analysis taking into account also the age and the histopathological grading. 
cox1 = coxph(survival_data ~ ploidy, data = stagec)
cox1
summary(cox1)

cox2 = coxph(survival_data ~ ploidy + age + grade + gleason, data = stagec)
summary(cox2)

# Plot Grade
fit_grade = survfit(survival_data ~ grade, data=stagec)

ggsurvplot(fit_grade,
           xlab = "Time [Years]",
           conf.int = TRUE,
           linetype = "strata",  # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF", "red","green"),
           fun = "event")

ggsurvplot(fit_grade,
           xlab = "Time [Years]",
           risk.table = TRUE,
           conf.int = TRUE,
           linetype = "strata",  # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF", "red","green"))

# Plot Gleason
fit_gleason = survfit(survival_data ~ gleason, data=stagec)

ggsurvplot(fit_gleason,
           xlab = "Time [Years]",
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("red","#FFCC00","#FFFF33","green","#336600","#66FFFF","#0066CC","#663399"),
           fun = "event")

ggsurvplot(fit_gleason,
           xlab = "Time [Years]",
           ggtheme = theme_bw(),   # Change ggplot2 theme
           risk.table = TRUE,      # Add risk table
           palette = c("red", "#FFCC00", "#FFFF33", "green", "#336600", "#66FFFF", "#0066CC", "#663399"))

# Plot Age groups
# Groups: 1 (46 - 55) => , 2 (56 - 65), 3 (66 - 75)

stagec$age_group = findInterval(age, c(46, 56, 66))
fit_ageGroup = survfit(survival_data ~ age_group, data=stagec)

ggsurvplot(fit_ageGroup,
           xlab = "Time [Years]",
           conf.int = TRUE,
           linetype = "strata",  # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF", "red"),
           fun="event")

ggsurvplot(fit_ageGroup,
           xlab = "Time [Years]",
           risk.table = TRUE,
           conf.int = TRUE,
           linetype = "strata",  # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF", "red"))

