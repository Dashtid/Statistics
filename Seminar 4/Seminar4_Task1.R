##############################################################################

# Task 1: Measuring blood glucose

# To compare the laboratory facilities of two hospitals, blood samples from 48 patients 
# were analyzed for blood glucose with the equipment in the hospital laboratories for 
# clinical chemistry. The glucose concentrations (in mmol/l) are given in glukosdata.txt

# What conclusions can be drawn from these numbers? How are they best presented and analyzed?

##############################################################################

# --- Preparation --- #

# Reading in necessary libraries
library(irr)
library(tidyr)

# Setting Working Directory
setwd("...")

# ----- DATA ----- #
glukosdata = read.table("glukosdata.txt", dec=",", header=TRUE)
attach(glukosdata)

glukosdata$case = 1:48
glukosdata$diff = HospA - HospB
glukosdata$aver = (HospA + HospB) / 2
attach(glukosdata)

# ---- Visualization ---- #
boxplot(glucose ~ Hospital, data=longdata, # Box-plot
        ylab = "Glucose [mmol/l]")

# ---- Test if different ---- #
shapiro.test(HospA)       # p-value = 0.03493 
shapiro.test(HospB)       # p-value = 0.326
shapiro.test(HospA-HospB) # p-value = 0.326 => parametric test possible

hist(HospA - HospB, breaks=15,
     main = "",
     xlab = "Difference between HospA and HospB")

t.test(HospA, HospB, paired=TRUE)      # p-value = 7.304e-08
wilcox.test(HospA, HospB, paired=TRUE) # p-value = 1.359e-06

# ---- Correlation ---- #
plot(HospB, HospA)
modelCorr = lm(HospA ~ HospB)
summary(modelCorr)

abline(coef(modelCorr), col="red", lwd=2)
abline(0,1,col="blue", lwd=2)
legend("topleft",
       legend = c("Fit", "Optimal"),
       lty=c(1, 1),
       lwd=c(2.5, 2.5),
       col=c("red", "blue"),
       title = "Legend")

# ---- Bland-Altman Plot ----- #
plot(aver, diff,
     ylim = c(-4, 2),
     main = "Bland-Altman-Plot",
     xlab = "Average of HospA and HospB",
     ylab = "Difference between HospA and HospB")
abline(h=mean(diff))
abline(h=mean(diff) + 2 * sd(diff), lty="longdash")
abline(h=mean(diff) - 2 * sd(diff), lty="longdash")

# ---- Drifting deviation ---- #
plot(aver, diff,
     ylim = c(-4,2),
     main = "Bland-Altman-Plot - Drifting deviation",
     xlab = "Average of HospA and HospB",
     ylab = "Difference between HospA and HospB")
abline(h=mean(diff))
abline(h=mean(diff) + 2 * sd(diff), lty="longdash")
abline(h=mean(diff) - 2 * sd(diff), lty="longdash")

c= coef(driftingDev)
abline(c, col="red", lwd=2)

# ---- ICC ----- #
samples = glukosdata[c(1:2)]

icc(samples, model="twoway", type=("agreement"), unit="single")  
# ICC = 0.845 => good reliability

# ---- Linear Model ----- #
longdata = gather(glukosdata, Hospital, glucose, HospA:HospB)
attach(longdata)

glumodel = lmer(glucose ~ 1 + (1|case) + (1|Hospital))
summary(glumodel)
summ(glumodel) # ICC (case) = 0.85  ICC(Hospital) = 0.07
