# --- Preparation --- #

# Forcing install of effsize package
if(!require(effsize)){install.packages("effsize")}

# Reading in necessary libraries
library(effsize)
library(irr)
library(MASS)

# Setting Working Directory
setwd("...")

# Creating two data sets
kidneydata_raw = read.delim("kidneydata.txt")
kidneydata_altered = read.delim("kidneydata.txt")

# ------- Headers ------- #
# X: Patient ID
# before: Measurement before contrast injection
# after: Measurement after contrast injection


# The measurements before and after is in a scale between 1-5
# ---------------------------- #
# 1: tumor certainly not present  
# 2: tumor probably not present
# 3: inconclusive
# 4: tumor probably present
# 5: tumor certainly present
# ---------------------------- #

# Quick look at the data
kidneydata_raw

# ------- Changing values ------- #

# What we are going to do is changing all the values that is 5 to 1 and
# changing all the values that are 4 to 2 since these are corresponding levels 
# of certainty

# Changing all the values in BEFORE

for (i in 1:length(kidneydata_altered$before)) {
  
  if (kidneydata_altered$before[i] == 5) { 
    kidneydata_altered$before[i] = 1 }
  
  else if (kidneydata_altered$before[i] == 4) { 
    kidneydata_altered$before[i] = 2 }
}

# Changing all the values in AFTER

for (i in 1:length(kidneydata_altered$after)) {
  
  if (kidneydata_altered$after[i] == 5) { 
    kidneydata_altered$after[i] = 1 }
  
  else if (kidneydata_altered$after[i] == 4) { 
    kidneydata_altered$after[i] = 2 }
}

# --- Comparison for sanity check --- #

# The values from BEFORE the contrast injection
kidneydata_raw$before
kidneydata_altered$before

# # The values from AFTER the contrast injection
kidneydata_raw$after
kidneydata_altered$after


# ---- Creating histograms --- #

# This histogram has the following ordinal ranking
# ---------- #
# 1: Tumor certainly present or not present
# 2: Tumor maybe present or not present
# 3: Inconclusive
# ---------- #


# -- BEFORE -- #
hist(kidneydata_altered$before,
     main = paste("Tumor diagnosis BEFORE contrast injection"),
     breaks = c(0,1,2,3), 
     ylim = c(0, 25),
     xlab = "Ranging from 0 being very confident and 3 being inconclusive" )

# -- AFTER -- #
hist(kidneydata_altered$after,
     main = paste("Tumor diagnosis AFTER contrast injection"),
     breaks = c(0,1,2,3), 
     ylim = c(0, 25),
     xlab = "Ranging from 0 being very confident and 3 being inconclusive" )


# Changing into long format
longdata = gather(kidneydata_altered, contrast, values, before:after)
attach(longdata)

# --- Wilcoxon signed-rank test  --- #
wilcox.test(kidneydata_altered$after, kidneydata_altered$before, paired=TRUE, data=kidneydata_altered)


