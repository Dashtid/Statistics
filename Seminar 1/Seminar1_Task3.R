# --- Preparation --- #

library(MASS)
library(ggpubr)
library(dbplyr)

# Setting Working Directory
# setwd("/Den stora KTH mappen/CM2003 - Statistics in Medical Engineering/Seminar 1")      # Laptop
setwd("/Skola/KTH/CM2009 - Statistics in Medical Engineering/Seminarium/Seminar 1/Data")   # Desktop

# --- Main --- #

# Reading in data and attaching the headers to corresponding column
data = read.table("concdata.txt", header=TRUE, sep=",") 
attach(data)
before = c1; ##concentration before installation
after = c2; ## concentration after installation

# Visualize data with a boxplot
boxplot (before, after, names = c("Before", "After"), ylab="Concentration [ppm]");#box plot

# --- Tests --- #

# Paired Welch two sample t-test
t.test(before, after, paired=TRUE)

# Paired Wilcoxon Rank Sum test
wilcox.test(before, after, paired=TRUE)

# --- Distributions --- #

# Before
B = density(before)
plot(B,main="Density function for before data")

# After
A = density(after)
plot(A, main="Density function for after data")

# Differences
diff = before - after
densdiff = density(diff)
plot(densdiff, main="Density function for differences of before and after data")

# Shapiro-Wilk Normality Test
shapiro.test(diff)
