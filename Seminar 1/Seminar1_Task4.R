# --- Preparation --- #

library(MASS)
library(ggpubr)
library(dbplyr)

# Setting Working Directory
#setwd("/Den stora KTH mappen/CM2003 - Statistics in Medical Engineering/Seminar 1")      # Laptop
setwd("/Skola/KTH/CM2009 - Statistics in Medical Engineering/Seminarium/Seminar 1/Data")   # Desktop

# Reading in data
table = read.table("treatmentdata.txt", sep=(","))

# Suitable presentation of the data
ftable = table(table$treatment, table$response)
plot(ftable, ylab="Clinical Response", xlab="Type of Medicine", main="Rheumatoid Arthritis Medicine",col=c("blue","green","red"))

# Add dummy column to express the ordinal data
table$rank = ifelse(grepl(table$response, pattern = "improved"), 1, ifelse(grepl(table$response, pattern = "unchanged"), 0, -1))
attach(table)

plot(ftable, ylab="Clinical Response", xlab="Type of Medicine", main="Rheumatoid Arthritis Medicine",col=c("blue","green","red"))

# --- One-way ANOVA test --- #
table = aov(rank~treatment)
summary(table)

# Shapiro-Wilk normality test : to confirm whether this data is normally distributed
shapiro.test(rank)

# Kruskal-Wallis normality test
kruskal.test(rank~treatment) 

