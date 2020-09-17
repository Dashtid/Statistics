# --- Preparation --- #

# Reading in required libraries
library("car")
library("ggpubr")
library("dplyr")

# Setting working directory
setwd("...") 

# --- Task 1 --- #

# Reading int the given data
table1 = read.table("choldata.csv")

# Creating a model of cholesterol to triglyceride
fit1 = lm(table1$cholesterol ~ table1$triglyc)
summary(fit1) # Outputting a summary of this model

# Linear regression to check correlation between cholesterol and triglyceride
plot(table1$triglyc, table1$cholesterol, xlab="Triglycerid", 
     ylab="Cholesterol", pch=19, col=c("red", "blue", "green", "yellow"))
legend("bottomright", legend = c("A","B","C","D"), col=c("red","blue","green","yellow"), pch=19)
abline(fit1)

# Scatter plot with groups
scatterplot(table1$cholesterol ~ table1$triglyc|table1$group, 
            data = table1, xlab="Triglyc", ylab="Cholesterol")

# Splitting the data
sptable = split(table1, table1$group)

# Linear regression test between cholesterol and triglyceride by each groups
summary(lm(sptable$A$cholesterol ~ sptable$A$triglyc))
summary(lm(sptable$B$cholesterol ~ sptable$B$triglyc))
summary(lm(sptable$C$cholesterol ~ sptable$C$triglyc))
summary(lm(sptable$D$cholesterol ~ sptable$D$triglyc))

# Correlation test
cor.test(table1$triglyc, table1$cholesterol)

# Box plot for cholesterol
boxplot(table1$cholesterol ~ table1$group, xlab="Lipid-lowering Medicine", ylab="Cholesterol")

# Normality check for cholesterol using Shapiro test
shapiro.test(sptable$A$cholesterol)
shapiro.test(sptable$B$cholesterol)
shapiro.test(sptable$C$cholesterol)
shapiro.test(sptable$D$cholesterol)

# Doing q-q plots to normality of cholesterol
ggqqplot(sptable$A$cholesterol)
ggqqplot(sptable$B$cholesterol)
ggqqplot(sptable$C$cholesterol)
ggqqplot(sptable$D$cholesterol)

# Box-plot for triglyceride
boxplot(table1$triglyc ~ table1$group, xlab="Lipid-lowering Medicine", ylab="Triglycerid")

# Normality check for triglyceride
shapiro.test(sptable$A$triglyc)
shapiro.test(sptable$B$triglyc)
shapiro.test(sptable$C$triglyc)
shapiro.test(sptable$D$triglyc)

# Doing q-q plots to normality of triglyceride
ggqqplot(sptable$A$triglyc)
ggqqplot(sptable$B$triglyc)
ggqqplot(sptable$C$triglyc)
ggqqplot(sptable$D$triglyc)

# --- Cholesterol tests --- #

# ANOVA test for cholesterol
model1 = aov(table1$cholesterol ~ table1$group)
summary(model1)

# Linear regression for cholesterol
model2 = lm(table1$cholesterol ~ table1$group)
summary(model2)

# --- Triglyceride tests --- #

# Fisher's exact test for triglyceride
fisher.test(table1$triglyc, table1$group)

# Kruskal-Wallis test for triglyceride
kruskal.test(table1$triglyc, table1$group)
