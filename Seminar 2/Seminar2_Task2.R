# --- Preparation --- #

# Setting working directory
setwd("...")

# --- Task 2 --- #

raw = read.csv("nhanes2.csv", header=T ,sep=",", dec=".")
attach(raw)

# Test linear relationship
cor.test(weight, tcresult)

# Model for total cholesterol and weight
model1 = lm(tcresult ~ weight, data=raw)
summary(model1) # Summary of model
plot(model1)

# Model for total cholesterol and weight in regards to sex
model2 = lm(tcresult ~ weight+sex+weight:sex, data=raw)
summary(model2)

# Model for total cholesterol and weight in regards to race
model3 = lm(tcresult ~ weight+race+weight:race, data=raw)
summary(model3)

# Model for total cholesterol and weight in regards to age
model4 = lm(tcresult ~ weight+age+weight:age, data=raw)
summary(model4)

# Testing if race should be included based on model3
model5 = lm(tcresult ~ weight+race+age+weight:age, data=raw)
summary(model5)

# Running ANOVA test
anova(model4, model5)
anova(model1, model4)

# Plotting model for total cholesterol and weight in regards to age
plot(model4)







