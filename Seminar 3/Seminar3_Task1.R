# --- Preparation --- #

# Reading in necessary libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(cowplot)

# Setting Working Directory
setwd("...")

# Reading in Data
table1 = Pima.tr
attach(table1)

###############(A)################

# Choose an optimal predict model

# Age
m1 <- glm(type ~ age, family = binomial)
summary(m1)

# Number of pregnancies
m2 <- glm(type ~ npreg, family = binomial)
summary(m2)

# Skin fold thickness
m3 <- glm(type ~ skin, family = binomial)
summary(m3)

# BMI
m4 <- glm(type ~ bmi, family = binomial)
summary(m4)

# With all nonlaborotory data
mnon <- glm(type ~ age + npreg + skin + bmi, family = binomial)
summary(mnon)

# With only significant terms of above results
mbest <- glm(type ~ age + bmi, family = binomial)
summary(mbest)

# Logistic regression plot of age

table1 %>%
  mutate(prob = ifelse(type == "Yes", 1, 0)) %>%
  ggplot(aes(age, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Age",
    y = "Probability of being diabete-pos"
  )

# Logistic regression plot of bmi
table1 %>%
  mutate(prob = ifelse(type == "Yes", 1, 0)) %>%
  ggplot(aes(bmi, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "BMI",
    y = "Probability of being diabete-pos"
  )

########### example #############

# Predict with mbest model
age20 <- data.frame(age=20, bmi=c(20, 30, 40))
age50 <- data.frame(age=50, bmi=c(20, 30, 40))
probabilities20 <- predict(mbest, age20, type="response")
probabilities50 <- predict(mbest, age50, type="response")

# Check predicted probabilities
probabilities20
probabilities50

#############################
#############(B)#############

bestmodel <- glm(type ~ glu + bmi + age, family=binomial)
summary(bestmodel)

# Logistic regression plot of glucose
table1 %>%
  mutate(prob = ifelse(type == "Yes", 1, 0)) %>%
  ggplot(aes(glu, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabete-pos"
  )

### Calculate accuracy with Pima.tr ###

# Predict probability
prob_A <- predict(mbest, table1, type="response")
prob_B <- predict(bestmodel, table1, type="response")

# Classify diabetes with result probability
result_A <- ifelse(prob_A > 0.5, "Yes", "No")
result_B <- ifelse(prob_B > 0.5, "Yes", "No")

# Compare result with actual data
mean(result_A == type)
mean(result_B == type)

### Calculate accuracy with Pima.tr ###

# Load test data
testdata <- Pima.te

# Predict probability
prob_A <- predict(mbest, testdata, type="response")
prob_B <- predict(bestmodel, testdata, type="response")

# Classify diabetes with result probability
result_A <- ifelse(prob_A > 0.5, "Yes", "No")
result_B <- ifelse(prob_B > 0.5, "Yes", "No")

# Compare result with actual data
mean(result_A == testdata$type)
mean(result_B == testdata$type)

# Accuracy graph
predict_A <- data.frame(prob.db=mbest$fitted.values, db=type)
predict_B <- data.frame(prob.db=bestmodel$fitted.values, db=type)

# Order probability the way to increase
predict_A <- predict_A[order(predict_A$prob.db, decreasing = FALSE),]
predict_B <- predict_B[order(predict_B$prob.db, decreasing = FALSE),]

# Added index
predict_A$rank <- 1:nrow(predict_A)
predict_B$rank <- 1:nrow(predict_B)


ggplot(data=predict_A, aes(x=age, y=prob.db)) + geom_point(aes(color=db), alpha=1, stroke=2) + xlab("Index") + 
       ylab("Predicted probability of being diabetes-positive")

ggplot(data=predict_B, aes(x=rank, y=prob.db)) + geom_point(aes(color=db), alpha=1, stroke=2) + xlab("Index") + 
       ylab("Predicted probability of being diabetes-positive")

