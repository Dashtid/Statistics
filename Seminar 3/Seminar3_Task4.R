# --- Preparation --- #

# Reading in necessary libraries
library(mle4)
library(jtools)
library(lmerTest)
library(ggplot2)
library(ggeffects)
library("ggpubr")

# Setting Working Directory
setwd("...")

# Reading in data
oncdata = read.csv(file = "oncdata.csv", head = TRUE , sep=",")
attach(oncdata)

# Disease stage:    Stage 3 = 0 & Stage 4 = 1
# Treatment group:  Control = 0 & Treatment = 1

# ----------- Task 4 ----------- #

# Showing both Stage and Treatment

boxplot(TumourSize ~ Months) # Tumors are increasing
boxplot(TumourSize ~ Months + Treatment + Stage)

boxplot(TumourSize ~ Months, data = oncdata[oncdata$Treatment == 1,]) # increasing less (6.8 - 8-8)
boxplot(TumourSize ~ Months, data = oncdata[oncdata$Treatment == 0,]) # increasing more (7 - 10.5)

boxplot(TumourSize ~ Months, data = oncdata[oncdata$Stage == 1,]) # increasing more (7.5 - 10.5)
boxplot(TumourSize ~ Months, data = oncdata[oncdata$Stage == 0,]) # increasing less (6.8 - 9)

boxplot(TumourSize ~ Months + Treatment, data = oncdata[oncdata$Stage == 0,])
boxplot(TumourSize ~ Months + Treatment, data = oncdata[oncdata$Stage == 1,])

# Showing subject data
boxplot(TumourSize ~ Subject, data = oncdata)
hist(TumourSize, 40)

# Mixed-effects models
fit1_model = lmer(TumourSize ~ 1 + Months + Treatment + (1|Stage) + ( 1 | Subject)  , data = oncdata)
fit2_model = lmer(TumourSize ~  1 + Months + Treatment + Stage + ( 1 | Subject)  , data = oncdata)
fit3_model = lmer(TumourSize ~  1 + Months + Treatment + Months:Treatment + (1 | Subject)  , data = oncdata)
fit4_model = lmer(TumourSize ~  1 + Months + Treatment + Stage + Months:Treatment + (1 | Subject)  , data = oncdata)
fit5_model = lmer(TumourSize ~  1 + Months + Stage + Months:Treatment + (1 | Subject)  , data = oncdata)

# --- SUMMARIES --- #

# Model 1
summary(fit1_model) # default lme4
summ(fit1_model)    # jtools
ranova(fit1_model)  # lmerTest

# Model 2
summary(fit2_model) # default lme4
summ(fit2_model)    # jtools
ranova(fit2_model)  # lmerTest

# Model 3
summary(fit3_model) # default lme4
summ(fit3_model)    # jtools
ranova(fit3_model)  # lmerTest

# Model 4
summary(fit4_model) # default lme4
summ(fit4_model)    # jtools
ranova(fit4_model)  # lmerTest

# Model 5
summary(fit5_model) # default lme4
summ(fit5_model)    # jtools
ranova(fit5_model)  # lmerTest

# Doing an ANOVA test
anova(fit1_model, fit2_model, fit3_model, fit4_model, fit5_model)  


# Plotting the residuals of best model
plot(fit4_model, which = 1)

