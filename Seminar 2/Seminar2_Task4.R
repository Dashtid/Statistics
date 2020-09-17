# --- Preparation --- #

# Reading in required libraries
library(MASS)
library(ggpubr)
library(dbplyr)
library(splitstackshape)
library(tidyr)

# Setting Working Directory
setwd("...")

# ---- Reading & Attaching data ---- #

# --- Alternative 1 --- #

# Using this method the cnr values will become chr values,
# which needs to be converted to numerical
kvdata = read.csv(file = "kvdata.csv", head = TRUE, sep="")
attach(kvdata)

# Checking the data of both vectors and turning the cnr vector into numerical
summary(kvdata)
cnr = as.numeric(gsub(",", ".", cnr))

# --- Alternative 2 --- #

# A much more smoother way of doing the reading of the data,
# which produces two numerical vectors with attach command
kvdata_new = read.csv(file="kvdata.csv", sep="", dec=",", header=TRUE)
attach(kvdata_new)

# ----------- Task 4 ----------- # 

# Doing plotting of values to visualize
plot(kv,cnr, type="p",
     main = "Contrast-to-Noise ratio at different kV settings",
     xlab = "kV",
     ylab = "CNR")

# Just by observing the plot we can see that optimal kV setting is around 80-84 kV

# Fitting to polynomial model
poly = lm(cnr ~ poly(kv, 2, raw=TRUE), data=kvdata_new)
summary(poly)

# Creating predictions based on the polynomial model
predicts = predict(poly)
predicts_conf = predict(poly, interval="confidence")
predicts_predict = predict(poly, interval="predict")

# Plotting the predictions
plot(predicts)

# Doing plotting of values to visualize
plot(kv,cnr, type="p",
     main = "Contrast-to-Noise ratio at different kV settings",
     xlab = "kV",
     ylab = "CNR")
lines(kv, predicts, type="l", col="blue")
summary(poly)

# Creating arrays
mydata = cbind(kvdata_new, predicts)
mydata_conf = cbind(kvdata_new, predicts_conf)
mydata_predict = cbind(kvdata_new, predicts_predict)

# Creating the grid
p = ggplot(mydata, aes(kv, cnr)) + geom_point() 

# Plotting the intervals
p + geom_line(aes(y = lwr), color = "red",   data = mydata_predict, linetype = "dashed") +
        geom_line(aes(y = upr), color = "red",   data = mydata_predict, linetype = "dashed") +
        geom_ribbon(aes(ymin = lwr, ymax = upr), data = mydata_conf, alpha=0.4) +
        geom_line(aes(y = fit), data=mydata_conf, color = "blue")

# Outputting maximum values of the columns, just for reference purposes
colMax = function(data) sapply(data, max, na.rm = TRUE)
colMax(mydata_conf)

