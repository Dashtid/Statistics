# --- Preparation --- #

# Reading in required libraries
library(MASS)
library("dplyr")
library("ggpubr")
library(readxl)

# Setting Working Directory and reading in libraries
setwd("...")

################ Task 3 - COVID Data #######################

coviddata = read_excel("coviddata.xlsx")
coviddata = coviddata[nrow(coviddata):1,] # Reverse table
coviddata[is.na(coviddata)] = 0 # set NA to 0
attach(coviddata)

################# (Step 1) PREPARE DATA ####################

# Separate data for both countries
data_sweden = coviddata[coviddata$countriesAndTerritories=="Sweden",]
data_UK = coviddata[coviddata$countriesAndTerritories=="United_Kingdom",]

data_sweden$numberDay = 1:249
data_UK$numberDay = 1:249

# --- Sweden --- #

# Sweden 14 day moving average for deaths 
cumulative_deaths = data_sweden$deaths
for(i in 14:length(data_sweden$deaths)) 
  (cumulative_deaths[i]=sum(data_sweden$deaths[(i-13):i]) * 100000 / data_sweden$popData2019[i])
data_sweden$cumulative_deaths = cumulative_deaths

# Sweden all deaths
data_sweden$all_deaths = data_sweden$deaths
for(i in 2:length(data_sweden$deaths)) 
  (data_sweden$all_deaths[i] = data_sweden$all_deaths[i] + data_sweden$all_deaths[i-1])

# Sweden vectors
cum_cases_sw  = data_sweden$`Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`
av_cases_sw = cum_cases_sw / 14
cum_deaths_sw = data_sweden$cumulative_deaths
av_deaths_sw = cum_deaths_sw / 14
all_deaths_sw = data_sweden$all_deaths

# --- UK --- #

# UK 14 day moving average for deaths
cumulative_deaths = data_UK$deaths
for(i in 14:length(data_UK$deaths)) 
  (cumulative_deaths[i] = sum(data_UK$deaths[(i-13):i]) * 100000 / data_UK$popData2019[i])
data_UK$cumulative_deaths = cumulative_deaths

# UK all deaths
data_UK$all_deaths = data_UK$deaths
for(i in 2:length(data_UK$deaths)) 
  (data_UK$all_deaths[i] = data_UK$all_deaths[i] + data_UK$all_deaths[i-1])

# UK vectors
cum_cases_uk  = data_UK$`Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`
cum_deaths_uk = data_UK$cumulative_deaths
av_cases_uk = cum_cases_uk / 14
av_deaths_uk = cum_deaths_uk / 14
all_deaths_uk = data_UK$all_deaths

################# (Step 2) PLOT ####################

# Sweden - Raw Data
plot(dateRep[countriesAndTerritories=="Sweden"], cases[countriesAndTerritories=="Sweden"],
     type="h",
     xlab="Date", ylab="Cases", main="New Cases in Sweden")

plot(dateRep[countriesAndTerritories=="Sweden"], deaths[countriesAndTerritories=="Sweden"],
     type="h",
     xlab="Date", ylab="Deaths", main="New Deaths in Sweden")

# UK - Raw Data
plot(dateRep[countriesAndTerritories=="United_Kingdom"], cases[countriesAndTerritories=="United_Kingdom"],
     type="h", col="blue",
     xlab="Date", ylab="Cases", main="New Cases in the UK")

plot(dateRep[countriesAndTerritories=="United_Kingdom"], deaths[countriesAndTerritories=="United_Kingdom"],
     type="h", col="blue",
     xlab="Date", ylab="Deaths", main="New Deaths in the UK")

# PLotting of moving average - Cases 
plot(dateRep[countriesAndTerritories=="Sweden"], av_cases_sw,
     type="l", col="red", lwd="2",
     xlab="Date", ylab="Cases", 
     main="Confirmed COVID-19 cases per 100,000 - Moving average (14 days)")

lines(dateRep[countriesAndTerritories=="United_Kingdom"], av_cases_uk, 
      type="l", col="green", lwd="2")

legend("topleft",
       legend = c("Sweden","United Kingdom"),
       lty=c(1,1),
       lwd=c(2.5, 2.5),
       col = c("red", "green"),
       title = "Legend")

# Plotting of moving average - Deaths
plot(dateRep[countriesAndTerritories=="Sweden"], av_deaths_sw,
     type="l", col="red", lwd="2", ylim=c(0,1.5),
     xlab="Date", ylab="Deaths", 
     main="Confirmed COVID-19 deaths per 100,000 - Moving average (14 days)")

lines(dateRep[countriesAndTerritories=="United_Kingdom"], av_deaths_uk, 
      type="l", col="green", lwd="2")

legend("topleft",
       legend = c("Sweden","United Kingdom"),
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col = c("red","green"),
       title = "Legend")

################# (Step 3) Statistical model ####################

# ------------ Model SWE ------------ # 
model_sw = lm(av_deaths_sw ~ av_cases_sw)
summary(model_sw) # R-squared => 0.3962

# Producing predictions used for plotting
data_sw = data.frame(av_cases_sw,av_deaths_sw)
confi_sw1 = predict(model_sw, interval="confidence")
data_sw_confi = cbind(data_sw, confi_sw1)
predi_sw1 = predict(model_sw, interval="prediction")
data_sw_predi = cbind(data_sw, predi_sw1)

# Plotting the values
p = ggplot(data_sw, aes(av_cases_sw, av_deaths_sw)) +
  xlab("Cases/100,000") + ylab("Deaths/100,000") + ggtitle("Model Sweden") 
p + geom_point() +
  geom_line(aes(y = lwr), color = "red",   data = data_sw_predi, linetype = "dashed") +
  geom_line(aes(y = upr), color = "red",   data = data_sw_predi, linetype = "dashed") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), data = data_sw_confi, alpha=0.4) +
  geom_line(aes(y = fit), color = "blue",  data = data_sw_confi)

# ------------ Model SWE2 ------------ #
model_sw2 = lm(av_deaths_sw[1:150] ~ av_cases_sw[1:150])
summary(model_sw2) # R-squared => 0.9387

# Producing predictions used for plotting
data_sw2 = data.frame(av_cases_sw[1:150], av_deaths_sw[1:150])
confi_sw2 = predict(model_sw2, interval="confidence")
data_sw2_confi = cbind(data_sw2, confi_sw2)
predi_sw2 = predict(model_sw2, interval="prediction")
data_sw2_predi = cbind(data_sw2, predi_sw2)

# Plotting the values
p = ggplot(data_sw2, aes(av_cases_sw.1.150., av_deaths_sw.1.150.)) +
  xlab("Cases/100,000") + ylab("Deaths/100,000") + ggtitle("Model Sweden - Day 1 to 150")

p + geom_point() +
  geom_line(aes(y = lwr), color = "red",   data = data_sw2_predi, linetype = "dashed") +
  geom_line(aes(y = upr), color = "red",   data = data_sw2_predi, linetype = "dashed") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), data = data_sw2_confi, alpha=0.4) +
  geom_line(aes(y = fit), color = "blue",  data = data_sw2_confi)

# ----------- Model SWE3 ------------ #
model_sw3 = lm(av_deaths_sw[180:249] ~ av_cases_sw[180:249])
summary(model_sw3) # R-squared => 0.8542

# Producing predictions used for plotting
data_sw3 = data.frame(av_cases_sw[180:249], av_deaths_sw[180:249])
confi_sw3 = predict(model_sw3, interval="confidence")
data_sw3_confi = cbind(data_sw3, confi_sw3)
predi_sw3 = predict(model_sw3, interval="prediction")
data_sw3_predi = cbind(data_sw3, predi_sw3)

# Plotting the values
p = ggplot(data_sw3, aes(av_cases_sw.180.249., av_deaths_sw.180.249.)) +
  xlab("Cases/100,000") + ylab("Deaths/100,000") + ggtitle("Model Sweden - Day 180 to 249")

p + geom_point() +
  geom_line(aes(y = lwr), color = "red",   data = data_sw4_predi, linetype = "dashed") +
  geom_line(aes(y = upr), color = "red",   data = data_sw4_predi, linetype = "dashed") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), data = data_sw4_confi, alpha=0.4) +
  geom_line(aes(y = fit), color = "blue",  data = data_sw4_confi)

# ------------ Model UK ------------ #
model_uk = lm(av_deaths_uk ~ av_cases_uk)
summary(model_uk) # R-squared => 0.926

# Producing predictions used for plotting
data_uk = data.frame(av_cases_uk, av_deaths_uk)
confi_uk1 = predict(model_uk, interval="confidence")
data_uk_confi = cbind(data_uk, confi_uk1)
predi_uk1 = predict(model_uk, interval="prediction")
data_uk_predi = cbind(data_uk, predi_uk1)

# Plotting the values
p=ggplot(data_uk, aes(av_cases_uk, av_deaths_uk)) +
  xlab("Cases/100,000") + ylab("Deaths/100,000") + ggtitle("Model UK")

p+geom_point() +
  geom_line(aes(y = lwr), color = "red",   data = data_uk_predi, linetype = "dashed") +
  geom_line(aes(y = upr), color = "red",   data = data_uk_predi, linetype = "dashed") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), data = data_uk_confi, alpha=0.4) +
  geom_line(aes(y = fit), color = "blue",  data = data_uk_confi)

# ---- Comparing  Sweden and UK ---- #

plot(0, 0, xlim=c(0,10), ylim=c(0,2), col="white",       
     xlab="Cases/100,000", ylab="Deaths/100,000")

abline(coef(model_uk), col="red", lwd=3)
abline(coef(model_sw), col="blue", lwd=3)
abline(coef(model_sw2), col="darkgreen", lwd=3)
abline(coef(model_sw3), col="green", lwd=3)

legend("topleft",
       legend = c("UK", "Sweden", "Sweden Day 1-150", "Sweden Day 180-249"),
       lty=c(1, 1, 1, 1),
       lwd=c(2.5, 2.5, 2.5, 2.5),
       col = c("red","blue","darkgreen","green"),
       title = "Legend")