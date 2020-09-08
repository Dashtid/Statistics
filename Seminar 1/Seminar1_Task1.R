# --- Preparation --- #

library(MASS)
library(ggpubr)
library(dbplyr)

# Setting Working Directory
setwd("/Den stora KTH mappen/CM2003 - Statistics in Medical Engineering/Seminar 1")      # Laptop
# setwd("/Skola/KTH/CM2009 - Statistics in Medical Engineering/Seminarium/Seminar 1/Data")   # Desktop

# Reading in countrate data and attaching the headers to corresponding column
count_rate = read.table("countrate_data.csv",sep = ";", header=TRUE)
attach (count_rate)

# --- Task 1 A --- # 

# Fitting Poisson and Gaussian dist to countrate_data
poisson = fitdistr(countRate, "Poisson")
normal = fitdistr(countRate, "normal")

# --- Task 1 B --- #

# Calculating the mean of the data
mean = mean(countRate)

# Printing these values
sprintf("Mean of countrate is: %s", mean)
sprintf("Mean of Gaussian fit is: %s", normal$estimate[1])
sprintf("Lambda of Poisson fit is: %s", poisson$estimate[1])

# --- Task 1 C --- #

# Calculating Poisson and Gaussian distributions
pois = dpois(time, poisson$estimate[1])
norm = dnorm(time, normal$estimate[1], normal$estimate[2])

# Creating histogram and plotting it along with Poisson and Gaussian distributions
hist(countRate,breaks=20,prob=TRUE,
     main = "Count Rate Experiment",
     xlab = "Count Rate",
     ylab = "Density")
lines(pois,type="p",col="green",lwd=3,)
lines(norm,type="l",col="red",lwd=3,)
legend("topright",
       legend = c("Poisson","Gaussian"),
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col = c("green","red"),
       title = "Legend")

# --- Task 1 D --- #

# Calculating cumulative distributions of both Poisson and Gaussian for data
cumulative_pois = ppois(time, poisson$estimate[1], lower.tail = TRUE)
cumulative_norm = pnorm(time, normal$estimate[1], normal$estimate[2]) 

# Plotting these distributions in a 1x2 figure
par(mfrow=c(1,3))

# Gaussian
plot(cumulative_norm,type="l",
     main = "Cumulative Distributation - Gaussian",
     xlab = "Count Rate",
     ylab = "Probability",
     xlim = c(0,40))

# Poisson
plot(cumulative_pois,type="l",
     main = "Cumulative Distributation - Poisson",
     xlab = "Count Rate",
     ylab = "Probability",
     xlim = c(0,40))

# Plotting both on-top of each other
plot(cumulative_norm,type="l",col="red",
     main = "Cumulative Distributation - Both",
     xlab = "Count Rate",
     ylab = "Probability",
     xlim = c(0,40))
lines(cumulative_pois,type="l",col="green")
legend("bottomright",
       legend = c("Poisson","Gaussian"),
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col = c("green","red"),
       title = "Legend")


# --- Task 1 E --- #

# Creating variables needed for computing the 95 % confidence interval of the Gaussian distribution
norm1 = qnorm(0.975 ,normal$estimate[1], normal$estimate[2])
norm2 = qnorm(0.025 ,normal$estimate[1], normal$estimate[2])

# Creating variables needed for computing the 95 % confidence interval of the Poisson distribution
pois1 = qpois(0.975, poisson$estimate[1], lower.tail = TRUE)
pois2 = qpois(0.025, poisson$estimate[1], lower.tail = TRUE)

# Printing estimates with a 95 % confidence interval
sprintf("Estimate of countrate w. Gaussian distribution is between: %f and %f", norm2, norm1)
sprintf("Estimate of countrate w. Poisson distribution is between: %f and %f", pois2, pois1)

# --- Task 1 F --- #

# Calculating the probability of getting countrate of 15 cps
prob_norm = pnorm(15, normal$estimate[1], normal$estimate[2]) 
prob_pois = ppois(15, poisson$estimate[1], lower.tail = TRUE)

sprintf("Probability of getting 15 cps is: %f with according to Gaussian model", prob_norm)
sprintf("Probability of getting 15 cps is: %f with according to Poisson model", prob_pois)

# --- Task 1 G --- #

# Chi-squared test to test goodness of the fit
pois_goodness_data = dpois(11:30,fitP$estimate[1])
norm_goodness_data = dnorm(11:30,fitN$estimate[1],fitN$estimate[2])

chisq.test(histo$counts,p=pois_goodness_data, rescale.p=TRUE, simulate.p.value=TRUE)
chisq.test(histo$counts,p=norm_goodness_data, rescale.p=TRUE, simulate.p.value=TRUE)

# Doing a qq-test to check the performance of fit
ggqqplot(countRate)

# --- Task 1 H --- #

# Reading in countrate data and attaching the headers to corresponding column
count_rate_10s = read.table("countrate_data_10s_avg.csv",sep = ";", header=TRUE)
attach (count_rate_10s)

# Fitting Poisson and Gaussian dist to countrate_data
poisson_10s = fitdistr(countRate10savg, "Poisson")
normal_10s = fitdistr(countRate10savg, "normal")

# Random vector to get proper range for dpois/dnorm
x = 1:500

# Calculating Poisson and Gaussian distributions
pois_10s = dpois(x, poisson_10s$estimate[1])
norm_10s = dnorm(x, normal_10s$estimate[1], normal_10s$estimate[2])

# Creating histogram and plotting it along with Poisson and Gaussian distributions
hist(countRate10savg,breaks=66,prob=TRUE,
     main = "Count Rate 10s",
     xlab = "Count Rate",
     ylab = "Density")
lines(pois_10s,type="l",col="green",lwd=3,)
lines(norm_10s,type="l",col="red",lwd=3,)
legend("topright",
       legend = c("Poisson","Gaussian"),
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col = c("green","red"),
       title = "Legend")


