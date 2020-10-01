# --- Preparation --- #

# Reading in necessary libraries
library(MASS)
library(irr)
library(Metrics)

# Setting Working Directory
setwd("...")

# --- TASK 3 --- #

# Open data
pef = read.table("PEFdata.txt")

# Split data by model
spef = split(pef, pef$Model)

# Draw box-plots
boxplot(spef$old$day1, spef$old$day2)
boxplot(spef$new$day1, spef$new$day2)

# Histogram of old machine with 2 days
truehist(spef$old$day1, h=25,col="#ff990080", 
         border="#ff990080", axes=FALSE, xlab="", ylab="")
axis(side=1)
axis(side=2, col.axis = "#ff9900", col="#ff9900")
mtext("Day1", side=2, line=3)

par(new=TRUE)

truehist(spef$old$day2, h=25,col="#66990080",
         border="#66990080", axes=FALSE, xlab="Old machine", ylab="")
axis(side=4, col.axis="#669900", col="#669900")
mtext("Day2", side=4, line=3)

# Histogram of new machine with 2 days
truehist(spef$new$day1, h=25, col="#ff990080",
         border="#ff990080", axes=FALSE, xlab="", ylab="")
axis(side=1)
axis(side=2, col.axis = "#ff9900", col="#ff9900")
mtext("Day1", side=2, line=3)

par(new=TRUE)

truehist(spef$new$day2, h=25,col="#66990080", 
         border="#66990080", axes=FALSE, xlab="New machine", ylab="")
axis(side=4, col.axis="#669900", col="#669900")
mtext("Day2", side=4, line=3)

# Change data format to calculate ICC
spefold = data.frame(day1 = spef$old$day1, day2 = spef$old$day2)
spefnew = data.frame(day1 = spef$new$day1, day2 = spef$new$day2)

# ICC score test
icc(spefold, model="twoway", type=("agreement"), unit="single")
icc(spefnew, model="twoway", type=("agreement"), unit="single")

# Mean absolute error test
mae(spefold$day1, spefold$day2)
mae(spefnew$day1, spefnew$day2)


