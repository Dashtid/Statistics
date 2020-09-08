# --- Preparation --- #

library(MASS)
library(ggpubr)
library(dbplyr)

# Setting Working Directory
setwd("/Den stora KTH mappen/CM2003 - Statistics in Medical Engineering/Seminar 1")      # Laptop
# setwd("/Skola/KTH/CM2009 - Statistics in Medical Engineering/Seminarium/Seminar 1/Data")   # Desktop

# --- Main --- #

# Reading in data and attaching the headers to corresponding column
vcdata = read.table("vcdata.txt", sep = ",")
attach(vcdata) # Rows: V1 V2 V3

# See how many patients in which group
table(V2) 
data_control = vcdata[vcdata$V2=="control",]
data_patient = vcdata[vcdata$V2=="patient",]

# Calculating mean values
aggregate(V3 ~ V2, vcdata, mean) # calculate mean
mean(data_control$V3)   # 5.667
mean(data_patient$V3)   # 4.979

# Creating a boxplot to visualize data
boxplot(V3 ~ V2, 
        ylab = "Vital Capicity [l]", 
        xlab = "",
        main = "Vital Capacity for Patient Group and Control Group",
        names = c("Control Group","Patient Group"))

# Checking if data is normal distributed - Control Data
hist(data_control$V3, breaks=10, prob=TRUE,
     xlab = "Vital capacity [l]",
     main = "Histogram - Control Data")
ggdensity(data_control$V3)
ggqqplot(data_control$V3)

# Checking if data is normal distributed - Patient Data
hist(data_patient$V3, breaks=10, prob=TRUE,
     xlab = "Vital capacity [l]",
     main = "Histogram - Patient Data")
ggdensity(data_patient$V3)
ggqqplot(data_patient$V3)

# --- Unpaired t test --- #

t.test(V3 ~ V2)  # p = 0.04214
                 # confint = [0.03 2.35]

# --- Linear regression --- #

linmodel = lm(V3 ~ V2) 
summary(linmodel) # p = 0.05775
confint(linmodel) # confint = [-1.4  0.02]