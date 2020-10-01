# --- Preparation --- #

# Setting Working Directory
setwd("...")

# Reading in the data
raw = read.table("hipdata.txt", header=T, dec = ".")
attach(raw)

# --------- Bland-Altman plot --------- #

# Creating individual vectors of Observer A and Observer B
A = raw[, 1]
B = raw[, 2]
diff = A-B   # Differences vector of A and B

mean = numeric(38) # Empty mean vector 

# Calculate the mean difference of A and B for every observation
for (i in 1:38) {
  mean[i] = (A[i] + B[i])/2 # Input mean difference in vector mean
}

plot(mean,diff) # Plot mean and differences

d = mean(diff)         # Systematic error
sd = sd(diff)          # Standard deviation of diff
abline(h=d, col="red") # Line of systematic error in plot

# Upper and lower limit o agreement and lines in plot
upp = d + 1.96 * sd
low = d - 1.96 * sd

abline(h=upp, col="blue")
abline(h=low, col="blue")

# Quantify observations at each placement in Bland-Altman plot
zero = 0 #define number of 0

for (i in 1:38) {
  if(diff[i] == 0){
    zero = zero + 1
  }
}

plusone = 0 # Define number of +1  

for (i in 1:38) {
  if(diff[i] == 1){
    plusone = plusone + 1
  }
}

minusone = 0 # Define number of -1

for (i in 1:38) {
  if(diff[i] == -1 ){
    minusone = minusone + 1
  }
}

plustwo = 0 # Define number of +2 

for (i in 1:38) {
  if(diff[i] == 2){
    plustwo = plustwo + 1
  }
}

minustwo = 0 # Define number of -2

for (i in 1:38) {
  if(diff[i] == -2){
    minustwo = minustwo + 1
  }
}

# Defining number of observations placement in the plot as green text
text(0.7, 2, "n=2", col="green")
text(0.7, 1,"n=8", col="green")
text(0.7, -0.1, "n=20", col="green")
text(0.7, -1, "n=7", col="green")
text(0.7, -2, "n=1", col="green")

# --- ICC --- #
icc(raw)


