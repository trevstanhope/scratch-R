#Lab 2, Exercise 1
#Fit data to a binomial distribution by ML

# Set up - clear variables, load data

rm(list=ls())
dir = "C:\\Users\\Bioresource\\Documents\\scratch-R\\examples\\max_likelihood" # 
setwd(dir) # Set the dir variable to the path to your project
load('data1.RData')

# Plot data
hist(data1, xlab="Number of fish killed")

# Negative Log-Likelihood
# dataIn:   Vector giving number of successes in each sample
# nTrials:  The number of trials in each sample. Scalar
# p:        Hypothesized probability of success. Scalar
nllBinom <- function(p,dataIn,nTrials) {
  nll <- -sum(dbinom(dataIn, nTrials, p, log=T))
  return(nll)
}

# Calculate NLL across a range of possible p values
# Set up vector of p values and initialize vector to hold NLL results
pVec <- seq(0.6,0.8,0.01) # Min at 0.71
nllVec <- numeric(0)

# Run calculations for each hypothesized value of p
for (i in 1:length(pVec)) {
  nllVec <- c(nllVec, nllBinom(pVec[i], data1,10))
}

# Plot
plot(nllVec~pVec,type='b',ylab='NLL',xlab='p')

# Run optimization
optimOut <- optim(0.5,fn=nllBinom,dataIn=data1,nTrials=10,method="BFGS")
optimOut