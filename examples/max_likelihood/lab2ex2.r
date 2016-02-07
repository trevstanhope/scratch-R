#Lab 2, Exercise 2
#Fit data to a normal distribution by ML

##
#Set up - clear variables, load data
rm(list=ls())
load('data2.RData')

##
#Plot data
hist(data2)

##
#Function to calculate the NLL
#Arguments:
# dataIn  -Vector of data
# mu      -Hypothesized mean of distribution
# sigma   -Hypothesized standard deviation of distribution
nllNorm <- function(pars,dataIn) {
  mu <- pars[1]
  sigma <- pars[2]
  nll <- -sum(dnorm(dataIn,mu,sigma,log=T))
  return(nll)
}

##
#Calculate NLL across a range of possible parameter values

#Set up vectors of mu and sigma values and initialize matrix to hold NLL results
muVec <- seq(40,160,4)
sigmaVec <- seq(15,35,1)
nllOut <- matrix(data=NA,nrow=length(muVec),ncol=length(sigmaVec))

#Run calculations for each hypothesized value of mu and sigma
for (i in 1:length(muVec)) {
  for (j in 1:length(sigmaVec)) {
    nllOut[i,j] <- nllNorm(pars=c(muVec[i],sigmaVec[j]),dataIn=data2)
  }
}

#Plot
filled.contour(muVec,sigmaVec,nllOut,nlevels=30,color.palette=topo.colors,xlab="mu",
               ylab="sigma",main="NLL vs mu and sigma")

##
#Run optimization
optimOut <- optim(par=c(100,20),fn=nllNorm,dataIn=data2,method="BFGS")
optimOut