#Lab 2, Exercise 3
#Spiders and flies - linear model with normal errors

##
#Set up - clear variables, load data
rm(list=ls())
load('dataSpiders.RData')

##
#Plot data
plot(spiders~flies,data=dataSpiders)

##
#Function to calculate the NLL
#Arguments:
# pars    -Vector of model pars, c(b0, b1, sigma)
# xObs    -Observed x data
# yObs    -Observed y data
spiderLoss <- function(pars,xObs,yObs) {
  b0 <- pars[1]
  b1 <- pars[2]
  sigma <- pars[3]
  yFit <- b0 + b1*xObs
  e <- yObs-yFit
  nll <- -sum(dnorm(e,0,sigma,log=T))
  return(nll)
}

##
#Run optimization
parGuess <- c(0,0.15,5)
optimOut <- optim(par=parGuess,fn=spiderLoss,method="BFGS",xObs=dataSpiders$flies,
                  yObs=dataSpiders$spiders)
optimOut

parEst <- optimOut$par

##
#Look at model fits and residuals

#Function to calculate fits
spiderPredix <- function(pars,xObs) {
  b0 <- pars[1]
  b1 <- pars[2]
  yFit <- b0 + b1*xObs
  return(yFit)
}

#Calculate fits and residuals and make some plots
yFit <- spiderPredix(parEst,dataSpiders$flies)
res <- dataSpiders$spiders-yFit

plot(dataSpiders$spiders~yFit,xlab="Fits",ylab="Observed")
abline(0,1)

plot(res~yFit)

