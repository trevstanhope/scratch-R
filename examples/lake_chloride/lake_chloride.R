#######################################################
# Script for simulating chloride mass balance in a lake
# Feb 2015, modified from earlier versions incl SRC
# 
# lake_chloride/
# .. data/
# .. figs/
# .. docs/
# .. .. Ex1 - Simulation and Modeling.docx
# .. .. deSolve.pdf
# .. R/
# .. .. functions.R
# .. lake_chloride.R
#######################################################


## External Libraries and Functions
# Get the deSolve library: https://cran.r-project.org/web/packages/deSolve/index.html
# Extract the .zip to your R-path (e.g. C:/Program Files/R/library/deSolve)
library(deSolve)

## Workspace
rm(list=ls()) #Clear all variables from workspace
dir = "C:\\Users\\Bioresource\\Documents\\scratch-R\\examples\\lake_chloride" # 
setwd(dir) # Set the dir variable to the path to your project
#source("R/functions.R") # Load external .R files from 

## Assumptions
# 1. no precipitation
# 2. rates are constant
# 3. lake mixed
# 4. V_in = V_out

## Methodology
# 1. Define differential model
#   d(m_lake) / dt = C_land * v_land) + (c_flush * v_flush) - m_lake * (v_land + v_flush) / v_lake
# 2. Distretize model (Euler's Method)
#   ??(m_lake) / ??t = ... * ??t
#   ??(m_lake) = ...
#   m_lake(t+1) - m_lake(t) = ...
#   m_lake(t+1) = m_lake(t) + ...
# 3. 4th Order Runge-Kutta

## Constants
# Lake parameters
v_lake <- 3.77e+6  # lake volume, m3

# Runoff parameters - c, v, and m of runoff from land to lake
v_land <- 1.03e+4 # m3 day-1
c_land <- 70.0 # g m-3

# Flushing parameters - v of fresh water flushed into lake (with c=0)
v_flush <- 0 # m3 day-1. Can set this >0 to institute flushing
c_flush <- 0 # g m-3

# Initial conditions
c_lake_init <- 82.0 # initial concentration of Cl- in lake, g m-3 (= mg L-1)
m_lake_init <- c(m_lake=c_lake_init * v_lake) # initial mass of Cl- in the lake

## Solve ODE (uses ode() from the deSolve library)
# out <- ode(y, times, func, parms, method = ...)
# y:    the initial (state) values for the ODE system, a vector. If y has a name attribute,
#       the names will be used to label the output matrix.
#       times time sequence for which output is wanted; the first value of times must be the
#       initial time.
# func: either an R-function that computes the values of the derivatives in the ODE
#       system (the model definition) at time t, or a character string giving the name of
#       a compiled function in a dynamically loaded shared library.
#       If func is an R-function, it must be defined as: func <- function(t, y, parms,...).
#       t is the current time point in the integration, y is the current estimate of the variables
#       in the ODE system. If the initial values y has a names attribute, the names
#       will be available inside func. parms is a vector or list of parameters; ... (optional)
#       are any other arguments passed to the function.
#       The return value of func should be a list, whose first element is a vector containing
#       the derivatives of y with respect to time, and whose next elements are
#       global values that are required at each point in times. The derivatives must be
#       specified in the same order as the state variables y.
#       If func is a string, then dllname must give the name of the shared library (without                                                                           extension) which must be loaded before ode is called. See package vignette
#       "compiledCode" for more details.
# parms:parameters passed to func.
#       method the integrator to use, either a function that performs integration, or a list of
#       class rkMethod, or a string ("lsoda", "lsode", "lsodes","lsodar","vode",
#       "daspk", "euler", "rk4", "ode23", "ode45", "radau", "bdf", "bdf_d", "adams",

massFlowStrict <- function(times, m_lake, pars) {
  # Calculate the mass-flow rate
  rate <- c_land * v_land - (v_land + v_flush)*(1/v_lake) * m_lake
  return(list(rate)) # return rate as a list-type
}

massFlowRelaxed <- function(times, m_lake, ...) {
  # Calculate the mass-flow rate
  rate <- c_land * v_land - (v_land + v_flush)*(1/v_lake) * m_lake
  return(list(rate)) # return rate as a list-type
}

# 1. Next, define times for which we want values
times <- seq(1:1000)

# 2. Define parameter object
# This contain the names of variables to be used in the massFlowStrict() function
pars <- c(v_lake=v_lake,v_land=v_land,c_land=c_land,v_flush=v_flush,c_flush=c_flush)

# 3. Run the solver
out <- ode(m_lake_init, times, massFlowStrict, parms=pars, method="rk4")

## Display Results
dim(out) 
head(out)
tail(out)
plot(m_lake~times, data=out) # Simple plot
plot(m_lake~times, data=out, xlab="Day",ylab="Mass of Cl- (g)",type='l',col='red',lwd=3) # Slightly dressed up plot