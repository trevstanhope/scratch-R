### Weed Cultivation (Modeling Rodeo)
# This script defines two models: (1) continous human weeding, 
# and (2) discrete cultivation events with a cultivator implement.
# Both of these systems have pros/cons. For example,
# implements cannot be driven in the field in moist conditions,
# or humans are less effective per-day than a machine. However,
# by comparing these two systems and their associated limitations,
# we can estimate their performance. This model may be useful
# when considering the cost-benefit of human labor vs.
# investment required for adoption of machines. Real-world 
# examples of this are India and South Africa, where despite both nations
# having large agricultural economies, adoption of machinery has
# been limited due to an abundant agricultural workforce.

## Setup Workspace
library(deSolve)
rm(list=ls()) # Clear all variables from workspace
dir = "C:\\Users\\Bioresource\\Documents\\scratch-R\\examples\\weed_cultivation" # 
setwd(dir) # Set the dir variable to the path to your project

### Functions
## Soil Moisture Function
# This function is used to determine the change of soil moisture [mm/ha]
# Args:
#   rain [mm/d]
#   temp [deg]
#   gust [kmh]
# Constants:
#   C_temp [mm / deg / d]
#   C_gust [mm / kmh / d]
#   C_infiltration 
#   C_runoff [mm / d]
# Returns:
#   rate [mm / d]
C_temp = 0.001 # mm/ deg /d
C_gust = 0.001 # mm / kmh / d
C_infiltration = 15 # mm / d
C_runoff = 10 # mm / d
moistureFunction <- function(prev, rain, temp, gust) {
  rate <- rain - ((C_temp * temp + C_gust * gust) + C_infiltration + C_runoff)
  return(rate)
}
  
## Growth Function
# This function determines the rate at which the weeds propogate
# It is assumed that the weeds grow as a function of temperature, wind conditions, and rainfall
# Args:
#   rain [mm/d]
#   temp [deg]
#   gust [kmh]
# Constants:
#   K_rain [plants / mm / d]
#   K_temp [plants / deg / d]
#   K_gust [plants / kmh / d]
# Returns:
#   rate [plants/d]
K_growth = 1.0 # plants / d
K_rain = 0.010 # plants / mm
K_gust = 0.0003 # plants / kph
K_temp = 0.005 # plants / C
growthFunction <- function(prev, rain, temp, gust, eff, raineff) {
  rate <- prev * K_growth * (K_rain * rain + K_temp * temp + K_gust * gust) - ceiling(prev * (eff))
  return(rate)
}

### Simulation
## Variables
# Load Inputs from Data File Parameters
datafile = "weather.csv" # data file to use for weather data
data = read.csv(datafile)
gust <- data$gust  # [km/hr] Note: <31 is assumed to be 30
rain <- data$rain  # [mm]
temp <- data$temp  # [deg]
days = data$day # [days]
season_length = length(days)

# Constants
human_eff = 0.15 # in %
implement_eff = 0.95 # in %
raineff = 0.15 # in %
weeds_init = 1000.0 # Initial number of weeds in the field at the beginning of the season
implement_moisture_limit = 10.0 # mm/ha
human_moisture_limit = 50.0 # mm/ha
moisture_init = 100 # mm/ha
cultivation_events_max = 6 # total number of times the implement should be used in the field
days_between_events = 13
  
## Soil Moisture Model (NEEDED FOR SUBSEQUENT MODELS)
soil_moisture <- vector(length=season_length) 
soil_moisture[1] <- moisture_init
for (i in 2:season_length) {
  dmdt = moistureFunction(soil_moisture[i-1], rain[i], temp[i], gust[i])
  if (soil_moisture[i-1] + dmdt < 0) {
    soil_moisture[i] = 0
  }
  else{ 
    soil_moisture[i] = soil_moisture[i-1] + dmdt
  }
}

## Human Cultivation Model
weeds_human <- vector(length=season_length)
weeds_human[1] <- weeds_init # set first value to the initial weed-level
for (i in 2:season_length) {
  if (soil_moisture[i] > human_moisture_limit) {
    eff = 0
  }
  else {
    eff = human_eff
  }
  dwdt = growthFunction(weeds_human[i-1], rain[i], temp[i], gust[i], eff)
  if (weeds_human[i-1] + dwdt < 0) {
    weeds_human[i] = 0
  }
  else {
    weeds_human[i] = weeds_human[i-1] + dwdt
  }
}

## Implement Cultivation Model
cultivation_events = 0
days_since_last_event = 0
weeds_implement <- vector(length=season_length)
weeds_implement[1] <- weeds_init # set first value to the initial weed-level
for (i in 2:season_length) {
  
  # Rule-base to determine if weeding is possible for a given day
  if (cultivation_events > cultivation_events_max) {
    eff = 0
    days_since_last_event = days_since_last_event + 1
  }
  else {
    if (soil_moisture[i] > implement_moisture_limit) {
      eff = 0 # Do not Cultivate, too wet!
      days_since_last_event = days_since_last_event + 1
    }
    else {
      if (days_since_last_event > days_between_events) {
        cultivation_events = cultivation_events + 1 # Cultivate, good conditions!
        eff = implement_eff
        days_since_last_event = 0
      }
      else {
        eff = 0 # Do not cultivate, wait longer
        days_since_last_event = days_since_last_event + 1
      }
    }
  }
  
  # Growth rate
  dwdt = growthFunction(weeds_implement[i-1], rain[i], temp[i], gust[i], eff, raineff)
  if (weeds_implement[i-1] + dwdt < 0) {
    weeds_implement[i] = 0
  }
  else {
    weeds_implement[i] = weeds_implement[i-1] + dwdt
  }
}

## Display Results
plot(days, weeds_human, col="green", xlab="Days",ylab="Weed Emergence (#)",type='l',lwd=3)
lines(days, weeds_implement, col="red")