#############################################################
#                                                           #
#              Applied Macroeconometrics - Project          #
#           Victor Graff - Lucie Barette - Audin Roger      #
#                                                           #
#############################################################

library(urca)
library(vars)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(svars)
library(readxl)
library(TSstudio)
library(ggplot2)
library(tframePlus)
library(rio)
library(tseries)
library(ggplot2)
library(dplyr)
library(car)


# Importing data
setwd("C:/Users/ce pc/Dropbox/Mon Drive/Fac/S2/Applied macroeconometrics/Projet/Data")
data <- read_excel("données.xlsx")
data_GDP <- read_excel("données.xlsx", sheet = "GDP_QUARTERLY")
data <- data.frame(data)
data_GDP <- data.frame(data_GDP)


# Plotting  monthly time series
ts_brent <- ggplot(data,aes(x=observation_date, y=BRENT) ) + geom_line(color = "#00AFBB", size = 1)
ts_brent

ts_unemployment <- ggplot(data,aes(x=observation_date, y=UNEMPLOYMENT)) + geom_line(color = "#00AFBB", size = 1)
ts_unemployment

ts_Core_HICP <- ggplot(data,aes(x=observation_date, y=CORE_HICP)) + geom_line(color = "#00AFBB", size = 1)
ts_Core_HICP

ts_HICP_overall <- ggplot(data,aes(x=observation_date, y=HICP_OVERALL)) + geom_line(color = "#00AFBB", size = 1)
ts_HICP_overall

ts_HICP_energy <- ggplot(data,aes(x=observation_date, y=HICP_ENERGY)) + geom_line(color = "#00AFBB", size = 1)
ts_HICP_energy

ts_EURIBOR_3M <- ggplot(data,aes(x=observation_date, y=EURIBOR_3M)) + geom_line(color = "#00AFBB", size = 1)
ts_EURIBOR_3M

# Plotting quarterly time series
ts_GDP_quart <- ggplot(data_GDP,aes(x=observation_date, y=GDP)) + geom_line(color = "#00AFBB", size = 1)
ts_GDP_quart

# Time Series in quarterly
ts_brent <- ts(data$BRENT, start = c(1999,3), end=c(2021,12), frequency=12)
ts_brent <- as.quarterly(ts_brent, FUN=mean, na.rm=TRUE)

ts_unemployment <- ts(data$UNEMPLOYMENT, start = c(1999,3), end=c(2021,12), frequency=12)
ts_unemployment <- as.quarterly(ts_unemployment, FUN=mean, na.rm=TRUE)

ts_Core_HICP <- ts(data$CORE_HICP, start = c(1999,3), end=c(2021,12), frequency=12)
ts_Core_HICP <- as.quarterly(ts_Core_HICP, FUN=mean, na.rm=TRUE)

ts_HICP_overall <- ts(data$HICP_OVERALL, start = c(1999,3), end=c(2021,12), frequency=12)
ts_HICP_overall <- as.quarterly(ts_HICP_overall, FUN=mean, na.rm=TRUE)

ts_HICP_energy <- ts(data$HICP_ENERGY, start = c(1999,3), end=c(2021,12), frequency=12)
ts_HICP_energy <- as.quarterly(ts_HICP_energy, FUN=mean, na.rm=TRUE)

ts_EURIBOR_3M <- ts(data$EURIBOR_3M, start = c(1999,3), end=c(2021,12), frequency=12)
ts_EURIBOR_3M <- as.quarterly(ts_EURIBOR_3M, FUN=mean, na.rm=TRUE)


ts_GDP <- data_GDP[1:91, 2]
ts_GDP <- ts(ts_GDP, start = c(1999,3), end=c(2021,12), frequency=12)
ts_GDP <- as.quarterly(ts_GDP, FUN=mean, na.rm=TRUE)

# Stationarisation   
# ADF test
adf.test(ts_brent) # Not-stationary
adf.test(ts_unemployment) # Not-stationary
adf.test(ts_Core_HICP) # Not-stationary
adf.test(ts_HICP_overall)  # Not-stationary
adf.test(ts_HICP_energy)  # Not-stationary
adf.test(ts_EURIBOR_3M)  # Stationary
adf.test(ts_GDP) # Not-stationary

# Stationarization by differenciation
ts_brent_d=diff(ts_brent,lag=1,differences=1)
ts_unemployment_d=diff(ts_unemployment,lag=1,differences=1)
ts_Core_HICP_d=diff(ts_Core_HICP,lag=1,differences=1)
ts_HICP_overall_d=diff(ts_HICP_overall,lag=1,differences=1)
ts_HICP_energy_d=diff(ts_HICP_energy,lag=1,differences=1)
ts_GDP_d=diff(ts_GDP,lag=1,differences=1)
ts_EURIBOR_3M_d=diff(ts_EURIBOR_3M, lag=1,differences=1)


# Setting restriction matrix:
amat <- diag(7) # Identical matrix 7X7

# 1st column
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
amat[4,1] <- NA
amat[5,1] <- NA
amat[6,1] <- NA
amat[7,1] <- NA

# 2nd column
amat[3,2] <- NA
amat[4,2] <- NA
amat[5,2] <- NA
amat[6,2] <- NA
amat[7,2] <- NA

# 3rd column
amat[4,3] <- NA
amat[5,3] <- NA
amat[6,3] <- NA
amat[7,3] <- NA

# 4th column
amat[5,4] <- NA
amat[6,4] <- NA
amat[7,4] <- NA

# 5th column
amat[6,5] <- NA
amat[7,5] <- NA

# 6th column
amat[7,6] <- NA

svar <- cbind(ts_brent_d,ts_HICP_energy_d,ts_HICP_overall_d, ts_Core_HICP_d, ts_unemployment_d, ts_GDP_d, ts_EURIBOR_3M)
svar
colnames(svar) <- cbind("Brent", "HICP_energy", "HICP_overall", "HICP_Core", "Unemployment", "GDP","EURIBOR_3M")
svar <- na.omit(svar)
svar

# Determining lag ordering
lagselect <- VARselect(svar, lag.max = 8, type = "both")
lagselect$selection 
lagselect$criteria
# => AIC criteria => 8 lags

# Estimating the SVAR
model1 <- VAR(data.frame(svar), p = 8,  season = NULL, exog = NULL, type = "const")
SVARmod1 <- SVAR(model1, max.iter = 200, Amat = amat, Bmat = NULL, hessian = TRUE, estmethod = c("direct"))
SVARmod1



# Impulse Response Function 
# Brent on all
SVAR_brent_HICP_energy <- irf(SVARmod1, impulse = "Brent", response = "HICP_energy",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_brent_HICP_energy) 

SVAR_brent_HICP_overall <- irf(SVARmod1, impulse = "Brent", response = "HICP_overall",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_brent_HICP_overall)

SVAR_brent_HICP_Core <- irf(SVARmod1, impulse = "Brent", response = "HICP_Core",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_brent_HICP_Core)

SVAR_brent_Un <- irf(SVARmod1, impulse = "Brent", response = "Unemployment",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_brent_Un)

SVAR_brent_GDP <- irf(SVARmod1, impulse = "Brent", response = "GDP",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_brent_GDP)

SVAR_brent_EURIBOR_3M <- irf(SVARmod1, impulse = "Brent", response = "EURIBOR_3M",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_brent_EURIBOR_3M)

# HICP Energy on all
SVAR_HICP_energy_HICP_overall <- irf(SVARmod1, impulse ="HICP_energy", response ="HICP_overall",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_HICP_energy_HICP_overall)

SVAR_HICP_energy_HICP_Core <- irf(SVARmod1, impulse ="HICP_energy", response ="HICP_Core",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_HICP_energy_HICP_Core)

SVAR_HICP_energy_Unemployment <- irf(SVARmod1, impulse ="HICP_energy", response ="Unemployment",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_HICP_energy_Unemployment)

SVAR_HICP_energy_GDP <- irf(SVARmod1, impulse ="HICP_energy", response ="GDP",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_HICP_energy_GDP)

SVAR_HICP_energy_EURIBOR_3M <- irf(SVARmod1, impulse ="HICP_energy", response ="EURIBOR_3M",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_HICP_energy_EURIBOR_3M)

# HICP Overall on all
SVAR_HICP_overall_HICP_Core <- irf(SVARmod1, impulse ="HICP_overall", response ="HICP_Core",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_HICP_overall_HICP_Core)

SVAR_HICP_overall_Unemployment <- irf(SVARmod1, impulse ="HICP_overall", response ="Unemployment",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_HICP_overall_Unemployment)

SVAR_HICP_overall_GDP <- irf(SVARmod1, impulse ="HICP_overall", response ="GDP",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_HICP_overall_GDP)

SVAR_HICP_overall_EURIBOR_3M <- irf(SVARmod1, impulse ="HICP_overall", response ="EURIBOR_3M",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_HICP_overall_EURIBOR_3M)

# HICP Core on all
SVAR_HICP_Core_Unemployment <- irf(SVARmod1, impulse ="HICP_Core", response ="Unemployment",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_HICP_Core_Unemployment)

SVAR_HICP_Core_GDP <- irf(SVARmod1, impulse ="HICP_Core", response ="Unemployment",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_HICP_Core_GDP)

SVAR_HICP_Core_EURIBOR_3M <- irf(SVARmod1, impulse ="HICP_Core", response ="EURIBOR_3M",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_HICP_Core_EURIBOR_3M)

# Unemployment on all
SVAR_Unemployment_GDP <- irf(SVARmod1, impulse ="Unemployment", response ="GDP",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_Unemployment_GDP)

SVAR_Unemployment_EURIBOR_3M <- irf(SVARmod1, impulse ="Unemployment", response ="EURIBOR_3M",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_Unemployment_EURIBOR_3M)

# GDP on all
SVAR_GDP_EURIBOR_3M <- irf(SVARmod1, impulse ="GDP", response ="EURIBOR_3M",  n.ahead = 40, width = 10, height = 6)
plot(SVAR_GDP_EURIBOR_3M)

# Forecast error variance decomposition
SVARfevd <- fevd(SVARmod1, n.ahead = 10)
SVARfevd
plot(SVARfevd)



  


  
  
  
  
  
  
  


