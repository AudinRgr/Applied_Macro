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

ts_EURIBOR_3M <- ts(data$EURIBOR_3M, start = c(1999,3), end=c(2021,11), frequency=12)
ts_EURIBOR_3M <- as.quarterly(ts_EURIBOR_3M, FUN=mean, na.rm=TRUE)

ts_GDP <- ts(data_GDP, start = c(1999,10), end=c(2021,12), frequency=4)
ts_GDP <- ts_GDP[,2]
#ts_GDP_quart <- as.quarterly(ts_GDP_quart, FUN=mean, na.rm=TRUE)

# ADF test
adf.test(ts_brent) # Not-stationary
adf.test(ts_unemployment) # Not-stationary
adf.test(ts_Core_HICP) # Not-stationary
adf.test(ts_HICP_overall)  # Not-stationary
adf.test(ts_HICP_energy)  # Not-stationary
adf.test(ts_EURIBOR_3M)  # Stationary
adf.test(ts_GDP) # Not-stationary


ts_brent_d=diff(ts_brent,lag=1,differences=1)
ts_unemployment_d=diff(ts_unemployment,lag=1,differences=1)
ts_Core_HICP_d=diff(ts_Core_HICP,lag=1,differences=1)
ts_HICP_overall_d=diff(ts_HICP_overall,lag=1,differences=1)
ts_HICP_energy_d=diff(ts_HICP_energy,lag=1,differences=1)
ts_GDP_d=diff(ts_GDP,lag=1,differences=1)
ts_EURIBOR_3M_d=diff(ts_EURIBOR_3M, lag=1,differences=1)





# Setting restriction matrix:
amat <- diag(7) # Identical matrix 4X4
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
amat[4,1] <- NA
amat[5,1] <- NA
amat[6,1] <- NA
amat[7,1] <- NA

amat[3,2] <- NA
amat[4,2] <- NA
amat[5,2] <- NA
amat[6,2] <- NA
amat[7,2] <- NA

amat[4,3] <- NA
amat[5,3] <- NA
amat[6,3] <- NA
amat[7,3] <- NA

amat[5,4] <- NA
amat[6,4] <- NA
amat[7,4] <- NA

amat[6,4] <- NA

amat[6,5] <- NA
amat[7,5] <- NA

amat[6,7] <- NA

# retaking EURIBOR 3M + refaire tourner => pb GDP
svar <- cbind(ts_brent_d,ts_HICP_energy_d,ts_HICP_overall_d, ts_Core_HICP_d, ts_unemployment_d, ts_EURIBOR_3M)
length
colnames(svar) <- cbind("BRENT", "HICP_energy", "HICP_overall", "HICP_Core", "Unemployment", "GDP", "Euribor3M")
svar

svar <- na.omit(svar) # à régler car missing comment justifier ???
svar

# Determining lag ordering
lagselect <- VARselect(svar, lag.max = 8, type = "both")

lagselect$selection 
#AIC(n)  HQ(n)  SC(n) FPE(n) 
# 8      5      2      7
# => 2 lags

# Optimal = 8 lags 
lagselect$criteria


model1 <- VAR(svar, p = 8, season = NULL, exog = NULL, type = "const")

SVARmod1 <- SVAR(model1, Amat = amat, Bmat = NULL, hessian = TRUE, estmethod = c("scoring", "direct"))

SVARmod1

SVARog <- irf(SVARmod1, impulse = "BRENT", response = "BRENT")
plot(SVARog) # model works => just plot correctly
SVARog 
plotIrf(SVAR, name = NULL, ylab = NULL, alpha = 0.3, n.ahead = NULL,
      filename = NULL, width = 10, height = 6, ...)

SVARHICP <- irf(SVARmod1, impulse = "BRENT", response = "HICP")
plot(SVARHICP)

SVARCore <- irf(SVARmod1, impulse = "HICP", response = "Unemployment")
plot(SVARCore)

SVAR_test <- irf(SVARmod1, impulse="BRENT", response = "Unemployment")
plot(SVAR_test)

SVAR_test2 <- irf(SVARmod1, impulse="BRENT", response = "GDP")
plot(SVAR_test2)

SVAR_test3 <- irf(SVARmod1, impulse = "HICP", respon)

#IFR








# Time series
#y <- ts(macro_data$`Output Gap`, start = c(2000,1,1), frequency = 4)
#pi <- ts(macro_data$CPI, start = c(2000,1,1), frequency = 4)
#r <- ts(macro_data$RRP, start = c(2000,1,1), frequency = 4)

# Plotting the series
#ts_plot(y, title = "Output Gap", Xtitle = "Time", Ytitle = "Output Gap")

#ts_plot(pi, title = "Inflation Rate", Xtitle = "Time", Ytitle = "Inflation Rate")

#ts_plot(r, title = "Overnight Reverse Repurchase Rate", Xtitle = "Time", Ytitle = "RRP")

# Setting restriction matrix:
#amat <- diag(3) # Identical matrix 3X3
#amat[2,1] <- NA
#amat[3,1] <- NA
#amat[3,2] <- NA
#amat
# Creation of matrix with NA to fill in

#svar <- cbind(y, pi , r)
#colnames(svar) <- cbind("Output Gap", "Inflation", "RRP")

# Determining lag ordering
#lagselect <- VARselect(svar, lag.max = 8, type = "both")
#lagselect$selection # Optimal = 5 lags 
#lagselect$criteria

#model1 <- VAR(svar, p = 5, season = NULL, exog = NULL, type = "const")

#SVARmod1 <- SVAR(model1, Amat = amat, Bmat = NULL, hessian = TRUE, estmethod = c("scoring", "direct"))

#SVARmod1


# IRF does not work




#SVARfevd <- fevd(SVARmod1, n.ahead = 10)
#SVARfevd
#plot(SVARfevd)
# can decompose effect of shock
  
  
  
  
  
  
  
  
  


