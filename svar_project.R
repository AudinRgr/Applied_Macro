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

setwd("C:/Users/ce pc/Dropbox/Mon Drive/Fac/S2/Applied macroeconometrics/Projet/Data")
data_Unem <- read_excel("Harmonized unemployment rate - monthly.xlsx", skip=10)
data_Unem
data_oil <- read_excel("Crude Oil Price.xlsx", skip=10)
data_oil
data_GDP <- read_excel("données.xlsx")
data_GDP


dataGDP <- read_excel("GDP.xls", skip=10)

data <- data.frame(data)
head(data)

# Plotting time series
ts_brent <- ggplot(data,aes(x=observation_date, y=BRENT) ) + geom_line(color = "#00AFBB", size = 1)
ts_brent

ts_unemployment <- ggplot(data,aes(x=observation_date, y=Unemployment) ) + geom_line(color = "#00AFBB", size = 1)
ts_unemployment

ts_Core.HICP <- ggplot(data,aes(x=observation_date, y=Core.HICP) ) + geom_line(color = "#00AFBB", size = 1)
ts_Core.HICP

ts_HICP <- ggplot(data,aes(x=observation_date, y=HICP) ) + geom_line(color = "#00AFBB", size = 1)
ts_HICP

# Time Series
ts_brent <- ts(data$BRENT, start = c(1997,5), end=c(2022,12), frequency=12)
ts_brent <- as.quarterly(ts_brent, FUN=mean, na.rm=TRUE)

ts_unemployment <- ts(data$Unemployment, start = c(1997,5), end=c(2022,12), frequency=12)
ts_unemployment <- as.quarterly(ts_unemployment, FUN=mean, na.rm=TRUE)


ts_HICP <- ts(data$HICP, start = c(1997,5), end=c(2022,12), frequency=12)
ts_HICP <- as.quarterly(ts_HICP, FUN=mean, na.rm=TRUE)

ts_GDP <- ts(dataGDP$EUNNGDP, start = c(1997,01), end=c(2022,12), frequency=4)
ts_GDP

#corriger TS GDP
# Setting restriction matrix:
amat <- diag(4) # Identical matrix 4X4
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
amat[4,1] <- NA
amat[4,2] <- NA
amat[4,3]<- NA
amat

svar <- cbind(ts_brent,ts_HICP,ts_unemployment, ts_GDP)
length
colnames(svar) <- cbind("BRENT", "HICP", "Unemployment", "GDP")
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
  
  
  
  
  
  
  
  
  


