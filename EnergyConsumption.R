#US Residential Energy Consumption
#http://www.eia.gov/totalenergy/data/monthly/index.cfm#consumption
energy1 <- read.csv("http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T02.01")

#Subset to TERCBUS (Total Energy Consumed by the Residential Sector)
energy2 <- subset(energy1,MSN=="TERCBUS")

#Subset to "my lifetime"
energy3 <- subset(energy2, energy2$YYYYMM > 199100)

#Remove Yearly Total
energy4 <- subset(energy3, energy3$YYYYMM%%100 != 13)

#Final dataframe for energy
energy <- energy4$Value
tail(energy)

#Plot Energy Consumption Time Series (Relabel X Axis to show years)
t <- length(energy)
plot(1:t, energy, type = 'b', ylab = "US Residential Energy (trillion Btu)", xlab = "Month")

#Model: ARIMA(1,1,1)x(1,1,1)_12

#Fit Model, Estimate Parameters
library(astsa)
energy.out <- sarima(energy,1,1,1,1,1,1,12)
energy.out$ttable

#Make Predictions for energy consumption over the next 2 years

energy.future <- sarima.for(energy,n.ahead = 27,1,1,1,1,1,1,12)

#Compute 95% prediction intervals
energy.future.L <- energy.future$pred - 2 * energy.future$se
energy.future.U <- energy.future$pred + 2 * energy.future$se

#table of predictions and prediction intervals
cbind(energy.future$pred,energy.future.L,energy.future.U)

#Graphic
plot(1:t, energy, type = 'b', ylab = "US Residential Energy (trillion Btu)", xlab = "Month",
     xlim = c(0,348), ylim = c(1000,3000))
lines(322:348, energy.future$pred, col = "darkorange2", type = 'b', pch = 19)
lines(322:348,energy.future.L,col="darkorange2",lty=2)
lines(322:348,energy.future.U,col="darkorange2",lty=2)

#Research Task: Predict future energy consumption values
#Data Features: Time Series, Monthly trends expected to coninue over the next 2 years

#Analysis Weaknesses: No explanitory variables, only seasonal predictors

#Challenge Question: Find another Research task and data
#Predict quarterly iPhone sales over the next two years
#Data from statista Q3 '07 to Q3 '17 at https://www.statista.com/statistics/263401/global-apple-iphone-sales-since-3rd-quarter-2007/