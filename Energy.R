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
energy <- data.frame(cbind('YYYYMM' = energy4$YYYYMM, 'Value' = energy4$Value))
tail(energy)

#Plot Energy Consumption Time Series (Relabel X Axis to show years)
myTicks <- seq(199000, 201500, by = 500)
plot(Value~YYYYMM, data = energy, type = 'b', xaxt = 'n', xlab = '',
     ylab = 'US Residential Energy Consumption (in Trillion Btu)')
axis(1, at=myTicks, labels = myTicks/100)
t <- length(energy)