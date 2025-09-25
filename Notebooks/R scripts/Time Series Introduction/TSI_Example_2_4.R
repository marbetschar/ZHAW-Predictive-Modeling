## Example Introduction to Time Series 2.4

# Read the Data
wd = getwd()
setwd(dirname(wd))
X.elec = read.table("~/work/data/AustralianElectricity.csv", 
                    sep = ";", header=T)
X.beer = read.table("~/work/data/AustralianBeer.csv", 
                    sep = ";", header=T)
setwd(wd)

# Convert to ts
X.elec.ts = ts(X.elec[, 2], start = c(1956, 1), end = c(1994, 2), 
               frequency = 4)
X.beer.ts = ts(X.beer[, 2], start = c(1956, 1), end = c(1994, 2), 
               frequency = 4)

# Bind Beer and Elec
X.ts = cbind(X.beer.ts, X.elec.ts)
# Plot
plot(X.ts, main="Beer and electricity production in Australia")
grid()

# aggregate the monthly data of the AirPassengers data
# to quarterly data
AP.quarter = aggregate(AirPassengers, nfrequency = 4)

AP.elec = ts.intersect(AP.quarter, X.elec.ts)
plot(AP.elec,
     main = "Air Passenger bookings and electricity production")
grid()

