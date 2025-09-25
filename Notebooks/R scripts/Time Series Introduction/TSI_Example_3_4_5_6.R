## Example Introduction to Time Series 3.4

# Read the Data
wd = getwd()
setwd(dirname(wd))
AirData = read.table("~/work/data/AirQualityUCI.csv", 
                    sep = ";", header = T, dec = ",")

setwd(wd)

# Convert to ts
AirTmp.ts = ts(AirData[, c(13)], start = c(1, 18), frequency = 24)
end(AirTmp.ts)

# Plot
plot(AirTmp.ts, main = "Air Temperature measurement: full data set",
     ylab = "Temperature [C]", xlab = "Time [d]", 
     ylim = c(0,50))
grid()

# Plot section
AirTmpWin.ts = window(AirTmp.ts, start = c(1, 18), end = c(20,
                                                           18))
plot(AirTmpWin.ts, main = "Air Temperature measurement: detail",
     ylab = "Temperature [C]", xlab = "Time [d]", ylim = c(0,
                                                           50))
grid()

## Example 3.5
cycle(AirTmp.ts)[1]

cycle(AirTmp.ts)[875]

# Boxplot
boxplot(AirTmpWin.ts ~ cycle(AirTmpWin.ts), col = "darkcyan",
        main = "Air temperature")
grid()

## Example 3.6
lag.plot(AirTmpWin.ts, pch = 20, main = "")
lag.plot(AirTmpWin.ts, pch = 20, main = "", set.lags = 10)

