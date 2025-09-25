## Example Mathematical Models for Time Series 3.2

# Read the Data
wd <- getwd()
setwd(dirname(wd))
AusEl = read.table("/work/hslu-predictive-modeling-in-R/data/AustralianElectricity.csv", 
                    sep = ";", header=T)
setwd(wd)

# Create Time Series and perform STL decomposition
AusEl.ts = ts(AusEl[, 2], frequency = 4)
res = stl(log(AusEl.ts), s.window = 16)
# the time series component contains 3 series where the
# third constitutes the remainder sequence
plot(res$time.series[, 3], 
     main = "Remainder: Australian Electricity")
grid()

# Read the Data
wd <- getwd()
setwd(dirname(wd))
AirData = read.table("/work/hslu-predictive-modeling-in-R/data/AirQualityUCI.csv", 
                     sep = ";", header = T, dec = ",")
setwd(wd)

# Create Time Series and perform STL decomposition
AirTmp.ts = ts(AirData[, c(13)], start = c(1, 18), 
               frequency = 24)
AirTmpWin.ts = window(AirTmp.ts, start = c(1, 18), 
                      end = c(20, 18))
res = stl(AirTmpWin.ts, s.window = 10)
# plot
plot(res$time.series[, 3], main = "Remainder: Air Temperature")
grid()