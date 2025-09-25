## Example Introduction to Time Series 3.7
# Filter
weights = c(0.5, rep(1, 11), 0.5) / 12
est.trend <- filter(AirPassengers, filter = weights, sides = 2)

# plot Original and filtered data
plot(est.trend, lwd = 2, ylim = c(100, 700))
lines(AirPassengers, col = "darkcyan")
legend("topleft", legend = c("data", "trend"), lty = 1,
       col = c("darkcyan", "black"))
grid()

## Example Introduction to Time Series 3.8

# Substract trend from data
est.season <- AirPassengers - est.trend
cyc <- factor(cycle(AirPassengers))
head(cyc)

length(cyc)

# Take mean and group by month
est.season.month <- tapply(est.season, cyc, mean, na.rm = T)
head(est.season.month)

length(est.season.month)

est.season <- est.season.month[cyc]

# Plot season estimates
plot(est.season, type = "l")
abline(h = 0)

## Example Introduction to Time Series 3.9
# Plot estimated remainder after removing trend and seasonal effects
est.rem <- AirPassengers - est.trend - est.season
plot(as.vector(est.rem), type = "l", ylab = "rem") # needs fix

# Logarithmic transform
log.data = log(AirPassengers)
# trend estimation of log data
est.trend.log <- filter(log.data, filter = weights, sides = 2)
# seasonality estimation for log data
est.season.log <- log.data - est.trend.log
est.season.month <- tapply(est.season.log, cyc, mean, na.rm = T)
est.season.log <- est.season.month[cyc]

# remainder term estimation for log data
est.rem.log <- log.data - est.trend.log - est.season.log

# Plot Remainder
plot(as.vector(est.rem.log), type = "l", ylab = "rem") 


# Alternative by decompose command
decomposed.data = decompose(log(AirPassengers))
plot(decomposed.data)

## Example Introduction to Time Series 3.10
stl.fit = stl(log(AirPassengers), s.window = 10)
plot(stl.fit)

monthplot(stl.fit)
