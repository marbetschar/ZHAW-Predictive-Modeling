## Example Forecasting Time Series 1.3

abs(polyroot(c(1, -0.5, 0.5, 0.1)))

# Simulate time series
set.seed(23)
plot(arima.sim(model = list(ar = c(0.5, -0.5, -0.1)), n = 200),
     ylab = "value", main = "AR(3) process")

## Example Forecasting Time Series 1.5
ac = ARMAacf(ar = c(0.5, -0.5, -0.1), lag.max = 40)
plot(ac, type = "h", main = "ACF of an AR(3) process")
grid()


## Example Forecasting Time Series 1.6
pacf(ac, main = "Partial autocorrelation of AR(3)")
