## Example Forecasting Time Series 1.7

X = sunspot.month
plot(X, main = "sunspot number")
X.year = aggregate(X, nfrequency = 1, FUN = mean)
lines(X.year, col = "red", lwd = 2)

# Box-Cox squarte root transformation
X.year.sq = (sqrt(X.year) - 1) * 2
plot(X.year.sq)

# Compute Autocorrelation and partial autocorrelation
par(mfrow = c(1, 2))
acf(X.year.sq, lag.max = 50, main = "ACF")
pacf(X.year.sq, lag.max = 50, main = "PACF")
abline(v = 9, col = "red", lty = 3)

## Example Forecasting Time Series 1.8
# Create model using Burg Method
model = ar(X.year.sq, method = "burg", aic = F, order.max = 9)
# Plot
plot(X.year.sq, ylim = c(-5, 30))
lines(X.year.sq - model$resid, col = "red")
legend("topleft", legend = c("true", "estimated"), col = c("black",
                                                           "red"), lty = 1, cex = 0.6)

# show model:
model

# Examine Residual plots:
par(mfrow=c(1,2))
hist(model$resid, nclass = 50, col = "darkred", 
     main = "Histogram of the residuals")
qqnorm(model$resid, col = "darkred", pch=20); grid()

