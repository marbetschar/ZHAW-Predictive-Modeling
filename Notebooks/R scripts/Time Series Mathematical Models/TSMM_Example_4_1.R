## Example Mathematical Models for Time Series 4.1
# create white noise
set.seed(123)
w = rnorm(1000, mean = 2, sd = 0.1)
# filter and define time series
MA = ts(filter(w, filter = rep(1, 3)/3, sides = 2))
MA = na.omit(MA)

# Auto covariance
ac = acf(MA, type = "covariance", lag.max = 50, 
         ylim = c(0, 0.004))
# theoretical autocovariance
sigma = 0.1
cv.true = rep(0, 1, length(ac$acf))
cv.true[1] = 3 * sigma^2/9
cv.true[2] = 2 * sigma^2/9
cv.true[3] = sigma^2/9
points(ac$lag + 0.3, cv.true, pch = 18, 
       col = "darkred", type = "h")
legend("topright", legend = c("estimated", "theoretical"),
       lty = 1, col = c("black", "darkred"))

## Example Mathematical Models for Time Series 4.2
# plot correlogram:
acf(MA, lag.max = 50)
