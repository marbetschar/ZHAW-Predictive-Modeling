## Example Forecasting Time Series 1.11
# Sunspot data:
X = sunspot.month
X.year = aggregate(X, nfrequency = 1, FUN = mean)
# Box-Cox squarte root transformation
X.year.sq = (sqrt(X.year) - 1) * 2
# create training set
X.year.sq.train = window(X.year.sq, start = 1749, end = 1989)
# build model
model = ar(X.year.sq.train, aic = F, order.max = 9, method = "burg")
# prdict future values
pred = predict(model, n.ahead = 23)
# do some plotting
plot(X.year.sq, lty = 3, ylim = c(-5, 30), xlim = c(1950,
                                                    2012))
lines(X.year.sq.train)
lines(pred$pred, col = "red")
lines(pred$pred + 1.96 * pred$se, col = "red", lty = 2)
lines(pred$pred - 1.96 * pred$se, col = "red", lty = 2)
grid()