## Example Forecasting Time Series 1.10
# Simulate A1 process
set.seed(123)
a1 = 0.7
a1.ts = arima.sim(model = list(ar = a1), n = 150)
a1.train.ts = window(a1.ts, start = 1, end = 130)
plot(a1.ts, lty = 3, main = "Prediction of an AR(1) process")
lines(a1.train.ts)
# estimate the model
model = ar(a1.train.ts, aic = F, method = "burg", order.max = 1)
# predict future values
pred = predict(model, n.ahead = 20)
# plot the predicted values
lines(pred$pred, col = "red", lwd = 2)
# confidence bands
lines(pred$pred + 1.96 * pred$se, col = "red", lty = 2)
lines(pred$pred - 1.96 * pred$se, col = "red", lty = 2)
grid()
