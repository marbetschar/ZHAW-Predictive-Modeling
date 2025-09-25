triv = rnorm(100)
plot(triv, type = "l")

model = arima(triv, order = c(1,0,1))
model


lines(triv - model$residuals, col ="red")


## simulate arma(1,1)
arma11 = arima.sim(model = list(ar = c(1.5, -0.9), ma = 1.2), n = 100)

plot(arma11, type = "l")

model1 = arima(arma11, order = c(2,0,1))
model1

fitted = arma11 - model1$residuals
lines(fitted, col ="red")
