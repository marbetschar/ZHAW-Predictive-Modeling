df = read.csv("~/work/data/temperatures.csv")

temp = df$Value

model = arima(temp, order = c(4,1,4))
model

plot(temp, type = "l", main = "Global temperature differences", 
     ylab = "temperature difference",  lwd = 2)
lines(temp - model$residuals, col = "red", lwd = 2)
legend("bottomleft", c("data", "ARIMA(4,1,4) model"), fill = c("black", "red"))

# first order diffence
plot(diff(temp), type = "l", lwd = 2, main = "Yearly difference for temperature", ylab = "difference")
grid()


# optimal model
P = c(1,2,3,4,5,6,7,8)
Q = c(1,2,3,4,5,6,7,8)
d = 1

AIC_mat = matrix(0, nrow = length(P), ncol = length(Q))

for (p in P){
  for (q in Q){
    model = arima(temp, order = c(p, d, q))
    AIC_mat[p, q] = model$aic
  }
} 

heatmap(AIC_mat, Colv = NA, Rowv = NA, scale="column", xlab = "q", ylab = "p")


# predict
temp_ts = ts(temp)
temp_train = ts(temp_ts[1:100])
temp_test = ts(temp_ts[101:107], start = 101)
model_opt = arima(temp_train, order = c(4,1,4))


pred_ts = predict((model_opt), n.ahead = 10)

plot(temp_train, xlim = c(0, 107), ylim = c(-1, 2), lwd =2, main ="ARIMA(4,1,4) model for temperature differences")
lines(temp_test, col = "blue", lwd=2)
lines(pred_ts$pred, col = "red", lwd=2)
lines(pred_ts$pred + 2*pred_ts$se, col = "red", lty = 2)
lines(pred_ts$pred - 2*pred_ts$se, col = "red", lty = 2)
legend("topleft", c("truth", "predicted"), fill = c("blue", "red"))
grid()


# Calculate the absolute errors
absolute_errors <- abs(temp_test - pred_ts$pred[1:7])

# Calculate the MAE
mae <- mean(absolute_errors)

# Print the MAE
print(paste("MAE:", mae))

# Calculate squared errors
squared_errors <- (temp_test - pred_ts$pred[1:7])^2

# Calculate the RMSE
rmse <- sqrt(mean(squared_errors))

# Print the RMSE
print(paste("RMSE:", rmse))

# Calculate the absolute percentage error
ape <- abs((temp_test - pred_ts$pred[1:7]) / temp_test)

# Calculate the MAPE
mape <- mean(ape) * 100

# Print the MAPE
print(paste("MAPE:", mape, "%"))

# Quartile Scores

pinball_loss <- function(y_true, y_pred, tau) {
  error <- y_true - y_pred
  loss <- ifelse(error > 0, tau * error, (1 - tau) * -error)
  mean(loss)
}


# Predict with confidence intervals
pred_ts <- predict(model_opt, n.ahead = 10, se.fit = TRUE)

# Mean forecast
mean_forecast <- pred_ts$pred

# Standard deviation of forecast errors
std_error <- pred_ts$se

# Function to compute quantile forecasts
compute_quantile_forecast <- function(mean_forecast, std_error, quantile) {
  return(mean_forecast + qnorm(quantile) * std_error)
}

# Calculate quantile forecasts
q25_forecasts <- compute_quantile_forecast(mean_forecast, std_error, 0.25)
q50_forecasts <- compute_quantile_forecast(mean_forecast, std_error, 0.50)
q75_forecasts <- compute_quantile_forecast(mean_forecast, std_error, 0.75)

# Print the quantile forecasts
print("25% Quantile Forecasts:")
print(q25_forecasts)
print("50% Quantile Forecasts:")
print(q50_forecasts)
print("75% Quantile Forecasts:")
print(q75_forecasts)

# Calculate pinball loss for each quantile
pinball_loss_25 <- pinball_loss(temp_test, q25_forecasts, 0.25)
pinball_loss_50 <- pinball_loss(temp_test, q50_forecasts, 0.50)
pinball_loss_75 <- pinball_loss(temp_test, q75_forecasts, 0.75)

# Print the pinball loss for each quantile
print(paste("Pinball Loss at 25% Quantile:", pinball_loss_25))
print(paste("Pinball Loss at 50% Quantile:", pinball_loss_50))
print(paste("Pinball Loss at 75% Quantile:", pinball_loss_75))
