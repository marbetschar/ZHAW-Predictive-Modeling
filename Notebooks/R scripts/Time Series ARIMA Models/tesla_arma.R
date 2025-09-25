# read data
df = read.csv("~/work/data/TeslaIdx1.csv", sep = "\t")
plot(df$Close, type="l")

log_ret = log(df$Close[2:1001]/df$Close[1:1000])
plot(log_ret, type="l")

model = arima(log_ret, order = c(1, 0, 1))
summary(model$arma)

plot(log_ret[100:200], type = "l", main = "Log-returns Tesla stock index", 
     ylab = "log return", ylim = c(-0.3, 0.3), lwd = 2)
lines(log_ret[100:200] - model$residuals[100:200], col = "red", lwd = 2)
legend("bottomleft", c("data", "ARMA(1,1) model"), fill = c("black", "red"))
       