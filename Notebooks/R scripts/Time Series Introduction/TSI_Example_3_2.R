## Example Introduction to Time Series 3.2

AP = AirPassengers
AP.back = lag(AP, k = 4)
AP.ahead = lag(AP, k = -5)
plot(AP, lwd = 2)
lines(AP.back, col = "darkcyan")
lines(AP.ahead, col = "darkred")
grid()
legend("topleft", 
       legend = c("original", "shifted back", "shifted ahead"), 
       lty = 1, col = c("black", "darkcyan", "darkred"))