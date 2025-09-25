## Example Mathematical Models for Time Series 1.2
# White noise w
w = ts(rnorm(1000))
plot(w, main = "White noise", ylab = "value")
grid()

## Example Mathematical Models for Time Series 1.3
# Filter signal with window 3
window = c(1, 1, 1)/3
v = filter(w, sides = 2, window)
plot(v, main = "MA process", ylab = "")


## Example Mathematical Models for Time Series 1.4
# Autoregressive filter 
ar = filter(w, filter = c(1.5, -0.9), method = "recursive")
plot(ar, main = "AR(2) process", ylab = "")
grid()