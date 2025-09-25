## Example Mathematical Models for Time Series 1.1
# Random samples d and cumulative sum x
d = sample(c(-1, 1), replace = T, size = 10000)
x = ts(cumsum(d))
# Plot
plot(x, main = "Random Walk", ylab = "y-deviation in [m]")
grid()

# Add drift  (delta)
set.seed(12)
d = sample(c(-1, 1), replace = T, size = 10000)
delta = 0.05
y = seq(from = 0, to = (10000 - 1) * delta, by = delta)
y = y + cumsum(d)

# plot
plot(ts(y), main = "Random Walk with drift", 
     ylab = "y-deviation in [m]")
lines(cumsum(d), col = "darkcyan")
grid()

