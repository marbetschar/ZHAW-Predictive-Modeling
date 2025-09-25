# Testing Model Assumptions Example 2.4
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
advertising <- read.csv("~/work/data/Advertising.csv")


# Create graph
par(mfrow = c(1, 2), pty="s")

# plot data points:
plot(sales ~ TV, col = "darkgrey", data = advertising)

# Fit Linear Model and  plot
fit <- lm(sales ~ TV, data = advertising)
abline(fit, col = "blue")

# Plot the Residuals in the second figure
plot(fitted(fit), resid(fit), col = "darkgrey", xlab = "Fitted",
ylab = "Residuals")

# Plot y = o
abline(h = 0, lty = "dashed", col = "blue")

