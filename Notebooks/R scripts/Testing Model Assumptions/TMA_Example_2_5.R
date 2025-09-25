# Testing Model Assumptions Example 2.5
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
advertising <- read.csv("~/work/data/Advertising.csv")


# Create graph
par(mfrow = c(1, 3))

# Fit Linear Model
fit <- lm(sales ~ TV, data = advertising)
# plot Residuals 
plot(fitted(fit), resid(fit), col = "darkgrey", 
     xlab = "Fitted Sales", ylab = "Residuals")
# Plot Regions and average of the running average
abline(v = c(9, 10, 17, 18))
points(c(9.5, 17.5), c(0.2, -0.1), pch = 20, col = "red", lwd = 3)

# In second graph, plot Residuals and running average
plot(fitted(fit), resid(fit), col = "darkgrey", 
     xlab = "Fitted Sales", ylab = "Residuals")
# The R-function ksmooth calculates the points on the
# red curve
fit_smooth <- ksmooth(fitted(fit), resid(fit), kernel = "box",
                      bandwidth = 2, n.points = 24, x.points = fitted(fit))
lines(fit_smooth, col = "red", lwd = 3)

# Alternative plot using R function directly:
plot(fit, col = "darkgrey", which = 1)
