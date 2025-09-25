# Testing Model Assumptions Example 2.6
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
In <- read.csv("~/work/data/Income.csv")


# Create graph
par(mar = c(4, 4, 0, 0) + 0.1)
par(mfrow = c(1, 2))

# Plot data raw data
plot(In[, 3] ~ In[, 2], col = "darkgrey", 
     xlab = "Years of Education", ylab = "Income")
# Fit and plot Linear Model
fit <- lm(In[, 3] ~ In[, 2], data = In)
abline(fit, col = "blue")
# Plot Residuals vs fitted Income
plot(fitted(fit), resid(fit), col = "darkgrey", xlab = "Fitted",
     ylab = "Residuals")
# Plot moving average, using loess.smooth() Method
lines(loess.smooth(fitted(fit), resid(fit)), col = "red",
      lwd = 3)
abline(h = 0, lty = "dashed", col = "blue")

## Simulations

# Create graph
par(mar=c(4,4,0,0)+0.1)

# simulate new residual plots and add smoothing curve
set.seed(7)
plot(fitted(fit), resid(fit), col="darkgrey",
     xlab="Fitted", ylab="Residuals")
for (i in 1:100)
{
  sresid <- sample(resid(fit), replace=TRUE)
  lines(loess.smooth(fitted(fit), sresid), col="lightgrey", lwd=1) }
lines(loess.smooth(fitted(fit), resid(fit)), col="red", lwd=1) 
abline(h=0, col="blue", lty="dashed")

