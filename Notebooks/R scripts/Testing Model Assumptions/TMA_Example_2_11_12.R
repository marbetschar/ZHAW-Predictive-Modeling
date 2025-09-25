# Testing Model Assumptions Example 2.11
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
advertising <- read.csv("~/work/data/Advertising.csv")


# Create graph
par(mfrow = c(1, 3))

# Fit Linear Model
fit <- lm(sales ~ TV, data = advertising)

# Plot histogram:
hist(resid(fit), col = "darkgrey", main = "", 
     xlab = "Residuals", ylab = "Frequency", xlim = c(-10, 10))
box()

# New graph in same figure:
par(new = T)

x <- seq(-10, 10, length = 100)
# Plot Normal density function estimated based on the data
plot(x, dnorm(x, mean(resid(fit)), sd(resid(fit))), xlab = "", 
     xlim = c(-10, 10), ylab = "", type = "l", col = "darkcyan", axes = F)

## EXample 2.12: 
# QQ Plot using fit lm 

# Create graph
par(new = F)
plot(fit, which = 2)

## Check deviation from centre line by sampling
qq <- qqnorm(rstandard(fit), main = "", ylab = "Standardized Residuals")
# for 100 ramdom samples
for (i in 1:100) {
  sresid <- rnorm(length(qq$x), mean(qq$y), sd(qq$y))
  lines(sort(qq$x), sort(sresid), col = "grey")
}
points(qq$x, qq$y, pch = 20)
qqline(rstandard(fit), lty = 2)
