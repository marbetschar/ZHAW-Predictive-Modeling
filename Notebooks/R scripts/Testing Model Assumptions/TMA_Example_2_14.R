# Testing Model Assumptions Example 2.14
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
advertising <- read.csv("~/work/data/Advertising.csv")


# Fit Linear Model
fit <- lm(sales ~ TV, data = advertising)

# Create graph
par(mfrow=c(2,2))
plot(fit, col="darkgrey")