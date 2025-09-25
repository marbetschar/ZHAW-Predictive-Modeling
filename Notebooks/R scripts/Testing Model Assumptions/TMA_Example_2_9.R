# Testing Model Assumptions Example 2.9
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
advertising <- read.csv("~/work/data/Advertising.csv")


# Create graph
par(mfrow = c(1, 1))

# Fit Linear Model
fit <- lm(sales ~ TV, data = advertising)

# Alternative plot using R function directly:
plot(fit, col = "darkgrey", which = 3)
