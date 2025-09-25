# Testing Model Assumptions Example 2.15
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
In <- read.csv("~/work/data/Income.csv")

# Fit Linear Model
fit <- lm(In[, 3] ~ In[, 2])

# Create graph
par(mar=c(4, 4, 3, 0) + 0.1)
par(mfrow=c(2, 2))
plot(fit, col="darkgrey")
