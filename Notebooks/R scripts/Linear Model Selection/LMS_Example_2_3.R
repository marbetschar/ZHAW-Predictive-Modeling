## Example Linear Model Selection Example 2.3
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Credit <- read.csv("~/work/data/Credit.csv")

# Find R^2 for the best model of each size:
reg <- regsubsets(Balance ~ ., data = Credit, method = "forward",
                  nvmax = 11)
reg.sum <- summary(reg)
round(reg.sum$rsq, 5)

# Alternatively, look at the adjusted R^2
round(reg.sum$adjr2, 5)

# Find Maximum:
which.max(reg.sum$adjr2)

# Plot the Results:
par(mfrow = c(1, 2))
# Left Graph showing R^2
plot(reg.sum$rsq, type = "l", col = "blue", xlab = "Number of Predictors",
     ylab = expression(R^2))
points(reg.sum$rsq, pch = 20)
# Right graph showing adjusted R^2
plot(reg.sum$adjr2, type = "l", col = "blue", xlab = "Number of Predictors",
     ylab = "adjusted R squared")
points(reg.sum$adjr2, pch = 20)
points(which.max(reg.sum$adjr2), reg.sum$adjr2[which.max(reg.sum$adjr2)],
       col = "red", pch = 2)