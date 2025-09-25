## Example Linear Model Selection Example 2.4
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Credit <- read.csv("~/work/data/Credit.csv")

# Regsubsets forward
library(leaps)
reg <- regsubsets(Balance ~ . , data=Credit, 
                  method="forward", nvmax=11)
reg.sum <- summary(reg)

# Print minimal cp
which.min(reg.sum$cp)

# Plot cp:
par(mfrow = c(1, 1))
plot(reg.sum$cp, type="l", col="blue", xlab="Number of Predictors",
     ylab="cp (AIC)")
points(reg.sum$cp, pch=20)
points(which.min(reg.sum$cp), reg.sum$cp[which.min(reg.sum$cp)],
       col="red",pch=2)