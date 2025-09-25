## Example Multiple Linear Regression 4.17
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Credit <- read.csv("~/work/data/Credit.csv")


# Find the correlation matrix
round(cor(Credit[, -c(1, 8:11)]), digit = 3)


## EXample 14.18
# Now check for multicollinearity using the VIF values:
library(car)
vif(lm(Balance ~ Age + Rating + Limit, data = Credit))

summary(lm(Balance ~ Age + Rating + Limit, data = Credit))$r.squared

## Example 14.19
# As we have seen the collinearity between limit and rating, we can chose to drop rating from the set
vif(lm(Balance ~ Age + Limit, data = Credit))

summary(lm(Balance ~ Age + Limit, data = Credit))$r.squared


