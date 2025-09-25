## Example Multiple Linear Regression 5.1
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Advertising <- read.csv("~/work/data/Advertising.csv")


# Fit model and write summary
summary(lm(sales ~ TV + radio + newspaper, data = Advertising))
mean(sales)

# The 95% confidence interval of the effect of each medium on sales:
round(confint(lm(sales ~ TV + radio + newspaper, 
                 data = Advertising)), digits = 3)

# Variance Inflation Factor tells us something about collinearity:
library(car)
round(vif(lm(sales ~ TV + radio + newspaper, 
             data = Advertising)), digits = 3)
