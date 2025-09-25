# Example Multiple Linear Regression 2.1

# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Advertising <- read.csv("~/work/data/Advertising.csv")


# Find Coefficients of the Multiple Linear Regression Model:
coef(lm(sales ~ TV + radio + newspaper, data = Advertising))

# in order to find more important data, we can use summary()":
summary(lm(sales ~ TV + radio + newspaper, data = Advertising))

# To find the correlation between the different predictors:
# Save the data accordingly:
TV <- Advertising[, 2]
radio <- Advertising[, 3]
newspaper <- Advertising[, 4]
sales <- Advertising[, 5]

# Find correlation: 

cor(data.frame(TV, radio, newspaper, sales))

