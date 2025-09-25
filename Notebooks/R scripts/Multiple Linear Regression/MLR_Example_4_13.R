## Example Multiple Linear Regression 4.13
library(readr)

# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Auto <- read.csv("~/work/data/Auto.csv")



# Create  figure
par(mfrow = c(1, 2))

# Left figure, including scatter data and fitted line
plot(mpg ~ horsepower, col = "darkgrey", ylab = "Miles per gallon", data = Auto)
abline(lm(mpg ~ horsepower, data = Auto), col = "blue")
# Right figure, Residuals vs fitted value.
plot(lm(mpg ~ horsepower, data = Auto), col = "darkgrey", which = 1)


# Now a nonlinear term is included;
round(summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))
      $coef, digits = 5)

# R squared is now given by:
summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$r.squared

# And again the figures:
# Left figure, including scatter data and fitted polynomial
plot(mpg ~ horsepower, col="darkgrey", ylab="Miles per gallon", 
     xlim=c(50,220), data=Auto)
# For the polynomial:
b <- summary(lm(mpg ~ horsepower + I(horsepower^2), 
                data=Auto))$coef[,1]
x <- seq(50,250,1)
lines(x, b[1] + b[2]*x + b[3]*x^2, col="blue")

# Right figure, showing a 5th oder polynomial
plot(mpg ~ horsepower, col="darkgrey", ylab="Miles per Gallon", 
     xlim=c(50,220), data=Auto)
# For the polynomial
b <- summary(lm(mpg ~ horsepower + I(horsepower^2) + 
                  I(horsepower^3) + I(horsepower^4) + 
                  I(horsepower^5), data=Auto))$coef[,1]
x <- seq(50,250,1)
lines(x, b[1] + b[2]*x + b[3]*x^2 + b[4]*x^3 + b[5]*x^4
      + b[6]*x^5, col="darkgreen")
