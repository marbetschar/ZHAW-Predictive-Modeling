## Example Multiple Linear Regression 4.14
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Advertising <- read.csv("~/work/data/Advertising.csv")


# Create figure and plot
par(mfrow = c(1, 2))
# Residuals vs fitted, including the cross term TV*radio in the right plot
plot(lm(sales ~ TV + radio, data = Advertising), 
     which = 1, col = "darkgrey")
plot(lm(sales ~ TV + radio + TV * radio, data = Advertising),
     which = 1, col = "darkgrey")

# Scale location plot:
plot(lm(sales ~ TV + radio, data = Advertising), 
     which = 3, col = "darkgrey")
plot(lm(sales ~ TV + radio + TV * radio, data = Advertising),
     which = 3, col = "darkgrey")

# Cooks distance:
plot(lm(sales ~ TV + radio, data = Advertising),
     which = 5, col = "darkgrey", cook.levels = c(0.1, 0.5, 1))
plot(lm(sales ~ TV + radio + TV * radio, 
        subset = -c(131, 156), data = Advertising), 
     which = 5, col = "darkgrey", cook.levels = c(0.1, 0.5, 1))

     
