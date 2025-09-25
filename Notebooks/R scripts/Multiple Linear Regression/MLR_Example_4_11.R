## Example Multiple Linear Regression 4.11
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Advertising <- read.csv("~/work/data/Advertising.csv")


# Fit model and write summary
summary(lm(sales ~ TV + radio + TV * radio, data = Advertising))
