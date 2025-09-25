# Example Multiple Linear Regression 3.5

# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Advertising <- read.csv("~/work/data/Advertising.csv")


# Find the confidence interval of the Predict sales, 
# given the budget for TV and radio:
predict(lm(sales ~ TV + radio, data = Advertising), 
        interval = "confidence",
        data.frame(TV = 100, radio = 20))

# Find the prediction interval:
predict(lm(sales ~ TV + radio, data = Advertising), 
        interval = "prediction",
        data.frame(TV = 100, radio = 20))