# Example Multiple Linear Regression 3.1

# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Advertising <- read.csv("~/work/data/Advertising.csv")


# Find the summary, including F Statistic
summary(lm(sales ~ TV + radio + newspaper, data = Advertising))
