# Example Multiple Linear Regression 4.4

# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Credit <- read.csv("~/work/data/Credit.csv")


# Scatterplot showing all pairs of quantitative variables
pairs(~Balance + Age + Cards + Education + Income + Limit +
        Rating, Credit, pch = ".", col = "darkcyan")
