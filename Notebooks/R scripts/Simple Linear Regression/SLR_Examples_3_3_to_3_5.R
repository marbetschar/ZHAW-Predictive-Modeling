# Example Simple Linear Regression 3.3
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Advertising <- read.csv("~/work/data/Advertising.csv")



# Summary
summary(lm(sales ~ TV, data = Advertising))

# EXample 3.4
# TWo sides p value
2 * (1 - pt(17.66518, df = 198))

# Example 3.5
confint(lm(sales ~ TV, data = Advertising), level = 0.95)

# Determine the 97.5% quantile of a t-distribution
qt(0.975, 20 - 2)
