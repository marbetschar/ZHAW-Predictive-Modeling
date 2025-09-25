# Example Multiple Linear Regression 1.3

library(scatterplot3d)

# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
income <- read.csv("~/work/data/Income2.csv")


# 3d Scatterplot:
s3d <- scatterplot3d(income[, 2], income[, 3], income[, 4], 
                     highlight.3d = T, type = "h", pch = 16, angle = 30,
                     xlab = "Years of Education", 
                     ylab = "Years of Experience",
                     zlab = "Income")

# Find coefficients of the linear model:
coef(lm(income ~ education + experience, data = income))

