## Example Linear Model Selection Example 2.5
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Credit <- read.csv("~/work/data/Credit.csv")

# Complete Procedure using Step:
# Define full and empty models:
f.full <- lm(Balance ~ Income + Limit + Rating + Cards +
               Age + Education + Gender + Student + Married + Ethnicity,
             data = Credit)
f.empty <- lm(Balance ~ NULL, data = Credit)

# Find the optimal solution based on minimal AIC using Step:
step(f.empty, direction = "forward", 
     scope = list(lower = f.empty, upper = f.full))

# Or set trace=0 for less output:
# Find the optimal solution based on minimal AIC using Step:
step(f.empty, direction = "forward", trace = 0,
     scope = list(lower = f.empty, upper = f.full))