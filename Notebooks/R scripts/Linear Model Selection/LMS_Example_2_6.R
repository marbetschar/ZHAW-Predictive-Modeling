## Example Linear Model Selection Example 2.6
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Credit <- read.csv("~/work/data/Credit.csv")

# Regsubsets forward
library(leaps)
reg <- regsubsets(Balance ~ . , data=Credit, 
                  method="forward", nvmax=11)
reg.sum <- summary(reg)
# Find minimum in BIC:
which.min(reg.sum$bic)

# Complete Procedure using Step:
# Define full and empty models:
f.full <- lm(Balance ~ Income + Limit + Rating + Cards +
               Age + Education + Gender + Student + Married + Ethnicity,
             data = Credit)
f.empty <- lm(Balance ~ NULL, data = Credit)

# Find the optimal solution based on minimal BIC using forward Step:
step(f.empty, direction = "forward", trace = 0, k = log(nrow(Credit)),
     scope = list(lower = f.empty, upper = f.full))

# Find the optimal solution based on minimal BIC using backward Step:
step(f.full, direction = "backward", trace = 0, k = log(nrow(Credit)),
     scope = list(lower = f.empty, upper = f.full))

# Find the optimal solution based on minimal BIC using Hybrid method:
step(f.empty, direction = "both", trace = 0, k = log(nrow(Credit)),
     scope = list(lower = f.empty, upper = f.full))
