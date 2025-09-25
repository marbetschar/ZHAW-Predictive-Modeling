## Example Linear Model Selection Example 2.1
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Credit <- read.csv("~/work/data/Credit.csv")

# Define Full and empty model
f.full <- lm(Balance ~ Income + Limit + Rating + Cards +
               Age + Education + Gender + Student + Married + Ethnicity,
             data = Credit)
f.empty <- lm(Balance ~ NULL, data = Credit)

# Add each variable separately and calculate RSS, AIS and Sum of Squares
add1(f.empty, scope = f.full)

# Update the empty model to contain Rating, as was found previously
f.1 <- update(f.empty, . ~ . + Rating)

# Search for the next Variable
add1(f.1, scope = f.full)

# Update the empty model to contain income, as was found previously
f.2 <- update(f.1, . ~ . + Income)

# Search for the next Variable
add1(f.2, scope = f.full)

# Alternatively, use the build-in "regsubsets" method
library(leaps)
reg <- regsubsets(Balance ~ ., data = Credit, method = "forward",
                  nvmax = 11)
reg.sum <- summary(reg)
reg.sum$which
