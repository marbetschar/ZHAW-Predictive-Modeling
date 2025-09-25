## Example Linear Model Selection Example 2.2
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Credit <- read.csv("~/work/data/Credit.csv")


# Again, define full and empty model:
f.full <- lm(Balance ~ Income + Limit + Rating + Cards +
               Age + Education + Gender + Student + Married + Ethnicity,
             data = Credit)
f.empty <- lm(Balance ~ NULL, data = Credit)

# Drop each variable separately and calculate RSS, AIS and Sum of Squares
drop1(f.full, scope = f.full)

# Update the model without Education
f.9 <- update(f.full, . ~ . - Education)

# Again, check which variable should be dropped next. 
drop1(f.9, scope = f.9)

# Alternatively, use the build-in "regsubsets" method
library(leaps)
reg <- regsubsets(Balance ~ ., data = Credit, method = "backward",
                  nvmax = 11)
reg.sum <- summary(reg)
reg.sum$which