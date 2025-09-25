## Example Multiple Linear Regression 4.5
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Credit <- read.csv("~/work/data/Credit.csv")


# Set contrast
Credit$Gender <- as.factor(Credit$Gender)
contrasts(Credit$Gender) <- contr.treatment(2, base=1)
contrasts(Credit$Gender)

# Print a summary 
model = lm(Balance ~ Gender, data = Credit)
round(summary(model)$coef, digits = 5)


## Example Multiple Linear Regression 4.6 
# Set another level as base:
contrasts(Credit$Gender) <- contr.treatment(2, base=2)
contrasts(Credit$Gender)

# Print a summary 
model = lm(Balance ~ Gender, data = Credit)
round(summary(model)$coef, digits = 5)


## Example Multiple Linear Regression 4.7
# Now use a -1/+1 coding scheme, instead of 0/+1
contrasts(Credit$Gender) <- contr.sum(2)
contrasts(Credit$Gender)

# Print a summary 
model = lm(Balance ~ Gender, data = Credit)
round(summary(model)$coef, digits = 5)


## Example Multiple Linear Regression 4.8
# Now we consider the case, where we have 3 levels instead of two.
# Set contrast
Credit$Ethnicity <- as.factor(Credit$Ethnicity)
contrasts(Credit$Ethnicity) <- contr.treatment(3, base=1)
contrasts(Credit$Ethnicity)

# Print a summary 
model = lm(Balance ~ Ethnicity, data = Credit)
round(summary(model)$coef, digits = 5)

