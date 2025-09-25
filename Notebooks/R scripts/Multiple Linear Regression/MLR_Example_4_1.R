# Example Multiple Linear Regression 4.1

# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Advertising <- read.csv("~/work/data/Advertising.csv")


# Compare two models using the anova() function:
anova(lm(sales ~ TV + radio, data = Advertising), 
      lm(sales ~ TV + radio + newspaper, data = Advertising))

# EXample Multiple Linear Regression 4.3
# Compare two models, now omitting tv data:
anova(lm(sales ~ radio + newspaper, data = Advertising),
      lm(sales ~ TV + radio + newspaper, data = Advertising))

# get an "overview" using drop1()
drop1(lm(sales ~ TV + radio + newspaper, data = Advertising),
      test = "F")