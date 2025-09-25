# Example Simple Linear Regression 2.4
# Read the Data
~
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Advertising <- read.csv("~/work/data/Advertising.csv")


# Linearization using lm()
lm(sales ~ TV, data = Advertising)

# Plot of the data:
plot(
  sales ~ TV,
  col = "darkcyan",
  xlab = "TV",
  ylab = "Sales",
  pch = 20,
  data = Advertising
)
# plot the linear model
abline(lm(sales ~ TV, data = Advertising), col = "blue")

# Summary output of regression model
summary(lm(sales ~ TV, data = Advertising))
