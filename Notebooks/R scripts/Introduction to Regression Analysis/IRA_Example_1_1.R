# Example Introduction to Regression Analysis 1.1
# Read the Data

wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
adv <- read.csv("~/work/data/Advertising.csv")
<<<<<<< HEAD
=======

>>>>>>> 3862254a959b23d9ded1e2798f9fd9b21965828f

# Plot the Dat~
par(mfrow = c(1, 3))

plot(sales ~ TV, col = "darkcyan", 
     xlab = "TV", ylab = "Sales", data = adv)
plot(sales ~ radio, col = "darkcyan", 
     xlab = "Radio", ylab = "Sales", data = adv)
plot(sales ~ newspaper, col = "darkcyan", 
     xlab = "Newspaper", ylab = "Sales", data = adv)
