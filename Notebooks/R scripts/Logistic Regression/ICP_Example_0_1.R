## Example Introduction to classification problems Example 0.1

# ISLR contains the Default data
library(ISLR)
# scales contains the `alpha` function for transparency
library(scales)
# indices of people who did not default
cond <- which(Default[, 1] == "No")
# randomly draw 333 among all people with default No
i <- sample(cond, 333)
plot(income[i] ~ balance[i], col = alpha("darkcyan", 0.5),
     xlim = c(0, 2500), ylim = c(0, 70000), xlab = "Balance",
     ylab = "Income", data = Default, pch = 19)
# indices of people who defaulted
i <- which(Default[, 1] == "Yes")
points(income[i] ~ balance[i], col = alpha("darkred", 0.5),
       pch = 19, xlab = "", ylab = "", data = Default)
grid()


# Boxplot
par(mfrow = c(1, 2))
boxplot(balance ~ default, col = c("darkcyan", "darkred"),
        xlab = "Default", ylab = "Balance", data = Default)
boxplot(income ~ default, col = c("darkcyan", "darkred"),
        xlab = "Default", ylab = "Income", data = Default)

