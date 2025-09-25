# Example Simple Linear Regression 4.4
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Advertising <- read.csv("~/work/data/Advertising.csv")



TV <- Advertising[, 2]
Verkauf <- Advertising[, 5]
predict(lm(Verkauf ~ TV),
        interval = "prediction",
        data.frame(TV = c(3, 100, 275)),
        level = 0.95)

# Plot
dat <- data.frame(TV = 0:300)
# Plot raw data
plot(
  sales ~ TV,
  col = "white",
  xlab = "TV",
  ylab = "Sales",
  pch = 20,
  ylim = c(0, 25),
  xlim = c(0, 300),
  data = Advertising
)

# Plot Interval Lines
abline(lm(sales ~ TV, data = Advertising), col = "blue")
pi <-
  predict(lm(sales ~ TV, data = Advertising),
          interval = "prediction",
          newdata = dat)
lines(
  dat$TV,
  pi[, 2],
  
  ylim = c(0, 25),
  xlim = c(0, 300),
  type = "l",
  xlab = "",
  ylab = "",
  col = "darkcyan",
  lwd = 2
)
lines(
  dat$TV,
  pi[, 3],
  ylim = c(0, 25),
  xlim = c(0, 300),
  type = "l",
  xlab = "",
  ylab = "",
  col = "darkcyan",
  lwd = 2
)
for (i in c(3, 100, 275)) {
  lines(c(i, i), c(-10, pi[i, 3]), lty = "dashed")
  lines(c(i, i), c(pi[i, 2], pi[i, 3]), col = "red", lwd = 3)
}
