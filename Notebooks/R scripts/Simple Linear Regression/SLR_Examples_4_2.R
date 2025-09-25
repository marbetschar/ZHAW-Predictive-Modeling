# Example Simple Linear Regression 4.2
# Read the Data
wd <- getwd()
print(setwd(dirname(wd)))
setwd(wd)
Advertising <- read.csv("~/work/data/Advertising.csv")



#Predict on basis of a linear model
predict(
  lm(sales ~ TV, data = Advertising),
  interval = "confidence",
  data.frame(TV = c(3, 100, 275)),
  level = 0.95
)

# Print confidence intervals for the expected values of y
dat <- data.frame(TV = 0:300)
plot(
  sales ~ TV,
  data = Advertising,
  col = "white",
  xlab = "TV",
  ylab = "Sales",
  pch = 20,
  ylim = c(0, 25),
  xlim = c(0, 300)
)
abline(lm(sales ~ TV, data = Advertising), col = "blue")
conf <- predict(lm(sales ~ TV, data = Advertising),
                interval = "confidence",
                newdata = dat)
lines(
  dat$TV,
  conf[, 2],
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
  conf[, 3],
  ylim = c(0, 25),
  xlim = c(0, 300),
  type = "l",
  xlab = "",
  ylab = "",
  col = "darkcyan",
  lwd = 2
)
for (i in c(3, 100, 275))
{
  lines(c(i, i), c(-10, conf[i, 3]), lty = "dashed")
  lines(c(i, i), c(conf[i, 2], conf[i, 3]), col = "red", lwd = 3)
}
