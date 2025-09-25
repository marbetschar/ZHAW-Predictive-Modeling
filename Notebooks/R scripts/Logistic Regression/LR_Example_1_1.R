## Example Logistic Regression Example 1.1

# indices of people who did not default
cond <- which(Default[, 1] == "No")
# randomly draw 333 people among people with default No
i <- sample(cond, 333)


plot(rep(0, length(i)) ~ balance[i], col = "darkcyan", 
     xlim = c(0, 2500), ylim = c(0, 1), 
     xlab = "Balance", ylab = "Probability for Default",
     pch = 20, cex = 0.7, data = Default)


par(new = T)

j <- which(Default[, 1] == "Yes")
plot(rep(1, length(j)) ~ balance[j], col = "darkred", 
     xlim = c(0, 2500), ylim = c(0, 1), 
     xlab = "", ylab = "", 
     pch = 20, cex = 0.7, data = Default)

regr.mod <- lm(
  (as.numeric(Default[c(i, j), 1]) - 1) ~ balance[c(i, j)], 
  data = Default)


beta0 <- regr.mod$coefficients[1]
beta1 <- regr.mod$coefficients[2]
abline(beta0, beta1, col = "blue", lwd = 2)

# Plot logistic function:
t <- seq(-7, 7, by = 0.1)
p <- exp(t)/(1 + exp(t))
plot(t, p, type = "l", xlab = "t", ylab = "p", col = "darkred")
grid()