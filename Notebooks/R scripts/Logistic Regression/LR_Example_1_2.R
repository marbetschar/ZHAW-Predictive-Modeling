## Example Logistic Regression Example 1.2
balance <- Default[, 3]

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

# Estimation of coefficients beta_0 und beta_0 and
# plotting of logistic regression curve
glm.fit <- glm( 
  (as.numeric(Default[c(i, j), 1]) - 1) ~ balance[c(i, j)],
  family = binomial, data = Default)
beta0 <<- summary(glm.fit)$coef[1]
beta1 <<- summary(glm.fit)$coef[2]
eq = function(x) {
  (exp(beta0 + beta1 * x)/(1 + exp(beta0 + beta1 * x)))
}
curve(eq, 0, 2700, add = T, col = "blue", lwd = 2)
grid()