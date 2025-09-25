## Example Random Forest 1.1

# Distribution properties:
mu = c(0, 1.1)
Sigma = matrix(0.7, nrow = 2, ncol = 2) + diag(2) * 0.3
Sigma

# Properties estimated from samples
require(MASS)
set.seed(123)
ex_data = mvrnorm(n = 2000, mu = mu, Sigma = Sigma)
Sigma_est = cov(ex_data)
Sigma_est

# Repeat sampling procedure B times
B = 1000
hat_s = rep(0, 1, B)
for (i in 1:B) {
  sampled_data = mvrnorm(n = 2000, mu = mu, Sigma = Sigma)
  hat_s[i] = cov(sampled_data)[1, 2]
}
mean(hat_s)

# Bootstrap procedure
B = 1000
hat_s_boot = rep(0, 1, B)
for (i in 1:B) {
  resampled_data = ex_data[sample(1:2000, replace = TRUE,
                                  size = 2000), ]
  hat_s_boot[i] = cov(resampled_data)[1, 2]
}

# plot results
par(mfrow = c(1, 2))
hist(hat_s, nclass = 50, col = "darkred", ylim = c(0, 80),
     xlab = "Estimators from simulated data", main = "")
rug(hat_s)
hist(hat_s_boot, nclass = 50, col = "darkcyan", 
     ylim = c(0, 80), 
     xlab = "Estimators from bootstrapped data", main = "")
rug(hat_s_boot)

# Confirm with boot package:
require(boot)

stat = function(dat, idx) {
  return(cov(dat[idx, 1], dat[idx, 2]))
}
hat_s_boot = boot(ex_data, stat, R = 1000)$t

