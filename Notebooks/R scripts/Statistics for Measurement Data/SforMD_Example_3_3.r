## Example 3.3 t-test fusion heat
# 5% interval on t distribution with n = 13
qt(.975,13-1)

# Fusion heat data: n-=13
x <- c(79.98, 80.04, 80.02, 80.04, 80.03,
       80.03, 80.04, 79.97, 80.05, 80.03,
       80.02, 80.00, 80.02)

# t-test directly
t.test(x, alternative = "two.sided",
       mu = 80.00, conf.level = 0.95)