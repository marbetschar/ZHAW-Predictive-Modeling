# Testing Model Assumptions Example 2.3

# Set Random seed
set.seed(7)
# Create random data along x^2 + 4
x <- seq(-4, 2, 0.4)
y <- -x^2 + 4 + rnorm(length(x), 0, 2)

# plot data
plot(x,y)
abline(lm(y ~ x))

# Summarize Linear model
summary(lm(y ~ x))

