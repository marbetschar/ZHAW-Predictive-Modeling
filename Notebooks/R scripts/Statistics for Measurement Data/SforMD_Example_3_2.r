## Example 3.2 t test
# Normally distributed data points:
x <- c(5.9, 3.4, 6.6, 6.3, 4.2, 2.0, 6.0, 4.8,
       4.2, 2.1, 8.7, 4.4, 5.1, 2.7, 8.5, 5.8,
       4.9, 5.3, 5.5, 7.9)

# Estimate mean and sigma
mean.x <-mean(x); mean.x
sigma.x <- sd(x); sigma.x

# t, Test statistic for the t-test
t <- (mean.x-5)/(sigma.x/sqrt(20)); t

# P(T =< t), probability that T is smaller or Eqaul t 
pt(t, df=20-1)

