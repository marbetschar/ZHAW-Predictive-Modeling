## Exampole Concrete Compression Strength
# Values orded by size:
x <- c(24.4, 27.6, 27.8, 27.9, 28.5, 30.1, 30.3, 31.7,
       32.2, 32.8, 33.3, 33.5, 34.1, 34.6, 35.8, 35.9,
       36.8, 37.1, 39.2, 39.7)
# Quantiles
alpha_k <- (seq(1, length(x),by=1)-0.5)/length(x)

quantile_theor <- qnorm(alpha_k, mean=mean(x),sd=sd(x))
quantile_empir <- sort(x)

# Q-Q-Plot
qqplot(quantile_theor, quantile_empir, xlab="Theoritcal Quantile", 
       ylab="Empirical Quantile") 
