# Example 2.1
# Probability density function
dunif(x=5, min=1, max=10)

# Probabilities
punif(q=5, min=1, max=10)
punif(4.8, 1, 10) - punif(1.2, 1, 10)

#Unifortmlyu distributed random variables
runif(5,min=1,max=10)


## Example 2.5
pexp(4, rate=3)


## Example 2.7
1-pnorm(130,mean=100, sd=15)

pnorm(115,mean=100, sd=15)-pnorm(85,mean=100, sd=15)


## Example 4.4
pnorm(5100,5000,sqrt(2500))

pbinom(5100,10000,.5)
