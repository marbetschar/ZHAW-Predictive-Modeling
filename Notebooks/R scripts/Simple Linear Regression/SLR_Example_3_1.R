# Example Simple Linear Regression 3.1
# Random data
x <- runif(100, -2, 2)
y <- 2 + 3 * x + rnorm(100, 0, 4)

# plots: (creates new Random data)
par(mfrow=c(1,3))
for (i in 1:3)
{
  x <- runif(100,-2,2)
  y <- 2+3*x+rnorm(100,0,4)
  plot(x,y,ylim=c(-10,10),xlim=c(-2,2),col="darkcyan")
  abline(c(2,3),col="red")
  abline(lm(y~x),col="blue")
}
