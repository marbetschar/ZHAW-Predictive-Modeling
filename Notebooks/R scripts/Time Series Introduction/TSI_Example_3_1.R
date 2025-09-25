## Example Introduction to Time Series 3.1

# define the Box-Cox transformation
box.cox <- function(x, lambda) {
  if (lambda == 0)
    log(x) else (x^lambda - 1)/lambda
}
# plot the original and the transformed data
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(AirPassengers, main = "Original", ylab = "", xlab = "")
plot(box.cox(AirPassengers, 2), main = "lambda = 2", ylab = "",
     xlab = "")
plot(box.cox(AirPassengers, -0.5), main = "lambda = -0.5",
     ylab = "", xlab = "")
plot(box.cox(AirPassengers, 0), main = "lambda = 0", ylab = "",
     xlab = "")

