set.seed(1)
x <- matrix (rnorm (20*2,2) , ncol =2)
y <- c(rep (-1,10) , rep (1 ,10) )
x[y==1,] = x[y==1,] + 1

dat <- data.frame(x=x, y=as.factor (y))

library(e1071)
svmfit1 <- svm(y~., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
svmfit2 <- svm(y~., data = dat, kernel = "linear", cost = 1, scale = FALSE)
svmfit3 <- svm(y~., data = dat, kernel = "linear", cost = 100, scale = FALSE)

plot(svmfit1, dat)
plot(svmfit2, dat)
plot(svmfit3, dat)

c(length(svmfit1$index),length(svmfit2$index),length(svmfit3$index))

