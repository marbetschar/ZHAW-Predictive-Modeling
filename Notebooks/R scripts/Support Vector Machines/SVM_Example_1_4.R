set.seed(3)
x <- matrix(rnorm (20*2,2) , ncol =2)
y <- c(rep (-1,10) , rep (1 ,10) )
x[y==1,1] = x[y==1,1] - 3
x[y==1,2] = x[y==1,2] + 3

dat <- data.frame(x=x, y=as.factor(y))
library (e1071)
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit , dat)
svmfit$index
summary(svmfit)
