# non-linear data
set.seed (1)
x=matrix (rnorm (200*2) , ncol =2)
x[1:100 ,]=x[1:100 ,]+2
x[101:150 ,]= x[101:150 ,] -2
y=c(rep (1 ,150) ,rep (2 ,50) )
dat=data.frame(x=x,y=as.factor (y))
plot(x, col=y, xlab="X1", ylab="X2")

# first fit at second order polynomial 
library(e1071)
train <- sample(200, 100)
svmfit <- svm(y~., data=dat[train,], kernel ="polynomial",
              d = 2, cost =1)
plot(svmfit , dat[train ,], xlab="X1", ylab="X2")

# Extra! Table:
pred <- predict(svmfit, newdata=dat[-train, ])
table(predict = pred, truth = dat[-train, ]$y)

#  increased cost to reduce training errors
train <- sample(200 ,100)
svmfit <- svm(y ~ ., data=dat[train,], kernel ="polynomial",
              d = 2, cost =1e5)
plot(svmfit , dat[train ,], xlab="X1", ylab="X2")
# Extra! Table:
pred <- predict(svmfit, newdata=dat[-train, ])
table(predict = pred, truth = dat[-train, ]$y)

# Tune 
# set.seed(1)
# train <- sample(200 ,100)
tune.out <- tune(svm , y~., data=dat[train,], kernel ="polynomial",  
                 ranges = list(cost=c(0.1, 1 ,10, 100), 
                               d=c(1, 2, 3, 4, 5, 6)))
summary(tune.out)

# Predict
bestmod <- tune.out$best.model
pred <- predict(bestmod, newdata = dat[-train, ])
cm <- table(predict = pred, truth = dat[-train, ]$y)
cm
