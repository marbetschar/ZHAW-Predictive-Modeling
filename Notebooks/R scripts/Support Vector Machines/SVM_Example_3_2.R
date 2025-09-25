# Radial kernel
set.seed(1)
library(e1071)
train <- sample(200 ,100)
svmfit <- svm(y~., data=dat[train,], kernel ="radial", gamma = 1, cost =1)
plot(svmfit , dat[train ,], xlab="X1", ylab="X2")
# Summary
summary(svmfit)

#Increased cost:
library(e1071)
train <- sample(200 ,100)
svmfit <- svm(y~., data=dat[train,], kernel ="radial", gamma=1, cost=1e5)
plot(svmfit , dat[train ,], xlab="X1", ylab="X2")

# Tune:
set.seed(1)
tune.out <- tune(svm , y~., data=dat[train,], kernel ="radial", ranges = list
                 (cost=c(0.1 ,1 ,10 ,100 ,1000), gamma=c(0.5, 1, 2, 3, 4)))
summary(tune.out)

cm <- table(true=dat[-train,"y"], pred=predict(tune.out$best.model, 
                                               newdata=dat[-train,]))
cm