library(glmnet)
set.seed(1)
Credit <- read.csv("~/work/data/Credit.csv")
Credit <- Credit[-1]
x <- model.matrix(Balance~.,data=Credit)[,-1]
y <- Credit$Balance

# Indices for training data set - random sampling with replacement
train <- sample(1:nrow(x), nrow(x)/2)
test =(-train)

# Indices for training data set - random sampling with replacement
y.test <- y[test]

#Fit ridge regression model
ridge.mod <- glmnet(x[train,],y[train],alpha=0)
ridge.pred <- predict(ridge.mod,s=4,newx=x[test,])

# MSE
mean((ridge.pred-y.test)^2)
mean((mean(y[train ])-y.test)^2)

# Ridge model with lambda -> infty
ridge.pred=predict(ridge.mod ,s=1e10 ,newx=x[test ,])
mean((ridge.pred-y.test)^2)


# Ridge-Model with: Least-Squares Model
ridge.pred <- predict(ridge.mod,s=0,newx=x[test,], 
                      exact=T, x=x[train,],y=y[train])
# MSE
mean((ridge.pred-y.test)^2)

