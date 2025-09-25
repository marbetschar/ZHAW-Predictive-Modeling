# Read in Advertising data set
library(glmnet)
set.seed(1)
Advertising <- read.csv("~/work/data/Advertising.csv")
Advertising <- Advertising[,-1]
x <- model.matrix(sales~.,data=Advertising)[,-1]
y <- Advertising$sales
# generate train and test data set
train <- sample(1:nrow(x), nrow(x)/2)
test =(-train)
y.test <- y[test]

# train lasso model
lasso.mod <- glmnet(x[train,],y[train],alpha=1)
# determine best value of lambda by means of cross-validation
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod,s=bestlam,newx=x[test,])
# Test MSE 
mean((lasso.pred-y.test)^2)
# Compute lasso model on entire data set and determine best lambda value
# by means of cv
out <- glmnet(x,y,alpha=1)
cv.out <- cv.glmnet(x,y,alpha=1)
bestlam <- cv.out$lambda.min
bestlam
predict(out,type="coefficients",s=bestlam)[1:4,]

