# Read in Credit data set and split it into train and test set
par(mar=c(4,4,0,0)+0.1)
library(glmnet)
set.seed(1)
Credit <- read.csv("~/work/data/Credit.csv")
Credit <- Credit[-1]
x <- model.matrix(Balance~.,data=Credit)[,-1]
y <- Credit$Balance
train <- sample(1:nrow(x), nrow(x)/2)
test =(-train)
y.test <- y[test]
# Fit and plot lasso model
lasso.mod <- glmnet(x[train,],y[train],alpha=1)
plot(lasso.mod)
# Find best lambda value by means of cross validation
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
# Prediction with best lambda value on test data set
lasso.pred <- predict(lasso.mod, s=bestlam ,newx=x[test,])
# Test MSE 
mean((lasso.pred-y.test)^2)

# Fit lasso on entire data set, cross-validation to determine optimal lambda
out <- glmnet(x,y,alpha=1)
cv.out <- cv.glmnet(x,y,alpha=1)
bestlam <- cv.out$lambda.min
bestlam
# Print coefficients
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:12,]
lasso.coef
