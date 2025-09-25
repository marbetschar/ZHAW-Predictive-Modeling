library (MASS)
library (gbm)
# Read in Boston data set
train = sample (1: nrow(Boston ), nrow(Boston )/2)
set.seed(1)
# Fit boosting model for regression setting
boost.boston =gbm(medv~.,data=Boston[train ,], 
                  distribution="gaussian",n.trees=5000 , interaction.depth =4)
# Plot variable importance
summary(boost.boston)

# Compute MSE on test data set
yhat.boost=predict(boost.boston, newdata=Boston[-train,], n.trees =5000)
boston.test=Boston[-train ,"medv"]
mean((yhat.boost - boston.test)^2)

# we can perform boosting with a different value of the shrinkage parameter 
# lambda; The default value is 0.001, but this is easily modified. Here we 
# take lambda = 0.2.
boost.boston =gbm(medv~.,data=Boston[train,], distribution="gaussian",
                  n.trees=5000, interaction.depth=4, shrinkage =0.2, verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)
# This results in a MSE which is lower than the previous MSE with lambda=0.01
