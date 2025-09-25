# The standardized ridge regression coefficients are displayed for
# the Credit data set, as a function of ln(lambda)
library(glmnet)
par(mar=c(4,4,0,0)+0.1)
Credit <- read.csv("~/work/data/Credit.csv")
Credit <- Credit[-1]
x <- model.matrix(Balance~.,data=Credit)[,-1]
y <- Credit$Balance
grid <- exp(seq(15,-5,length=100))
ridge.mod <- glmnet(x,y,alpha=0, lambda=grid, standardize=FALSE)
plot(ridge.mod, xvar="lambda", label=TRUE, col=1:11, lwd=3)
legend(10,300, c("Student","Rating","Limit","Income"), lwd=c(3,3,3,3),col=c(8,3,2,1))
