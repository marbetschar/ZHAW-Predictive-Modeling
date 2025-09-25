library(glmnet)
Credit <- read.csv("~/work/data/Credit.csv")
Credit <- Credit[-1]
x <- model.matrix(Balance~.,data=Credit)[,-1]
y <- Credit$Balance
# Values for Lambda
ridge.mod <- glmnet(x,y,alpha=0)
dim(coef(ridge.mod))

# Coefficient matrix: rows correspond to coefficients, 
# columns correspond to lambda values, here for column 50
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

# l2 Norm for all coefficients for given values of lambda 
sqrt(sum(coef(ridge.mod)[-1,50]^2))

# Coefficient matrix: rows correspond to coefficients, 
# columns correspond to lambda values, here for column 60
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
# l2 Norm for all coefficients for given values of lambda 
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50,type="coefficients")[1:12,]
