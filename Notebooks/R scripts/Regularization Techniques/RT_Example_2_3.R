library(glmnet)
Credit <- read.csv("~/work/data/Credit.csv")
Credit <- Credit[-1]
x <- model.matrix(Balance~.,data=Credit)[,-1]
y <- Credit$Balance
lasso.mod <- glmnet(x,y,alpha=1)

# Coefficient matrix: rows correspond to coefficients, 
# columns correspond to lambda values 
dim(coef(lasso.mod))

# Coefficient matrix: rows correspond to coefficients, 
# columns correspond to lambda values, here for column 50 
lasso.mod$lambda[50]
coef(lasso.mod)[,50]
# l2 Norm for all coefficients for given values of lambda 
sqrt(sum(coef(lasso.mod)[-1,50]^2))

# Coefficient matrix: rows correspond to coefficients, 
# columns correspond to lambda values, here for column 20
lasso.mod$lambda[20]
coef(lasso.mod)[,20]
# l2 Norm for all coefficients for given values of lambda 
sqrt(sum(coef(lasso.mod)[-1,20]^2))

predict(lasso.mod,s=50,type="coefficients")[1:12,]
