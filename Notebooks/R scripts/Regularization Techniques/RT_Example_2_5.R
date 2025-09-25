# CV: What is the best lambda value?
cv.out = cv.glmnet(x[train,],y[train],alpha =0)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam

# Compute ridge regression model for best lambda value
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test,], exact=T, x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=0)[1:12,]

