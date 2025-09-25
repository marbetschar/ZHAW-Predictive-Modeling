library(ISLR)
names(Khan)

dim(Khan$xtrain )

dim(Khan$xtest )

length(Khan$ytrain)

length(Khan$ytest)

table(Khan$ytrain )

table(Khan$ytest )

# Fit SVM
dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm(y~., data=dat, kernel ="linear", cost =10)
summary(out)


table(out$fitted, dat$y)


# Fit om Test data
dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

