library(caTools)
library(mice)
library(iml)
library(randomForest)

titanic <- read.csv('~/work/data/titanic.csv')

set.seed(42)

titanic['random_cat'] <- as.factor(sample.int(3, nrow(titanic), replace=TRUE))
titanic['random_num'] <- rnorm(nrow(titanic), mean=0, sd=3)

categorical_columns <- list('pclass', 'sex', 'embarked', 'random_cat')
numerical_columns <- list('age', 'sibsp', 'parch', 'fare', 'random_num')

X <- titanic[unlist(c(categorical_columns, numerical_columns, 'survived'))]
X['embarked'] <- as.factor(X$embarked)
X['survived'] <- as.factor(X$survived)
X['sex'] <- as.factor(X$sex)
imputed <- mice(X, m=5, maxit = 40, print=FALSE)
imputed_data <- complete(imputed, 5)
sample <- sample.split(imputed_data$survived, SplitRatio = 0.7)
train_data  <- imputed_data[sample==TRUE,]
test_data   <- imputed_data[sample==FALSE,]


rf <- randomForest(survived ~., data=train_data)
rf

# Accuracy on training set
mean(predict(rf, data=train_data) == train_data$survived)

# Accuracy on test set
mean(predict(rf, newdata=test_data) == test_data$survived)

# Caret variable importance
varImpPlot(rf, main="Variable importance on train data")

# Permutation feature importance from iml
mod <- Predictor$new(rf, data=train_data)
mod_test <- Predictor$new(rf, data=test_data)

eff <- FeatureImp$new(mod, loss='ce')
eff$plot()

eff_test <- FeatureImp$new(mod_test, loss='ce')
eff_test$plot()

# Permutation feature importance from iml
mod <- Predictor$new(rf, data=train)
mod_test <- Predictor$new(rf, data=test)

eff <- FeatureImp$new(mod, loss='ce')
eff$plot()

eff_test <- FeatureImp$new(mod_test, loss='ce')
eff_test$plot()
