## Example Logistic Regression Example 3.1
# Logistic model
glm.fit <- glm(default ~ balance, family = binomial, data = Default)
# predict balance=1000:
predict(glm.fit, type = "response", 
        newdata = data.frame(balance = 1000))

# predict balance=2000:
predict(glm.fit, type = "response", 
        newdata = data.frame(balance = 2000))


## Example 3.2
# predict the class probabilites for the training data
pred.prob <- predict(glm.fit, type = "response")
# predict class by thresholding
pred.class <- as.integer(pred.prob > 0.5)
# compute classification error
true.class <- as.integer(Default$default == "Yes")
err.train <- mean(abs(pred.class - true.class))
err.train


## Example 3.3 
# Confusion table
cm = table(pred.class, true.class, 
           dnn = c("predicted","true"))
addmargins(cm)


## Exaple 3.8
# F1-Score
library(MLmetrics)
F1_Score(y_pred = pred.class, y_true = true.class, 
         positive = "1")



## EXample 3.9
F1_Score(y_pred = pred.class, y_true = true.class, 
         positive = "0")



## Example 3.10
# resample
idx.yes = which(Default$default == "Yes")
idx.no = sample(which(Default$default == "No"), replace = F,
                size = 333)
idx = c(idx.yes, idx.no)
Default.ds = Default[idx, ]
glm.fit.ds <- glm(default ~ balance, family = binomial,
                  data = Default.ds)
# predict the class probabilites for the training data
pred.prob.ds <- predict(glm.fit.ds, type = "response")
# predict class by thresholding
pred.class.ds <- as.integer(pred.prob.ds > 0.5)
# compute training classification error
true.class.ds <- as.integer(Default.ds$default == "Yes")
err.train.ds <- mean(abs(pred.class.ds - true.class.ds))
# compute confusion matrix
cm = table(pred.class.ds, true.class.ds, 
           dnn = c("predicted", "true"))



addmargins(cm)

# F1 scores
F1_Score(y_pred = pred.class.ds, y_true = true.class.ds,
         positive = "0")
F1_Score(y_pred = pred.class.ds, y_true = true.class.ds,
         positive = "1")


