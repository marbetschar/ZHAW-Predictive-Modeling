## Example Logistic Regression Example 5.1
# classification error
cost <- function(true.class, est.prob) {
  mean((est.prob < 0.5) & true.class == 1 | 
         (est.prob > 0.5) & true.class == 0)}

# resample
idx.yes = which(Default$default == "Yes")
idx.no = sample(which(Default$default == "No"), replace = F,
                size = 333)
idx = c(idx.yes, idx.no)
Default.ds = Default[idx, ]

# logistic regression fit on downsampled data set
glm.fit <- glm(default ~ balance + income + student, family = binomial,
               data = Default.ds)
summary(glm.fit)$coef

# predict class by thresholding
pred.class <- as.integer(predict(glm.fit, 
                                 type = "response") > 0.5)

# compute confusion matrix
true.class <- as.integer(Default.ds$default == "Yes")
cm = table(pred.class, true.class, 
           dnn = c("predicted", "true"))
addmargins(cm)

# cross validated error
cv.error <- boot::cv.glm(glmfit = glm.fit, data = Default.ds,
                   cost = cost, K = 5)
cv.error$delta[1]
