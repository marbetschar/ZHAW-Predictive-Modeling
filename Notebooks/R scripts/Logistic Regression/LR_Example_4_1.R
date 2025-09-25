## Example Logistic Regression Example 4.1
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
glm.fit.ds <- glm(default ~ balance, family = binomial,
                  data = Default.ds)

# cross validated error
cv.error <- boot::cv.glm(glmfit = glm.fit.ds, data = Default.ds,cost = cost, K = 5)
cv.error$delta[1]
