## Example Logistic Regression Example 2.2
# lOgistic model
glm.fit <- glm(default ~ balance, family = binomial, data = Default)
# print summary
summary(glm.fit)$coef
# Print confidence interval
confint(glm.fit)