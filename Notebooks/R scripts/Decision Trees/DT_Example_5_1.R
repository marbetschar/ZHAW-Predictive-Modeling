require(tree)
# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Set categorical features as factor
heart$AHD <- as.factor(heart$AHD)
heart$Thal <- as.factor(heart$Thal)
heart$ChestPain <- as.factor(heart$ChestPain)
label = as.integer(heart$AHD)
cols = c("darkcyan", "darkred")

# logistic regression
lr.model = glm(AHD~MaxHR + Age, data = heart, family = "binomial")
plot(Age~MaxHR, data = heart, col = cols[label], pch=20)
coeffs = lr.model$coefficients
abline(-coeffs[1]/coeffs[3], -coeffs[2]/coeffs[3])
text(100, 70, labels = "Yes")
text(160, 30, labels = "No")
grid()

# classification error 
cost <- function(true.class, est.prob) { 
  mean((est.prob < 0.5) & true.class==1 |
         (est.prob > 0.5) & true.class==0)
}

# cross validated error
require(boot)
cv.error.lr <- cv.glm(glmfit=lr.model, data = heart, cost = cost, K = 5)

#decision tree
tree.model = tree(AHD~MaxHR + Age, data = heart)
tree.cv = cv.tree(tree.model, FUN = prune.misclass, K = 10)
idx = which.min(tree.cv$dev)
size = tree.cv$size[idx]
best.tree = prune.tree(tree.model, best = size)
plot(Age~MaxHR, data = heart, col = cols[label], pch=20)
partition.tree(best.tree, add = T)
grid()
cv.error.tree <- tree.cv$dev[idx]/303

