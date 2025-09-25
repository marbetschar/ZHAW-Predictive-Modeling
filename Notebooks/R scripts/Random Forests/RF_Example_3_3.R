## Example Random Forest 3.3

# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Set categorical features as factor
heart$AHD <- as.factor(heart$AHD)

require(randomForest)
set.seed(123)


rf.model = randomForest(AHD ~ MaxHR + Age, data = heart,
                        na.action = na.omit, mtry = 2, ntree = 200)
tree.model = tree(AHD ~ MaxHR + Age, data = heart, method = "gini")
mhr = seq(min(heart$MaxHR), max(heart$MaxHR), by = 0.1)
age = seq(min(heart$Age), max(heart$Age), by = 0.1)

pred.forest = function(m, a) {
  return(as.integer(predict(rf.model, data.frame(Age = a,
                                                 MaxHR = m, type = "class"))))
}
pred.tree = function(m, a) {
  return(as.integer(predict(tree.model, data.frame(Age = a,
                                                   MaxHR = m), type = "class")))
}
z.rf = outer(mhr, age, pred.forest)
z.tree = outer(mhr, age, pred.tree)

# plot 
image(mhr, age, as.matrix(z.rf), col = c("darkcyan", "darkred"))
image(mhr, age, as.matrix(z.tree), col = c("darkcyan", "darkred"))
