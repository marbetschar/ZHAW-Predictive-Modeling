## Example Decision Trees 3.1

# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Set categorical features as factor
heart$AHD <- as.factor(heart$AHD)
heart$Thal <- as.factor(heart$Thal)
heart$ChestPain <- as.factor(heart$ChestPain)

require(tree)

# sample training and test data
set.seed(123)
n = length(heart[, 1])
idx.train = sample(n, 250)
idx.test = (1:n)[-idx.train]

# grow tree
tree.model = tree(AHD ~ ., data = heart[idx.train, ])

# predict class
pred.class = predict(tree.model, newdata = heart[idx.test,],
                     type = "class")
true.class = heart$AHD[idx.test]

# confusion matrix
addmargins(table(pred.class, true.class))

err.test = mean(abs(as.integer(pred.class) - as.integer(true.class)))
err.test
