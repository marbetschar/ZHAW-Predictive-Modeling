## Example Random Forest 0.1

# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Set categorical features as factor
heart$AHD <- as.factor(heart$AHD)
heart$Thal <- as.factor(heart$Thal)
heart$ChestPain <- as.factor(heart$ChestPain)

set.seed(123)
require(tree)
n = nrow(heart)
idx1 = sample(1:n, replace = F, size = n/2)
train1 = heart[idx1, ]
train2 = heart[-idx1, ]
par(mfrow = c(1, 2))

# tree 1
tree.fit1 = tree(AHD ~ ., data = train1)
plot(tree.fit1)
text(tree.fit1, cex = 0.4)

# tree 2
tree.fit2 = tree(AHD ~ ., data = train2)
plot(tree.fit2)
text(tree.fit2, cex = 0.4)
