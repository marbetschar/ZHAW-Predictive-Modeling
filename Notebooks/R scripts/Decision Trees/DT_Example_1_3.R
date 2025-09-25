## Example Decision Trees 1.3

# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Set categorical features as factor
heart$AHD <- as.factor(heart$AHD)
heart$Thal <- as.factor(heart$Thal)
heart$ChestPain <- as.factor(heart$ChestPain)

require(tree)

tree.model = tree(AHD~., data = heart)
plot(tree.model, type="proportional")
text(tree.model, all = F, cex=0.7)
