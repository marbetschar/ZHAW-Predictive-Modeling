## Example Decision Trees 1.2

# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Set categorical features as factor
heart$AHD <- as.factor(heart$AHD)
heart$Thal <- as.factor(heart$Thal)
heart$ChestPain <- as.factor(heart$ChestPain)

require(tree)
# default controls
tc = tree.control(nobs = 303, mincut = 5, minsize = 10,
                  mindev = 0.01)
# grow tree
tree.model = tree(AHD ~ Age + MaxHR, data = heart, control = tc, )
# plot tree and label splits
plot(tree.model)
text(tree.model, cex = 0.8)

# plot partition (only for two predictor case)
label = as.integer(heart$AHD)
cols = c("darkcyan", "darkred")
partition.tree(tree.model)
points(Age ~ MaxHR, data = heart, col = cols[label], pch = 20)
