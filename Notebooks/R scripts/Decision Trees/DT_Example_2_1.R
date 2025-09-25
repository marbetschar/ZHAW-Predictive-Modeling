## Example Decision Trees 2.1

# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Set categorical features as factor
heart$AHD <- as.factor(heart$AHD)

label = as.integer(heart$AHD)
cols = c("darkcyan", "darkred")

require(tree)
# Default: entropy
tree.model = tree(AHD ~ MaxHR + Age, data = heart)
plot(tree.model)
text(tree.model, cex=0.8)

partition.tree(tree.model)
points(Age ~ MaxHR, data = heart, col = cols[label], pch=20)

# Gini index
tc = tree.control(303, mincut = 5, minsize = 60, mindev = 0.01)
tree.model = tree(AHD ~ MaxHR + Age, data = heart, 
                  split = "gini", control = tc)
plot(tree.model)
text(tree.model, cex=0.8)

partition.tree(tree.model)
points(Age ~ MaxHR, data = heart, col = cols[label], pch=20)
