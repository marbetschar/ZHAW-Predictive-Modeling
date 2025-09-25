library(tree)
library(ISLR)

# Read the Data
data("Hitters")

# clean data from rows with missing cases 
Hitters <- Hitters[complete.cases(Hitters),]
Hitters$Salary <- log(Hitters$Salary)

# create Tree model
setup <- tree.control(nrow(Hitters), mincut=5, minsize=10, mindev =0.05)
h.tree<-tree(Salary ~ Hits +Years, data=Hitters,control=setup)
plot(h.tree,lwd=3)
text(h.tree, pretty=0, cex=1.2, col="blue")

# plot partition (only for two predictor case)
partition.tree(h.tree)
points(Hits ~ Years, data = Hitters,  pch=20)
