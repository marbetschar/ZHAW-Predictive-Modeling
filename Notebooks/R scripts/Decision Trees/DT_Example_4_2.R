## Example Decision Trees 4.2

# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Set categorical features as factor
heart$AHD <- as.factor(heart$AHD)
heart$Thal <- as.factor(heart$Thal)
heart$ChestPain <- as.factor(heart$ChestPain)

require(tree)
# tree controls for large tree
tc = tree.control(nobs = 303, mincut = 1, minsize = 2, mindev = 1e-10)
# grow large tree
tree.model = tree(AHD ~ ., data = heart, control = tc)
# k-fold cross-validation (K=10)
tree.cv = cv.tree(tree.model, FUN = prune.tree, method = "misclass",
                  K = 10)
opt.alpha = tree.cv$k[which.min(tree.cv$dev)]
opt.tree = prune.tree(tree.model, method = "misclass", k = opt.alpha)
plot(opt.tree)
text(opt.tree, cex=0.8)
summary(opt.tree)

#loop through alpha
size <- tree.cv$size
err.train.prune <- size
count <- 0
for (i in size) {
  count <- count + 1
  res.tree = prune.tree(tree.model, best=i)
  if( i > 1){
    pred.train <- predict(res.tree, newdata = heart, type="class")
    err <- summary(res.tree)$misclass
    err.train.prune[count] = err[1]
  } else {
    err.train.prune[count] <- min(res.tree$frame$yprob)*res.tree$frame$n
  }
}

plot(tree.cv, type="b", pch=20, col = "darkcyan", ylim = c(0, 140))
points(size, err.train.prune, type="b", pch=20, col = "darkred")
grid()
legend(35, 130, legend = c("Training", "Cross-validation"), 
       col = c("darkred", "darkcyan"), pch = 20)
