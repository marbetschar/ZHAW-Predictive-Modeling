## Example Random Forest 2.1

# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Set categorical features as factor
heart$AHD <- as.factor(heart$AHD)
heart$Thal <- as.factor(heart$Thal)
heart$ChestPain <- as.factor(heart$ChestPain)

require(tree)
# generate test and training set for evaluation
set.seed(123)
idx = sample(1:nrow(heart), replace = F, size = nrow(heart)/5)
testing = heart[idx, ]
training = heart[-idx, ]
n_train = nrow(training)
true.class = as.integer(testing$AHD)

# tree control for large tree
tc = tree.control(nobs = n_train, mincut = 1, minsize = 2,
                  mindev = 1e-10)

# define function for the mode
myMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# number of bootstrap samples
B = 1000
# init vectors
pred.class = matrix(0, nrow = B, ncol = nrow(testing))
test.error = rep(0, B)
# bootstrap loop
for (b in 1:B) {
  # resample
  cur_sample = training[sample(1:n_train, size = n_train,
                               replace = T), ]
  # grow large tree on resampled values
  cur_tree = tree(AHD ~ ., data = cur_sample, control = tc)
  # predict the test data on the current tree
  pred.class[b, ] = predict(cur_tree, newdata = testing,
                            type = "class")
  # majority vote (here we use our myMode function)
  cur.pred = apply(as.matrix(pred.class[1:b, ]), FUN = myMode,
                   MARGIN = 2)
  test.error[b] = sum(cur.pred != true.class)/nrow(testing)
}
# display results
plot(1:B, test.error, type = "l", col = "darkred")
grid()


