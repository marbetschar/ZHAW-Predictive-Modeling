## Example Random Forest 2.2

# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Set categorical features as factor
heart$AHD <- as.factor(heart$AHD)
heart$Thal <- as.factor(heart$Thal)
heart$ChestPain <- as.factor(heart$ChestPain)

# number of observations
n = nrow(heart)
true_class = as.integer(heart$AHD)

# tree control for large tree
tc = tree.control(nobs = n, mincut = 1, minsize = 2, mindev = 1e-10)

# define function for the mode
myMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# number of bootstrap samples
B = 1000
# init vectors
pred.class = matrix(0, nrow = B, ncol = n)
cur_pred = vector("integer", length = n)
oob.error = rep(0, B)

# init list for OOB samples
oob_samples = list()

# bootstrap loop
for (b in 1:B) {
  # sample
  cur_idx = sample(1:n, size = n, replace = T)
  # bootstrap sample
  cur_sample = heart[cur_idx, ]
  # store OOB samples
  oob_samples[[b]] = (1:n)[-cur_idx]
  # grow large tree on resampled values
  cur_tree = tree(AHD ~ ., data = cur_sample, control = tc)
  # predict the complete data on the current tree
  pred.class[b, ] = predict(cur_tree, newdata = heart,
                            type = "class")
  
  # loop over all observations and compute OOB error
  for (i in 1:n) {
    oob_list = lapply(oob_samples, FUN = function(x) {
      match(i, x)
    })
    oob_idx = which(!is.na(oob_list))
    # majority vote
    cur_pred[i] = myMode(pred.class[oob_idx, i])
  }
  
  oob.error[b] = sum(cur_pred != true_class)/n
}

# display results
plot(1:B, oob.error, type = "l", col = "darkcyan", 
     ylim = c(0.1, 0.3), ylab = "")
lines(1:B, test.error, col = "darkred")

grid()
legend("bottomright", legend = c("test error", "OOB error"),
       col = c("darkred", "darkcyan"), lty = 1)

