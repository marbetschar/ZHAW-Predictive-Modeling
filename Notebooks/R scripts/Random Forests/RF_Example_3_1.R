## Example Random Forest 3.1

# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Set categorical features as factor
heart$AHD <- as.factor(heart$AHD)
heart$Thal <- as.factor(heart$Thal)
heart$ChestPain <- as.factor(heart$ChestPain)

require(randomForest)
set.seed(123)

# build bagged tree from data set with m = p
bag.model = randomForest(AHD ~ ., data = heart, na.action = na.omit,
                         mtry = ncol(heart) - 1, ntree = 200)

plot(bag.model$err.rate[, 1], type = "l", col = "darkcyan",
     ylim = c(0.1, 0.3), main = "OOB error estimates", ylab = "error rate")

grid()

# build random forest from data set with m = sqrt(p)
rf.model = randomForest(AHD ~ ., data = heart, na.action = na.omit,
                        mtry = round(sqrt(ncol(heart))), ntree = 200)

lines(rf.model$err.rate[, 1], type = "l", col = "darkred")

legend("topright", legend = c("bagging", "random forest"),
       col = c("darkcyan", "darkred"), lty = 1)

## Example Random Forest 3.2
importance(rf.model)

varImpPlot(rf.model, 
           main="Variable importance of random forest model")
