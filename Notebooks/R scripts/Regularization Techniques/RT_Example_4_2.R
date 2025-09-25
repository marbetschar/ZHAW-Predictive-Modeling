## Example Random Forest 3.1

# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Load packages 
require(randomForest)
require(gbm)
set.seed(123)

# Set categorical features as factor
# heart$AHD <- as.factor(heart$AHD)
heart$AHD <- ifelse(heart$AHD == "Yes",1,0)
heart$Thal <- as.factor(heart$Thal)
heart$ChestPain <- as.factor(heart$ChestPain)

# Generate training and test dataset
heart_train <- sample(1:nrow(heart), 0.7*nrow(heart))

# generate vector containing test error for boosting


n.trees <- seq(1,3100,by=25)
test_error_boosting <- numeric(length(n.trees))
k <<- 0
for (i in n.trees){
  k <- k +1  
  boost_model = gbm(AHD ~ ., data = heart[heart_train,], distribution = "bernoulli", 
                    n.trees = i, shrinkage = 0.001,
                    interaction.depth = 3)  
  pred_heart <- ifelse(predict(boost_model, heart[-heart_train,], n.trees = i) > 0.5,"Yes","No")
  cr <- sum(diag(table(pred_heart, heart$AHD[-heart_train]))) / sum(table(pred_heart, heart$AHD[-heart_train]))
  test_error_boosting[k] <- 1 - cr
}

plot(n.trees,test_error_boosting, type = "l", col = "darkcyan",
     ylim = c(0.1, 0.5), ylab = "Test classification error")
grid()

# Build bagged tree from data set with m = p
heart$AHD <- as.factor(heart$AHD)
test_error_bagging <- numeric(length(n.trees))
k <<- 0
for (i in n.trees){
  k <- k +1  
  bagging.model <- randomForest(AHD~., data = heart[heart_train,], na.action = na.omit,
                                mtry = ncol(heart) - 1, ntree = i)
  pred_heart <- predict(bagging.model, heart[-heart_train,])
  cr <- mean(na.omit(pred_heart == heart$AHD[-heart_train]))
  test_error_bagging[k] <- 1 - cr
}

lines(n.trees, test_error_bagging, type = "l", col = "darkred")

# Build random forest from data set with m = sqrt(p)
test_error_rf <- numeric(length(n.trees))
k <<- 0
for (i in n.trees){
  k <- k +1  
  rf.model <- randomForest(AHD~., data = heart[heart_train,], na.action = na.omit,
                                mtry = round(sqrt(ncol(heart))), ntree = i)
  pred_heart <- predict(rf.model, newdata=heart[-heart_train,]) 
  cr <- mean(na.omit(pred_heart == heart$AHD[-heart_train]))
  test_error_rf[k] <- 1 - cr
}

lines(n.trees, test_error_rf, type = "l", col = "darkgreen")




legend("topright", legend = c("boosting","bagging", "random forest"),
       col = c("darkcyan", "darkred"), lty = 1)

