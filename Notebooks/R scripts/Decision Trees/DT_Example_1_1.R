## Example Decision Trees 1.1

# Read the Data
heart <- read.csv("~/work/data/Heart.csv", header=T)

# Set categorical features as factor
heart$AHD <- as.factor(heart$AHD)
heart$Thal <- as.factor(heart$Thal)
heart$ChestPain <- as.factor(heart$ChestPain)

require(xtable)
xtable(head(heart[,2:9]), caption = "Excerpt of the Heart data set (8 out of 13 predictors).", label = "tab:Heart1")

# Plot results
label = as.integer(heart$AHD)
cols = c("darkcyan", "darkred")
plot(MaxHR~Age, data = heart, col = cols[label] , pch=20)
legend("bottomleft", legend = c("no heart disease", "heart disease"), 
       col = cols, pch=20)
grid()
