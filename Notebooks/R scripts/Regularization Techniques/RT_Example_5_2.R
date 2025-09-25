library(caTools)
library(gbm)
library(iml)

boston <- read.csv('~/work/data/boston.csv')
set.seed(42)

sample <- sample.split(boston$medv, SplitRatio = 0.5)
train  <- boston[sample==TRUE,]
test   <- boston[sample==FALSE,]

model_gbm <- gbm(
    medv ~ .,
    data=train,
    distribution='gaussian',
    n.trees=5000,
    interaction.depth=4,
    shrinkage=0.001
) 

# Print MDI importance
print(model_gbm)
summary(model_gbm)

# Plot PDP for rm and lstat
# FeatureEffects not working with gbm 2.1.5
# mod <- Predictor$new(model_gbm, data=train)
# eff <- FeatureEffects$new(mod, feature=c('rm', 'lstat'), method='pdp')
# eff$plot()
par(mfrow = c(1, 2)) 
plot(model_gbm, i="rm")
plot(model_gbm, i="lstat")
