# EXample 5.1
# Principle component loadings;
pr.out <- prcomp(USArrests[,c("Murder","Assault")])
pr.out$rotation[,1]

# Principle component scores
pr.out <- prcomp(USArrests[,c("Murder","Assault")])
head(pr.out$x)

# Second component loadings
pr.out <- prcomp(USArrests[,c("Murder","Assault")])
pr.out$rotation[,2]


