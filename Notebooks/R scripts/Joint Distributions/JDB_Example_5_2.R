# Example 5.2
# Principle component
pr.out <- prcomp(USArrests[,c("Murder","Assault")],scale=FALSE)
# Principle Variance
pr.var <- pr.out$sdev^2
# Principle variance explained
pve <- pr.var/sum(pr.var)
pve