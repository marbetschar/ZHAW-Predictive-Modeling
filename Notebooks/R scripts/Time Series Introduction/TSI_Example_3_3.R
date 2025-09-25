# Read Tesla file
X = read.table("~/work/data/Tesla.csv", sep="\t", header=T, dec = ",")
Xts = ts(X[,2])
time = seq(as.Date(X[1,1]), length.out=19, by="3 months")
plot(Xts, main="Stock Index Tesla, Inc.", axes = F, ylab="value", xlab="")
axis(2)
axis(1, labels=time, at=seq(1, by=63, length.out = length(time)))
grid()

logRet = ts(diff(log(X[,2])))
plot(logRet, main="Log returns of Tesla, Inc. ", ylab="log returns", axes=F, xlab="")
axis(2)
axis(1, labels=time, at=seq(1, by=63, length.out = length(time)))
grid()
