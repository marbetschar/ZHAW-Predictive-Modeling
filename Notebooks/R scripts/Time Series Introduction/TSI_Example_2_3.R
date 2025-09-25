## Example Introduction to Time Series 2.3

# Read the Data

X.beer = read.table("~/work/data/AustralianBeer.csv", sep = ";", header=T)


# Convert to ts
X.beer.ts = ts(X.beer[, 2], 
               start = c(1956, 1), end = c(1994, 2), 
               frequency = 4)
# Print summary
summary(X.beer.ts)

# Plot
plot(X.beer.ts, ylab = "Beer [Ml]", 
     main = "Beer production in Australia")
X.ts.w = window(X.beer.ts, 
                start = c(1980, 3), end = c(1990, 1))

lines(X.ts.w, col = "darkred", lwd = 2)
grid()

# Print Summary
summary(X.ts.w)
