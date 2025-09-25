# EXample 3.1
library(plot3D)

# Image settings:
par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 1))

# x and y grid, mesh M
x <- seq(0, 1,length.out=50)
y <- seq(0, 1,length.out=50)
M <- mesh(x, y)

# plot surface
surf3D(x = M$x,
       y = M$y,
       z = 2*M$x+2*M$y-4*M$x*M$y,
       phi = 30, 
       theta = 30,
       bty = "b2",
       nticks=5,
       ticktype="detailed")