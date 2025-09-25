#(a)
  
?USArrests

#(b)
data <- USArrests

dat <- as.matrix(USArrests[,c("Murder","Assault","Rape")])

    
#(c)

stars(dat,draw.segments = TRUE, key.loc = c(15,1))

# Compared to the other states, there are only a few cases 
# of murder and rape and an average number of assaults in Rhode 
# Island. North Dakota is the state with the lowest number of 
# crimes overall, the lengths of all segments are 0.
  
  
#(d)
    
summary(dat)

# Since PC1 is in the direction of the largest variance we would 
# expect the variable Assault to get the	largest loadings for PC1.
  
  
#(e)

pc <- prcomp(dat, scale=FALSE)
pc$rotation

#Yes, the variable Assault clearly gets the largest loadings for PC1. 
# Note: There is a cutoff in the princomp function which is by default 
# at 0.1. If a loading is below that cutoff, it only shows blank 
# spaces in the output.
    
#(f) From now on we work with scaled data.

pc <- prcomp(dat, scale=TRUE)
pc$rotation
    
# PC1 is a weighted average of the three violence measures 
# (PC1=-0.583 Murder - 0.608 Assault - 0.539 Rape). States with 
# a large number of crimes have a large negative score of PC1.
    
#(g) 
pc$rotation[,2]
   
# The states with a high PC2 score are states with a low 
# number of Rape and at the same time high number of Murder.
    
#(h)

summary(pc)
    
# PC1 and PC2 explain approximately $94\%$ of the total variance.
    
#(i)
   
cor(pc$x[,1], pc$x[,2]) #they are uncorrelated!
    
 
plot(pc$x[,2]~pc$x[,1], pch="", ylab="PC2", xlab="PC1")
text(pc$x[,1],pc$x[,2],abbreviate(row.names(dat),5))
    
      
#(j)
# As the loadings for PC1 are all negative, the most violent state according to PC1 is Florida and the least violent one North Dakota. The state with the highest PC2 score is Mississippi. The same conclusions can be drawn from the star plot: Florida's star plot has very large segments for Assault and Murder with Rape being a slightly smaller segment, North Dakota's segments aren't
# even visible, as it has the smallest values for all three variables among all states and thus, the length	of the segments is equal to zero. The segment for Murder in the star plot of Mississippi is very large compared to
# the one for Rape. States which are close together in this plot as for example Alaska, Nevada and California (i.e. have similar scores for PC1 and PC2) also look very similar in the star plot. States that are far apart have very different shapes in the star plot.
