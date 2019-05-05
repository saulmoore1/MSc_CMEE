#!/usr/bin/env R

################################
### SCRIPT FOR DEMONSTRATION ###
###    Tidyverse Functions   ###
###   (apply, plyr & dplyr)  ###
################################
library("plyr")
library("dplyr")

M <- matrix(rnorm(100), 10, 10)
RowMeans <- apply(M, 1, mean)
ColMeans <- apply(M, 2, mean)

RowVariance <- apply(M, 1, var)
ColVariance <- apply(M, 2, var)

SomeOperation <- function(v){
  # A function to multiply vector (row/column) by 100 if the sum of the contents of the vector is positive
  if(sum(v) > 0){
    return(v * 100)
  }
  return(v)
}
Result <- apply(M, 1, SomeOperation)

x <- 1:20
y <- factor(rep(letters[1:5], each = 4))
# Add up the values in x within each subgroup defined by y:
print(tapply(x, y, sum))

# Replicate function
attach(iris)
print(iris)
set.seed(1)
by(iris[,1:2], iris$Species, colMeans)
by(iris[,1:2], iris$Petal.Width, colMeans)
print(replicate(10, runif(5))) # Replicate in a column-wise fashion

set.seed(1)
d <- data.frame(year = rep(2000:2002, each = 3),
                count = round(runif(9, 0, 20)))
print(d)
ddply(d, "year", function(x) {
  mean.count <- mean(x$count)
  sd.count <- sd(x$count)
  cv <- sd.count / mean.count
  return(data.frame(cv.count = cv, sd.count = sd.count, mean.count = mean.count))
})

ddply(d, "year", summarise, mean.count = mean(count))

ddply(d, "year", transform, total.count = sum(count))

ddply(d, "year", mutate, mu = mean(count), sigma = sd(count), cv = sigma/mu)

# Plotting with plyr
par(mfrow = c(1, 3), mar = c(2, 2, 1, 1), oma = c(3, 3, 0, 0))
d_ply(d, "year", transform, plot(count, main = unique(year), type = "o"))
mtext("count", side = 1, outer = TRUE, line = 1)
mtext("frequency", side = 2, outer = TRUE, line = 1)
