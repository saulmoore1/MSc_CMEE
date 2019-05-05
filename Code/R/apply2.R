#!/usr/bin/env R

# setwd("/home/saul/Documents/cmeecoursework/Week3/Code")
# rm(list = ls())

# There are a family of functions called '*apply', that vectorise your code for you
# Use 'apply' when you want to apply a function to the rows or columns of a matrix

# Or you can use 'apply' to define your own functions too:
SomeOperation <- function(v){
  if(sum(v) > 0){
    return(v * 100)
  }
  return(v)
}
# If the sum of the data is greater than 0, times the sum by 100 and return result, otherwise, just return the sum

M <-  matrix(rnorm(100), 10, 10) # Generate a square matrix of 100 random, uniformly distributed numbers
print(apply(M, 1, SomeOperation)) # Using the 'apply' function to iterate of the looped operation above

# Other types of *apply functions:
# 'lapply' is best for lists
# 'sapply' for when you are applying a function to each element of a list in turn, but you want a vector back, rather than a list
# 'vapply' optimized, faster version of 'sapply' for when you want faster code
# 'mapply' for several data structures (e.g. vectors, lists) and you want to apply a function to the 1st elements of each, and then the 2nd elements of each, etc., coercing the result to a vector/array as in 'sapply'
# 'tapply' is particularly useful, because it allows you to apply a function to subsets of a vector in a dataframe
