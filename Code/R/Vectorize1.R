#!/usr/bin/env R
# setwd("/home/saul/Documents/cmeecoursework/Week3/Code")
rm(list = ls())

# CHAPTER 9 - ADVANCED TOPICS IN R

# Vectorisation, using tools such as 'plyr'
# R is slow at running cycles/loops, because, unlike C which compiles the code before execution,
# R doesn't know what you're going to do until it reads the code to perform

M <- matrix(runif(1000000), 1000, 1000) 
# Creates one million random numbers, just like that!
# And then organises them into a matrix 1000 by 1000 in dimension!

# The following is a simple function to sum up all the numbers
# in the matrix and calculate the system time taken and return it
SumAllElements <- function(M){
  Dimensions <- dim(M)
  Tot <- 0
  for (i in 1:Dimensions[1]) {
    for (j in 1:Dimensions[2]) {
      Tot <- Tot + M[i,j]
    }
  }
   return(Tot) 
}
print(system.time(SumAllElements(M))) # Takes ~0.496 secs
print(system.time(sum(M))) # Takes ~0.002 secs = ~250x faster!

# The 'sum' function is a built-in function in R that is optimised for performance
# Hence it is mumch faster at summing up the matrix than the function described above
