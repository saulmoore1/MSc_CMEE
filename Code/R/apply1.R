#!/usr/bin/env R
# rm(list=ls())
# setwd("~/Documents/cmeecoursework/Week3/Code")

# A simple script to demonstrate the 'apply' function in R to calculate mean and variance of rows/columns in a matrix


# There are a family of functions called '*apply', that vectorise your code for you
# Use 'apply' when you want to apply a function to the rows or columns of a matrix

M <- matrix(rnorm(100), 10, 10) # Build a random matrix

RowMeans <- apply(M, 1, mean) # Take the mean of each row
print(RowMeans)

RowVars <- apply(M, 1, var) # Calculate the variance
print(RowVars)

ColMeans <- apply(M, 2, mean) # Now the mean of the columns
print(ColMeans)

# If you want row/column means or sums for a 2D matrix, 
# be sure to investigate the highly optimized, lightning-quick colMeans, rowMeans, colSums, rowSums.
