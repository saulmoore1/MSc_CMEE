#!/usr/bin/env R
# setwd("/home/saul/Documents/cmeecoursework/Week3/Code")

# Script introducing the 'break' function in R - for breaking out of loops

i <- 0 # Initialize 'i'
  while(i < Inf){ # While i is less than inf
    if(i == 20){ 
      break } # Break out of the while loop!
    else {
      cat("i equals ", i, " \n") # Print i equals..(what i equals that iteration)
      i <- i + 1 # Update 'i', adding +1 each iteration
  }
}

# i starts at 0, and increases each iteration in increments of 1 
# Once i reaches 20, the 'break' function is called, and this exits the loop