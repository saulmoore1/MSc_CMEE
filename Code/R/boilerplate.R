#!/usr/bin/env R
# rm(list=ls())
# setwd("~/Documents/cmeecoursework/Week3/Code")

# A simple boilerplate to introduce functions and how to write them

MyFunction <- function(Arg1, Arg2) {
  
  # Statements involving Arg1, Arg2:
  print(paste("Argument", as.character(Arg1), "is a", class(Arg1))) # print Arg1's type
  print(paste("Argument", as.character(Arg2), "is a", class(Arg2))) # print Arg2's type
  
  return (c(Arg1, Arg2)) # This is optional, but useful to see whats going on
}

MyFunction(1,2) # Test the function, with arguments 1 and 2 respectively
MyFunction("Riki", "Tiki") # A different test, with strings
