#!/usr/bin/env R
# setwd("/home/saul/Documents/cmeecoursework/Week3/Code")

# Using 'next' to skip iterations in a (while/for) loop

for(i in 1:10){
  if((i %% 2) == 0) # '%%' is the modulo function - if i divided by 2 gives remainder 0, then
    next # Pass to next iteration of the loop
  print(i)
}

# This loops results in a string of printed numbers from 1:10, 
# not including numbers that may be divided by two, hence only odd numbers are returned