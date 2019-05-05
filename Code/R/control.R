#!/usr/bin/env R
rm(list = ls())

# Simple script to introduce for/if/else/while loops in R

# 'If' statements:
a <- TRUE # Assign boolean
if (a == TRUE){ # If a is true
    print("a is TRUE") # print a
} else { # or if it isnt true...
    print("a is FALSE") # print false
}

z <- runif(1) # Generates a random number
if (z <= 0.5) { 
print ("Less than a half")}
# If the random number is less than 0.5, then print it

# 'For' loop using a sequence:
for(i in 1:100){ # Seq from 1 to 100, increments of 1 only (use 'sep()' function for decimals)
  j <- i * i # j <- i squared
  print(paste(i, " squared is", j )) # print result
}

for(species in c('Heliodoxa rubinoides',
                 'Boissonneaua jardini',
                 'Sula nebouxii')) # Concatenate vector of strings
{
  print(paste('The species is', species)) # Use print(paste(text,variable))
}

# 'for' loop using a vector
v1 <- c("a","bc","def")
for (i in v1){
  print(i) # Reutrn the strings, on new lines
}

# While loops, normally finite operations, where a range is specified
i <- 0
while (i<100){ # While i is less than 100..
  i <- i+1 # Add one
  print(i^2) # Print i squared
}

