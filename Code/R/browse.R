#!/usr/bin/env R
# setwd("/home/saul/Documents/cmeecoursework/Week3/Code/")

# Debugging - fixing errors in your code
# Using 'stopifnot()', 'traceback()', and 'browser()' (also look up 'recover()')
# Also, a more technical approach to debugging using 'debug(fn)' and 'undebug(fn)'

# Using 'browser()' example:
Exponential <- function(N0 = 1, r = 1, generations = 10){
  # Runs a simulation of exponential growth
  # Returns a vector of length = 10 generations
  
  N <- rep(NA, generations)
  
  N[1] <- N0
  for(t in 2:generations){Ricker <- function(N0=1, r=1, K=10, generations=50)
  {
    # Runs a simulation of the ricker model
    # Returns a vector of length generations
    N <- rep(NA, generations)
    # Creates a vector of NA
    N[1] <- N0
    for (t in 2:generations)
    {
      N[t] <- N[t-1] * exp(r*(1.0-(N[t-1]/K)))
    }
    return (N)
  }
  plot(Ricker(generations=10), type="l")
    N[t] <- N[t-1] * exp(r)
# browser() # interrupt here to debug if you wish. Within the browser();
# n: single-step
# c: exit browser and continue
# q: exit browser and abort, return to top-level
  }
  return(N)
}
plot(Exponential(), type = "l", main = "Exponential growth")
