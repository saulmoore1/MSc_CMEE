#!/usr/bin/env R
# setwd("/home/saul/Documents/cmeecoursework/Week3/Code")

# Run a simulation that involves sampling from a population

x <- rnorm(50) # Generate the population, 50 random no.s
doit <- function(x) {
  x <- sample(x, replace = TRUE) # 'sample' takes a sample of the specified size from the elements of x, with or without replacement
  if(length(unique(x)) > 30) { # Only take the mean if the sample was sufficient (>30)
    print(paste("Mean of this sample was:", as.character(mean(x)))) # print mean, as character string 
  }
}

# Run 100 iterations using vectorization:
result <- lapply(1:100, function(i) doit(x)) # 'lapply' function used to apply the calculation to lists

# Or, using a 'for' loop (slower):
result <- vector("list", 100) # Preallocate/initialise
for(i in 1:100){
  result[[i]] <- doit(x)
}

