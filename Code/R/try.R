#!/usr/bin/env R
# setwd("/home/saul/Documents/cmeecoursework/Week3/Code")

# sample.R script modified to demonstrate the 'error-catching' ability of 'try'

x <- rnorm(50) # Generate the population
doit <- function(x) {
  x <- sample(x, replace = TRUE)
  if(length(unique(x)) > 30) { # Only take the mean if the sample was sufficient
    print(paste("Mean of this sample was:", as.character(mean(x))))
    }
  else {
    stop("Couldn't calculate mean: too few unique points!")
# 'stop' stops execution of the current expression and executes an error action
  }
}

# Using 'try' with vectorization:
result <- lapply(1:100, function(i) try(doit(x), FALSE))

# Using 'try' in a 'for' loop:
result <- vector("list", 100) # Preallocate/initialise
for(i in 1:100){
  result[[i]] <- try(doit(x), FALSE)
}

# Check out also, 'tryCatch'