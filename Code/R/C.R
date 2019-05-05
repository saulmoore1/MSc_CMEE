#!/usr/bin/env R

## Dynamic loading the C function into the current R environment
dyn.load("../Sandbox/doubler.so")

## Calling the 'double_me' function in C and feeding it an integer
.Call("double_me", x = as.integer(4))

## Function for counting the prime numbers between 1 and the given limit
count.primes.R <- function(limit) {
  prime = 1
  n_prime = 2 # Counting 2 as the first prime
  
  while (prime < limit)
  {
    is_prime = 0
    
    for(divisor in 2:(prime/2))
    {
      if(prime %% divisor == 0)
      {
        is_prime = 1
        break
      }
    }
    
    if (is_prime == 0) {
      n_prime <- n_prime + 1
    }
    
    prime <- prime + 1
  }
  
  return(n_prime)
}
(time_R <- system.time(results_R <- count.primes.R(10000)))

## Loading the compiled function
dyn.load("../Sandbox/prime.so")

## Bench marking the speed of this function
(time_C <- system.time(results_C <- .C("count_primes_C", limit = as.integer(10000), n_primes = as.integer(0))))

## Did both functions returned the same values?
results_C$n_primes == results_R ## I want a TRUE!

## What was the actual time difference?
time_R[[3]] / time_C[[3]] ## C is that many times faster!

dyn.load("../Sandbox/prime.so")

## Faster C code for prime number searching in R - with wrapper function in R
prime.R <- function(x) {
  .Call("count_primes_C", limit = as.integer(x))
}
prime.R(1000)
