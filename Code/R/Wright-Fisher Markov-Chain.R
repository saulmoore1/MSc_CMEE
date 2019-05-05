#!/usr/bin/env R
# Evolutionary Modelling 1

WF <- function(N){
  result <- matrix(nc=2*N+1, nr=2*N+1)
  for (i in 1:nrow(result)){
    result[i,] <- dbinom(0:(2*N), size=2*N, prob=(i-1)/(2*N))
  }
  return(result)
} 

M<-WF(N=2)
WF(N=8)
dim(WF(8)) # 2N+1 by 2N+1

WF(2) %*% WF(2) # Matrix multiplication - M^2 --> probability to get from time step 't' to time step 't+2' (considering all possible paths)

M^30 # Allele frequencies after 30 generations

# Simulating Genetic Drift
# Handout 1 - WF model simulation - change in allele frequency over time

sim.genetic.drift <- function(p=0.5, t=10, N=20){
  # Input parameters (arguments): p (initial alle frequency), N (initial pop size), and t (no. generations)
  population <- list()
  length(population) <- t+1
  names(population) <- rep(NA, t+1)
  for(i in 1:(t+1)) {
    names(population)[i] <- paste(c('generation',i-1), collapse = '')
  }
  allele.freq <- rep(NA, t+1)
  
  # Initialise the population
  k <- ceiling(2*N*p)
  population[[1]] <- matrix(sample(c(rep(0, k), rep(1, 2*N-k))), nrow=2)
  allele.freq[1] <- sum(population[[1]]==0)/(2*N)
  for (i in 2:(t+1)) {
    population[[i]] <- matrix(sample(0:1, size=2*N, replace = TRUE, prob = c(allele.freq[i-1], 1-allele.freq[i-1])), nrow=2)
    population[[i]] <- matrix(sample(population[[i-1]], size=2*N, replace=TRUE), nrow=2) # Another way of sampling
    allele.freq[[i]] <- sum(population[[i]]==0)/(2*N)
  }
return(list(population=population, allele.freq=allele.freq))
}

M1 <- sim.genetic.drift(N=15, p=0.5, t=10)
M1

sim.genetic.drift2 <- function(p=0.5, t=10, N=20){
  # Input parameters (arguments): p (initial alle frequency), N (initial pop size), and t (no. generations)
  population <- list()
  length(population) <- t+1
  names(population) <- rep(NA, t+1)
  for(i in 1:(t+1)) {
    names(population)[i] <- paste(c('generation',i-1), collapse = '')
  }
  allele.freq <- rep(NA, t+1)
  
  # Initialise the population
  k <- ceiling(2*N*p)
  population[[1]] <- matrix(sample(c(rep(0, k), rep(1, 2*N-k))), nrow=2)
  allele.freq[1] <- sum(population[[1]]==0)/(2*N)
  for (i in 2:(t+1)) {
    population[[i]] <- matrix(sample(0:1, size=2*N, replace = TRUE, prob = c(allele.freq[i-1], 1-allele.freq[i-1])), nrow=2)
    population[[i]] <- matrix(sample(population[[i-1]], size=2*N, replace=TRUE), nrow=2) # Another way of sampling
    allele.freq[[i]] <- sum(population[[i]]==0)/(2*N)
  }
  return(list(population=population, allele.freq=allele.freq))
}

M2 <- sim.genetic.drift2(N=15, p=0.5, t=10)
M2
# hist(M1)
# hist(M2)
# t.test(M1, M2)

sim.genetic.drift3 <- function(p=0.5, t=10, N=20) { # Neglects individual allele frequency and considers just the total population, much faster but less information
  # NB - the sum of i.i.d bernoulli trials follows a binomial distribution
  allele.freq <- rep(NA, t+1)
  allele.freq[1] <- p
  for(i in 2:(t+1)) {
    allele.freq[i] <- rbinom(1, size=2*N, prob = allele.freq[i-1]) / (2*N) # Returns allele count, scaled by pop size (2*N)
  }
  return(allele.freq)
}

M3 <- sim.genetic.drift3()
M3

############################
# Speed test
############################
system.time(sim.genetic.drift(N=20000, t=2000, p=0.5))
system.time(sim.genetic.drift3(N=20000, t=2000, p=0.5)) # Trade-off between speed and information

############################
# Monte Carlo simulation
############################
# Mean and Variance of allele frequency
sim1 <- function(N=200, p=0.5){
  t = 1
  allele.freq <- p
  while (allele.freq > 0 & allele.freq < 1) {
    allele.freq <- rbinom(1, size=2*N, prob = allele.freq) / (2*N)
    # if (t %% 10000 == 0) {
    #   print(paste("No. simulations:",t))
    # }
    t = t + 1
  }
  if (allele.freq == 0){
    print(paste("EXTINCTION after",t,"generations"))
  }
  if (allele.freq == 1){
    print(paste("FIXATION after",t,"generations"))
  }
  return(t)
}  
sim1(N=20000, p=0.5)

sim2 <- function(N=20, p=0.5, runs=1000){
  t <- numeric()
  t.mean <- numeric()
  t.var <- numeric()
  for(i in 1:N) {
    for(j in 1:runs) {
      t <- c(t,sim1(N=20, p=0.5))
    }
    t.mean <- c(t.mean, mean(t, na.rm = T)) # Average result should converge on the true value of allele frequency - 'law of large numbers'
    t.var <- c(t.var, var(t, na.rm = T))
  }
  return(t.mean)
}
sim2(N=20, p=0.5, runs=1000)

########################################
# Example
########################################

# SIMULATE GENETIC DRIFT
sim.genetic.drift<-function(p=0.5, t=10, N=50)
{
  # SO THE INPUT ARGUMENTS HAVE BEEN DEFINED ABOVE
  # p IS THE ALLELE FREQ FOR THE FIRST ALLELE (ALLELE 0)
  # t IS THE NUMBER OF GENERATIONS
  # N IS THE EFFECTIVE POPULATION SIZE
  
  # DEFINE OUTPUTS
  # 1) THE ALLELE CONFIG FOR THE ENTIRE POPULATION OVER TIME
  # REPORT AS A LIST
  population<-list()
  length(population)<-t+1
  # OPTIONAL, TO GIVE NAMES TO EVERY ELEMENTS OF population
  names(population)<-rep(NA, t+1)
  for (i in 1:length(population))
  {names(population)[i]<-paste(c('generation', i-1), collapse='')}
  
  # 2) I WOULD ALSO LIKE TO RETURN THE ALLELE FREQ OVER TIME
  # AS A VECTOR
  allele.freq<-rep(NA, t+1)
  
  # TO ASSIGN INITIAL ALLELE COUNTS
  k<-ceiling(2*N*p)
  population[[1]]<-matrix(sample(c(rep(0, k), rep(1, 2*N-k))), nr=2)
  # THE INITIAL ALLELE FREQ
  allele.freq[1]<-sum(population[[1]]==0)/(2*N)
  
  
  # PROPAGATION
  for (i in 2:(t+1))
  {
    # THE ALLELE CONFIG AT THE NEXT GEN
    population[[i]]<-matrix(
      sample(0:1, size=2*N, 
             prob=c(allele.freq[i-1], 1-allele.freq[i-1]), 
             replace=TRUE), 
      nr=2)
    # THE ALLELE FREQ AT THE NEXT GEN
    allele.freq[i]<-sum(population[[i]]==0)/(2*N)
  }
  
  # WHAT DO YOU WISH TO RETURN WHEN YOU EXIT YOUR FUNCTION?
  return(list(population=population, allele.freq=allele.freq))
}

sim.genetic.drift()
#################################################################

################################################################
# VERSION 2
# SIMULATE ONLY THE ALLELE COUNTS (FREQ) OF THE POPULATION
sim.genetic.drift2<-function(p=0.5, t=10, N=50)
{
  
  # SAME INPPUT ARGUMENTS, OF COURSE
  
  # DEFINE OUTPUTS
  # ONLY INTERESTED IN THE ALLELE FREQ RATHER THAN THE ALLELE CONFIG OF EVERY SINGLE INDIVIDUALS
  allele.freq<-rep(NA, t+1)
  
  # INITIALISATION
  allele.freq[1]<-ceiling(2*N*p)/(2*N)
  # allele.freq[i]<-p
  
  # PROPAGATIONG
  for (i in 2:(t+1))
  {allele.freq[i]<-rbinom(1, size=2*N, prob=allele.freq[i-1])/(2*N)}
  
  # WHAT DO YOU WISH TO RETURN?
  return(allele.freq)
}

sim.genetic.drift2()
###################################################################



