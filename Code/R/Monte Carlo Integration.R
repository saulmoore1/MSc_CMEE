#!/usr/bin/env R
###############################################
# Day 2 - Monte Carlo integration & Gene drive
###############################################
x <- runif(20000,0,1) # Sample 20000 numbers from a random uniform distribution of 0 and 1's
mean(sqrt(1-x^2))
pi/4 # Roughly equal

x <- rexp(20000,1) # Random numbers from exponential distribution
mean(x*exp((-1)*x))

drive.drift.selection <- function(N=500, M=500, t=10, p=0.5, R=2) {
  population <- list()
  length(population) <- t+1
  size <- rep(NA, t+1)
  size[1] <- N
  allele.freq <- rep(NA, t+1)
  allele.freq[1] <- p
  k <- ceiling(2*N*p)
  population[[1]] <- matrix(sample(c(rep(0, k), rep(1, 2*N-k))), nrow=2)
  bh <- function(N, R, M){
    return(R*N/(1+N/M))
  }
  # First genotype (00), heterozygotes (01), and homozygous positive (11) is lethal
  # fitness <- c(1, 1, 0) # homozygous 11 (position 3) is lethal 
  # gamete.0 <- c(1, 1-d, 0)
  # gamete.1 <- c(0, d, 1)
  # while (allele.freq > 0 & allele.freq < 1) {
    for (i in 2:(t+1)) {
      population[[i]] <- population[[i-1]][,colSums(population[[i-1]])!=2]
      allele.freq[[i]] <- sum(population[[i-1]]==0)/(2*N)
      population[[i]] <- matrix(sample(0:1, size=2*N, replace = TRUE, prob = c(allele.freq[i-1], 1-allele.freq[i-1])), nrow=2)
      size <- length(population[[i]]) # Population size must be made to decrease each iteration (in addition to the removal of lethal 11 homozygotes)
    }
  return(list(population=population, allele.freq=allele.freq))
}  
drive.drift.selection(N=10, M=10, t=3, p=0.5, R=2)

##############################################
# Example
##############################################
# SIMULATING DRIVE-DRIFT-SELECTION BALANCE - Assumes random mating
drive.drift.selection <- function(q=0.5, d=0.8, N=500, t=10, R0=2, M=500) {
  # INPUT PARAMETERS: q = allele freq of lethal allele, d = homing rate (drive), N = initial pop size, t = time step (generation)
  
  # FUNCTION TO CALCULATE GENOTYPIC FREQUENCY
  cal.geno.freq <- function(population) {
    temp<-apply(population, 2, sum)
    # IF THE COLUMN SUM IS 0 THEN 00 HOMOZYGOTE, IF SUM IS 1 THEN HETEROZYGOTE, if sum is 2 then colummn is lethal 11 HOMOZYGOTE
    return(c(sum(temp==0), sum(temp==1), sum(temp==2))/length(temp))
  }
  
  # BEVERTON-HOLT MODEL FOR POPULATION DYNAMICS - Parameters R0 and M, together determine carrying capacity
  bh <- function(N, R0, M) {
    return(R0*N/(1+N/M))
  }
  
  # FITNESS COST
  # THE FIRST GENOTYPE IS 00, SECOND 01 HETEROZYGOTES, ASSUMING NO LOSS/ADVANTAGE IN FITNESS, AND THIRD 11 IS LETHAL
  fitness<-c(1, 1, 0)
  
  # GAMETES PRODUCED BY EACH GENOTYPE
  gamete.0<-c(1, 1-d, 0)
  gamete.1<-c(0, d, 1)
  
  
  # USE A LIST TO STORE ALL THE POPULATION OUTPUTS
  # THE LIST CONSISTS OF t+1 MATRICES TO REPRESENT ALL THE INDIVIDUALS IN EACH GENERATION
  # EACH COLUMN IN A MATRIX REPRESENTS AN INDIVIDUAL
  population<-list()
  length(population)<-(t+1)
  
  # TO STORE THE POPULATION SIZE
  population.size<-rep(NA, t+1)
  population.size[1]<-N
  
  # TO STORE THE ALLELE FREQ OF q
  allele.freq.q<-rep(NA, t+1)
  allele.freq.q[1]<- q
  
  # THE INITIAL POPULATION - ASSUME HW EQUILIBRIUM (ie. BINOMIAL SAMPLING)
  k<-ceiling(2*N*q)
  population[[1]]<-matrix(sample(c(rep(0, 2*N-k), rep(1, k))), nr=2)
  # population[[1]]<-matrix(sample(0:1, size=2*N, replace=TRUE, prob=c(1-q, q)), nr=2)
  
  # FOR EACH TIME STEP
  for (i in 1:t) {
    # CALCULATE THE GENOTYPIC FREQ, THEN THE FREQ AFTER SELECTION, AND FINALLY THE GAMETIC FREQ (CONSIDERING THE DRIVE)
    genotype.freq<-cal.geno.freq(population[[i]])
    freq.after.selection<-genotype.freq*fitness/sum(genotype.freq*fitness)
    gametic.freq<-c(sum(gamete.0*freq.after.selection), sum(gamete.1*freq.after.selection))
    
    # CALCULATE THE POPULATION SIZE FOR THE NEXT TIME STEP USING THE BH MODEL, AND THE LETHAL 11 GENOTYPE IS ALSO CONSIDERED
    population.size[i+1]<-floor(bh(population.size[i]*(1-genotype.freq[3]), R0=R0, M=M))
    
    # REPRODUCTION ONLY IF POPULATION SIZE >= 1
    if (population.size[i+1] >= 1) {
      # REPRODUCTION. USE THE gametic.freq FOR SAMPLING
      population[[i+1]]<-matrix(sample(0:1, size=2*population.size[i+1], replace=TRUE, prob=gametic.freq), nr=2)
      
      # THE NEW ALLELE FREQ OF q
      allele.freq.q[i+1]<-sum(population[[i+1]]==1)/(2*population.size[i+1])
    } else {
      print('Population collapsed!')
      return(list(population=population, population.size=population.size, allele.freq.q=allele.freq.q))
    }
  }
  return(list(population=population, population.size=population.size, allele.freq.q=allele.freq.q))
}

###########################################
# TRY IT! 
result<-drive.drift.selection(q=0.1, d=0.8, N=500, t=1000000, R0=2, M=500)
plot(result$population.size ~ seq(1, res, by=1), type="l")




