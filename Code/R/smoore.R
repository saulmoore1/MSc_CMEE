#!/usr/bin/env R
rm(list=ls())
graphics.off()

#############################################################
# High Performance Computing Programming Exercises in R:
# Neutral Theory Simulations
#############################################################

#1 - Function to calculate length of given vector (species richness)
species_richness <- function(richness){ # Define function 'species_richness'
  length(unique(richness)) # Calculate number of unique values in given community vector
}
# community <- c(1,4,4,5,1,6,1,2) # Define community vector (eg. species 1, species 4, species 4, etc)
# species_richness(community) # Call the function, should return '5'


#2 - Function to generate a sequence of integers of specified length 
initialise_max <- function(Max){
  return(seq(1,Max,by=1)) # 'seq' creates a seqeuence of numbers from 1 to given value for 'Max', by integer increments of 1
}
# initialise_max(7) # Create sequence 1 to 7


#3 - Function to generate an alternative initial state for the neutral simulation (initial monodominance of one species)
initialise_min <- function(Min){
  rep(1,Min) # 'rep' creates a vector of repeated input value, in this case 1, to fill the species pool with only one initial species
}
# initialise_min(4) # Repeat speciesID 1, for 'Min' number of times


#4 Function to choose two different random integer numbers from 1 to x
choose_two <- function(x){
  a <- seq(1,x,by=1) # Create sequence from 1 to x, call it 'a'
  return(sample(a,2)) # Sample uses a seed to choose 2 'pseudo-random' numbers from sequence length x
}
# choose_two(4) # Sequence length 4, a = (1,2,3,4), choose 2 random numbers from 'a'


#5 Function to perform a simple neutral model simulation
neutral_step <- function(x){
  CT <- choose_two(length(x)) # Choose two numbers from vector x
  x[CT[1]] <- x[CT[2]] # Replace the first chosen species ID with the second
  return(x) # Return new community
}
# y <- c(10,5,13) # Example input vector
# neutral_step(y)


#6 Function to simulate neutral theory model with time series of richness
neutral_time_series <- function(initial = initialise_max(7),duration = 20,interval = 2) { # Define default parameters, if none are given when calling the function at the commandline
  time_series <- species_richness(initial) # Record initial species richness as first record in time series
  community <- initial
  for (i in 1:duration) { # Perform the neutral step function for 'duration' number of times
    community <- neutral_step(community) # At each interval, take the species richness of the community and add it to results
    if (i %% interval == 0) { # If the modulus of i by interval = 0, ie. at every interval
      richness <- species_richness(community) # Record the species richness at that interval
      time_series <- c(time_series, richness) # Append richness to time series list, overwriting with the updated list each iteration
    } 
  }
  return(list(time_series, community)) # Return a list of [1] species richness at each interval from 1:duration, and [2] the final community composition
}
# neutral_time_series()


#7 Plot time series graph of neutral model simulation
question_7 <- function() {
  # pdf("../Results/Question_7.pdf") # Initiate a PDF to save the plot
  plot(seq(1,10010,10), neutral_time_series(initialise_max(100),10000,10)[[1]], xlab="Time Steps", ylab="Species Richness") # Plot elements in first column of neutral_time_series output (time series of species richness recordings)
  # Perform neutral time series with initial conditions of maximal diversity, system size = 100 individuals, 10000 time steps, richness recorded every 10 time steps
  # dev.off() # Close device window and complete the save
}
# question_7()


#8 Function to perform step of neutral model with speciation
neutral_step_speciation <- function(community,v) { 
  random <- runif(1,min=0,max=1) # Generate a random number between 0 and 1
  CT <- choose_two(length(community)) # Choose two species in the community
  if (random > v) { # If the random number is greater than the speciation rate (v)..
    community[CT[1]] <- community[CT[2]] # No speciation: Choose two replaces one species with another, as in 'neutral_step'
  } else { # Else (if the random number generated is less than v)..
    community[CT[1]] <- max(community) + 1 # Speciation: Add a new species to the community, with species ID of +1 larger than the previous largest species ID to be sure that it is unique
  }
   return(community)
}
# neutral_step_speciation(c(10,5,13), v=0.2) # Call the function, with speciation rate probability 0.2


#9 Function to simulate neutral time series with speciation
neutral_time_series_speciation <- function(initial = initialise_max(7),duration = 20,interval = 2, v=0.2) { # With default parameters
  community <- initial
  time_series <- species_richness(initial) # Determine richness of initial community
  for (i in 1:duration) { # For default 20 cycles..
    community <- neutral_step_speciation(community, v) # Call neutral_step_speciation. Record community, where initial community of each loop = final community of previous loop
    if (i %% interval == 0) { # If modulus of interval = 0 (aka if multiple of interval / aka at every interval)..
      richness <- species_richness(community) # Record community richness
      time_series <- c(time_series, richness) # Append to richness to results list
    } 
  }
  return(list(time_series, community)) # Return results list, with final community appended
}
# neutral_time_series_speciation()


#10 Neutral theory simulation with speciation, 
# Plot species richness over time for a max initial community of 100 species and for an alternative min initial community of 1 species, each 100 individuals
question_10 <- function(){
  # pdf("../Results/Question_10.pdf")
  plot(seq(1,10010,10), neutral_time_series_speciation(initialise_max(100),10000,10,0.1)[[1]], xlab="Time Series", ylab="Species Richness", col="blue", type="l") # 100 different species, run for 10000 cycles, richness measured every 10 cycles, speciation rate of 0.1
  lines(seq(1,10010,10), neutral_time_series_speciation(initialise_min(100),10000,10,0.1)[[1]], col="red") # The alternative initial condition: monodominant species
  legend(8000, 100, c("Max", "Min"), col = c("blue", "red"), lty = c(1,1)) # Add a figure legend for clarification
  # dev.off()
}
# question_10()


#11 Function to calculate species abundance distributions of the neutral simulations
species_abundance <- function(community) {
  abundance <- as.vector(sort(table(community), decreasing = TRUE)) 
  # 'table' builds counts of each factor (species) in the community and 'sort' sorts Community by decreasing order
  return(abundance) # Return result
}
# species_abundance(c(1,5,3,6,5,6,1,1)) # Returns '3 2 2 1'


#12 Function to bin abundances into 'octave classes'
# Species abundances can be 'binned' into logarithmic categories, usually using base 2, which gives bins of abundance 0-1, abundance 2-3, abundance 4-7, abundance 8-15, etc, in classes called octaves
octaves <- function(abundance) {
  # Create a vector of zeros of length equal to the maximum number of powers of 2 divisible into the largest species abundance recording for the community
  vec <- seq(0, 0, length=floor(max(log2(abundance) + 1)))  # 'floor' function rounds down to the nearest integer value
  # Create bins based on the power of 2 divisible into the abundance value for each species
  # ie. Octave bin1 = 1, bin2 = 2-3, bin3 = 4-7, bin4 = 8-15, bin5 = 16-31, bin6 = 32-63, bin7 = 64-127, ...etc
  bin <- floor(log2(abundance) + 1)
  for (i in 1:length(bin)) {
    vec[bin[i]] <- vec[bin[i]] + 1 # Could use the 'tabulate' function here
  }
  return(vec)
}
# octaves(c(100,64,63,5,4,3,2,2,1,1,1,1))


#13 Function to accept two input vectors and calculate their sum
sum_vect <- function(x, y){
  if (length(x) > length(y)){ # If x is longer than y, add zeros to y to extend to length of x
    y <- c(y, rep(0,length=(length(x)-length(y))))
    sum <- x + y # Sum vectors
  } else { # Else, if y is longer than x, then add zeros to x to extend to length of y
    x <- c(x, rep(0,length=(length(y)-length(x))))
    sum <- x + y # Sum vectors
  } 
  return(sum) # Return result
}
# sum_vect(c(1,3), c(1,0,5,2))


#14 Neutral model simulation
question_14 <- function() {
  # Perform the burn-in period (10000 time steps, to get used to initial conditions), and take output abundance as initial abundance for the 'for' loop
  community <- neutral_time_series_speciation(initialise_max(100), 10000, 10, 0.1) # Perform neutral_time_series_speciation
  abundance <- species_abundance(community[[2]]) # Calculate initial species abundance of the community by calling the function 'species_abundance'
  octave <- octaves(abundance) # Calculate the octave structure of initial abundance
    for (i in 1:100) { # for 100 cycles..
      community <- neutral_time_series_speciation(community[[2]], 1000, 10, 0.1)
      abundance <- species_abundance(community[[2]]) # Calculate the species abundance and the octave classes from the community at each interval
      octaves <- octaves(abundance)
      octave <- sum_vect(octave, octaves) # Calculate cumulative octave vector over the 100 cycles
    }
  average <- octave/101 # Divide by total to get average octave distribution (100 cycles, +1 to include octave of initial abundance)
  # pdf("../Results/Question_14.pdf")
  bar <- barplot(average, xlab = "Mean Species Abundance Octave", ylab = "Species Richness", width = 0.83) # Plot barplot (Preston diagram) of average octave class
  axis(1, at = bar, las = 1, labels = seq(1,length(average),by=1)) # Add to x-axis, mid-point bar labels
  # dev.off()
}
# question_14()


# Challenge Question A
challenge_A <-function(n) { # Where 'n' is the number of repeats of each simulation
  Max <- numeric() # Create a empty richness vectors
  Min <- numeric() # For max and min diversity initial conditions
  for (i in 1:n){ # Loop through and conduct max and min diversity neutral simulations 'n' number of times
    MaxSim <- neutral_time_series_speciation(initialise_max(100), 10000, 10, 0.1) # Run simulations for max diversity
    MinSim <- neutral_time_series_speciation(initialise_min(100), 10000, 10, 0.1) # Run simulations for min diversity
    MaxSim <- MaxSim[[1]] # Return the richness, the first element of neutral_time_series_speciation output, for min and max
    MinSim <- MinSim[[1]]
    Max <-sum_vect(Max, MaxSim) # Sum up the richness vectors for repeat simulations
    Min <-sum_vect(Min, MinSim)
  }
  MeanMax <- Max/n # Mean species richness
  MeanMin <- Min/n
  SEMax <- qnorm(0.972)*(sd(Max)/sqrt(length(Max))) # Find the standard error corresponding to CI 97.2%
  SEMin <- qnorm(0.972)*(sd(Min)/sqrt(length(Min)))
  UMax <- MeanMax + SEMax # Finding the confidence intervals
  LMax <- MeanMax - SEMax
  UMin <- MeanMin + SEMin
  LMin <- MeanMin - SEMin
  # Plotting
  x <- seq(0,10000,by=10) # Generate x-data for plotting - a vector of equal length/interval to simulated y-data (richness)
  # pdf("../Results/Challenge_A.pdf") # Open device window for saving
  plot(x,MeanMax,xlab="Time Step", ylab="Species Richness", type="l", col="blue", lwd=2, ylim=c(0,100)) # Plot with initial max population diversity
  lines(x, UMax,  col="blue", lty=4)# Add upper CI
  lines(x, LMax,  col="blue", lty=4)# Add lower CI
  lines(x, MeanMin, col="red", type="l", lwd=2) # Add Min simulation data
  lines(x, UMin,  col="red", lty=4)# Add upper CI
  lines(x, LMin,  col="red", lty=4)# Add lower CI
  legend(5000,100, # Add a legend
         c("Initial Max","Initial Min", "97.2% confidence intervals"),
         lty=c(1,1,4), lwd=c(2,2,1), # Set line type/width/colour for legend
         col=c("blue","red","black"))
  # dev.off() # Close device
}
# challenge_A(10)


# Challenge Question B
challenge_B <- function(size, sims) {
  initialise_random <- function(size) { # Function to define intital richness
    initial <- sample(size, size, replace = TRUE) 
    # Sample with replacement, n number of times, with replacement, such that individuals have equal likelihood of taking any species identity from 1-n.
    return(initial)
  }
  mat_ts <- matrix(0,nrow=sims,ncol=1001,byrow=TRUE) # Create an empty matrix, of correct dimensions for storing time series
  for (i in 1:sims) { # For each simulation...
    time_series <- neutral_time_series_speciation(initialise_random(size), 10000, 10, 0.1) # Perform neutral time series
    time_series <- time_series[[1]] # Save richness vectors in time_series
    mat_ts[i,] <- time_series # Overwrite the empty matrix, row-for-row, with time_series data
  }
  Mean_ts <- apply(mat_ts,2,mean) # 'Apply' function to find row means for time_series data - mean richness per initial state
  Std_Error <- qnorm(0.95)*apply(mat_ts,2,sd) # Work out the standard error for 95% confidence interval
  CI_upper <- Mean_ts + Std_Error # Upper CI
  CI_lower <- Mean_ts - Std_Error # Lower CI
  # Plotting
  x <- seq(0,10000,by=10) # Generate x-data for the plot
  # pdf("../Results/Challenge_B.pdf") # Open device window
  plot(x,Mean_ts,xlab="Time Step", ylab="Mean Species Richness", type="l", col="black", lwd=2, xlim=c(0,10000), ylim=c(0,size))
  lines(x,CI_upper, col="red", lty=4) # Add upper CI
  lines(x,CI_lower, col="red", lty=4) # Add lower CI
  legend(6000,size, # Add a legend
         c("Simulation", "95% confidence intervals"),
         lty=c(1,4),
         col=c("black","red"), cex = 0.75)
  # dev.off()
}
# challenge_B(100, 100)


######################################################################
# Neutral Theory Simulation using HPC
######################################################################

#15 A much larger simulation, with more repeat readings, using HPC
cluster_run <- function(speciation_rate, size, wall_time, rand_seed, interval_rich, interval_oct, burn_in_time) {
  set.seed(rand_seed) # Set the seed for random number generation
  community <- initialise_max(size) # Maximal diversity initial conditions
  start <- proc.time()[[3]] # Record start time from internal clock
  finish <- start + wall_time*60 # Convert wall-time from minutes to seconds within the function
  gentime <- size/2 # One generation is taken to be a birth or death for every individual in the system (size/2)
  gencount <- 0 # Initialise a 'counter' to record the number of generations the simulation is run for
  time_series <- species_richness(community) # Record initial species richness in time_series
  octave <- list(octaves(species_abundance(community))) # Record the species abundance
  while (proc.time()[[3]] < finish) { # Until finish time..
    for (i in 1:gentime) { # 1 generation = size/2 time steps
      community <-  neutral_step_speciation(community, speciation_rate) # Neutral step speciation for each time step
    }
    gencount <- gencount + 1 # Increase gencount each iteration to track generation number
    if ((gencount <= burn_in_time) && (gencount %% interval_rich == 0)) { 
    # Until 'burn_in_time', and only at intervals of 'interval_rich', record species richness of the community, add append to 'time_series'
      time_series <-  c(time_series, species_richness(community))
    }
    if (gencount %% interval_oct == 0) {
    # At intervals of 'interval_oct' generations (ie. where the modulus (remainder) is 0), record octave of species abundance
      octave <- c(octave, list(octaves(species_abundance(community))))
    }
  }
  final_community <- tail(community, 1) # Record final community composition
  run_time <- proc.time()[[3]] - start # Calculate the total run time of the simulation
  # save(time_series, octave, final_community, run_time, gencount, speciation_rate, size, wall_time, rand_seed,
  #      interval_rich, interval_oct, burn_in_time, file=paste0("../Results/Neutral",rand_seed,".Rdata")) # Save results together (different data types) in an R data file, named according to the rand_seed set for the simulation
}

speciation_rate = 0.006493 # Probability of speciation each time step
size = 1000 # System size (Number of individuals)
wall_time = 10 # Duration of simulation (in minutes)
rand_seed = 1 # Set the seed for the pseudo-random number generation of the simulation
interval_rich = 10 # Interval between richness calculations (generations)
interval_oct = 10 # Interval between octave calculations (generations)
burn_in_time = 10000 # Period of acclimatisation to initial conditions (generations)
# cluster_run(speciation_rate, size, wall_time, rand_seed, interval_rich, interval_oct, burn_in_time)


#17 Code to read in and process the cluster output files
question_17 <- function() {
  Sum_500 <- numeric() # Intitialise empty list for 
  Sum_1000 <- numeric()
  Sum_2500 <- numeric()
  Sum_5000 <- numeric()
  n_500 <- 0 
  n_1000 <- 0
  n_2500 <- 0
  n_5000 <- 0
  for (i in 1:100){ # For iteration numbers 1-100 (loop through the Rdata files)
    # print(paste("Loading file", i, "/100")) # Print whenever a new file os being loaded
    # load(paste0("../Results/smoore/Neutral", i, ".Rdata")) # Where i = rand_seed number for the simulation
      if(size==500){
        for(j in octave[(burn_in_time/interval_oct + 1):length(octave)]){ # Only use data of abundance octaves after the burn-in time is up
        Sum_500 <- sum_vect(Sum_500, j) # Sum the octave vectors 
        n_500 <- n_500 + 1 # Tally the total number of octaves summed up
        }
      }
      if(size==1000){
        for(j in octave[(burn_in_time/interval_oct + 1):length(octave)]){
          Sum_1000 <- sum_vect(Sum_1000, j)
          n_1000 <- n_1000 + 1
        }
      }
      if(size==2500){
        for(j in octave[(burn_in_time/interval_oct + 1):length(octave)]){
          Sum_2500 <- sum_vect(Sum_2500, j)
          n_2500 <- n_2500 + 1
        }
      }
      if(size==5000){
        for(j in octave[(burn_in_time/interval_oct + 1):length(octave)]){
          Sum_5000 <- sum_vect(Sum_5000, j)
          n_5000 <- n_5000 + 1
        }
      } 
  }
  # Calculate mean abundance per octave class for simulations with initial community size = 500, 1000, 2500, and 5000, separately
  Mean_500 <- Sum_500/n_500
  Mean_1000 <- Sum_1000/n_1000
  Mean_2500 <- Sum_2500/n_2500
  Mean_5000 <- Sum_5000/n_5000
  speciation_rate = 0.006493
  # save(Mean_500, Mean_1000, Mean_2500, Mean_5000, speciation_rate, file="../Results/Question17.Rdata") # Save heterogeneous data in Rdata file
    
  # Multi-panelled plot of mean octave barplots for the four different comunity sizes
  # pdf('../Results/Question_17.pdf')
  par(mfrow=c(2,2)) # Initiate a 4-in-1 plot window
  a <- barplot(Mean_500, ylim = c(0,4), xlab = 'Mean Abundance (as octaves)', ylab = 'Species Richness') # Plot mean of simulations of system size = 500
  axis(1, at = a, las = 1, cex = 0.75, labels = seq(1,length(Mean_500),by=1))
  mtext('Community size: 500 individuals', line = 1, cex = 0.75)
  b <- barplot(Mean_1000, ylim = c(0,8), xlab = 'Mean Abundance (as octaves)', ylab = 'Species Richness') # Plot mean of simulations of system size = 1000
  axis(1, at = b, las = 1, cex = 0.75, labels = seq(1,length(Mean_1000),by=1))
  mtext('Community size: 1000 individuals', line = 1, cex = 0.75)
  c <- barplot(Mean_2500, ylim = c(0,20), xlab = 'Mean Abundance (as octaves)', ylab = 'Species Richness') # Plot mean of simulations of system size = 2500
  axis(1, at = c, las = 1, cex = 0.75, labels = seq(1,length(Mean_2500),by=1))
  mtext('Community size: 2500 individuals', line = 1, cex = 0.75)
  d <- barplot(Mean_5000, ylim = c(0,40), xlab = 'Mean Abundance (as octaves)', ylab = 'Species Richness') # Plot mean of simulations of system size = 5000
  axis(1, at = d, las = 1, cex = 0.75, labels = seq(1,length(Mean_5000),by=1))
  mtext('Community size: 5000 individuals', line = 1, cex = 0.75)
  # dev.off()# Close the device window
}
# question_17()


# Challenge Question C - plot a graph of species richness ~ simulation generation
challenge_C <- function(){
  Sum_ts500 <-  numeric() # Create an object to store cumulative richness, for each simulated community size
  Sum_ts1000 <-  numeric()
  Sum_ts2500 <-  numeric()
  Sum_ts5000 <-  numeric()
  for (i in 1:100){ # Parse through the 100 HPC cluster Rdata files, iteratively
    # print(paste("Loading file",i,"/100")) # To show whenever a new file os being loaded
    # load(paste0("../Results/smoore/Neutral", i, ".Rdata")) # Load in the data, setting the value of 'iter' as 'i' for the loop
      if (size==500){
        Sum_ts500 <- sum_vect(Sum_ts500, time_series) # If size is 500, sum them
      }
      if (size==1000){
        Sum_ts1000 <- sum_vect(Sum_ts1000, time_series) # If size is 1000, sum them
      }
      if (size==2500){
        Sum_ts2500 <- sum_vect(Sum_ts2500, time_series) # If size is 2500, sum them
      }
      if (size==5000){
        Sum_ts5000 <- sum_vect(Sum_ts5000, time_series) # If size is 5000, sum them
      }
    }
  Mean_ts500 <- Sum_ts500/25 # Divide by total number of repeat simulations, to find mean richness
  Mean_ts1000 <- Sum_ts1000/25
  Mean_ts2500 <- Sum_ts2500/25
  Mean_ts5000 <- Sum_ts5000/25
  speciation_rate = 0.006493
  # save(Mean_ts500, Mean_ts1000, Mean_ts2500, Mean_ts5000, speciation_rate, file="../Results/Challenge_C.Rdata") # Save time_series (richness) means and speciation rate, as instructed
  # Plotting
  # pdf('../Results/Challenge_C.pdf') # Open device (save plot)
  par(mfrow=c(2,2)) # Split the device window into 4 sections, to view 4-in-1 graphs
  # Mean richness is logged to better estimate the duration of the burn-in
  plot(log(Mean_ts500), xlab = 'Generation', ylab = 'log(Species Richness)', xlim=c(0,500), cex=0.5)
  mtext('Community size: 500 individuals', line = 1, cex = 0.75) # Plot log(mean richness)
  plot(log(Mean_ts1000), xlab = 'Generation', ylab = 'log(Species Richness)', xlim=c(0,500), cex=0.5)
  mtext('Community size: 1000 individuals', line = 1, cex = 0.75)
  plot(log(Mean_ts2500), xlab = 'Generation', ylab = 'log(Species Richness)', xlim=c(0,500), cex=0.5)
  mtext('Community size: 2500 individuals', line = 1, cex = 0.75)
  plot(log(Mean_ts5000), xlab = 'Generation', ylab = 'log(Species Richness)', xlim=c(0,500), cex=0.5)
  mtext('Community size: 5000 individuals', line = 1, cex = 0.75)
  # dev.off() # Close device
}
# challenge_C()


# Challenge D - Coalescence
# challenge_D <- function(J,v){ # J is population size, v is speciation rate
#   lineages <- initialise_min(J) # Initialise a vector length, J, of 1's
#   abundances <- 0 # Intialise an empty abundances vector
#   N = J # Initialise a number, N
#   theta = v*((J-1)/(1-v)) # Calculate theta from pop size and speciation rate
#   if (N > 1) { 
#     lineages[j] <- sample(lineages,1) # Sample 1 random number from 'lineages'
#     randnum <- runif(1,0,1) # Generate 1 random number between 0 and 1
#     if (randnum < (theta/(theta+N-1))) { 
#       abundances <- c(abundances, lineages[j]) # Append to 'abundances'
#     }
#     if (randnum >= (theta/(theta+N-1))) {
#       lineages[i] <- sample(lineages[-j],1) # Sample again, but not the same
#       lineages[i] = lineages[i] + lineages[j] # Sum the two numbers
#     }
#     lineages <- lineages # Remove 'lineages[j]' from 'lineages' vector
#     N = N - 1 # Decrease N by 1 to match 'length(lineages)'
#   }
#   abundances <- c(abundances, lineages) # Add the last number to abundance
# }
# challenge_D(10, 0.006493)


########################################################################
# Fractals in Nature
########################################################################

#18 Calculating fractal dimensions
# (a)
# Width = 3
# Size = 8
# Size = Width^D
# log(Size) = D*log(Width)
# D = log(Size)/log(Width)
# D = 1.892789

# (b)
# Width = 3
# Size = 20
# Size = Width^D
# D = log(Size)/log(Width)
# D = 2.726833


#19 The Chaos Game - The Serpinski Gasket
chaos_game <- function() {
  a <- c(0,0) # Define coordinates for points a,b,c and start position x
  b <- c(3,4)
  c <- c(4,1)
  x <- c(0,0)
  xcoords <- c(a[1], b[1], c[1]) # Define first element as x coord
  ycoords <- c(a[2], b[2], c[2]) # Second element is y-coord
  # pdf("../Results/Chaos_Game.pdf") # Open device window for saving
  plot(xcoords, ycoords, xlab = "x", ylab = "y") # Plot the x and y coords
  points(x[1],x[2], cex = 0.2) # Plot a small point at 'x' on the graph
  points <- list(a,b,c) # Make a list to be sampled from
  for (i in 1:5000) { # Plot 5000 points, where 
    rand_choice <- sample(points, 1) # Sample from the list, such that a new
    x[1] <- (rand_choice[[1]][1] + x[1])/2 # Move x half-way toward the randomly chosen point
    x[2] <- (rand_choice[[1]][2] + x[2])/2
    points(x[1], x[2], cex = 0.1) # Plot a small point
  }
  # dev.off() # Close the device
}
# chaos_game()


# Challenge E
challenge_E <- function() {
  a <- c(0,0) # These starting coordinates set up an equilateral Serpinski triangle
  b <- c(8,0)
  c <- c(4,15)
  x <- c(1,10)
  xcoords <- c(a[1], b[1], c[1]) # Define x coords
  ycoords <- c(a[2], b[2], c[2]) # Define y coords
  # pdf("../Results/Challenge_E.pdf") # Open new plot to be saved
  plot(xcoords, ycoords, xlab = "x", ylab = "y")
  points(x[1],x[2], cex = 0.75, pch=20, col="red") # Plot a small point at 'x' on the graph
  points <- list(a,b,c) # Concatenate into a list for sampling
  for (i in 1:100) { # Plot first 100 points in a different colour, to study how the function plots
    rand_choice <- sample(points, 1) # Randomly sample the list(a,b,c)
    x[1] <- (rand_choice[[1]][1] + x[1])/2 # Distance to move can be altered (../3?.../100?...)
    x[2] <- (rand_choice[[1]][2] + x[2])/2
    points(x[1], x[2], cex = 0.75, col="red", pch=20)
  }
  for (i in 101:10000) { # Plot the rest of the points
    rand_choice <- sample(points, 1)
    x[1] <- (rand_choice[[1]][1] + x[1])/2
    x[2] <- (rand_choice[[1]][2] + x[2])/2
    points(x[1], x[2], cex = 0.2, col="black", pch=20)
  }
  # dev.off()
}
# challenge_E()


#20 Function to plot a line from a start point to an end point specified by a given angle (direction) and length
turtle <- function(start_position, direction, length) {
  x <- start_position[1] + length*cos(direction) # Use trigonometry to calculate new x coordinate
  y <- start_position[2] + length*sin(direction) # New y coordinate
  finish <- c(x,y) # Concatenate vector of coordinates
  lines(c(start_position[1], finish[1]), c(start_position[2], finish[2]), type = "l", col = "black", lwd = 1) # Draw a line between the points
  return(finish)
}
# turtle(c(0,5), pi/2, 4)


#21 Create a function 'elbow' 
elbow <- function(start_position, direction, length) {
  finish <- turtle(start_position, direction, length) 
  # Call turtle function, and store output as 'finish'
  turtle(finish, (direction-(pi/4)), 0.95*as.numeric(length)) 
  # Call turtle again to draw a new line starting from the finish coordinates of the previous turtle call, but with a new direction, of angle = -pi/4 radians
}
# elbow(c(0,5), pi/2, 4)


# 22 Spiral - iterative function of 'elbow'
spiral <- function(start_position, direction, length) {
  finish <- turtle(start_position, direction, length) # Call the plotting turtle function
  spiral(finish, (direction-(pi/4)), 0.95*as.numeric(length)) # Then call itself (creates an infinite recursive loop)
  # Function continues to call itself with ever-decreasing line length until error is returned, and breaks out of loop
}
# spiral(c(-7.5,0), pi/3, 6)

#23 spiral_2
spiral_2 <- function(start_position, direction, length) {
  if (length > 0.5) { # Condition to break out of the loop (plot only when line length > 0.5)
    finish <- turtle(start_position, direction, length) # Call plotting function
    spiral_2(finish, (direction-(pi/4)), 0.95*as.numeric(length)) # Function calls itself
  } else { # 'Else' conditional statement (ie. if line length <= 0.5)
    return() # Return out of loop
  }
}
# spiral_2(c(-7.5,0), pi/3, 6)


#24 'tree'
tree <- function(start_position, direction, length) {
  finish <- turtle(start_position, direction, length)
  if (length > 0.1) { # Conditional 'if': If line length > 0.1..
    # Call itself twice, to call turtle to plot two lines each iteration, in opposite directions, like the branching of a tree:
    tree(finish, (direction+(pi/4)), 0.65*as.numeric(length)) # Line length decreases more dramatically than in 'spiral' function
    tree(finish, (direction-(pi/4)), 0.65*as.numeric(length))
  } else { # Else, return out of loop
    return()
  }
}
# tree(c(-4,-7.5), pi/3, 6)


#25 'fern'
fern <- function(start_position, direction, length) {
  finish <- turtle(start_position, direction, length)
  if (length > 0.1) { # To prevent infinite recursion
    fern(finish, (direction + pi/4), 0.38*as.numeric(length)) # One branch goes 45 degrees to the left
    fern(finish, (direction - 2*pi), 0.87*as.numeric(length)) # The other branch goes directly up
  } else {
    return() # Return from loop if line length violates the 'if' condition
  }
}
# fern(c(0,-10), pi/2, 6)


#26 'fern_2'
fern_2 <- function(start_position, direction, length, dir) {
  dir <- -1*dir # Flip the direction to the opposite direction each iteration
  finish <- turtle(start_position, direction, length)
  if (length > 0.05) { # If length > 0.05, call itself twice, one branch to the left/right each iteration, the other branch continues straight
    fern_2(finish, (direction + dir*(pi/4)), 0.38*as.numeric(length), -dir) 
    # Call with direction parameter = (direction + angle), so the angle increases each iteration - fern sways in the wind!
    fern_2(finish, (direction + 2*pi), 0.87*as.numeric(length), dir)
  } else {
    return()
  }
}
# fern_2(c(0,0), pi/2, 5, -1)


# Challenge F
challenge_F <- function(start_position, direction, length, dir) {
  turtle1 <- function(start_position, direction, length) { # Turtle function nested inside
    x <- start_position[1] + length*cos(direction)
    y <- start_position[2] + length*sin(direction)
    finish <- c(x,y)
    lines(c(start_position[1], finish[1]), c(start_position[2], finish[2]), type = "l", col = "darkolivegreen4", lwd = 1) # Set colours to differ between fern simulations
    return(finish)
  }
  dir <- -1*dir
  finish <- turtle1(start_position, direction, length)
  if (length > 0.03) {
    challenge_F(finish, (direction - dir*(pi/4)), 0.45*as.numeric(length), dir)
    challenge_F(finish, (direction - pi/128), 0.87*as.numeric(length), dir) # Angle set to change each iteration by '-pi/128' (a small amount, so the fern 'sways in the wind')
  } else {
    return()
  }
}
challenge_F2 <- function(start_position, direction, length, dir) {
  turtle2 <- function(start_position, direction, length) {
    x <- start_position[1] + length*cos(direction)
    y <- start_position[2] + length*sin(direction)
    finish <- c(x,y)
    lines(c(start_position[1], finish[1]), c(start_position[2], finish[2]), type = "l", col = "darkolivegreen3", lwd = 1) # Change the colour
    return(finish)
  }
  dir <- -1*dir # Inverts the function each iteration to mimic the branching patterns of leaves
  finish <- turtle2(start_position, direction, length)
  if (length > 0.03) {
    challenge_F2(finish, (direction - dir*(pi/4)), 0.45*as.numeric(length), dir)
    challenge_F2(finish, (direction - pi/80), 0.87*as.numeric(length), dir)
  } else {
    return()
  }
}
challenge_F3 <- function(start_position, direction, length, dir) {
  turtle3 <- function(start_position, direction, length) {
    x <- start_position[1] + length*cos(direction)
    y <- start_position[2] + length*sin(direction)
    finish <- c(x,y)
    lines(c(start_position[1], finish[1]), c(start_position[2], finish[2]), type = "l", col = "darkolivegreen4", lwd = 1)
    return(finish)
  }
  dir <- -1*dir
  finish <- turtle3(start_position, direction, length)
  if (length > 0.03) {
    challenge_F(finish, (direction + dir*(pi/4)), 0.45*as.numeric(length), dir)
    challenge_F(finish, (direction + pi/128), 0.87*as.numeric(length), dir)
  } else {
    return()
  }
}
challenge_F4 <- function(start_position, direction, length, dir) {
  turtle4 <- function(start_position, direction, length) {
    x <- start_position[1] + length*cos(direction)
    y <- start_position[2] + length*sin(direction)
    finish <- c(x,y)
    lines(c(start_position[1], finish[1]), c(start_position[2], finish[2]), type = "l", col = "darkolivegreen2", lwd = 1)
    return(finish)
  }
  dir <- -1*dir
  finish <- turtle4(start_position, direction, length)
  if (length > 0.03) {
    challenge_F4(finish, (direction + dir*(pi/4)), 0.45*as.numeric(length), dir)
    challenge_F4(finish, (direction + pi/64), 0.87*as.numeric(length), dir)
  } else {
    return()
  }
}
# pdf("../Results/Challenge_F.pdf")
# par(bg = "grey") # Also options for par(col="grey") or par(lwd=2), etc
# plot(NULL, xlab = "Width", ylab = "Height", xlim = c(-20,20), ylim = c(-20,20))
# title(main = "Fractal Fern Simulation in R")
# challenge_F(c(-2,-18), pi/2, 5, 1)
# challenge_F2(c(-2,-18), pi/2.1, 4.5, 1)
# challenge_F3(c(-2,-18), pi/2, 4, 1)
# challenge_F4(c(-2,-18), pi/2.1, 4.5, 1)
# dev.off()


# Challenge G
challenge_G<-function(s,d,l,h){
  t<-function(s,d,l){
    x=s[1]+l*cos(d)
    y=s[2]+l*sin(d)
    f=c(x,y)
    lines(c(s[1],x),c(s[2],y))
    return(f)
  }  
  h=-1*h
  f=t(s,d,l)
  if(l>.05){
    challenge_G(f,(d+h*(pi/4)),0.38*l,-h)
    challenge_G(f,(d+2*pi),0.87*l,h)
  }else{
    return()
  }
}
# plot(NULL,xlim=c(-20,20),ylim=c(0,40), axes=F)
# challenge_G(c(0,0),pi/2,5,-1)
