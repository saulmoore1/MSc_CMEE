#!/usr/bin/env R
rm(list=ls())

# Load necessary dependencies
library(abc)
library(coda)
source("functions.R")
load("../Data/polar.brown.sfs.Rdata")
ls()

# Eyeball data
cat(polar.brown.sfs)

# Plot site frequency spectrum
pdf("../Results/PolarBrownSFS.pdf")
plot2DSFS(polar.brown.sfs, xlab = "Polar", ylab = "Brown", main = "2D-SFS")
dev.off()

# Calculate number of chromosomes
nChroms.polar <- nrow(polar.brown.sfs)-1
nChroms.polar
nChroms.brown <- ncol(polar.brown.sfs)-1
nChroms.brown

# Calculate the number of sites
nrSites <- sum(polar.brown.sfs, na.rm = T)
nrSites

# Calculate observed site frequency summary statistics
obsSummaryStatistics <- calcSummaryStats(polar.brown.sfs)
obsSummaryStatistics

# Define number of simulations
nSim <- 1000

# Set path to 'ms' software
ms <- "~/Downloads/ms/msdir/ms"

# Name output
fout <- "../Results/ms.txt"

# Initialise empty matrix to store the T parameter values
ParaT <- matrix(nrow = nSim, ncol = 1)

# Initialise matrix for simulation summary statistics
simSummaryStatistics <- matrix(nrow = nSim, ncol = length(obsSummaryStatistics))

for (i in 1:nSim){
  # Parameter is discrete, bounded
  T <- runif(1, min=2e5, max=7e5)
  # Simulate data
  simulate(T = T, M = 0, nrSites, ms_dir = ms, fout = fout)
  # Assign simulated data to a vector
  SimulatedSFS <- fromMStoSFS(fout, nrSites, nChroms.polar, nChroms.brown)
  # Calculate summary statistics
  SimSumStat <- calcSummaryStats(SimulatedSFS)
  # Store parameter value
  ParaT[i,] <- T
  # Store simulated summary statistics
  simSummaryStatistics[i,] <- SimSumStat
  # Print statement
  if (i %% 100 == 0) {
    print(paste("model run",i))
  }
}

# load("../Results/simulations.txt")

# Append statistics to matrix
totSummaryStatistics <- rbind(simSummaryStatistics, obsSummaryStatistics)
# Standardize parameter value (mean = 0 and sd = 1)
scaledSummaryStatistics <- scale(totSummaryStatistics)
# Retrieve scaled target stat values
scaledobsSummaryStatistics <- scaledSummaryStatistics[1001,]
# Isolate simulated scaled values
scaledsimSummaryStatistics <- scaledSummaryStatistics[-c(1001), ]

# Compare stats to paramter values
pdf("../Results/SummaryStats_Params.pdf")
par(mfrow=c(3,3))
plot(scaledsimSummaryStatistics[,"fst"] ~ ParaT, xlab = "Parameter (T)", ylab = "Simulated Summary Statistic (Fst)")
plot(scaledsimSummaryStatistics[,"pivar1"] ~ ParaT)
plot(scaledsimSummaryStatistics[,"pivar2"] ~ ParaT)
plot(scaledsimSummaryStatistics[,"sing1"] ~ ParaT)
plot(scaledsimSummaryStatistics[,"sing2"] ~ ParaT)
plot(scaledsimSummaryStatistics[,"doub1"] ~ ParaT)
plot(scaledsimSummaryStatistics[,"doub2"] ~ ParaT)
plot(scaledsimSummaryStatistics[,"pef"] ~ ParaT)
plot(scaledsimSummaryStatistics[,"puf"] ~ ParaT)
dev.off()
# No obvious relationship, so choose statistics with best inherent properties

# Correlation between summary statistics
pdf("../Results/Pairs.pdf")
pairs(scaledsimSummaryStatistics[,"fst"] ~ scaledsimSummaryStatistics[,"pivar1"] + scaledsimSummaryStatistics[,"pivar2"] + scaledsimSummaryStatistics[,"sing1"] + scaledsimSummaryStatistics[,"sing2"] + scaledsimSummaryStatistics[,"doub1"] + scaledsimSummaryStatistics[,"doub2"] + scaledsimSummaryStatistics[,"pef"] + scaledsimSummaryStatistics[,"puf"])
dev.off()
# The majority of statistics appear to be somewhat correlated
# Summary statistics show that "pef" and "puf" are strongly correlated
cor.test(scaledsimSummaryStatistics[,"pef"], scaledsimSummaryStatistics[,"puf"])
# Yes, very strong negative correlatation.
# 'Fst' looks to be the best measure of genetic diversity
# 'Fst' correlation with other summary statistics:
cor.test(scaledsimSummaryStatistics[,"fst"], scaledsimSummaryStatistics[,"pivar1"])
cor.test(scaledsimSummaryStatistics[,"fst"], scaledsimSummaryStatistics[,"pivar2"])
cor.test(scaledsimSummaryStatistics[,"fst"], scaledsimSummaryStatistics[,"sing1"])
cor.test(scaledsimSummaryStatistics[,"fst"], scaledsimSummaryStatistics[,"sing2"])
cor.test(scaledsimSummaryStatistics[,"fst"], scaledsimSummaryStatistics[,"doub1"])
cor.test(scaledsimSummaryStatistics[,"fst"], scaledsimSummaryStatistics[,"doub2"])
cor.test(scaledsimSummaryStatistics[,"fst"], scaledsimSummaryStatistics[,"pef"])
cor.test(scaledsimSummaryStatistics[,"fst"], scaledsimSummaryStatistics[,"puf"])
# Still considerable correlation with the other measures, pivar2 appears not to be significantly correlated with Fst
# Continue analysis with Fst and pivar2

# Retrieve pivar2 and Fst
scaledsimFst <- scaledsimSummaryStatistics[,"fst"]
scaledsimFst <- as.data.frame(scaledsimFst)
names(scaledsimFst)[names(scaledsimFst)=="scaledsimFst"] <- "Fst"
scaledsimPivar2 <- scaledsimSummaryStatistics[,"pivar2"]
scaledsimPivar2 <- as.data.frame(scaledsimPivar2)
names(scaledsimPivar2)[names(scaledsimPivar2)=="scaledsimPivar2"] <- "pivar2"

# Assign parameter name
ParaT <- as.data.frame(ParaT)
names(ParaT)[names(ParaT)=="ParaT"] <- "Time"

# Perform Approximate Bayesian Computation (ABC)
ABCFst <- abc(target = scaledobsSummaryStatistics["fst"], param = ParaT,
           sumstat = scaledsimFst, tol = 0.05, method = "loclinear")
summary(ABCFst, intvl = .95)

ABCPivar2 <- abc(target = scaledobsSummaryStatistics["pivar2"], param = ParaT,
              sumstat = scaledsimPivar2, tol = 0.05, method = "loclinear")
summary(ABCPivar2, intvl = .95)

ABC <- abc(target = scaledobsSummaryStatistics, param = ParaT,
                 sumstat = scaledsimSummaryStatistics, tol = 0.05, method = "loclinear")
summary(ABCPivar2, intvl = .95)

# ABC diagnostic plots
pdf("../Results/ABC_Diagnostics.pdf")
par(mfrow=c(3,1))
plot(ABCFst, param = ParaT)
plot(ABCPivar2, param = ParaT)
plot(ABC, param = ParaT)
dev.off()

# Posterior density histograms with mean parameter value
pdf("../Results/Histograms.pdf")
par(mfrow=c(1,2))
hist(ABCFst, true = 447123.4, col.true = "red")
hist(ABCPivar2, true = 693883.21, col.true = "red")
dev.off()

# Compute Highest Posterior Density
HPDinterval(as.mcmc(ABCFst$adj.values), prob = 0.95)

