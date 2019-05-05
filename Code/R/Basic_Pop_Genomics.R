#!/usr/bin/env R
rm(list = ls())

library(dplyr)
library(ggplot2)
library(reshape2)

g <- read.table(file="../Data/Population_Genomics_Practical/H938_chr15.geno", header = TRUE) # Read genomic data for chr15 H938
head(g)

# How many SNPs are there? 
dim(g) # 19560 SNPs

# Calculate the no. counts for each locus

g <- mutate(g, nObs = nA1A1 + nA1A2 + nA2A2)
# 'mutate' function sums the rows for each column (could also use colSums function)
# nA1A1 - number of A1/A1 homozygotes (p^2), nA1A2 - number of A1/A2 heterozygotes (pq), and nA2A2 - number of A2/A2 homozygotes (q^2)

head(g) # To confirm the new column 'nObs' - number of observations
summary(g$nObs)

# Quick plot using 'ggplot'
pdf("../Results/Histogram_nObs.pdf")
qplot(nObs, data = g)
dev.off()
# Data are from 938 individual humans. nObs below 938 result from a difficulty in array data when calling the genotype, so no genotype could be recorded
# Indeed, summary(g$nObs) shows minimum nObs was 887

# Compute the genotype frequencies
g <- mutate(g, p11 = nA1A1/nObs, p12 = nA1A2/nObs, p22 = nA2A2/nObs)
head(g) # 'mutate' appended the results to the table

# Compute allele frequencies from genotype frequencies
g <- mutate(g, p1 = p11 + 0.5*p12, p2 = p22 + 0.5*p12) # 'mutate' appended the results to the table
head(g)

# Plot frequency of major allele (A2) against the frequency of the minor allele (A1)
pdf("../Results/Allele_freq.pdf")
qplot(p1, p2, data = g)
dev.off()
# Beware of the axes, it should be that p2 > p1
# Note that there is a linear relationship between p2 and p1. 
# Equation: p1 + p2 = 1

# Plotting genotype on allele frequencies
# We need to have 'tidy' data - one row per pair of points to be plotted
# First, subset the data using 'select' and pipe into the melt command, which re-formats the data
gTidy <- select(g, c(p1, p11, p12, p22)) %>% melt(id='p1', value.name = "Genotype.Proportion")
head(gTidy)
dim(gTidy) 
# NB: Now 3x as long as the no. SNPs in the dataset, since there are 3 possible genotypes for each SNP (long format)

pdf("../Results/Genotype_prop.pdf")
ggplot(gTidy) + geom_point(aes(x = p1,
                               y = Genotype.Proportion,
                               color = variable,
                               shape = variable))
dev.off()

# Equations inferred from Hardy-Weinberg Equation: p1^2 + p2^2 + 2p1p2 = 1
# Where:
# p11 = (p1)^2
# p12 = 2p1(1 - p1)
# p22 = (1 - p1)^2

pdf("../Results/Hardy_Weinberg.pdf")
ggplot(gTidy)+ geom_point(aes(x=p1,y=Genotype.Proportion, color=variable,shape=variable)) + 
  stat_function(fun=function(p) p^2, geom="line", colour="red",size=2.5) + 
  stat_function(fun=function(p) 2*p*(1-p), geom="line", colour="green",size=2.5) + 
  stat_function(fun=function(p) (1-p)^2, geom="line", colour="blue",size=2.5)
dev.off()

# The Hardy-Weinberg equation appears to be a pretty damn good fit to the data
# However, there appears to be a systematic deficiency of heterozygotes and an excess of homozygotes
# Why? Because it is an actual population and so there is structure to the population
# There is obviously not random mating. People are more likely to mate with people geographically closer to them.

# Testing Hardy-Weinberg, using the Pearson's Chi-sq test: X2 =  Σi􏰕 (oi − ei)2 / ei
# Append output as addtional column 'X2'
g <- mutate(g, X2 = (nA1A1 - nObs*p1^2)^2 / (nObs*p1^2) +
  (nA1A2 - nObs*2*p1*p2)^2 / (nObs*2*p1*p2) + 
  (nA2A2 - nObs*p2^2)^2 / (nObs*p2^2))

# Append column 'pval' for p-values
g <- mutate(g, pval = 1 - pchisq(X2, 1))
head(g$pval)

# MULTIPLE TESTING PROBLEM
# The p-value gives us the frequency at which that the observed departure from 
# expectations (or a more extreme departure) would occur if the null hypothesis is true. 
# If the data are relatively rare under the null (e.g. p-value < 5%), we reject the null hypothesis, 
# and we would infer that the given SNP departs from Hardy-Weinberg expectations. 
# This is problematic here though. The problem is that we are testing many, many SNPs! 
# Even if the null is universally true, 5% of our SNPs would be expected to be rejected, which is huge!
# This is called the multiple testing problem.
# Some approaches to this include the Bonferroni approach and the False-Discovery Rate (FDR)

sum(g$pval < 0.05, na.rm = TRUE) # 14314!
sum(g$pval > 0.05, na.rm = TRUE) # 5246!

# Fisher classically said that under the null hypothesis, the p-values of a well-designed test should be 
# distributed uniformally between 0 ad 1
pdf("../Results/Pval.pdf")
qplot(pval, data = g)
dev.off()

# Plotting Expected vs. Observed Heterozygosity
pdf("../Results/HetExpvsObs.pdf")
qplot(2*p1*(1-p1), p12, data = g) + geom_abline(intercept = 0, slope=1, color="red", size=1.5)
dev.off()
# Most of the points fall below the y=x line, displaying a clear systematic deficiency of heterozygotes
# It is this general pattern that is contributing to the departure from the HW equation seen in the Chisq statistics

# Average deficiency of heterozygotes relative to the expected proportion
pDefHet <- mean((2*g$p1*(1-g$p1)-g$p12) / (2*g$p1*(1-g$p1)))
pDefHet # 0.11, or 11% difference between expected and observed for a global sample of humans
# The true figure is thought to be ~10%, which is surprisingly large for such a global sample size
# Most alleles are globally widespread and not sufficiently geographically clustered to generate correlations among the uniting alleles,
# This is because all humans derived, relatively recently (100,000-150,000 years ago), from some relatively-not-so-ancestral population in Africa

# Finding specific loci that are large departures from Hardy-Weinberg
g <- mutate(g, F = (2*p1*(1-p1)-p12) / (2*p1*(1-p1))) 
head(g)

# Compute the same relative deficiency you computed above, but let’s look at it per SNP. 
# This number is referred to as 'F' by Sewall Wright and has connections directly to correlation coefficients
# If we assume there is no inbreeding within populations, this number is an estimator of FST (a quantity that appears often in population genetics).

pdf("../Results/F_SNP.pdf")
plot(g$F, xlab = "SNP number")
dev.off()
# A few SNPs show a very high or very low F value.
# When a high or low F value is due to genotyping error, it likely only affects a single SNP. 
# However, when there is some population genetic force acting on a region of the genome, it likely affects multiple SNPs in the region. 

# Let’s take a local average in a sliding window of SNPs across the genome, computing an average F over every 5 consecutive SNPs 
# (in real data analysis we might use 100kb or 0.1cM windows)

movingavg <- function(x, n=5){stats::filter(x, rep(1/n,n), sides = 2)} # 'stats:filter' command calls the filter function from the 'stats' library

pdf("../Results/F_SNP_Average.pdf")
plot(movingavg(g$F), xlab="SNP number")
dev.off()
# There is one large spike where the average F is approximately 60% in the dataset! 
# Let’s extract the SNP id for the largest value, and look at the dataframe

outlier=which (movingavg(g$F) == max(movingavg(g$F),na.rm=TRUE)) 
g[outlier,]
##########################################################################
# CHR        SNP       A1 A2 nA1A1 nA1A2 nA2A2 nObs  p11      p12       p22
# 5927  15 rs12440301  A  G   246   173   519  938 0.2622601 0.184435 0.5533049
#  p1        p2       X2   pval        F
# 5927 0.3544776 0.6455224 334.3032    0 0.5969925
##########################################################################

# This SNP resides very close to the gene SLC24A5, an extremely varied gene responsible for the skin pigmentation differences between humans
# The high F value observed here is because natural selection has led to a geographic clustering of alleles in this gene region

# We didn’t discuss how genotyping errors might create Hardy-Weinberg departures, 
# but if we were doing additional analyses, we could use Hardy-Weinberg departures 
# to filter them from our data. It’s common practice to do so, but with a Bonferonni 
# correction and using data from within populations to do the filtering.
