#!/usr/bin/env R
rm(list=ls())

x <- rnorm(1000, m=0, sd=1) # also dnorm / qnorm 
x2 <- runif(1000, min=0, max=2) 
x3 <- rpois(1000, lambda=10)
mean(x)
sd(x)
var(x)
median(x)
quantile(x, 0.05)
range(x)
min(x)
max(x)
sum(x)
graphics.off()
par(mfcol=c(3,1))
par(mfg=c(1,1)); hist(x, col=rgb(1,1,0), main='Gaussian \n n = 1000', breaks=20)
par(mfg=c(2,1)); hist(x2, col=rgb(0,1,1), main='Uniform \n n = 1000', breaks=20)
par(mfg=c(3,1)); hist(x3, col=rgb(1,0,1), main='Poisson \n n = 1000', breaks=20)

# Load the data
genome <- read.csv('../Data/GenomeSize.csv', header = T)

# Data visualisation and wrangling
head(genome)
str(genome)
summary(genome)
tapply(genome$BodyWeight, genome$Suborder, mean, na.rm=TRUE) # mean
tapply(genome$BodyWeight, genome$Suborder, length) # sample size
tapply(genome$BodyWeight, genome$Suborder, var, na.rm=TRUE) # variance

# You can also remove NA's by subsetting
BodyWt_no_NA <- subset(genome, !is.na(BodyWeight))
str(BodyWt_no_NA)

dev.off()
hist(genome$GenomeSize, breaks=10, col="blue")
plot(density(genome$GenomeSize, bw=0.1), main="Genome Size\nDensity Plot")

