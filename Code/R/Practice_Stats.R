#!/usr/bin/env R
# setwd("/home/saul/Documents/cmeecoursework/SparrowStats/Code/")

rm(list=ls())

d <- read.table("../Data/SparrowSize.txt", header = TRUE)
str(d)
names(d)
head(d)

length(d$Tarsus) # 1770 row entries for sparrow tarsi

hist(d$Tarsus) # Histogram of tarsi, roughly a normal distribution

mean(d$Tarsus, na.rm = TRUE) # Because there are NA's in the data

median(d$Tarsus, na.rm = TRUE)

mode(d$Tarsus) # Returns 'numeric', as we are looking at continuous data (1.1, 1.2, 1.23, etc)

par(mfrow = c(2, 2))
hist(d$Tarsus, breaks = 3, col = "grey")
hist(d$Tarsus, breaks = 10, col = "grey")
hist(d$Tarsus, breaks = 30, col = "grey")
hist(d$Tarsus, breaks = 100, col = "grey")

# The number of breaks = the number of bins (aka bars) used to draw the histogram
# At some point, the resolution the resolution is larger than that of the measuring device. No finer observations were recorded!
#  IF YOU MEASURE IN km, DO NOT PRESENT RESULTS IN mm!!!
# Clearly the mode is somewhere between 18 and 19.
# To get a more precise definition of the mode:

install.packages("modeest")
require(modeest)

?modeest # This package intends to provide estimators of the mode of univariate unimodal (and sometimes multimodal) data and values of the modes of usual probability distributions

mlv(d$Tarsus) # Again, problems!!

d2 <- subset(d, d$Tarsus!="NA") # Subset the dataset to remove missing values
length(d$Tarsus)
length(d2$Tarsus)

mlv(d2$Tarsus) # Mode (most likely value): 18.57361
median(d$Tarsus, na.rm = TRUE) # Median: 18.6
mean(d$Tarsus, na.rm = TRUE) # Mean: 18.52335

# Fairly similar. If the distribution is perfectly normal, they should be identical
# As skew increases, these three measures diverge

range(d$Tarsus, na.rm = TRUE)
range(d2$Tarsus) # NAs already removed

var(d$Tarsus, na.rm = TRUE)
var(d2$Tarsus)
sum((d2$Tarsus - mean(d2$Tarsus))^2)/(length(d2$Tarsus) - 1) # This is the variance!

sqrt(var(d2$Tarsus))
sd(d2$Tarsus) # Standard deviation is the square root of the variance

# Z-scores and Quantiles

# Z-values are derived from a standardized normal distribution, 
# with a mean of 0 and a standard deviation of 1. 
# Z-scores are super useful for standardised comparisons in traits between completely different taxa, for example

zTarsus <- (d2$Tarsus - mean(d2$Tarsus))/sd(d2$Tarsus) # Function 'scale()' in R does this calculation for you
sd(zTarsus)
hist(zTarsus)

set.seed(123)
znormal <- rnorm(1e+06)
hist(znormal, breaks = 100) # A beautifully 'almost-perfect' normal distribution
summary(znormal)

qnorm(c(0.025, 0.975)) # Returns the 2.5% and 97.5% quantiles of the data
pnorm(.Last.value) # Returns the corresponding probabilities

par(mfrow = c(1,2))
hist(znormal, breaks = 100)
abline(v = qnorm(c(0.25, 0.5, 0.75)), lwd = 2) # 'lwd' specifies line width
abline(v = qnorm(c(0.025, 0.975)), lwd = 2, lty = "dashed") # 'lty' specifies line type
plot(density(znormal))
abline(v = qnorm(c(0.25, 0.5, 0.75)), col = "gray")
abline(v = qnorm(c(0.025, 0.975)), lty = "dotted", col = "black")
abline(h = 0, lwd = 3, col = "blue")
text(2, 0.3, "1.96", col = "red", adj = 0)
text(-2, 0.3, "-1.96", col = "red", adj = 1)

# The 95% CI is a very important property - it is the range of values that encompasses the population
# true value with 95% probability (error 5% of the time is maximum error allowed before we are forced to accept the null)

boxplot(d2$Tarsus~d2$Sex.1, col = c("red", "blue"), ylab = "Tarsus Length (mm)")
# Make sure you know what the boxes and whiskers represent:
?boxplot


# Standard errors - a good way to display uncertainty

d1 <- subset(d, d$Tarsus!="NA")
# The standard error in tarsi length is the sqrt of (variance/sample size):
seTarsus <- sqrt(var(d1$Tarsus)/length(d1$Tarsus))
seTarsus # 0.021

# To subset your data to look at just 2001, for example:
d2001 <-subset(d1, d1$Year == 2001)
seTarsus2001 <- sqrt(var(d2001$Tarsus)/length(d2001$Tarsus))
seTarsus2001 # 0.103

# The standard error (se) of 2001 is about 4 times the se for the total population
# 'The sqrt law of sample size' - to double your precision, you must quadruple your sample size!

# 95% CI = +/-1.96 * se 
# NB: only for sample sizes >50 observations
