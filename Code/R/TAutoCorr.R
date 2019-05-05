#!/usr/bin/env R
# setwd("~/Documents/cmeecoursework/Week3/Code/")
rm(list = ls())
set.seed(1)

# Script to compare annual mean temperatures throughout the 20th Century for Key West, Florida, and determine whether the data is significantly correlated to predict future temperatures in the region

# Load annual temperatures for Key West, Florida for the 20th Century:
load("../Data/KeyWestAnnualMeanTemperature.RData")
summary(ats)

Temperature <- ats$Temp[1:(length(ats$Temp) - 1)] # Remove the last year, as you cannot correlate with the next year as there is no data yet
Temperature2 <- ats$Temp[2:(length(ats$Temp))] # Remove the first year, as you would not be able to correlate it with any previous year

pdf("../Results/TempPlot.pdf")
plot(Temperature2 ~ Temperature, ylab = "Temperature for Years: 1902-2000", xlab = "Temperature for Years: 1901-1999", pch=16, col="orange", cex=0.8)
abline(lm(Temperature2 ~ Temperature), col="orange", lwd=2)
dev.off()

# Correlation coefficient calculation
# Cannot use standard p-value calculated for a correlation coefficient (using 'cor' function) because measurements of
# climatic variables in successive time-points in a time series (successive seconds, minutes, hours, months, years) are not independent
Corr <- cor(Temperature, Temperature2) # Correlation coefficient: ~ 0.326
Perms <- 100000
PermCorr <- rep(NA, Perms) # Randomly permute the time series 10000 times
for (i in 1:length(PermCorr)) {
  PermTemp <- sample(ats$Temp, length(ats$Temp), replace = FALSE)
  PermCorr[i] <- cor(ats$Temp, PermTemp) # Computes the correlation coefficient for each randomly permuted year sequence, and stores it
}

ApproxPValue <- sum(PermCorr > Corr)/length(PermCorr)
print(ApproxPValue)
