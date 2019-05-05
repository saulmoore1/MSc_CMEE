#!/usr/bin/env R

##### UG R STATISTICS DEMONSTRATION (YEAR 2) #####

setwd("~/Documents/Other/Code/")
rm(list=ls())
graphics.off()

GaussianSample <- rnorm(1000, m=10, sd=1)
PoissonSample <- rpois(1000, lambda=10)

par(mfrow=c(1,2))
par(mfg=c(1,1)); hist(GaussianSample, col=rgb(0.5,1,0), main="n = 1000", breaks=20)
par(mfg=c(1,2)); hist(PoissonSample, col=rgb(1,0.5,1), main="n = 1000", breaks=20)
dev.off()

GenomeData <- read.csv("~/Documents/iccompbiostat/Data/GenomeSize.csv", header=T)
head(GenomeData)
str(GenomeData)
levels(GenomeData$Family)
tapply(GenomeData$BodyWeight, GenomeData$Family, mean, na.rm=T) # tapply to calculate mean, by family, removing bodyweight NAs
tapply(GenomeData$BodyWeight, GenomeData$Family, length) # Number of species per family
tapply(GenomeData$BodyWeight, GenomeData$Suborder, length) # Per Suborder
tapply(GenomeData$BodyWeight, GenomeData$Suborder, var, na.rm=T) # Variance in body weight among the two suborders

summary(GenomeData) # All these things, straight away!

Times100 <- function(x){
  y <- rep(NA, length(x))
  for(i in 1:length(x)){
    y[i] = x[i]*100
  }
  return(y)
}

tapply(GenomeData$BodyWeight, GenomeData$Family, Times100) # tapply using custom function

# Subsetting to remove NA values altogether
GenomeData_BodyWeight_NAs_Removed <- subset(GenomeData, !is.na(BodyWeight))

longth <- function(x){
  return(length(x[,1]))
}

longth(GenomeData_BodyWeight_NAs_Removed) # How many rows in my dataframe after body weight NAs have been removed?



boxplot(GenomeData$GenomeSize ~ GenomeData$Suborder)
require(RColorBrewer)
boxplot(log(GenomeData$BodyWeight) ~ GenomeData$Family, ylab="log Body Weight (g)", xlab="Suborder", col=brewer.pal(length(unique(GenomeData$Family)), "Set3"), na.rm=TRUE)

load("~/Documents/iccompbiostat/Data/mammals.Rdata")
gd <- rep(levels(mammals$GroundDwelling))
print(gd) # Two-level factor (binary, yes/no)

t1 <- rep(levels(mammals$TrophicLevel), each=2)
print(t1)
model <- lm(logCvalue ~ TrophicLevel + GroundDwelling, data = mammals)
summary(model)
anova(model)
predVals <- data.frame(GroundDwelling = gd, TrophicLevel = t1)
predVals$predict <- predict(model, newdata = predVals)
