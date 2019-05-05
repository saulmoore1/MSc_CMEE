#!/usr/bin/env R
# setwd("/home/saul/Documents/cmeecoursework/Week3/Code")
rm(list=ls()) # Clear workspace to remove any objects that may be stored in R

# A script that (1) draws and saves three lattice graphs by feeding interaction type: predator mass; prey mass; and size ratio of prey mass/predator mass; and (2) calculate the mean and median predator mass, prey mass and predator-prey size-ratios to a csv file.

# Import lattice
library(lattice)

# Read data from csv
MyDF <- read.csv("../Data/EcolArchives-E089-51-D1.csv", header = TRUE)
# Return size of data frame
dim(MyDF)

# Open an empty pdf file for the lattice densityplot
pdf("../Results/Pred_Lattice.pdf", 11.7, 8.3)
# Predator mass as a function of feeding interaction type
print(densityplot(~log(Predator.mass) | Type.of.feeding.interaction, data = MyDF))
dev.off()

pdf("../Results/Prey_Lattice.pdf", 11.7, 8.3) # Open an empty pdf file for the lattice densityplot
print(densityplot(~log(Prey.mass) | Type.of.feeding.interaction, data = MyDF))
dev.off()

pdf("../Results/SizeRatio_Lattice.pdf", 11.7, 8.3)
print(densityplot(~log(Predator.mass/Prey.mass) | Type.of.feeding.interaction, data = MyDF))
dev.off()

# Calculate mean mass
MeanPredMass <- tapply(log(MyDF$Predator.mass), MyDF$Type.of.feeding.interaction, mean) 
MeanPreyMass <- tapply(log(MyDF$Prey.mass), MyDF$Type.of.feeding.interaction, mean)
MeanPredPrey <- tapply(log(MyDF$Predator.mass/MyDF$Prey.mass), MyDF$Type.of.feeding.interaction, mean)

# Calculate median mass
MedianPredMass <- tapply(log(MyDF$Predator.mass), MyDF$Type.of.feeding.interaction, median) 
MedianPreyMass <- tapply(log(MyDF$Prey.mass), MyDF$Type.of.feeding.interaction, median)
MedianPredPrey <- tapply(log(MyDF$Predator.mass/MyDF$Prey.mass), MyDF$Type.of.feeding.interaction, median)

Mean <- c(MeanPredMass, MeanPreyMass, MeanPredPrey) # Concatenate the means
Median <- c(MedianPredMass, MedianPreyMass, MedianPredPrey) # Concatenate the medians

# Create a new dataframe to store the calculations
FeedingType <- levels(MyDF$Type.of.feeding.interaction)
level <- nlevels(MyDF$Type.of.feeding.interaction)

FeedingType <- rep(FeedingType, 3)
Pred <- rep("LogPredMass", level) # Assign variable names for the three operations
Prey <- rep("LogPreyMass", level)
Ratio <- rep("LogSizeRatio", level)

Operation <- c(Pred, Prey, Ratio) # Concatenate the operations
 
DF <- data.frame(Operation, FeedingType, Mean, Median) # Create the dataframe
write.csv(DF, "../Results/PP_Results.csv", row.names = FALSE) # Save the dataframe as .csv file

# Some nice (unused) plots:

# boxplot(log(MyDF$Predator.mass) ~ MyDF$Type.of.feeding.interaction, # Predator mass as a function of (tilda '~') feeding interaction type
#         xlab = "Feeding Interaction Type", ylab = "log(Predator Mass)", # Axes labels
#         col = 2:length(MyDF$Type.of.feeding.interaction), # Colour boxplots by feeding interaction type
#         main = "Predator Mass varies with Feeding Interaction Type", ylim = c(-15,15))

# boxplot(log(MyDF$Prey.mass) ~ MyDF$Type.of.feeding.interaction, # Prey mass as a function of (tilda '~') feeding interaction type
#         xlab = "Feeding Interaction Type", ylab = "log(Prey Mass)", # Axes labels
#         col = 2:length(MyDF$Type.of.feeding.interaction), # Colour boxplots by feeding interaction type
#         main = "Prey Mass varies with Feeding Interaction Type", ylim = c(-25,10))

# plot(log(MyDF$Predator.mass/MyDF$Prey.mass) ~ MyDF$Type.of.feeding.interaction,
#      xlab = "Type of Feeding Interaction", ylab = "Predator/Prey Size Ratio",
#      main = "Predator-prey size ratio as a function of feeding interaction type",
#      col = 2:length(MyDF$Type.of.feeding.interaction))
