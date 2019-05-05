#!/usr/bin/env R
#setwd("/home/saul/Documents/cmeecoursework/Week3/Code/")

# This function calculates the heights of trees from the angle of elevation and the distance from the base, using the trigonometric formula
# Height = distance * tan(radians)

# Arguments:
# Angle.degrees       The angle of elevation
# Distance.m		      The distance from base

# Input: 
# Comma separated values file '../Data/trees.csv'

# Output:
# 'Tree.Height.m' 		The height of the tree, same unit as 'Distance.m'
# Comma-separated-values file '../Results/TreeHts.csv'

DF <- read.csv("../Data/trees.csv", header = TRUE) # Read 'trees.csv' file from the Data directory, using relative paths
DF # View the dataframe

# Function:
TreeHeight <- function(Angle.degrees, Distance.m){
  radians <- Angle.degrees * pi / 180 # Convert to radians for easier calculation
  TreeHeight.m <- Distance.m * tan(radians) # Basic trigonometry, simple stuff
  # print(paste("Tree height is:", TreeHeight.m)) # Print result
  return(TreeHeight.m) # Return the tree height (m) for each row in the dataframe
}

TreeHeight.m <- TreeHeight(DF$Angle.degrees, DF$Distance.m) # Calculate tree height using the function 'TreeHeight' specified above with the angle to the top of the tree and distance to tree as inputs

DF["TreeHeight.m"] <- TreeHeight.m # Append the new column 'TreeHeight.m' to the dataframe 'DF'
# DF$TreeHeight.m <- TreeHeight.m # Same thing...

write.csv(DF, "../Results/TreeHts.csv") # Create an output .csv file in the Results directory
