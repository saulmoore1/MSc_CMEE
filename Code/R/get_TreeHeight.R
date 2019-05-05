#!/usr/bin/env R
# setwd("/home/saul/Documents/cmeecoursework/Week3/Code/")
rm(list = ls())

# This function calculates the heights of trees from the angle of elevation and the distance from the base, using the trigonometric formula
# Height = distance * tan(radians)

# Arguments:
# degrees     The angle of elevation
# distance    The distance from base

# Output:
# The height of the tree, same unit as 'distance'

# Requirements:

# Function remains the same, the way we source the input files is different
TreeHeight <- function(Angle.degrees, Distance.m, data = DF){
  radians <- Angle.degrees * pi / 180
  TreeHeight.m <- Distance.m * tan(radians)
  print(paste("Tree height is:", TreeHeight.m))
  
  return(TreeHeight.m)
}

args <- commandArgs(TRUE) 
# The 'commandArgs' function allows the script to be called directly from the bash terminal command line
# with the file name specified as follows: "RScript get_TreeHeight.R trees.csv"

DF <- read.csv(args[1], header = TRUE) # Need to make this read any .csv input file
# DF <- read.csv(file.choose()) # Choose your own file manually, through Nautilus!!!
TreeHeight.m <- TreeHeight(DF$Angle.degrees, DF$Distance.m)

# Append new column to dataframe
DF["TreeHeight.m"] <- TreeHeight.m

# Strip relative path and file extension
name <- tools::file_path_sans_ext(basename(args[1]))
# Alternatively...
# Output <- file.path(gsub(".csv", "_treeheights.csv", args[1])) # Using 'gsub' function to modify file name
# Output <- file.path(gsub("../Data/", "../Results/", Output)) # Using 'gsub' to change output file directory

write.csv(DF, paste0("../Results/", name, "_treeheights.csv"))
# write.csv(DF, Output)  

          
