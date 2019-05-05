#!/usr/bin/env R
# setwd("/home/saul/Documents/cmeecoursework/Week3/Code/")
rm(list = ls())

# A simple R script to illustrate R input-output.

MyData <- read.csv("../Data/trees.csv", header = TRUE) # Import 'trees', with headers
write.csv(MyData, "../Results/MyData.csv") # Write it out as a new file
write.table(MyData[1,], file = "../Results/MyData.csv",append=TRUE) # Append to it (WARNING)
write.csv(MyData, "../Results/MyData.csv", row.names=TRUE) # Write row names
write.table(MyData, "../Results/MyData.csv", col.names=FALSE, sep = ",") # Ignore column names

# Try:
# source("basic_io.R")
