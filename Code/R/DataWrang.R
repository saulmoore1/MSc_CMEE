#!/usr/bin/env R
# rm(list=ls())
# setwd("~/Documents/cmeecoursework/Week3/Code")

################################################################
################## Wrangling the Pound Hill Dataset ############
################################################################

############# Load the dataset ###############
MyData <- as.matrix(read.csv("../Data/PoundHillData.csv",header = F)) # header = false because the raw data don't have real headers
MyMetaData <- read.csv("../Data/PoundHillMetaData.csv",header = T, sep=";", stringsAsFactors = F) # header = true because we do have metadata headers
# Loading data as.matrix() and setting header and stringsAsFactors (R automatically converts columns to factors, or treatment variables) guarantees that the data are imported 'as is', so you can wrangle them
# Otherwise R 'read.csv' will default create a data.frame, converting the first row to collumn headers, convert everything to factors, etc


############# Inspect the Dataset ###############
head(MyData) # Show the beginning of the matrix
dim(MyData) # Return the dimensions of the matrix (45, 60)
str(MyData)


############# Transpose the Data ###############
MyData <- t(MyData) # 't' is transpose
head(MyData)
dim(MyData) # (60, 45) Note: dimensions have switched


############# Replace species absences with zeros/NA ###############
MyData[MyData == ""] = NA # Zero assumes a true absence


############# Convert raw matrix to data frame ###############
TempData <- as.data.frame(MyData[-1,],stringsAsFactors = F) #stringsAsFactors = F is important! Tells R not to treat the variable data columns as factors
# '[]' accesses the file, and '-1' deletes the first row from TempData, so it can be used as a column name as follows...
colnames(TempData) <- MyData[1,] # assign column names from original data


############# Convert from wide to long format  ###############
require(reshape2) # load the reshape2 package

# Check out '?melt', a function in the 'reshape2' package/module
MyWrangledData <- melt(TempData, id=c("Cultivation", "Block", "Plot", "Quadrat"), 
variable.name = "Species", value.name = "Count")
MyWrangledData[, "Cultivation"] <- as.factor(MyWrangledData[, "Cultivation"])
MyWrangledData[, "Block"] <- as.factor(MyWrangledData[, "Block"])
MyWrangledData[, "Plot"] <- as.factor(MyWrangledData[, "Plot"])
MyWrangledData[, "Quadrat"] <- as.factor(MyWrangledData[, "Quadrat"]) # Manually convert the appropriate treatments to factors/grouping variables
MyWrangledData[, "Count"] <- as.numeric(MyWrangledData[, "Count"]) # Convert count to numeric data, observations
# This is the format we want R to read the data in

str(MyWrangledData)
head(MyWrangledData)
dim(MyWrangledData) # 2419 by 6! Long format.

############# Start exploring the data (extend the script below)!  ###############

print(MyWrangledData$Species)
print(MyWrangledData$Cultivation)
print(MyWrangledData$Count)

par(mfcol=c(1,2))
par(mfg = c(1,1))
Hist1 <- hist(log(MyWrangledData$Count) ~ MyWrangledData$Species, xlab = "Species", ylab = "log(Abundance)", col = "lightblue", border = "pink", main = "Total abundance as a function of plot")

par(mfg = c(1,2))
Hist2 <-hist(log(MyWrangledData$Count), xlab = "", ylab = "Count", col = "lightblue", border = "pink", main = "")

head(MyWrangledData)
boxplot(MyWrangledData$Plot, MyWrangledData$Cultivation)
