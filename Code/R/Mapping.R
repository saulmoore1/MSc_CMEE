#!/usr/bin/env R
# setwd("~/Documents/cmeecoursework/Week3/Code/")
rm(list = ls())

# Script to investigate mapping in R using the 'maps' package

require(maps)

load("../Data/GPDDFiltered.RData")

pdf("../Results/GPDDMap.pdf") # Open empty pdf plot
map(database = "world", regions = ".")
points(gpdd$long, gpdd$lat, col = "red", cex = 0.6) # x = longitude, y = latitude
# Do not need to assign name 'gpdd', as it is already assigned within the .RData file. Cool! This was the same with 'ats' in the 'TAutoCor' practical
dev.off()

# Main sampling locations are Europe and North America. There was only one sampling location in Africa, and only a couple in Japan. 
# There is not a representative cover of the continents Asia, Australasia (Oceania?), Africa and South America.
# So any comparison between Europe/North America and the other continents introduces a pseudo-replication bias. 
# All sampling locations but one are clustered in the Northern Hemisphere, so no true comparison can be drawn between the Northern and Southern Hemispheres.
