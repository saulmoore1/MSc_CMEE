#!/usr/bin/env R
# setwd("~/Documents/cmeecoursework/Week3/Code/")
rm(list = ls())

require(plyr)

MyDF <- read.csv("../Data/EcolArchives-E089-51-D1.csv", header = TRUE)

# model <- function(d){
#   mod <- lm(Predator.mass ~ Prey.mass, data = d)
#   data.frame(slope = summary(mod)$coefficients[2], intercept = summary(mod)$coefficients[1],
#   r.squared = summary(mod)$r.squared, f.statistic = summary(mod)$fstatistic[1,4],
#   p.value = summary(mod)$coefficients[1,5])
# }

FeedingType <- c(levels(MyDF$Type.of.feeding.interaction))
Lifestage <- c(levels(MyDF$Predator.lifestage))
Location <- c(levels(MyDF$Location))

Results <- c("FeedingType", "PredLifeStage", "Location", "Slope", "Intercept", "R.squared", "F.Statistic", "p.value") # Concatenate the output

for (i in FeedingType) { 
  for (j in Lifestage) {
    for (k in Location) { # For 'Location'  within 'Lifestage' within 'FeedingType'
      if (length(MyDF$Predator.mass[(MyDF$Type.of.feeding.interaction == i) & (MyDF$Predator.lifestage == j) & (MyDF$Location == k)]) >2) {
        model <- lm(MyDF$Predator.mass[(MyDF$Type.of.feeding.interaction == i) & (MyDF$Predator.lifestage == j) 
                                       & (MyDF$Location == k)] ~ MyDF$Prey.mass[(MyDF$Type.of.feeding.interaction == i) & (MyDF$Predator.lifestage == j) & (MyDF$Location == k)])
        assign((paste(i, j, k, sep = "")), c(paste(i), paste(j), paste(k), summary(model)[[4]][2], summary(model)[[4]][1], summary(model)$r.squared, summary(model)$fstatistic[[1]], summary(model)[[4]][8]))
        Results <- rbind(Results, get(paste(i, j, k, sep = "")))
      } else {
        assign((paste(i, j, k, sep = "")), c(paste(i), paste(j), paste(k), "NA", "NA", "NA", "NA", "NA"))
        Results <- rbind(Results, get(paste(i, j, k, sep = "")))
        next
      }
    }
  }
}


write.table(Results, "../Results/PP_Regress_loc_Results.csv", row.names = FALSE, col.names = FALSE, sep = ",")