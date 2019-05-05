#!/usr/bin/env R
# setwd("~/Documents/cmeecoursework/Week3/Code/")
rm(list = ls())

MyDF <- read.csv("../Data/EcolArchives-E089-51-D1.csv", header = TRUE) # Read Ecological Archives dataset

# Define variables for ease of use
FeedingType <- c(levels(MyDF$Type.of.feeding.interaction)) 
Lifestage <- c(levels(MyDF$Predator.lifestage))

require(ggplot2)

# Plotting the Figure 8.8, as best as I can:
pdf("../Results/Figure_8.8.pdf") # Open empty plot window
print(ggplot(MyDF, aes(x = Prey.mass, y = Predator.mass, group = Predator.lifestage, colour = Predator.lifestage)) +
  geom_point(shape = 3) +
  geom_smooth(method = "lm", size = 0.7, fullrange = TRUE) + 
  facet_grid(Type.of.feeding.interaction ~.) + 
  scale_y_log10(breaks = c(1e-06, 1e-02, 1e+02, 1e+06)) + 
  scale_x_log10(breaks = c(1e-07, 1e-03, 1e+01)) +
  labs(x = "Prey Mass in grams", y = "Predator mass in grams") +
  guides(col = guide_legend(ncol = 6, byrow = TRUE)) +
  coord_fixed(ratio = 0.44) +
  theme_bw() +
  theme(legend.position="bottom") +
  theme(panel.border = element_rect("grey50", fill = NA, size = 0.5), # Customise the theme as much as possible!
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line("grey90"),
        panel.grid.minor = element_line("grey98"), axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"), legend.title = element_text(face = "bold", size = 8), legend.text = element_text(size = 8), strip.text = element_text(size = 7)))
dev.off()

# That will have to do. I need my sleep!
# plot.margin = unit(c(3,4,3,4), "cm")

Results <- c("FeedingType", "PredLifeStage", "Slope", "Intercept", "R.squared", "F.Statistic", "p.value") # Concatenate the output

# A 'for' loop to iterate through the dataframe for lifestage as a subset of feeding type, run linear models for each combination of interactions and assign the variable name 'Results' to the model coefficients
for (i in FeedingType) {
  for (j in Lifestage) {
    if (length(MyDF$Predator.mass[(MyDF$Type.of.feeding.interaction == i) & (MyDF$Predator.lifestage == j)]) > 2) {
      model <- lm(MyDF$Predator.mass[(MyDF$Type.of.feeding.interaction == i) 
      & (MyDF$Predator.lifestage == j)] ~ MyDF$Prey.mass[(MyDF$Type.of.feeding.interaction == i) & (MyDF$Predator.lifestage == j)])
      assign((paste(i, j, sep = "")), c(paste(i), paste(j), summary(model)[[4]][2], summary(model)[[4]][1], summary(model)$r.squared, summary(model)$fstatistic[[1]], summary(model)[[4]][8]))
      Results <- rbind(Results, get(paste(i, j, sep = "")))
    } else {
      assign((paste(i, j, sep = "")), c(paste(i), paste(j), "NA", "NA", "NA", "NA", "NA"))
      Results <- rbind(Results, get(paste(i, j, sep = "")))
      next
    }
  }
}

# linear model of predator mass as a function of prey mass for subset piscivorouspostlarva/juvenile returns no p-value for the interaction
# linear model of predator mass as a function of prey mass for subset planktivorousjuvenile returns no p-value for the interaction
# You cannot run a linear model with just two values
# NAs are incorporated into results table where applicable

# Guess I could have used 'ddply'...

write.table(Results, "../Results/PP_Regress_Results.csv", row.names = FALSE, col.names = FALSE, sep = ",") # Write the results table and save to results directory
