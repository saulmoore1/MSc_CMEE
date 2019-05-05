#!/usr/bin/env R

rm(list=ls())
setwd("~/Documents/cmeecoursework/SparrowStats/Code/")

d <- read.table("../Data/SparrowSize.txt", header = TRUE)

d1 <- subset(d, d$Wing!="NA")
model3 <- lm(Wing ~ as.factor(BirdID), data = d1)
anova(model3)

#######################################################################
# Analysis of Variance Table
# 
# Response: Wing
# Df Sum Sq Mean Sq F value    Pr(>F)    
# as.factor(BirdID)  617 8147.3 13.2047  8.1734 < 2.2e-16 ***
#   Residuals         1077 1740.0  1.6156                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#######################################################################

# Individual birds have consistent wing-length. 
# The repeated measurements on individual birds differed less than between bird measurements
# Confirming that the measurements of wing length were repeatable

require(dplyr)

d1 %>% group_by(BirdID) %>% summarise(count=length(BirdID))

d1 %>% group_by(BirdID) %>% summarise(count=length(BirdID)) %>% summarise(length(BirdID))
# A more elegent alternative; 'a' = 618 observations

d1 %>% group_by(BirdID) %>% summarise(count=length(BirdID)) %>% summarise(sum(count))
# Denominator: 1695

d1 %>% group_by(BirdID) %>% summarise(count=length(BirdID)) %>% summarise(sum(count^2))
# Numerator: 7307

# Numerator/Denominator
7307/1695
#  4.3109

# Now subtract fraction from the denominator to get (n0)
# 1695-7307/1695
# 'a' is 618, so its really 1/617 (a/a-1)
(1/617)*(1695-7307/1695)
# 2.740177

# So n0 = 2.74, its pretty close to 3, a sort of centrality measure of how many observations we have  for each group

# Now to calculate repeatability
model3 <- lm(Wing ~ as.factor(BirdID), data=d1)
anova(model3)

#####################################################################
# Analysis of Variance Table
# 
# Response: Wing
# Df Sum Sq Mean Sq F value    Pr(>F)    
# as.factor(BirdID)  617 8147.3 13.2047  8.1734 < 2.2e-16 ***
#   Residuals         1077 1740.0  1.6156                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#####################################################################

# So the reapeatability is

((13.20-1.62)/2.74)/(1.62+(13.20-1.62)/2.74)
# 0.7229