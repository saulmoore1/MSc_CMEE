#!/usr/bin/env R

rm(list=ls())
setwd("~/Documents/cmeecoursework/SparrowStats/Code/")

daphnia <- read.delim("../Data/daphnia.txt")
summary(daphnia)

# Outliers
par(mfrow = c(1,2))
plot(Growth.rate ~ Detergent, data = daphnia)
plot(Growth.rate ~ Daphnia, data = daphnia)

# Homogeneity of variances
# This is an important model assumption of linear regression analyses in general (eg ANOVAs)
# RULE OF THUMB (for ANOVAs) - The ratio between the largest and smallest variance should be <4

require(dplyr)

daphnia %>% group_by(Detergent) %>% summarise(variance=var(Growth.rate))

daphnia %>% group_by(Daphnia) %>% summarise(variance=var(Growth.rate))

######################################################################
# A tibble: 3 × 2
# Daphnia  variance
# <fctr>     <dbl>
#   1  Clone1 0.3313181
# 2  Clone2 1.5300977
# 3  Clone3 1.5289960
######################################################################

# Here, the ratio of variances for Clone1 against the other two is greater than 4, but not much more, its borderline
# Keep this in mind when drawing conclusions and discussing model limitations
# eg. "The assumption of normality was violated - ratio of largest and smallest variance was ~5, which is sightly too much, and might bias the least square estimators"
# Consider the consequences of this in your report

hist(daphnia$Growth.rate)
# Not hugely normally distributed! Linear regression assumes normality, but it is reasonably robusst against violations
# Regression towards the mean - more observations = more likely to be normally distributed

# Look for an exceessive amount of zeros
# Is there colinearity among the covariates? Well, we only have categories here, so it doesnt apply

# MODEL DAPHNIA

seFun <- function(x){
  sqrt(var(x)/length(x))
}

detergentMean <- with(daphnia, tapply(Growth.rate, INDEX = Detergent, FUN = mean))
detergentSEM <- with(daphnia, tapply(Growth.rate, INDEX = Detergent, FUN = seFun))

cloneMean <- with(daphnia, tapply(Growth.rate, INDEX = Daphnia, FUN = mean))
cloneSEM <- with(daphnia, tapply(Growth.rate, INDEX = Daphnia, FUN = seFun))

par(mfrow = c(2,1), mar = c(4,4,1,1))
barMids <- barplot(detergentMean, xlab = "Detergent Type", ylab = "Population growth rate", ylim = c(0,5))
arrows(barMids, detergentMean - detergentSEM, barMids, detergentMean + detergentSEM, code = 3, angle = 90)
barMids <- barplot(cloneMean, xlab = "Daphnia clone", ylab = "Population growth rate", ylim = c(0,5))
arrows(barMids, cloneMean - cloneSEM, barMids, cloneMean + cloneSEM, code = 3, angle = 90)

# From the output graphs, the differences in the means for the detergents dont look like they matter, but we should test whether they have any explanatory power
# We can do this by adding both variables into the formula describing the model
# y ~ x + z  # Variable 'z' added to the model

daphniaMod <- lm(Growth.rate ~ Detergent + Daphnia, data = daphnia)
anova(daphniaMod)

#####################################################################
# Analysis of Variance Table
# 
# Response: Growth.rate
# Df Sum Sq Mean Sq F value    Pr(>F)    
# Detergent  3  2.212  0.7372  0.6422    0.5906    
# Daphnia    2 39.178 19.5889 17.0635 1.064e-06 ***
#   Residuals 66 75.768  1.1480                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#####################################################################

# So, genotype is important for determining the population growth rate of the Daphnia measured, but the detergents have no significant effect

summary(daphniaMod)

#####################################################################
# Call:
#   lm(formula = Growth.rate ~ Detergent + Daphnia, data = daphnia)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.25917 -0.72208 -0.06135  0.71041  2.28597 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      2.87280    0.30930   9.288 1.34e-13 ***
#   DetergentBrandB  0.12521    0.35715   0.351    0.727    
# DetergentBrandC  0.06968    0.35715   0.195    0.846    
# DetergentBrandD -0.32660    0.35715  -0.914    0.364    
# DaphniaClone2    1.73725    0.30930   5.617 4.21e-07 ***
#   DaphniaClone3    1.29884    0.30930   4.199 8.19e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.071 on 66 degrees of freedom
# Multiple R-squared:  0.3533,	Adjusted R-squared:  0.3043 
# F-statistic: 7.211 on 5 and 66 DF,  p-value: 1.944e-05
####################################################################

# Summary table confirms that none of the estimated mean differences for the detergent types differ from the first
# Clones 2 and 3 differ from Clone 1
# However, adj R^2: 0.3, so over half the variation in the data is not explained by the model

detergentMean - detergentMean[1] # Look at the difference in means between detergent 1 and the others

cloneMean - cloneMean[1] # Same for mean of clone 1 and the others

daphniaANOVAMod <- aov(Growth.rate ~ Detergent + Daphnia, data = daphnia)
summary(daphniaANOVAMod)

####################################################################
# Df Sum Sq Mean Sq F value   Pr(>F)    
# Detergent    3   2.21   0.737   0.642    0.591    
# Daphnia      2  39.18  19.589  17.063 1.06e-06 ***
#   Residuals   66  75.77   1.148                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
####################################################################

# Performing a Tukey post-hoc analysis
daphniaModHSD <- TukeyHSD(daphniaANOVAMod)
daphniaModHSD

####################################################################
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = Growth.rate ~ Detergent + Daphnia, data = daphnia)
# 
# $Detergent
# diff        lwr       upr     p adj
# BrandB-BrandA  0.12521198 -0.8161307 1.0665547 0.9850797
# BrandC-BrandA  0.06968013 -0.8716625 1.0110228 0.9973423
# BrandD-BrandA -0.32660105 -1.2679437 0.6147416 0.7972087
# BrandC-BrandB -0.05553185 -0.9968745 0.8858108 0.9986474
# BrandD-BrandB -0.45181303 -1.3931557 0.4895296 0.5881893
# BrandD-BrandC -0.39628118 -1.3376239 0.5450615 0.6849619
# 
# $Daphnia
# diff        lwr       upr     p adj
# Clone2-Clone1  1.737246  0.9956362 2.4788555 0.0000013
# Clone3-Clone1  1.298845  0.5572351 2.0404544 0.0002393
# Clone3-Clone2 -0.438401 -1.1800107 0.3032086 0.3378930
####################################################################

# Tukey test gives us even the upper and lower 95% confidence intervals!
# No matter which pair of detergents compared, detergents do not differ significantly in thier effect on pop. growth  rate

par(mfrow=c(2,1), mar=c(4,4,1,1))
plot(daphniaModHSD)

par(mfrow=c(2,2))
plot(daphniaMod)

# Diagnostic plots 1 & 3 (the left hand side) should show a 'starry night' distribution
# The QQ-plot (plot 2) is good at least, no outliers

# MULTIPLE REGRESSION
timber <- read.delim("../Data/timber.txt")
summary(timber)

# Outliers
par(mfrow=c(2,2))
boxplot(timber$volume) # One outlier in 'volume' subset, probably not a measurement error or typo, as it is not too large
boxplot(timber$girth)
boxplot(timber$height)


