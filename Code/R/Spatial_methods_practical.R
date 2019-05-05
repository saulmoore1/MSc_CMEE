#!/usr/bin/env R
# setwd("/home/saul/Documents/cmeecoursework/Week5/Code/")
rm(list = ls())

# Import/load required packages
require("rgdal")
library(raster)

# SPATIAL MODELLING IN R - David Orme

# First, load each raster into R and store it as an object
rich <- raster('../Data/avian_richness.tif')
aet <- raster('../Data/mean_aet.tif')
temp <- raster('../Data/mean_temp.tif')
elev <- raster('../Data/elev.tif')

# Histograms, four-in-one plot
pdf("../Results/Raster_hist.pdf")
par(mfrow=c(2,2))
hist(rich, main='Avian species richness')
hist(aet, main='Mean AET')
hist(temp, main='Mean annual temperature')
hist(elev, main='Elevation')
dev.off()

# Plot as raster plots, four-in-one
pdf("../Results/Raster_plot.pdf")
par(mfrow=c(2,2), mar=c(3,3,1,1)) # 'mar' allows you to specify plot margins
plot(rich, main='Avian species richness')
plot(aet, main='Mean AET')
plot(temp, main='Mean annual temperature')
plot(elev, main='Elevation')
dev.off()

data <- stack(rich, aet, elev, temp) # 'stack' allows us to superimpose the four rasters into a single object 
# (NB: this only works when all rasters have equal projection, extent and resolution)
print(data)
data_df <- as(data, 'SpatialPixelsDataFrame') # 'as' allows us to convert an object to a different format
summary(data_df)

# Exploring the data - plot a map of richness data
# 'spplot' allows spatial plots of this new dataframe
pdf("../Results/Avian_Richness_spplot.pdf")
spplot(data_df, zcol = 'avian_richness', col.regions = heat.colors(20), scales = list(draw = TRUE), 
       main = 
'Spatial Plot of Afrotropical 
Bird Species Richness')
dev.off()

# Now lets plot three figures in a single panel, richness as a function of each environmental variable:
pdf("../Results/Plot_vars.pdf")
par(mfrow = c(1,3))
plot(avian_richness ~ mean_aet, data = data_df, main = "Richness ~ Mean AET")
plot(avian_richness ~ mean_temp, data = data_df, main = "Richness ~ Mean Temperature")
plot(avian_richness ~ elev, data = data_df, main = "Richness ~ Elevation")
dev.off()

# Correlations and Spatial Data - Correlation coefficient correction using the Clifford Test
source("clifford.test.R")
cor.test(~avian_richness + mean_aet, data = data_df) # Correlation coefficient = 0.529725
clifford.test(as.matrix(rich), as.matrix(aet))

#############################################################################################
# Correlation test accounting for autocorrelation (Clifford et al., 1989)
# 
# Data - Matrix A: as.matrix(rich) 
# - Matrix B: as.matrix(aet) 
# 
# rpearson    n         ncc    var.xy      var.r      ess        w       t           p
# 0.529725 2484 7.14048e+14 115724216 0.07316509 14.66772 1.958387 2.74931 0.008446161
#############################################################################################

# 'ess' new effective sample size
# 't' t-value, 'p' p-value

# Measuring Spatial Autocorrelation - Moran's I statistic

# Load the spatial dependence analysis package
library(spdep)
neighbours <- dnearneigh(data_df, d1 = 0, d2 = 150)
neighbours.lw <- nb2listw(neighbours, zero.policy = TRUE)
rich.moran <- moran.test(data_df$avian_richness, neighbours.lw, zero.policy = TRUE)
rich.moran

#############################################################################################
# Moran I test under randomisation
# 
# data:  data_df$avian_richness  
# weights: neighbours.lw  
# 
# Moran I statistic standard deviate = 88.419, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
#        0.9264783441     -0.0004032258      0.0001098905 
#############################################################################################

# Local Indicators of Spatial Autocorrelation (LISA)
rich.lisa <- localmoran(data_df$avian_richness, neighbours.lw, zero.policy = TRUE)
data_df$rich_lisa <- rich.lisa[, 1]
pdf("../Results/LISA_plot.pdf")
spplot(data_df, zcol = 'rich_lisa', col.regions = heat.colors(20), scales = list(draw = TRUE))
dev.off()

# As we might predict, avian richness shows strong positive spatial autocorrelation,
# which seems to be particularly strong in the mountains around Lake Victoria

# Spatial AutoRegressive (SAR) Models

# Statistical model that predicts the value of a response variable in a cell using
# the predictor variables and values of the response variable in neighbouring cells
# This is why they are called autoregressive models: they fit the response variable 
# partly as a response to itself

simple_model <- lm(avian_richness ~ mean_aet + elev + mean_temp, data = data_df)
summary(simple_model)

############################################################################################
# Call:
#   lm(formula = avian_richness ~ mean_aet + elev + mean_temp, data = data_df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -354.09  -53.25   -0.93   52.64  325.58 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 189.452772  21.328794   8.882  < 2e-16 ***
#   mean_aet      0.176412   0.004724  37.342  < 2e-16 ***
#   elev          0.076027   0.005490  13.849  < 2e-16 ***
#   mean_temp    -4.178441   0.722002  -5.787 8.05e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 82.4 on 2480 degrees of freedom
# Multiple R-squared:  0.4554,	Adjusted R-squared:  0.4548 
# F-statistic: 691.4 on 3 and 2480 DF,  p-value: < 2.2e-16
############################################################################################

sar_model <- errorsarlm(avian_richness ~ mean_aet + elev + mean_temp, data = data_df, listw = neighbours.lw, zero.policy = TRUE)
summary(sar_model)

############################################################################################
# Call:errorsarlm(formula = avian_richness ~ mean_aet + elev + mean_temp, 
#                 data = data_df, listw = neighbours.lw, zero.policy = TRUE)
# 
# Residuals:
#   Min          1Q      Median          3Q         Max 
# -218.359135   -8.991092   -0.054733    9.643101  109.813234 
# 
# Type: error 
# Regions with no neighbours included:
#   1311 1817 2204 
# Coefficients: (asymptotic standard errors) 
# Estimate  Std. Error z value  Pr(>|z|)
# (Intercept) -1.2964e+02  3.3814e+01 -3.8340 0.0001261
# mean_aet     5.6957e-02  6.9750e-03  8.1659 2.220e-16
# elev         4.8364e-02  6.7019e-03  7.2165 5.336e-13
# mean_temp    3.1011e+00  1.2642e+00  2.4530 0.0141688
# 
# Lambda: 0.99573, LR test value: 6522.3, p-value: < 2.22e-16
# Asymptotic standard error: 0.0011623
# z-value: 856.7, p-value: < 2.22e-16
# Wald statistic: 733940, p-value: < 2.22e-16
# 
# Log likelihood: -11219.94 for error model
# ML residual variance (sigma squared): 362.3, (sigma: 19.034)
# Number of observations: 2484 
# Number of parameters estimated: 6 
# AIC: 22452, (AIC for lm: 28972)
############################################################################################

data_df$simple_fit <- predict(simple_model)
data_df$sar_fit <- predict(sar_model)

pdf("../Results/Model_pred.pdf")
spplot(data_df, c("avian_richness", "simple_fit", "sar_fit"), col.regions = heat.colors(20), scales = list(draw = TRUE), 
       main = "Afrotropical Bird Richness compared with Simple Fit 
and Spatial Auto-Regression (SAR) models")
dev.off()

data_df$simple_resid <- residuals(simple_model)
data_df$sar_resid <- residuals(sar_model)
colPal <- colorRampPalette(c("cornflowerblue", "grey", "firebrick"))
colours <- colPal(21)
breaks <- seq(-600, 600, length = 21)

pdf("../Results/Model_resid.pdf")
spplot(data_df, c("simple_resid", "sar_resid"), col.regions = colours, at = breaks, scales = list(draw = TRUE))
dev.off()

# Correlograms
library(ncf)
data_xy <- data.frame(coordinates(data_df))
rich.correlog <- correlog(data_xy$x, data_xy$y, data_df$avian_richness, increment = 100, resamp = 0)
pdf("../Results/Correlogram.pdf")
plot(rich.correlog)
dev.off()

pdf("../Results/Correlogram_control.pdf")
par(mfrow = c(1,2))
rich.correlog <- data.frame(rich.correlog[1:3])
plot(n ~ mean.of.class, data = rich.correlog, type = "o", subset = mean.of.class < 5000)
abline(h = 0)
simple.correlog <- correlog(data_xy$x, data_xy$y, data_df$simple_resid, increment = 100, resamp = 0)
sar.correlog <- correlog(data_xy$x, data_xy$y, data_df$sar_resid, increment = 100, resamp = 0)
simple.correlog <- data.frame(simple.correlog[1:3])
sar.correlog <- data.frame(sar.correlog[1:3])
plot(correlation ~ mean.of.class, data = simple.correlog, type = "o", subset = mean.of.class < 5000)
lines(correlation ~ mean.of.class, data = sar.correlog, type = "o", subset = mean.of.class < 5000, col = "red")
abline(h = 0)
dev.off()

