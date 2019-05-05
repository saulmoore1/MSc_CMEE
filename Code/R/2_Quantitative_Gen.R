#!/usr/bin/env R
rm(list=ls())
setwd("~/Documents/cmeecoursework/Week10/Code/")
require("MCMCglmm")
require("pedantics")

PhenData <- read.table("../Data/PhenData.txt", header = T)
str(PhenData) # Structure of 'PhenData'
head(PhenData)

dat <- subset(PhenData, PhenData$WingLength!="NA")
table(table(PhenData$BirdID))
# Gives us an idea about how often birds have been measured from wing length
# Repeatability describes how much variation is due to between-individual differences
# Because total phenotypic variance (Vp) = between-individual differences (Vi) + residual (within-individual) variance (Vr)
# Vp = Vi + Vr

m <- MCMCglmm(WingLength~1, random=~BirdID, data=dat, nitt=1000, burnin=0)
# We will use the package MCMCglmm to estimate the variance components (random effects) 
# 'Winglength~1' to tell R that we do not want to investigate fixed-effects ie. only calculate the intercept
# Write 'random=' to tell R to  estimate specifically random effects
# 'nitt' specifies number of iterations, or duration

# MCMCglmm estimates the solution of the mixed model using Markov chain Monte Carlo (MCMC) Bayesian methods
# Bayesian methods work better than (restricted) maximum likelihood methods (REML) and a lot better than linear least squares method
# when it comes to modelling linear mixed models (GLMMs are better for more complex random effects models)

plot(m$Sol) # Plot of fixed effects of m
plot(m$VCV)

m <- MCMCglmm(WingLength~1, random = ~BirdID, data = dat)

plot(m$Sol)
plot(m$VCV)

autocorr(m$Sol)
autocorr(m$VCV)

m1 <- MCMCglmm(WingLength~1, random = ~BirdID, data=dat, nitt = 100000, burnin = 10000, verbose = FALSE)
plot(m1$VCV)
autocorr(m1$Sol)
autocorr(m1$VCV)
summary(m1)

posterior.mode(m1$VCV[,"BirdID"])
HPDinterval(m1$VCV[,"BirdID"])
posterior.mode(m1$VCV[,"units"])
HPDinterval(m1$VCV[,"units"])

head(m1$VCV[,"BirdID"])
str(m1$VCV[,"BirdID"])
m1$VCV[,"BirdID"]
Vp1=(m1$VCV[,"BirdID"]+m1$VCV[,"units"])
posterior.mode(Vp1)
HPDinterval(Vp1)
var(dat$WingLength)

R1 <- m1$VCV[,"BirdID"]/(m1$VCV[,"BirdID"]+m1$VCV[,"units"])
posterior.mode(R1)
HPDinterval(R1)

# An Animal Model to estimate heritability - Pedigree analysis
fp <-  read.table("../Data/fp.txt", header = TRUE)
str(fp)
head(fp)

drawPedigree(fp, dots = "y")
measurePed <- fp
measurePed$Measured <- ifelse(measurePed$animal %in% dat$BirdID == "TRUE", 1, 0)
head(measurePed) # Added new column 'Measured'

# Pruning the pedigree
drawPedigree(measurePed, dots = "Y", dat = measurePed$Measured, retain = "informative")

dat$animal <- dat$BirdID

m2 <- MCMCglmm(WingLength~1, random=~animal+BirdID, data = dat, ped = fp, nitt = 100000, burnin = 10000, verbose = FALSE)

plot(m2$Sol)
plot(m2$VCV)
autocorr(m2$Sol)
autocorr(m2$VCV)
summary(m2)

VP2 <- (m2$VCV[,"BirdID"]+m2$VCV[,"animal"]+m2$VCV[,"units"])
R2 <- (m2$VCV[,"BirdID"]+m2$VCV[,"animal"])/VP2
h2 <- (m2$VCV[,"animal"])/VP2
PE <- (m2$VCV[,"BirdID"])/VP2
posterior.mode(R2)
HPDinterval(R2)

posterior.mode(h2)
HPDinterval(h2)

posterior.mode(PE)
HPDinterval(PE)
