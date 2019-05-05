#!/usr/bin/env R
rm(list = ls())

#####################################################
# Day 4 Population Dynamics Practical
#####################################################

setwd("Week10/Code")

dat <- read.table("../Data/BTdat.txt", header = T)
names(dat)
str(dat)

# Is early	laying	linked	with	a	higher	reproductive	success?
plot(dat$clutch ~ dat$laydate, pch=19, cex=0.7)

# Looks like there is a trend towards smaller clutch sizes with later laying dates

# Lets clean the data
dat <- subset(dat, dat$clutch < 30) # Subset didn't seem to remove any observations? Sometimes it does this, but it has still removed the multiple bird eggs per nest outlier
dat <- subset(dat, dat$laydate!="NA") # 13 observations removed

plot(jitter(dat$clutch) ~ dat$laydate, pch=19, cex=0.7) # Jitter adds a teeny bit of noise to tease apart multiple data points of the same value, just for plotting, so that it looks nicer!
# Still	some	very	large	clutches,	but	it's	all	within	reason.	Is there variation	between	years?

boxplot(dat$laydate ~ dat$year)
# Clearly laying date varies with year - makes sense. It might be colder/less food/etc in different years

# We	cannot	compare	raw	laying	dates	across	year	because	obviously,	late	and	
# early	are	relative	to	spring	phenology	each	year.	We	can	fix	that	by	standardizing	laying	
# date	each	year.	This	is	different	from	overall	standardizing	in	that	this	time,	we	do	it	
# seperately	by	year.	

dat <- subset(dat, dat$laydate!="NA")
dat <- subset(dat, dat$clutch!="NA")
dat <- subset(dat, dat$year!="NA")
dat <- subset(dat, dat$female!="NA")
library(plyr)
dat.st <- ddply(dat, c("year"), transform, ld.std = scale(laydate)) # Standardise laying date across years, so they are all at the same scale
head(dat.st)

par(mfrow = c(2,1))
boxplot(dat.st$ld.std ~ dat.st$year, xlab = "Year", ylab = "Annual z-scores of laying date")
plot(jitter(dat.st$clutch) ~ dat.st$ld.std, pch=19, cex=0.7, xlab = "Annual z-scores of laying date", ylab = "Clutch size (number eggs)")

dev.off()
par(mfrow = c(1,2))
plot((dat.st$laydate) ~ dat.st$ld.std, pch=19, cex=0.7, xlab = "Annual Z-scores of laying date", ylab = "Laying date (April days)")
plot((dat.st$laydate) ~ scale(dat.st$laydate), pch=19, cex=0.7, xlab = "Z-scores of laying date", ylab = "Laying date (April days)")

# Next, lets have a look at clutch size
# z-standardise to compare everything to the mean, so we can ignore units
# How good are you compared to everyone else, and not also compared to the year
# Throughout years, but without actually considering 'year', if you like :)

boxplot(dat.st$clutch ~ dat.st$year)

dat.st <- ddply(dat.st, c("year"), transform, clutch.std = scale(clutch))
head(dat.st)
boxplot(dat.st$clutch.std ~ dat.st$year, ylab = "Annual z-scores clutch size", xlab = "Year")
plot(jitter(dat.st$clutch.std) ~ dat.st$ld.std, pch=19, cex=0.7, xlab = "Annual z-scores of laying date", ylab = "Annual z-scores clutch size")
# The	seasonal	decline	of	relative	clutchsize	with	relative	laying	date	becomes	ever more	pronounced

# Now we can calculate the selection gradient for each year
# Strength of selection can be inferred from the fitness profile (clutch size as proxy of fitness)

# The	strength	of	the	selection	acting	on	the	trait	can	be	expressed	as	the	linear	regression	of	
# fitness	on	the	trait,	thus	the	slope	of	a	linear	regression	of	clutchsize	(our	fitness	proxy)	on	
# the	laying	date	(the	trait).	This	selection	gradient	can,	for	our	sake,	also	be	interpreted	as	
# the	strength	of	selection	-	if	it	is	positive,	and	we	have	heritability	for	the	trait,	there	is	
# scope	for	evolution	to	happen.	We	calculate	the	seleciton	gradient	for	each	year	separately.

# Store parameter estimates
est <- rep(0,12)
err <- rep(0,12)
selec_grad <- cbind(est, err) # Bind the columns together
selec_grad <- as.data.frame(selec_grad) # as a dataframe
head(selec_grad)

for (i in 2002:2013){
  m <- lm(dat.st$clutch.std[dat.st$year==i] ~ dat.st$ld.std[dat.st$year==i])
  j <- i-2001
  selec_grad$est[j] <- m$coefficients[2]
  selec_grad$err[j] <- summary(m)$cov.unscaled[2,2]
}

selec_grad

selec_grad$Lower <- selec_grad$est - 1.96*(selec_grad$err)
selec_grad$Upper <- selec_grad$est + 1.96*(selec_grad$err)

year <- c(2002:2013)
plot(selec_grad$est ~ year, pch = 19, ylim = c(-0.6, -0.3))
segments(year, selec_grad$est - 1.96*(selec_grad$err), year, selec_grad$est + 1.96*(selec_grad$err))

# There	is	significant,	and	strong,	selection	for	earlier	laying,	because	all	of	our	calcluated	
# gradients	are	negative.	In	some	years,	it	is	stronger,	some	years	less	so.	So	now	if	we	could	
# show	that	laying	date	is	heritable,	we	could	conclude	that	it	is	possible	for	microevolution	
# to	occur...

require(MCMCglmm)
mod1 <-  MCMCglmm(ld.std ~ 1, random = ~female, data = dat.st) # Why 1? Fixed vs random effects?
# G-structure ~female post.mean = mean of the overall individual variance for each female's laying date (mean  Vi)
summary(mod1)
plot(mod1$VCV) # Pretty bad model fit, even the mean varies over time

Repeatability <- mod1$VCV[,"female"]/(mod1$VCV[,"female"] + mod1$VCV[, "units"])
# The	last	line	calculates	the	ratio	of	variance	explained	by	between-individual	differences	
# over	total	phenotypic	variance.	Then	we	can	display	the	mode	and	95	credibility	interval:	
osterior.mode(Repeatability)
HPDinterval(Repeatability)

# Next, calculate the heritability of standardised laying date. To do this we need the pedigree
p <- read.table("../Data/BTped.txt", header = T) # Read in the pedigree
library(MasterBayes)
po <- orderPed(p) # Order the pedigree

mod2 <- MCMCglmm(ld.std ~ 1, random = ~female + animal, pedigree = po, data = dat.st, burnin = 50000, nitt = 100000)
summary(mod2)
save(mod2, file = "../Results/mod2.Rdata")
Heritability <- mod2$VCV[,"animal"]/(mod2$VCV[,"female"] + mod2$VCV[, "animal"] + mod2$VCV[,"units"])
# Heritability = Vag / (Vi + Vag + Vr)
posterior.mode(Heritability)
HPDinterval(Heritability)

dev.off()
require(pedantics)
drawPedigree(po, dots = "y")
measurePed <- po
measurePed$Measured <- ifelse(measurePed$animal %in% dat$animal == "TRUE", 1, 0)
head(measurePed)
drawPedigree(measurePed, dots = "Y", dat = measurePed$Measured, retain = "informative")
pedStatSummary(pedigreeStats(po, graphicalReport = "n"))
