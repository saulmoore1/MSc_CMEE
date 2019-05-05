#!/usr/bin/env R
rm(list=ls())

d <- read.table("../Data/SparrowSize.txt", header=TRUE)
str(d)
names(d)
head(d)

# Explore	variance	in	tarsus	and	wing	among	and	between	individuals	and	cohorts	in	house	sparrows

# The	prediction	is	that	both	traits	are	repeatable,	that	means,	in	both	traits,	variation	between	individuals	should	be	larger	than	variation	within	individuals.	Therefore,	we	expect	that	BirdID	explains	a	lot	of	variance.	We	expect	that	cohort	explains	some	variance,	too,	because	individuals	from	the	same	cohort	are	exposed	to	the	same	conditions	during	growing	up,	which	is	when	adult	size	is	determined.	We	know	that	we	have	some	NAs	in	the	data,	and	therefore	we	should	remove	these.	We	also	know	that	sex	is	important	in	size	variables,	so	we	will	want	to	correct	for	that.

# To do this, we will use a range of linear models, including bivariate models
dat <- d[which(d$Tarsus!="NA" & d$Wing!="NA" & d$Sex!="NA" & d$Cohort!="NA"),]
d1 <- data.frame(d$Tarsus, d$Wing, d$Sex)
pairs(d1, pch=19, cex=0.7)

cor.test(dat$Wing,dat$Tarsus)
var(dat$Tarsus)
var(dat$Wing)
mean(dat$Tarsus)
mean(dat$Wing)

# First, build the maximal model, but first with covariance restriction (~idh)
# We want Cohort and BirdID as random effects, and we know we need Sex as a fixed effect
library(MCMCglmm)
pdf("../Results/MCMCglmm.pdf")
par(mfrow=c(2,10))
MaxNoCov <- MCMCglmm(cbind(Tarsus,Wing) ~ trait-1 + trait:Sex, random=~idh(trait):BirdID + idh(trait):Cohort, rcov= ~ idh(trait):units, family=c("gaussian","gaussian"), data=dat, verbose=FALSE)
plot(MaxNoCov)
return()
dev.off()
# From the plot, there seems to be a large effect of sex on tarsus length, as well as wing length (density plot does not overlap zero). BirdID plots for both traits also look sensible, although a little thin. However, the plot for 'cohort' is horrendous; v.bad for tarsus, bad for wing. Residuals (units) look ok. So its probably a good call to run this model for a bit longer - is cohort worthwhile keeping in the model? 

MaxNoCov <- MCMCglmm(cbind(Tarsus,Wing)~trait-1+trait:Sex, random=~idh(trait):BirdID+idh(trait):Cohort, rcov=~idh(trait):units, family=c("gaussian","gaussian"), data=dat, nitt=100000, verbose=FALSE)
plot(MaxNoCov)
summary(MaxNoCov)
# The	variance	explained	by	cohort	is	nearly	zero	in	tarsus,	but	~24%	in	wing length. Some variance in wing length is explained by 'cohort', so 'cohort' should remain in the model. 




