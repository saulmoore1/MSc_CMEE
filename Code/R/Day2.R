#!/usr/bin/env R

rm(list=ls())

d <- read.table("../Data/DataForMMs.txt", header = T)
str(d)

max(d$Individual) # 100 individuals
d$Individual <- as.factor(d$Individual)
names(d)

pdf("../Results/Unicorns_histogram.pdf")
par(mfrow=c(2,3))
hist(d$LitterSize)
hist(d$Size)
hist(d$Hornlength)
hist(d$Bodymass)
hist(d$Glizz)
hist(d$SexualActivity)
dev.off()

# Hypothesis 1 - There	is	sexual	dimorphism	(trimorphism!)	in	unicorn	body	mass,	size,	and	horn length

pdf("../Results/Unicorns_boxplot.pdf")
par(mfrow=c(1,3))
boxplot(d$Bodymass~d$Sex)
boxplot(d$Size~d$Sex)
boxplot(d$Hornlength~d$Sex)
dev.off()

# Does not look like there is sexual dimorphism from the boxplots. Spread of the data looks similar. But we will need a proper analysis. 

# There is pseudo-replication in the dataset - multiple observations of the same individual, recorded as independent observations. So a simple t-test or regilar linear model wont cut it. 
# Instead we run a linear mixed model, with 'individual' as a random effect

require(lme4)
h1m1 <- lmer(Bodymass~Sex+(1 | Individual), data=d) 
# REsidual Maximum Likelihood (REML) fit, Indiviual = random effect
summary(h1m1)
# Summary confirms that there is no effect of size on body mass. 
# NB: Lmer does not provide p-values (there is controversy over whether p-values should be stated or not for REML estimated linear mixed models)

h1m2 <- lmer(Bodymass~Sex+(1 | Individual)+(1 | Family), data=d)
summary(h1m2)
# It seems like 'family' explains nearly as much variance as 'individual',
# However, the standard deviation is quite large. So let's use a likelihood ratio test to see if adding this extra parameter to the model improves the model

require(lmtest)
lrtest(h1m1, h1m2)
# Well adding that extra parameter has improved the model fit statistically significantly. The loglikelihood of the reduced model with family as a random effect is -2630, 17 units higher. The chisq test supports this. So when investigating body mass, be sure to add in family!
# Interestingly, the model diagnostics also show that there is a lot of variation between individuals. However, SD is huge. So its uncertain. 

 # Hypotheses: unicorn body	mass is	sexually	'trimorphic'.
# Method: linear	mixed	model	with	body	mass	as	response	variable, and	sex	as	a three-level	fixed	factor	(with	female	as	response).	
# Each	unicorn	was	measured	20	times - to	account	for	this	pseudo-replication,	we	added	individual	ID	as	a	random	effect	on	the	intercept.	
# Family	group	explained	a	large	amount	of	variation	and	was	also	added	as	a	random	effect	on	the	intercept	to	account	for	the	nested	structure	of	the	data.	
# Results:	No	sexual	'trimorphism'	in	body	mass.	(present	a	table	with	the	results	from	the	mixed	model).	

# Now	run	the	same	analyses	for	size	and	horn	length!	
rm(list=ls())
graphics.off()
 
d <- read.table("../Data/DataForMMs.txt", header=T)
str(d)
max(d$Individual)
d$Individual<- as.factor(d$Individual)
names(d)
hist(d$Hornlength)
boxplot(d$Hornlength~d$Sex)
h1m3 <- lm(Hornlength~Sex, data=d)
h1m4 <- lmer(Hornlength~Sex+(1 | Individual), data=d)
h1m5 <- lmer(Hornlength~Sex+(1 | Individual)+(1 | Family), data=d)
summary(h1m4)
summary(h1m5)
lrtest(h1m4,h1m5) # 'lrtest' for comparing nested (generalised) linear models

hist(d$Size)
boxplot(d$Size~d$Sex)
h1m6 <- lm(Size~Sex, data=d)
h1m7 <- lmer(Size~Sex+(1 | Individual), data=d)
h1m8 <- lmer(Size~Sex+(1 | Individual)+(1 | Family), data=d)
summary(h1m7)
summary(h1m8)
lrtest(h1m7,h1m8)
AIC(h1m3,h1m4,h1m5,h1m6,h1m7,h1m8) # Why the regular lineaer models a better fit??

# Hypothesis 2 - Heavy	unicorns,	corrected	for	sex	and	glizz,	have	longer	horns	
# It seems that we do not need to correct for sex, but what about glizz?
# Is there any relationship between body mass and hornlength?
plot(Bodymass ~ Hornlength, data=d, pch=19, cex=0.5)
# There	seems	to	be	a	higher	variance	in	body	mass	in	very	short	horned	and	very	long	horned	unicorns,	compared	with	averagely	horned	unicorns.
h2m1 <- lmer(Bodymass ~ Hornlength+Glizz+(1 | Individual)+(1 | Family), data=d)
summary(h2m1)

h2m2 <- lmer(Bodymass ~ Hornlength+(1 | Individual)+(1 | Family), data=d)
summary(h2m2)
# there	is	a	somewhat	postive	association	between	hornlength	and	bodymass,	but	the	plot	is	misleading,	tbh.	Go	ahead	and	check	the	residuals	out:

plot(h2m2)
# In	the	real	world,	we'd	investiage	further	to	find	out	why	variance	is	so	low	for	average-horned	unicorns.
plot(Bodymass ~ Hornlength, data=d, pch=19, cex=0.5)
abline(0.15619, 0.05447)

# Hypothesis 3