#!/usr/bin/env R
rm(list=ls())

gala <- read.table("../Data/gala.txt", header = T)
str(gala)
plot(Species ~ log(Area), data = gala)
# The	problem	is	that	the	data	is	count	data:	there	is	increasing	variance	and	the	data	is	bounded	below	at zero.

# Let's use a GLM, accounting for this increase in variance by specifying a Poisson error distribution in the model, which sets the link fulnction to be used:
gala$lgArea <- log(gala$Area)
galaMod <- glm(Species ~ lgArea, data=gala,family=poisson(link=log))
summary(galaMod)
# Summary doesn't	include	R-squared.	This	can't	be	defined	for	a	GLM, since	the	residual	sums	of	squares	don't	make	sense	as	a	measure	of	model	fit,	but	we	can	calculate	the	proportion	of	the	null	deviance	explained,	which	does	a	similar	job
(galaMod$null.deviance - galaMod$deviance)/galaMod$null.deviance
# 0.8143775

par(mfrow=c(1,2))
plot(galaMod, which=c(1,2)) # Select which model diagnostic plots to view in the 2-by-1 graphical device window
# We can use 'predict()' to give us predictions from the model, ON THE SCALE OF THE RESPONSE.
exp(0.337737) # Interpret the slope (back-transform log(data))
# 1.40172 - rate of increase (slope), ie. 1.40% increase in species for every unit increase in area.
dev.off()
# predict for a neat sequence of log area values
pred <- expand.grid(lgArea = seq(-5, 9, by=0.1))
head(pred)
tail(pred)
pred$fit <- predict(galaMod, newdata = pred, type = "response")
head(pred)
pdf("../Results/Species-Area.pdf")
plot(Species ~ log(Area), data=gala)
lines(fit ~ lgArea, data = pred, col="red")
dev.off()

# Amphibian road kills in Portugal

# Now	it's	your	turn	to	fit	a	Poisson	GLM	that	predicts	the	number	of	road	kills	(TOT.N)	as	a	function	of	distance	to	a	nearby	natural	park	(D.PARK).	Go	all	the	way	and	interpret	the	parameter	estimates!

roadkill <- read.table("../Data/RoadKills.txt", header = T)
head(roadkill)
str(roadkill)
plot(TOT.N ~ D.PARK, data = roadkill, xlab="Distance to Park", ylab="Number of Road Kills")
roadkillMod <- glm(TOT.N ~ D.PARK, data = roadkill, family = poisson(link=log))
summary(roadkillMod)
pdf("../Results/RoadKillsMod.pdf")
par(mfrow=c(2,2))
plot(roadkillMod)
dev.off()
pred2 <- expand.grid(D.PARK = seq(0,25000,by=1))
head(pred2)
tail(pred2)
pred2$fit2 <- predict(roadkillMod, newdata = pred2 , type = "response")
head(pred2)
pdf("../Results/RoadKills.pdf")
plot(TOT.N ~ D.PARK, data = roadkill, xlab="Distance to Park", ylab="Number of Road Kills")
lines(fit2 ~ D.PARK, data = pred2, col="red")
dev.off()

# Species Richness in a grassland plot
species <- read.table("../Data/species.txt", header = T)
head(species)
str(species)
mfull <- glm(Species ~ pH*Biomass, data=species, family = poisson)
m2 <- glm(Species ~ pH + Biomass, data=species, family=poisson)
require(lmtest)
lrtest(mfull,m2)
# So	the	second	model	has	a	lower	Loglikelihood	than	the	full	model	(-259	vs	-251	for	the	full	model).	The	difference	is	statistically	significant,	which	means	the	full	model	explains	the	data	better.
AIC(mfull,m2) # Go for mfull model
step(mfull) # 'step' tries step-by-step to remove interactions and then parameters one at a time to simplify the model for you, based on AIC


# Binomial Errors - Endemicity on the Galapagos Islands
rm(list=ls())
gala <- read.delim("../Data/gala.txt")
str(gala)

# Does the proportion of endemic species vary with island area? 
gala$propEnd <- gala$Endemics / gala$Species
gala$lgArea <- log(gala$Area)
plot(propEnd ~ lgArea, data = gala, cex=log(Species/2))
# Plot sets point size (cex) to show the number of species behind each esstimated proportion. Looks like fewer endemics of larger islands, but is this significant?
# Binomial(link=logit) error distribution
# Must tell model what the no. species are, and the proportion of endemics, both as response variable (matrix of two columns - no. successes (endemics) and no. failures (non-endemics), such that the row sums give the total no. species)
resp <- with(gala, cbind(Endemics, Species-Endemics)) # Bind the two response variable columns together
galaMod <- glm(resp ~ lgArea, data=gala, family=binomial(link=logit))
# Or you could simply specify the proportion using 'weights=Species'
galaMod2 <- glm(propEnd ~ lgArea, weights=Species, data=gala, family=binomial(link=logit))

# Binomial	GLMs	haveanova(galaMod, test="Chisq")	an	analysis	of	deviance	table	and	use	a chi-sq test	on	the	change	in	deviance	rather	than	an	F test.	It	tells	us	how	much	deviance	is	explained	by	lgArea,	and	how	much	residual	error	(or	deviance)	is	left.

anova(galaMod, test="Chisq")
1-pchisq(44.053,1) # Another test for deviance
summary(galaMod)
(galaMod$null.deviance - galaMod$deviance)/galaMod$null.deviance
# Deviance = 0.29
par(mfrow=c(2,2))
plot(galaMod, which=c(1:4))# Diagnostics are not wonderful
dev.off()

# Like	for	the	poisson	data	in	the	last	practical,	we	can	work	out	the	predicted	values	on	the	proportion	scale	the	easy	way:
# Predict for a neat seq of log area values...
pred <- expand.grid(lgArea = seq(-5, 9, by=0.1))
pred$fit <- predict(galaMod, newdata=pred, type="response")

# Plot log data and model lines
plot(propEnd ~ lgArea, data=gala)
lines(fit ~ lgArea, data=pred, col="red")

# To get confidence limits, we need to use the inverse link function to convert proportions into logit values and vice-versa
pred <- expand.grid(lgArea = seq(-5,9,by=0.1))
predMod <- predict(galaMod, newdata = pred, se.fit = TRUE)

# Get the fit and CI upper/lower:
pred$fit <- predMod$fit
pred$se.fit <- predMod$se.fit
pred$confint <- predMod$se.fit*qt(0.975, df=galaMod$df.residual)

# Plot the logit transformed proportion data and the model lines:
plot(qlogis(propEnd) ~ log(Area), data=gala)
lines(fit ~ lgArea, data=pred, col="red")
lines(fit + confint ~ lgArea, data = pred, col="red", lty=2)
lines(fit - confint ~ lgArea, data = pred, col="red", lty=2)

# We	can	now	back-transform	them	on	to	the	data.	This	means	we	need	to	know	the	inverse	link	function	for	our	model, which is:
plot(propEnd ~ lgArea, data=gala, cex=log(Species/2))
lines(plogis(fit) ~ lgArea, data=pred, col="red")
lines(plogis(fit + confint) ~ lgArea, data=pred, col="red", lty=2)
lines(plogis(fit - confint) ~ lgArea, data=pred, col="red", lty=2)


# Predicting threat in Galliformes
galliformes <- read.table("../Data/galliformesData.txt", header = T)
str(galliformes) # view the structure of the dataframe
galliformes <- na.omit(galliformes) # Ignore NA values

# First convert the threat status column from IUCN categories to a simple threatened vs. not-threatened numeric variable (binary):
galliformes$ThreatBinary <- ifelse(galliformes$Status04 %in% c("1_(LC)","2_(NT)"), 0, 1) # Using a pipe function ('%in%')

# The	life	history	variables	we'll	use	are	body	mass,	geographic	range,	clutch	size	and	elevational	range.	If	we	check	those,	they	all	show	strong	right	skew - the	points	are	clumped	over	to	the	left:
pairs(ThreatBinary ~ Range + Mass + Clutch + ElevRange, data=galliformes)

# Log-transoformation necessary
galliformes$lgMass <- log(galliformes$Mass) 	
galliformes$lgRange <- log(galliformes$Range) 	
galliformes$lgClutch <- log(galliformes$Clutch) 	
galliformes$lgElevRange <- log(galliformes$ElevRange)
pairs(ThreatBinary ~ lgRange + lgMass + lgClutch + lgElevRange, data=galliformes) # THIS IS CORRECT, says Julia. Worksheet is wrong.

par(mfrow=c(2,2))
plot(ThreatBinary ~ lgRange, data=galliformes)
plot(ThreatBinary ~ lgMass, data=galliformes)
plot(ThreatBinary ~ lgClutch, data=galliformes)
plot(ThreatBinary ~ lgElevRange, data=galliformes)
dev.off()

galliMod <- glm(ThreatBinary ~ (lgRange + lgMass + lgClutch + lgElevRange)^2, data=galliformes, family=binomial(link=logit))
step(galliMod)
