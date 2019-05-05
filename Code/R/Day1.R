#!/usr/bin/env R
rm(list=ls())

d <- read.table("../Data/SparrowSize.txt", header = TRUE)
str(d)
names(d)
head(d)

# Centrality and Spread
hist(d$Tarsus, main="", xlab="Sparrow tarsus length (mm)", col="grey")
mean(d$Tarsus, na.rm = TRUE) # 18.52335 # Mean = a measure of centrality
var(d$Tarsus, na.rm = TRUE) # 0.7404059 # Variance = a measure of spread
sd(d$Tarsus, na.rm = TRUE) # 0.8604684 # Standard deviation = a measure of spread

# Converting histogram of frequency of observation into a probability of observation (density) - More professional
hist(d$Tarsus, main="", xlab="Sparrow tarsus length (mm)", col="grey", prob=TRUE) # 'prob=TRUE' tells R to plot density not frequency on the histogram
lines(density(d$Tarsus, na.rm = TRUE), col="red", lwd=2) # Density plot
abline(v=mean(d$Tarsus, na.rm = TRUE)-sd(d$Tarsus, na.rm = TRUE), col="blue", lwd=2, lty=5) # 68.2% CI (+/- sd from the mean)
abline(v=mean(d$Tarsus, na.rm = TRUE)+sd(d$Tarsus, na.rm = TRUE), col="blue", lwd=2, lty=5)
abline(v=mean(d$Tarsus, na.rm = TRUE)-2*sd(d$Tarsus, na.rm = TRUE), col="green", lwd=2, lty=5) # 95% CI (+/- 2*sd from the mean)
abline(v=mean(d$Tarsus, na.rm = TRUE)+2*sd(d$Tarsus, na.rm = TRUE), col="green", lwd=2, lty=5)

t.test(d$Tarsus~d$Sex)

par(mfrow=c(2,1))
hist(d$Tarsus[d$Sex==1], main="", xlab="Male sparrow tarsus length (mm)", col="grey", prob=TRUE)
lines(density(d$Tarsus[d$Sex==1], na.rm = TRUE), lwd=2) # Subset data to look at just males (Sex==1)
abline(v=mean(d$Tarsus[d$Sex==1], na.rm = TRUE), col="red", lwd=2)
abline(v=mean(d$Tarsus[d$Sex==1], na.rm = TRUE)-sd(d$Tarsus[d$Sex==1], na.rm = TRUE), col = "blue", lwd=2, lty=5)
abline(v=mean(d$Tarsus[d$Sex==1], na.rm = TRUE)+sd(d$Tarsus[d$Sex==1], na.rm = TRUE), col = "blue", lwd=2, lty=5)

hist(d$Tarsus[d$Sex==0], main="", xlab="Female sparrow tarsus length (mm)", col="grey", prob=TRUE)
lines(density(d$Tarsus[d$Sex==0], na.rm = TRUE), lwd=2) # Subset data to look at just females (Sex==0)
abline(v=mean(d$Tarsus[d$Sex==0], na.rm = TRUE), col="red", lwd=2)
abline(v=mean(d$Tarsus[d$Sex==0], na.rm = TRUE)-sd(d$Tarsus[d$Sex==0], na.rm = TRUE), col = "blue", lwd=2, lty=5) # 68.2% CI lower bound
abline(v=mean(d$Tarsus[d$Sex==0], na.rm = TRUE)+sd(d$Tarsus[d$Sex==0], na.rm = TRUE), col = "blue", lwd=2, lty=5) # 68.2% CI upper bound

dev.off() # Close plotting window

# Variance
var(d$Tarsus, na.rm = TRUE)
sd(d$Tarsus, na.rm = TRUE)
sd(d$Tarsus, na.rm = TRUE)^2 # sd^2 is the same as the variance
sqrt(var(d$Tarsus, na.rm = TRUE)) # sqrt(var) is the same and the sd

# Variance are ADDITIVE. Therefore, they may be partitioned to see the respective variances: var(tarsus + wing) = var(tarsus) + var(wing)

d1 <- subset(d, d$Tarsus!="NA")
d1 <- subset(d1, d1$Wing!="NA")
sumz <- var(d1$Tarsus) + var(d1$Wing)
test <- var(d1$Tarsus + d1$Wing)
sumz # 6.576499
test # 8.172773

plot(jitter(d1$Wing), d1$Tarsus, pch=19, cex=0.4) # Seems to be covariance between wing length and tarsus length (Biologically, this is to be expected, since a larger bird should increase in size proportionally..)

# If	two	variables	are	collinear	(that	means	in	stats	speak	that	they	are	not	independent, they covary), then	we	can't	use	the	additive	variance	rule

# If you sum	up two	variables, then	the	variance	of	that	summed-up	variable is the sum	of the two variances and twice their covariance
# var(tarsus + wing) = var(tarsus) + var(wing) + 2COV(tarsus + wing)

cov(d1$Tarsus, d1$Wing)
sumz <- var(d1$Tarsus) + var(d1$Wing) + 2*cov(d1$Tarsus, d1$Wing)
test <- var(d1$Tarsus + d1$Wing)
sumz
test # Now they are the same!


# When	you	multiply	a	variable	with	a	constant	its	variance	equals	the	variance	multiplied	with	the	same	constant,	but	squared:
# (10^2)*var(tarsus) = var(tarsus*10)
var(d1$Tarsus*10) # 74.03658
var(d1$Tarsus)*10^2 # 74.03658


# Linear Models
uni <- read.table("../Data/RUnicorns.txt", header=T)
str(uni)
head(uni)
mean(uni$Bodymass) # 10.39162
sd(uni$Bodymass) # 2.786788
var(uni$Bodymass) # 7.766185
hist(uni$Bodymass)

mean(uni$Hornlength) # 5.709
sd(uni$Hornlength) # 1.229192
var(uni$Hornlength) # 1.510912
hist(uni$Hornlength)

plot(uni$Bodymass~uni$Hornlength, pch=19, xlab="Unicorn horn length", ylab="Unicorn body mass", col="blue")
mod <- lm(uni$Bodymass~uni$Hornlength)
abline(mod, col="red")
res <- signif(residuals(mod), 5) # 'signif' returns the result to 5 sig. figs
pre <- predict(mod)
segments(uni$Hornlength, uni$Bodymass, uni$Hornlength, pre, col="black") # Draw line segments between pairs of points

hist(uni$Bodymass)
hist(uni$Hornlength)
hist(uni$Height)
# No zero-inflation, ,what about colinearity?
cor.test(uni$Hornlength, uni$Height)
# Horn length and height seem to be independent
boxplot(uni$Bodymass~uni$Gender)
# Something odd from the boxplots - females are heavier, but also have more variance

par(mfrow=c(2,1))
boxplot(uni$Bodymass~uni$Pregnant)
plot(uni$Hornlength[uni$Pregnant==0], uni$Bodymass[uni$Pregnant==0], pch=19, xlab="Horn length", ylab="Body mass", xlim=c(2,10), ylim=c(6,19))
points(uni$Hornlength[uni$Pregnant==1], uni$Bodymass[uni$Pregnant==1], pch=19, col="red")

dev.off()

boxplot(uni$Bodymass~uni$Pregnant)
plot(uni$Hornlength[uni$Gender=="Female"], uni$Bodymass[uni$Gender=="Female"], pch=19, xlab="Horn length", ylab="Body mass", xlim=c(2,10), ylim=c(6,19))
points(uni$Hornlength[uni$Gender=="Male"], uni$Bodymass[uni$Gender=="Male"], pch=19, col="red")
points(uni$Hornlength[uni$Gender=="Undecided"], uni$Bodymass[uni$Gender=="Undecided"], pch=19, col="green")

boxplot(uni$Bodymass~uni$Glizz)
plot(uni$Hornlength[uni$Glizz==0], uni$Bodymass[uni$Glizz==0], pch=19, xlab="Horn length", ylab = "Body mass", xlim = c(2,10), ylim = c(6,19))
points(uni$Hornlength[uni$Glizz==1], uni$Bodymass[uni$Glizz==1], pch=19, col="red")

FullModel <- lm(uni$Bodymass~uni$Hornlength + uni$Gender + uni$Pregnant + uni$Glizz)
summary(FullModel)

# Maybe exclude the pregnant females, as only females can get pregnant and there are only two pregnant females..try excluding them to get better model fit

u1 <- subset(uni, uni$Pregnant==0)
FullModel <- lm(u1$Bodymass~u1$Hornlength + u1$Gender + u1$Glizz)
summary(FullModel)
# So we do not need Gender to explain differences in Body mass...

ReducedModel <- lm(u1$Bodymass~u1$Hornlength + u1$Glizz)
summary(ReducedModel)

plot(u1$Hornlength[u1$Glizz==0], u1$Bodymass[u1$Glizz==0], pch=19, xlab="Horn length", ylab="Body mass", xlim=c(2,10), ylim=c(6,19))
points(u1$Hornlength[u1$Glizz==1],u1$Bodymass[u1$Glizz==1], pch=19, col="red")
abline(ReducedModel)
# Looking	at	the	warning	message:	The	abline	is	only	plotted	for	the	first	two	of	three	regression	coefficients.	That	means,	it's	plotted	only	for	horn	length,	but	that	estimate	already	takes	Glizz	into	account!

ModForPlot <- lm(u1$Bodymass~u1$Hornlength) # Doesn't have Glizz in the model, because it's already taken into account
summary(ModForPlot)
plot(u1$Hornlength[u1$Glizz==0], u1$Bodymass[u1$Glizz==0], pch=19, xlab="Horn length", ylab="Body mass", xlim=c(2,10), ylim=c(6,19))
points(u1$Hornlength[u1$Glizz==1], u1$Bodymass[u1$Glizz==1], pch=19, col="red")
abline(ModForPlot)
# That	looks	indeed	more	true,	but	it	does	not	reflect	the	biological	effects
# Parameter estimate for horn length is 0.4 greater in the ModForPlot compared to the ReducedModel
# R squared drops from 61% data explained by both horn length and glizz, to 46% data explained by just horn length
# We can only assume glizz explains 61-46 = 15% of the data, if glizz and horn length are independent (because of the additive rule for variances)
boxplot(u1$Hornlength~u1$Glizz)
# It seems that variances differ between groups, which is kind of ok

t.test(u1$Hornlength~u1$Glizz)
# While	it	seems	that	unicorns	with	longer	horns	are	more	likely	to	wear	glizz,	it's	not	the	cleanest	relationship...	

# Both	variables	are	not	fully	independent,	so	we	cannot	simply	assume	that	the	difference	in	R	squares	is	also	the	difference	in	variance	explained	by	each	variable
# But we are confident that unicorns with longer horns are heavier. so too if they wear glizz
# There	is also	some	collinearity	between	horn	length	and	glizz,	such	that	unicorns	with	longer	horns	have	more	glizz,	or	maybe,	unicorns	with	more	glizz	grow	longer	horns.	It's	unclear!

par(mfrow=c(2,2))
plot(ReducedModel) # Point 14 is a bit odd..
View(u1) # Sample size, and thus statistical power, is a bit on the low side

dev.off()

plot(u1$Hornlength[u1$Glizz==0], u1$Bodymass[u1$Glizz==0], pch=19, xlab="Horn length", ylab="Body mass", xlim=c(3,8), ylim=c(6,15))
points(u1$Hornlength[u1$Glizz==1], u1$Bodymass[u1$Glizz==1], pch=19, col="red")
abline(ReducedModel)



###############################################################################
# Linear	models	-	interpretation	of	interactions	-	two-level	fixed	factor and	continuous	variable
rm(list=ls())
dat <- read.table("../Data/data.txt", header = TRUE)
head(dat)
str(dat)

fullmodel <- lm(species_richness~fertilizer*method, data = dat)
summary(fullmodel) # The interaction is statistically significant
# First,	we	can	interpret	the	parameter	estimate	for	the	fertilizer	in	conventional	grassland.	We	can	interpret	it	directly	because	the	reference	level	of	the	fixed	factor	(method)	is	conventional	(the	level	that	is	not	mentioned	here).	So,	per	unit	fertilizer	more	applied,	we	find	0.78	fewer	species	in	conventionally	managed	grassland.	

# On	average,	there	are	1.19	more	species	in	organic	grassland	(parameter	estimate	of	the	fixed	factor	for	the	level	organic).	The	interaction	tells	us	that	in	organic	grassland,	per	unit	fertilizer,	there	is	0.81	more	species,	in	addition	to	the	other	effect.
# yi = b0(intercept) + b1(fertiliser)*xi + b2(method)*xi + b3(method)*b(fertiliser)*xi + Ei(residuals)

# b1(fertiliser)*xi + b2(method)*xi + b3(method)*b(fertiliser)*xi is equal to 0 when we plug in 0 for method. Convenient! So new parameter estimates are summed up to interpret addtional interactions. 
# eg. -0.78 + 0.81 = 0.03, so	really,	the	slope	for	organic	grassland	is	0.03.
# Every	unit	of	fertilizer	more,	increases species	richness	by	0.03. BUT, this is MUCH smaller than the std. error (~0.1), so we cannot conclude that organic grasslands have any statistically significant effect.

# The	tricky	thing	is	to	interpret	it	correctly.	The	fact	that	the	interaction	is	statistically	significant	only	tells	us	that	there	is	a	statistically	significant	difference	between	the	slope	of	the	conventional	and	that	of	the	organic	grassland.	This	difference	is	statistically	significantly	different	from	zero	-	meaning	the	slope	of	the	organic	grassland	differs	from	the	slope	of	the	conventional	grassland.	It	does	not	tell	us	whether	one	or	the	other	is	statistically	significantly	different	from	zero.

# We can conclude:
# That there	is	a	statistically	significantly	negative	effect	of	increasing	amounts	of	fertilizer	on	conventional	grassland,	where	they	lose	just	under	1	species	per	unit	fertilizer	applied.
# On average, oragnic grasslands have ~1 species more than conventional grassland - just under 1 species lost per unit fertiliser

plot(dat$species_richness[dat$method=="conventional"]~dat$fertilizer[dat$method=="conventional"], pch=16, xlim=c(0,50), ylim=c(0,70), col="grey", ylab="Species richness", xlab="Fertiliser (units)")
points(dat$fertilizer[dat$method=="organic"], dat$species_richness[dat$method=="organic"], pch=16, col="black")

par(mfrow=c(2,2))
plot(fullmodel)
dev.off()

# Linear	models	-	interpretation	of	interactions	-	three-level	fixed	factor	and	continuous	variable	
rm(list=ls())
d <- read.table("../Data/Three-way-Unicorn.txt", header = TRUE)
str(d)
names(d)
head(d)


# Back to Unicorns: Explore	whether	the	relationship	between	horn	length	and	bodymass	is	different	between	the	genders

# This	time	we'll	turn	our	hypothesis	around:	We	suggest	that	only	unicorns	who	are	fat	are	able	to	grow	a	long	horn.	Thus,	this	time,	hornlength	is	our	response	variable,	and	body	mass	the	explanatory	variable.

mean(d$Bodymass)
sd(d$Bodymass)
var(d$Bodymass)
par(mfrow=c(1,2))
hist(d$Bodymass, main="")

mean(d$HornLength)
sd(d$HornLength)
var(d$HornLength)
hist(d$HornLength, main="")

dev.off()

plot(d$HornLength[d$Gender=="male"]~d$Bodymass[d$Gender=="male"], xlim=c(70,100), ylim=c(0,18), pch=19, xlab="Bodymass (kg)", ylab="Horn length (cm)")
points(d$Bodymass[d$Gender=="female"], d$HornLength[d$Gender=="female"], col="red", pch=19) # Note that when using a 'comma' or a 'tilda' you write the model formula the right way round! (y~x OR x,y)
points(d$Bodymass[d$Gender=="not_sure"], d$HornLength[d$Gender=="not_sure"], col="green", pch=19)

mod <- lm(HornLength ~ Gender * Bodymass, data = d)
summary(mod)
# The	intercept	is	confusing,	and	statistically	significantly	different	from	zero,	but	since	we	didn't	standardize	our	data,	it	doesn't	tell	us	much.
# Female	seems	to	be	the	reference	category,	as	male	and	not	sure	are	explicitly	mentioned.	
# The	interaction	between	male	and	bodymass	is	statistically	significant,	that	means,	the	difference	between	this	one	and	the	slope	for	females	(bodymass,	0.63)	is	statistically	significantly	different	from	zero.	
# The	interaction	for	the	third	gender	is	not	statistically	significant,	although	close.
# For	females,	we	can	calculate	the	hornlength	by	the	formula:	
# Hornlength	of	females	=	-42.31	+	0.63*Bodymass	
# Given	that	bodymass	ranges	between	70	and	100,	this	seems	to	make	quite	a	lot	of	sense.	Clearly,	the	slope	for	females	is	positive,	which	is	what	we'd	expect	from	our	plot	(red	dots).	With	an	increase	of	1kg,	hornlength	increases	by	0.63	cm.

# For	males,	we	would	ignore	the	stuff	for	not_sure	unicorn.	And	then	the	formula	would	be	as	follows:
# Hornlength	of	males	=	-42.31	+	114.50	+	0.63	*	Bodymass	-	1.32	*	Bodymass		
# which	translates	into:	Hornlength	of	males	=	72.19	-	0.69*Bodymass		
# A strong, negative slope. 
# The standard error of the interaction 'Gendermale:Bodymass' (0.12) is much smaller than the slope (-0.69). So less within-group variance than between-group, so we can say that the slope of the males is significantly different from zero. Not	only	is	it	different	from	the	positive	slope	of	the	females,	it's	also	statistically	significant.

# So	with	an	increase	of	1kg	in	body	mass,	horn	length	actually	decreases	by	0.69cm

# Rule: never	interpret	a	main	effect	in	the	presence	of	an	interaction.	# Hornlength	of	not_sure	=	-42.31	+	12.81	+	0.63	*	Bodymass	-	0.18	*	Bodymass
# So, Hornlength	of	not_sure	=	-29.5	+	0.45*Bodymass
# This	slope	was	not	statistically	significantly	different	from	the	female	slope.	The	reason	for	this	is	if	you	take	the	difference	of	both	slopes	(0.63-0.45	=	0.18)	it	is	just	not	larger	than	twice	the	SE	(0.2)
#  However,	this	slope	is	statistically	significantly	different	from	zero	-	because	0.45	is	much	larger	than	twice	the	SE	-	0.2.	So,	even	if	this	effect	size	was	significant	in	the	model,	when	we	do	our	interpretatio correctly,	we	can	still	confirm	or	reject	a	hypothesis.	


