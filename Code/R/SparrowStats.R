#!/usr/bin/env R

# Stats with Sparrows
rm(list=ls())

d <- read.table("../Data/SparrowSize.txt", header = TRUE)
str(d)
names(d)
head(d)

hist(d$Tarsus, main="Sparrow tarsus length (mm)", col="grey", probability = TRUE)

mean(d$Tarsus, na.rm = TRUE)
var(d$Tarsus, na.rm = TRUE)
sd(d$Tarsus, na.rm = TRUE)

lines(density(d$Tarsus,na.rm = TRUE),lwd=2)
abline(v=mean(d$Tarsus,na.rm=TRUE),col="red",lwd=2)
abline(v = mean(d$Tarsus, na.rm = TRUE)-sd(d$Tarsus, na.rm = TRUE), col = "blue",lwd = 2, lty=5) 	
abline(v = mean(d$Tarsus, na.rm = TRUE)+sd(d$Tarsus, na.rm = TRUE), col = "blue",lwd = 2, lty=5)

t.test(d$Tarsus~d$Sex)

par(mfrow=c(2,1)) 	
hist(d$Tarsus[d$Sex==1], main="", xlab="Male sparrow tarsus length (mm)", col ="grey", 	 prob=TRUE)
lines(density(d$Tarsus[d$Sex==1],na.rm=TRUE), lwd = 2) 	
abline(v = mean(d$Tarsus[d$Sex==1], na.rm = TRUE), col = "red",lwd = 2) 	
abline(v = mean(d$Tarsus[d$Sex==1], na.rm = TRUE)-sd(d$Tarsus[d$Sex==1], na.rm = TRUE), col = "blue",lwd = 2, lty=5) 	
abline(v = mean(d$Tarsus[d$Sex==1], na.rm = TRUE)+sd(d$Tarsus[d$Sex==1], na.rm = TRUE), col = "blue",lwd = 2, lty=5) 	

hist(d$Tarsus[d$Sex==0], main="", xlab="Female sparrow tarsus length (mm)", col="grey", prob=TRUE) 	
lines(density(d$Tarsus[d$Sex==0],na.rm=TRUE), lwd = 2) 	
abline(v = mean(d$Tarsus[d$Sex==0], na.rm = TRUE), col = "red",lwd = 2) 	
abline(v = mean(d$Tarsus[d$Sex==0], na.rm = TRUE)-sd(d$Tarsus[d$Sex==0], na.rm = TRUE), col = "blue",lwd = 2, lty=5) 	
abline(v = mean(d$Tarsus[d$Sex==0], na.rm = TRUE)+sd(d$Tarsus[d$Sex==0], na.rm = TRUE), col = "blue",lwd = 2, lty=5) 	
dev.off() 	

var(d$Tarsus,na.rm=TRUE)
sd(d$Tarsus,na.rm=TRUE)
sd(d$Tarsus,na.rm=TRUE)^2	
sqrt(var(d$Tarsus,na.rm=TRUE))

d1<-subset(d, d$Tarsus!="NA")	
d1<-subset(d1, d1$Wing!="NA")	
sumz<-var(d1$Tarsus)+var(d1$Wing)	
test<-var(d1$Tarsus+d1$Wing)
sumz
test

plot(jitter(d1$Wing),d1$Tarsus, pch=19, cex=0.4)

cov(d1$Tarsus,d1$Wing)
sumz<-var(d1$Tarsus)+var(d1$Wing)+2*cov(d1$Tarsus,d1$Wing)
test<-var(d1$Tarsus+d1$Wing)
sumz
test

var(d1$Tarsus*10)
var(d1$Tarsus)*10^2





