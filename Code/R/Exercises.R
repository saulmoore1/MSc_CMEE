#!/usr/bin/env R
rm(list=ls())

###############################
# 1. Exercises - Functions
###############################
pdf("../Results/Trig_functions")
plot(NULL, xlim=c(0,8*pi),ylim=c(-10,10), xlab="x", ylab="y")
x <- seq(0,8*pi,by=0.1)
y1 <- sin(x)
y2 <- sin(x + pi/2) + 5
y3 <- cos(x)
y4 <- cos(x + pi/2) - 5
y5 <- sin(x)/cos(x) # equal to tan(x)
y6 <- cos(x)/sin(x) # cot(x)
points(x,y1, type="l", col="black")
points(x,y2, type="l", col="red")
points(x,y3, type="l", col="blue")
points(x,y4, type="l", col="green")
points(x,y5, type="l", col="purple")
points(x,y6, type="l", col="orange")
legend("topright", c("y1 = sin(x)", "y = sin(x + pi/2)+5", "y = cos(x)", "y = cos(x + pi/2)-5", "y = tan(x)", "y = cot(x)"), fill=c("black", "red", "blue", "green", "purple", "orange"))
dev.off()

pdf("../Results/Exp_functions")
plot(NULL, xlim=c(0,5), ylim=c(0,5), xlab="x", ylab="y")
x <- seq(0,5,by=0.1)
y1 <- exp(x)
y2 <- exp(-x)
y3 <- 10^x
y4 <- 10^(-x)
points(x,y1, type="l", col="black")
points(x,y2, type="l", col="red")
points(x,y3, type="l", col="blue")
points(x,y4, type="l", col="green")
labs <- c("y1 = exp(x)", "y2 = exp(-x)", "y3 = 10^x", "y4 = 10^(-x)")
legend("topright", labs, fill=c("black","red","blue","green"))
dev.off()

# Varying A
pdf("../Results/Exp_functions_A")
plot(NULL, xlim=c(-5,5), ylim=c(0,5), xlab="x", ylab="y")
x <- seq(-5,5,by=0.1)
A <- 1
y1 <- A*exp(x)
y2 <- A*exp(-x)
y3 <- A*10^x
y4 <- A*10^(-x)
points(x,y1, type="l", col="black")
points(x,y2, type="l", col="red")
points(x,y3, type="l", col="blue")
points(x,y4, type="l", col="green")
A <- 1.5
y1 <- A*exp(x)
y2 <- A*exp(-x)
y3 <- A*10^x
y4 <- A*10^(-x)
points(x,y1, type="l", col="black", lty=2)
points(x,y2, type="l", col="red", lty=2)
points(x,y3, type="l", col="blue", lty=2)
points(x,y4, type="l", col="green", lty=2)
A <- 2
y1 <- A*exp(x)
y2 <- A*exp(-x)
y3 <- A*10^x
y4 <- A*10^(-x)
points(x,y1, type="l", col="black", lty=3)
points(x,y2, type="l", col="red", lty=3)
points(x,y3, type="l", col="blue", lty=3)
points(x,y4, type="l", col="green", lty=3)
labs <- c("y1 = A*exp(x)", "y2 = A*exp(-x)", "y3 = A*10^x", "y4 = A*10^(-x)")
legend("topright", labs, fill=c("black","red","blue","green"))
text(4,3,labels = "A=1 (Solid)")
text(4,2.5,labels = "A=1.5 (Dashed)")
text(4,2,labels = "A=2 (Dotted)")
dev.off()

# Varying k
pdf("../Results/Exp_functions_k")
plot(NULL, xlim=c(-5,5), ylim=c(0,5), xlab="x", ylab="y")
x <- seq(-5,5,by=0.1)
A <- 1
k <- 1
y1 <- A*exp(k*x)
y2 <- A*exp(k*(-x))
y3 <- A*10^(k*x)
y4 <- A*10^(k*(-x))
points(x,y1, type="l", col="black")
points(x,y2, type="l", col="red")
points(x,y3, type="l", col="blue")
points(x,y4, type="l", col="green")
k <- 1.5
y1 <- A*exp(k*x)
y2 <- A*exp(k*(-x))
y3 <- A*10^(k*x)
y4 <- A*10^(k*(-x))
points(x,y1, type="l", col="black", lty=2)
points(x,y2, type="l", col="red", lty=2)
points(x,y3, type="l", col="blue", lty=2)
points(x,y4, type="l", col="green", lty=2)
k <- 2
y1 <- A*exp(k*x)
y2 <- A*exp(k*(-x))
y3 <- A*10^(k*x)
y4 <- A*10^(k*(-x))
points(x,y1, type="l", col="black", lty=3)
points(x,y2, type="l", col="red", lty=3)
points(x,y3, type="l", col="blue", lty=3)
points(x,y4, type="l", col="green", lty=3)
labs <- c("y1 = A*exp(k*x)", "y2 = A*exp(k*(-x))", "y3 = A*10^(k*x)", "y4 = A*10^(k*(-x))")
legend("topright", labs, fill=c("black","red","blue","green"))
text(4,3,labels = "k=1 (Solid)")
text(4,2.5,labels = "k=1.5 (Dashed)")
text(4,2,labels = "k=2 (Dotted)")
dev.off()

# Hyperbolic functions
pdf("../Results/Hyper_functions")
plot(NULL, xlim=c(-5,5), ylim=c(-5,5), xlab="x", ylab="y")
x <- seq(-5,5,by=0.1)
y1 <- sinh(x)
y2 <- cosh(x)
y3 <- tanh(x)
y4 <- cosh(x)/sinh(x)
points(x,y1, type="l", col="black")
points(x,y2, type="l", col="red")
points(x,y3, type="l", col="blue")
points(x,y4, type="l", col="green")
legend("topright", c("y1=sinh(x)","y2=cosh(x)","y3=tanh(x)","y4=coth(x)"), fill=c("black","red","blue","green"))
dev.off()

# Approximating hyperbolic functions
n <- seq(0,10, by=0.5)
y1 <- sinh(x)
y2 <- cosh(x)
y3 <- tanh(x)
y4 <- cosh(x)/sinh(x)
y5 <- x + (1/factorial(2))*x^2
y6 <- x + (1/factorial(3))*x^3
y7 <- exp(x)/2
y8 <- exp(-x)/2
y9 <- -exp(-x)/2

pdf("../Results/RMSD.pdf")
par(mfrow=c(3,2))
dif <- ((y1-y5)^2)
RMSD <- c()
for(i in 1:length(n)) {
  RMSD <- c(RMSD, sqrt(sum(dif[1:i]))/i)
}
plot(RMSD~n, type="l", col="black", lty=1, main=expression(paste("RMSD: ", sqrt(sum((y[1]-y[5])^2)))/n))

dif <- ((y2-y6)^2)
RMSD <- c()
for(i in 1:length(n)) {
  RMSD <- c(RMSD, sqrt(sum(dif[1:i]))/i)
}
plot(RMSD~n, type="l", col="black", lty=2, main=expression(paste("RMSD: ", sqrt(sum((y[2]-y[6])^2)))/n))

dif <- ((y1-y7)^2)
RMSD <- c()
for(i in 1:length(n)) {
  RMSD <- c(RMSD, sqrt(sum(dif[1:i]))/i)
}
plot(RMSD~n, type="l", col="blue", lty=1, main=expression(paste("RMSD: ", sqrt(sum((y[1]-y[7])^2)))/n))

dif <- ((y2-y7)^2)
RMSD <- c()
for(i in 1:length(n)) {
  RMSD <- c(RMSD, sqrt(sum(dif[1:i]))/i)
}
plot(RMSD~n, type="l", col="blue", lty=2, main=expression(paste("RMSD: ", sqrt(sum((y[2]-y[7])^2)))/n))

n <- seq(-10,0, by=0.5)
dif <- ((y1-y8)^2)
RMSD <- c()
for(i in 1:length(n)) {
  RMSD <- c(RMSD, sqrt(sum(dif[1:i]))/i)
}
plot(RMSD~n, type="l", col="red", lty=1, main=expression(paste("RMSD: ", sqrt(sum((y[1]-y[8])^2)))/n))

dif <- ((y2-y9)^2)
RMSD <- c()
for(i in 1:length(n)) {
  RMSD <- c(RMSD, sqrt(sum(dif[1:i]))/i)
}
plot(RMSD~n, type="l", col="red", lty=2, main=expression(paste("RMSD: ", sqrt(sum((y[2]-y[9])^2)))/n))
dev.off()

# Logarithmic functions
pdf("../Results/Log_functions.pdf")
x <- seq(0,100, by=1)
y1 <- log(x)
y2 <- -log(x)
y3 <- log10(x)
y4 <- log2(x)
plot(NULL, xlim=c(0,100), ylim=c(-4,4), ylab="y", xlab="x")
points(y1~x, type="l", col="black")
points(y2~x, type="l", col="red")
points(y3~x, type="l", col="blue")
points(y4~x, type="l", col="green")
legend("topright", c("y1 = ln(x)", "y2 = -ln(x)", "y3 = log10(x)", "y4 = log2(x)"), fill=c("black", "red", "blue", "green"))
dev.off()

# True Diversity and Shannon Index
N=100
Pi <- 1/N
S <- -N*(sum(Pi*log2(Pi)))
H <- -N*(sum(Pi*log(Pi)))
D <- exp(H)
print(paste("S:",S,"H:",H,"D:",D))
sumx <- 0
for (i in 1:N) {
  Pi <- 1/2^i
  x <- Pi*log2(Pi)
  sumx <- sumx + x
}
S <- -N*sumx
sumy <- 0
for (i in 1:N) {
  Pi <- 1/2^i
  y <- Pi*log(Pi)
  sumy <- sumy + y
}
H <- -N*sumy
D <- exp(H)

# Function superposition
x <- seq(-5,5,by=0.1)
plot(NULL, xlim=c(-5,5), ylim=c(-10,10), xlab="x", ylab="y")
y1 = -2*x
y2 = x^3
y3 = y1 + y2
points(y1~x, type="l", col="black")
points(y2~x, type="l", col="red")
points(y3~x, type="l", col="blue")

# Function modulation
x <- seq(-5,5,by=0.1)
plot(NULL, xlim=c(-5,5), ylim=c(-1,1), xlab="x", ylab="y")
y1 = sin(x)
y2 = -sin(x)
y3 = y1*y2
points(y1~x, type="l", col="black")
points(y2~x, type="l", col="red")
points(y3~x, type="l", col="blue")

#############################
# 2. Exercises - Derivatives
#############################
# Indeterminate limits of type 0/0
x <- seq(1e-10,0.4,0.01)
y1 <- exp(x)*(1-exp(-x))
y2 <- -(1+x)*log(1-x)
pdf("../Results/Indeterminate_Limits.pdf")
plot(y1 ~ x, xlab="x", ylab="f(x)", type="l", lty=1, lwd=1)
points(y2 ~ x, type="l", lty=2, lwd=1)
text(x=0.2, y=0.2, pos=4, labels=expression(e^x*(1-e^-x)), cex=0.9)
text(x=0.2, y=0.3, pos=2, labels=expression(-(1+x)*log(1-x)), cex=0.9)
dev.off()

# Logistic of Verhulst Curve
N0 <- seq(1,100,by=1)
N0 = 100
K = 200
t <- seq(1,100,by=1)
r = 2

Verhulst <- function(N0, K, t) {
  
}
Nt <- K*N0/(N0 + (K + N0)*exp(-r*t))

# River management
x <- seq(-10,10,0.1)
R1 <- x^2
R2 <- x-2
x2 <- seq(0.5,1.375,0.1)
Channel <- -x2+0.8
pdf("../Results/Rivers.pdf",5,5)
plot(R1 ~ x, type="l", xlab="Distance (km)", ylab="Distance (km)", xlim=c(-5,5), ylim=c(-5,5), lty=1, lwd=3, col="blue")
points(R2 ~ x, type="l", lty=1, lwd=3, col="blue")
text(3, 0.3, pos=4, labels="y = x - 2")
text(2.5, 4.5, pos=4, labels=expression(paste("y = ", x^2)))
points(Channel ~ x2, type="l", lty=2, lwd=2, col="blue")
text(0.3,-1,pos=3, labels="Channel", cex=0.7)
points(x=1.375, y=-0.625)
points(x=0.5, y=0.25)
dev.off()

# Functions with no antiderivative
x <- seq(-15.01,15.01,0.1)
pdf("../Results/No_Antiderivative.pdf")
Gaussian <- exp(-x^2)
Sinc <- sin(x)/x
plot(Gaussian ~ x, ylim=c(-0.3,1), type="l", lty=1, lwd=2, col="salmon")
points(Sinc~ x, type="l", lty=1, lwd=2, col="lightblue")
legend("topright", c("Gaussian", "Sinc"), fill=c("salmon", "lightblue"))
dev.off()

# Hysteresis
x <- seq(0,10,0.1)
L = 5 # curve's maximum point
x01 = 2 # value of sigmoid midpoint
x02 = 3
k1 = 1.5 # steepness
k2 = 1
y1 <- L/(1 + exp(-k1*(x-x01)))
y2 <- L/(1 + exp(-k2*(x-x02)))
pdf("../Results/Hysteresis.pdf")
plot(y1 ~ x, type="l", lty=1, lwd=3, col="skyblue", xlab=expression(alpha), ylab="RAS-GTP")
points(y2 ~ x, type="l", lty=1, lwd=3, col="salmon")
polygon(c(x, rev(x)), c(y1, rev(y2)),
        col = "grey60", border = NA)
text(3.6,3.8, labels="Hysteresis Loop", srt=60, cex=1.5, col="white")
dev.off()

# Logistic or Verhulst Equation (ODEs)
r = 2
K = 500
N <- seq(0,500,1)
dNdT <- r*(1-(N/K))*N
pdf("../Results/Verhulst_ODE.pdf")
plot(dNdT ~ N, type="l", lty=1, lwd=3, col="lightblue", xlab="N", ylab=expression(dN/dT))
text(400, 225, labels = "K = 500")
dev.off()
