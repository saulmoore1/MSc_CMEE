#!/usr/bin/env R
rm(list=ls())
############################
# Two Island MLE Exercise
############################

load("../../Week13/Data/two_island.RData") # Load the RData file
ls() # 3 objects

M <- optim(par=c(10,0.1), log.likelihood.migration.model, method='L-BFGS-B',
      lower=c(10,0), upper=c(10000,0.5),
      control=list(fnscale=-1), pop1=sample.island.A, pop2=sample.island.B, t=8, n=100, hessian=T)
M
# Hessian Matrix
# -0.0007754579    -1.64175
# -1.6417496909 -8259.37540

# DEFINE THE RANGE OF PARAMETERS TO BE PLOTTED
N<-seq(100, 1000, 20)
m<-seq(0, 0.5, 0.02)
# THE LOG-LIKELIHOOD VALUE IS STORED IN A MATRIX
log.likelihood.value<-matrix(nr=length(N), nc=length(m))
# COMPUTE THE LOG-LIKELIHOOD VALUE FOR EACH PAIR OF PARAMETERS
for (i in 1:length(N)) {
  for (j in 1:length(m)) {
    log.likelihood.value[i,j]<-
      log.likelihood.migration.model(parm=c(N[i],m[j]), pop1 = sample.island.A, pop2 = sample.island.B, t=8, n=100)
  }
}
# WE ARE INTERESTED IN KNOWING THE LOG-LIKELIHOOD VALUE RELATIVE TO THE MAXIMA
log.likelihood.value<-log.likelihood.value-M$value
# FUNCTION FOR 3D PLOT
persp(N, m, log.likelihood.value, theta=60, phi=30,
      xlab='Effective population size (Ne)', ylab='Migration rate (m)', zlab='Log likelihood value',
      col='blue')

# CONTOUR PLOT
contour(N, m, log.likelihood.value, xlab='Effective population size (Ne)', ylab='Migration rate (m)', xlim=c(100,1000), ylim=c(0, 0.5),
        levels=c(-1:-20, -10), cex=2)
points(M$par[1], M$par[2], pch=3)
contour.line1<-contourLines(N, m, log.likelihood.value,
                            levels=-1.92)[[1]] # Rule of thumb is -1.92
lines(contour.line1$x, contour.line1$y, col='red',
      lty=2, lwd=2)
grid(nx = NULL, ny = NA)
grid(nx = NA, ny = NULL)

# REFINED CONTOUR PLOT
N<-seq(100, 1000, 1)
m<-seq(0, 0.5, 0.005)
# THE LOG-LIKELIHOOD VALUE IS STORED IN A MATRIX
log.likelihood.value<-matrix(nr=length(N), nc=length(m))
# COMPUTE THE LOG-LIKELIHOOD VALUE FOR EACH PAIR OF PARAMETERS
for (i in 1:length(N)) {
  for (j in 1:length(m)) {
    log.likelihood.value[i,j] <-
      log.likelihood.migration.model(parm=c(N[i],m[j]), pop1 = sample.island.A, pop2 = sample.island.B, t=8, n=100)
  }
}
# WE ARE INTERESTED IN KNOWING THE LOG-LIKELIHOOD VALUE RELATIVE TO THE MAXIMA
log.likelihood.value<-log.likelihood.value-M$value

contour(N, m, log.likelihood.value, xlab='Effective population size (Ne)', ylab='Migration rate (m)', xlim=c(200,700), ylim=c(0.05, 0.2),
        levels=c(-1:-10, -1), cex=2)
# DRAW A CROSS TO INDICATE THE MAXIMUM
points(M$par[1], M$par[2], pch=3)

contour.line1<-contourLines(N, m, log.likelihood.value,
                            levels=-1.92)[[1]] # Rule of thumb is -1.92
lines(contour.line1$x, contour.line1$y, col='red',
      lty=2, lwd=2)
grid(nx = NULL, ny = NA)
grid(nx = NA, ny = NULL)
abline(h=max(contour.line1$y), lty=4, col="red")
abline(h=min(contour.line1$y), lty=4, col="red")
abline(v=max(contour.line1$x), lty=4, col="red")
abline(v=min(contour.line1$x), lty=4, col="red")
print(paste("Predicted CI for parameter N (not considering m) is:",min(contour.line1$x),",",max(contour.line1$x)))
print(paste("Predicted CI for parameter m (not considering N) is:",min(contour.line1$y),",",max(contour.line1$y)))

contour.line2<-contourLines(N, m, log.likelihood.value,
                            levels=-2.99)[[1]] # Rule of thumb is -1.92
lines(contour.line2$x, contour.line2$y, col='blue',
      lty=2, lwd=2)
abline(h=max(contour.line2$y), lty=4, col='blue')
abline(h=min(contour.line2$y), lty=4, col='blue')
abline(v=max(contour.line2$x), lty=4, col='blue')
abline(v=min(contour.line2$x), lty=4, col='blue')
print(paste("Joint CI for parameter N, considering m, is:",min(contour.line2$x),max(contour.line2$x)))
print(paste("Joint CI for parameter m, considering N, is:",min(contour.line2$y),max(contour.line2$y)))

points(500, 0.12, pch=3) # Point lies outside of the joint confidence interval, hence we can reject H0

# Variance-covariance matrix is the negative inverse of the Hessian matrix
variance.matrix<-(-1)*solve(M$hessian) # Return inverse Hessian matrix - all second order partial derivatives
variance.matrix
# 2226.573209 -0.442585026
#   -0.442585  0.000209049
# Variance of N (2226.6) and of m (0.0002)
# Negative covariance between N and m (-0.44)

# Confidence intervals calculated based on the variance-covariance structure (from Hessian matrix) and approximate normality
c(M$par[1]-1.96*sqrt(variance.matrix[1]), M$par[1]+1.96*sqrt(variance.matrix[1]))
c(M$par[2]-1.96*sqrt(variance.matrix[4]), M$par[2]+1.96*sqrt(variance.matrix[4]))
