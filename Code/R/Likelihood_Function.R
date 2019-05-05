#!/usr/bin/env R

binomial.likelihood <- function(p){
  choose(10,7)*p^7*(1-p)^3
}

binomial.likelihood(p=0.1) # Reutrns: 8.748e-06

p <- seq(0,1,0.01)
likelihood.values <- binomial.likelihood(p)
plot(p, likelihood.values, type="l")
abline(h=max(likelihood.values), lty=4)
abline(v=p[likelihood.values==max(likelihood.values)[1]], lty=4)
grid(nx = NULL, ny = NA)

log.binomial.likelihood <- function(p) {
  log(binomial.likelihood(p=p))
}
p <- seq(0,1,0.01)
log.likelihood.values <- log.binomial.likelihood(p)
plot(p, log.likelihood.values, type="l")
abline(h=max(log.likelihood.values), lty=4)
abline(v=p[log.likelihood.values==max(log.likelihood.values)[1]], lty=4)
grid(nx = NULL, ny = NA)
max(log.likelihood.values)
# Point at which the function is maximised remains the same (p = ~0.7)

optimize(binomial.likelihood, interval = c(0,1), maximum = TRUE) 
# Maximum = x at max y (0.6999843), Objective=max(likelihood.values) (0.2668279)
# Not exactly 0.7 because of rounding errors - theoretical = 0.7

recapture.data <- read.csv("../Data/recapture.csv", header=T)
plot(recapture.data$day, recapture.data$length_diff, xlab="Day", ylab="Difference in length", pch=4)
# THE LOG-LIKELIHOOD FOR THE LINEAR REGRESSION
# PARAMETERS HAVE TO BE INPUT AS A VECTOR
regression.log.likelihood<-function(parm, dat)
{
  # DEFINE THE PARAMETERS parm
  # WE HAVE THREE PARAMETERS: a, b, sigma. BE CAREFUL OF THE ORDER
  a <- parm[1] # Slope
  b <- parm[2] # Intercept
  sigma <- parm[3] # Variance of the errors
  # DEFINE THE DATA dat
  # FIRST COLUMN IS x, SECOND COLUMN IS y
  x <- dat[,1]
  y <- dat[,2]
  # DEFINE THE ERROR TERM
  error.term <- (y-a-b*x)
  # REMEMBER THE NORMAL pdf?
  density <- dnorm(error.term, mean=0, sd=sigma, log=T) 
  # log=T for LogLikelihood
  # THE LOG-LIKELIHOOD IS THE SUM OF INDIVIDUAL LOG-DENSITY
  return(sum(density))
}

# JUST TO SEE WHAT THE LOG-LIKELIHOOD VALUE IS WHEN a=1, b=1, and sigma=1
# YOU MAY TRY ANY DIFFERENT VALUES
regression.log.likelihood(c(1,1,1), dat=recapture.data)
# -452.6903

# TO OPIMISE THE LOG-LIKELIHOOD FUNCTION IN R -  to find peak
# optimize() IS ONE-DIMENSIONAL,
# optim() GENERALISES TO MULTI-DIMENSIONAL CASES
optim(par=c(1,1,1), regression.log.likelihood, method='L-BFGS-B',
      lower=c(-1000,-1000,0.0001), upper=c(1000,1000,10000),
      control=list(fnscale=-1), dat=recapture.data, hessian=T)
# sigma cannot be negative - lowest value = 0.0001
# par=c(1,1,1) - Initial values for the parameters
# log.likelihood.regression - The function you wish to be optimised
# method=‘L-BFGS-B’ - Optimisation algorithm
# lower=c(-1000,-1000,0.0001) - Lower bound of your parameter space
# upper=c(1000,1000,10000) - Upper bound of your parameter space
# control=list((fnscale=-1)) - fnscale=-1 means to maximise

# REGRESSION WITH THE BUILT-IN lm()
m<-lm(length_diff~day, data=recapture.data)
summary(m)

n<-nrow(recapture.data)
sqrt(var(m$residual)*(n-1)/n)

# You always need to provide an initial parameter vector by par=
# Choice of method can be tricky for advanced users: See R help for details. If you use L-BFGS-B as your method, then you need to specify the upper and lower bound of the parameter values for searching for the maximum. No need to specify if you use Nelder-Mead

# If you wish to maximise a function, put fnscale=-1 in your control list, default is to minimise. You can put multiple control parameters in the control list.

# Precision can be adjusted by tolerance or maximum number of iterations, say maxit or abstol within control

# The Hessian matrix provide information about the variance-covariance
# structure of your parameter estimates

# Try multiple sets of initial parameters and ensure they all converge to the same estimates

# “Stumble around” the parameter space towards the best parameters, just like a drunkard trying to stumble home (the best place).

# Not every step is in the right direction, and it takes some time to go home.

# Ideal if the drunkard find his place. But also he may get stuck at the local maximum. 

# THE LOG-LIKELIHOOD FUNCTION FOR M1 WITHOUT AN INTERCEPT
regression.no.intercept.log.likelihood<-function(parm, dat)
{
  # DEFINE THE PARAMETERS
  # NO INTERCEPT THIS TIME
  b <- parm[1]
  sigma <- parm[2]
  # DEFINE THE DATA
  # SAME AS BEFORE
  x<-dat[,1]
  y<-dat[,2]
  # DEFINE THE ERROR TERM, NO INTERCEPT HERE
  error.term <- (y-b*x)
  # REMEMBER THE NORMAL pdf?
  density<-dnorm(error.term, mean=0, sd=sigma, log=T)
  # LOG-LIKELIHOOD IS THE SUM OF DENSITIES
  return(sum(density))
}

regression.no.intercept.log.likelihood(c(1,1), dat=recapture.data)

# PERFORMING LIKELIHOOD-RATIO TEST
M1<-optim(par=c(1,1), regression.no.intercept.log.likelihood,
          dat=recapture.data, method='L-BFGS-B',
          lower=c(-1000,0.0001), upper=c(1000,10000),
          control=list(fnscale=-1), hessian=T)
M2<-optim(par=c(1,1,1), regression.log.likelihood,
          dat=recapture.data, method='L-BFGS-B',
          lower=c(-1000,-1000,0.0001), upper=c(1000,1000,10000),
          control=list(fnscale=-1), hessian=T)
# THE TEST STATISTIC D
D<-2*(M2$value-M1$value)
D # Likelihood-ratio test statistic = 3.047676

# CRITICAL VALUE
qchisq(0.95, df=1) # df=3-2 => 3.841459
# So we accept the hypothesis that the intercept is zero at α = 0.05 (Same conclusion is drawn from lm() using anova table)

# rchisq = rnorm family => generate random numbers from chisq distribution
# dchisq = dnorm/dbinom family => density 
# qchisq = quantile, eg. value at 0.95 (95% CI)

regression.non.constant.var.log.likelihood<-function(parm, dat)
{
  b<-parm[1]
  sigma<-parm[2]
  x<-dat[,1]
  y<-dat[,2]
  error.term<-(y-b*x)
  density<-dnorm(error.term, mean=0, sd=x*sigma, log=T) # REMEMBER THE NORMAL pdf - Look up BOX-COX transformation???
  return(sum(density))
}
regression.non.constant.var.log.likelihood(c(1,1), dat=recapture.data)
# MAXIMISE THE LOG-LIKELIHOOD
# HOW ABOUT CALLING IT M4?
M4<-optim(par=c(1,1), regression.non.constant.var.log.likelihood,
          dat=recapture.data, method='L-BFGS-B',
          lower=c(-1000,0.0001), upper=c(1000,10000),
          control=list(fnscale=-1))
M4

# Confidence interval
# DEFINE THE RANGE OF PARAMETERS TO BE PLOTTED
b<-seq(2, 4, 0.1)
sigma<-seq(2, 5, 0.1)
# THE LOG-LIKELIHOOD VALUE IS STORED IN A MATRIX
log.likelihood.value<-matrix(nr=length(b), nc=length(sigma))
# COMPUTE THE LOG-LIKELIHOOD VALUE FOR EACH PAIR OF PARAMETERS
for (i in 1:length(b))
{
  for (j in 1:length(sigma))
  {
    log.likelihood.value[i,j]<-
      regression.no.intercept.log.likelihood(parm=c(b[i],sigma[j]),
                                             dat=recapture.data)
  }
}
# WE ARE INTERESTED IN KNOWING THE LOG-LIKELIHOOD VALUE
# RELATIVE TO THE MAXIMA
log.likelihood.value<-log.likelihood.value-M1$value
# FUNCTION FOR 3D PLOT
persp(b, sigma, log.likelihood.value, theta=30, phi=20,
      xlab='b', ylab='sigma', zlab='log.likelihood.value',
      col='blue')

# CONTOUR PLOT - ie. as viewed from above
contour(b, sigma, log.likelihood.value, xlab='b', ylab='sigma',
        xlim=c(2.5, 3.9), ylim=c(2.0, 4.3),
        levels=c(-1:-5, -10), cex=2)
# DRAW A CROSS TO INDICATE THE MAXIMUM
points(M1$par[1], M1$par[2], pch=3)

contour.line1<-contourLines(b, sigma, log.likelihood.value,
                           levels=-1.92)[[1]] # Rule of thumb is -1.92
lines(contour.line1$x, contour.line1$y, col='red',
      lty=2, lwd=2)
grid(nx = NULL, ny = NA)
grid(nx = NA, ny = NULL)
# 95% CI for sigma is [2.23, 3.74]
# 95% CI for b is [2.75, 3.57]
# 95% CI for 1 parameter = 0.5*chisq1 = 1.92
abline(h=max(contour.line1$x, contour.line1$y), lty=4, col="red")
abline(h=min(contour.line1$x, contour.line1$y), lty=4, col="red")
print(paste("CI for parameter sigma is:",max(contour.line1$x, contour.line1$y),",",min(contour.line1$x, contour.line1$y)))
abline(v=max(contour.line1$x), lty=4, col="red")
abline(v=min(contour.line1$x), lty=4, col="red")
print(paste("CI for parameter b is:",max(contour.line1$x),",",min(contour.line1$x)))
# 3.737046
# 2.23367
# 3.577263
# 2.749373
# But we do not know the joint confident region for both (b,sigma)
# 95% CI for 2 joint parameters = 0.5*chisq2 = 2.99
contour.line2<-contourLines(b, sigma, log.likelihood.value,
                           levels=-2.99)[[1]] # Rule of thumb is -1.92
lines(contour.line2$x, contour.line2$y, col='blue',
      lty=2, lwd=2)
# The joint confidence region is wider than the confidence interval for 1 parameter alone, this is because of 'multiple comparison'
# Point [3.5, 2.3] lies outside of the joint 95% CI for both parameters (b given sigma), despite lying within the bounds of the 95% CI when considering each individual parameter in turn. ie. when both parameters are considered together (variance-covariance), the confidence interval is constrained such that the point actually lies outside of the true 95% CI for both parameters.
points(3.5,2.3, pch=4)
# This point lies actually outside of the confidence region when both parameters are considered together. 
abline(h=max(contour.line2$x, contour.line2$y), lty=4, col="blue")
abline(h=min(contour.line2$x, contour.line2$y), lty=4, col="blue")
abline(v=max(contour.line2$x), lty=4, col="blue")
abline(v=min(contour.line2$x), lty=4, col="blue")
max(contour.line2$x, contour.line2$y) # 4.041608
min(contour.line2$x, contour.line2$y) # 2.116497
max(contour.line2$x) # 3.691743
min(contour.line2$x) # 2.635445

# optim() GENERALISES TO MULTI-DIMENSIONAL CASES
# WITH HESSIAN MATRIX
result<-optim(par=c(1,1), regression.no.intercept.log.likelihood,
              method='L-BFGS-B',
              lower=c(-1000,0.0001), upper=c(1000,10000),
              control=list(fnscale=-1), dat=recapture.data, hessian=T) # Hessian = TRUE returns the Hessian matrix
# GET BACK THE HESSIAN MATRIX
result$hessian

# THE VARIANCE-COVARIANCE MATRIX IS THE NEGATIVE OF
# THE INVERSE OF THE HESSIAN MATRIX.
# BY solve() FUNCTION
variance.matrix<-(-1)*solve(result$hessian) # Return inverse Hessian matrix - all second order partial derivatives
variance.matrix

# Q3 (iii)
coin.log.likelihood <- function(p,n,y){
  # p is the parameter
  # n is the number of trials
  # y is the number of heads
  return(lchoose(n,y) + y*log(p) + (n-y)*log(1-p))
}

H0 <- coin.log.likelihood(p=0.5, n=50, y=35)
H0 # Log likelihood value of -6.215

H1 <- coin.log.likelihood(p=0.7, n=50, y=35)
H1 # As expected, a higher log likelihood value of -2.1

D <- 2*(H1-H0)
D # D-statistic is 8.228
# Df = H1-H0 = 1-0 = 1
qchisq(0.95, df=1)
# 8.228 > 3.841, therefore we can reject H0 and accept H1, that the coin is unfair (loaded)
# This demonstrates the effect of sample size! 

p <- seq(0,1,0.01)
coin.log.likelihood.value <- sapply(p, coin.log.likelihood, n=50, y=35) # Sapply works with vectors
coin.log.likelihood.value <- coin.log.likelihood.value - max(coin.log.likelihood.value) # Zero stanardise the plot
plot(p, coin.log.likelihood.value, type="l", lwd=2)
abline(h=max(coin.log.likelihood.value), lty=4)
abline(v=p[coin.log.likelihood.value==max(coin.log.likelihood.value)[1]], lty=4)
grid(nx = NULL, ny = NA)
grid(nx = NA, ny = NULL)
plot(p, coin.log.likelihood.value, type="l", lwd=2, xlim=c(0.4, 0.9), ylim=c(-3,0))
abline(h=-1.92, col="red", lty=4)

# Lower 95% CI
uniroot(function(p){
  coin.log.likelihood(p=p, n=50, y=35) - coin.log.likelihood(p=0.7, n=50, y=35) + 1.92},
  interval = c(0.01, 0.7)
  ) # 0.5652006

# Upper 95% CI
uniroot(function(p){
  coin.log.likelihood(p=p, n=50, y=35) - coin.log.likelihood(p=0.7, n=50, y=35) + 1.92},
  interval = c(0.7, 1)
) # 0.8148278

# Question 4
flowering <- read.table("../Data/flowering.txt", header = TRUE)
flowering
names(flowering)

par(mfrow=c(1,2))
plot(flowering$Flowers, flowering$State)
plot(flowering$Root, flowering$State)
# TWO ARGUMENTS: parm IS A VECTOR OF PARAMETERS,
# dat IS THE INPUT DATASET
logistic.log.likelihood<-function(parm, dat) {
  # DEFINE PARAMETERS
  a<-parm[1]
  b<-parm[2]
  c<-parm[3]
  # DEFINE RESPONSE VARIABLE, WHICH IS THE FIRST COLUMN OF dat
  State<-dat[,1]
  # SIMILARLY DEFINE OUR EXPLANATORY VARIABLES
  Flowers<-dat[,2]
  Root<-dat[,3]
  # MODEL OUR SUCCESS PROBABILITY
  p<-exp(a+b*Flowers+c*Root)/(1+exp(a+b*Flowers+c*Root))
  # THE LOG-LIKELIHOOD FUNCTION
  log.like<-sum(State*log(p)+(1-State)*log(1-p))
  return(log.like)
}
# TRY
logistic.log.likelihood(c(0,0,0), dat=flowering)
M1 <- optim(par=c(0,0,0), logistic.log.likelihood,
            dat=flowering, method='L-BFGS-B',
            lower=c(-1000,-4,-1000), upper=c(1000,1000,1000),
            control=list(fnscale=-1), hessian = FALSE)
M1
# Parameters: a = 0.9614547, b = -0.1064155, c = 6.6003380
# Associated log-likelihood value: -27.03405

logistic.log.likelihood.int<-function(parm, dat)
{
  # DEFINE PARAMETERS
  a<-parm[1]
  b<-parm[2]
  c<-parm[3]
  d<-parm[4]
  # DEFINE RESPONSE VARIABLE, WHICH IS THE FIRST COLUMN OF dat
  State<-dat[,1]
  # SIMILARLY DEFINE OUR EXPLANATORY VARIABLES
  Flowers<-dat[,2]
  Root<-dat[,3]
  
  # MODEL OUR SUCCESS PROBABILITY
  p<-exp(a+b*Flowers+c*Root+d*Flowers*Root)/(1+exp(a+b*Flowers+c*Root+d*Flowers*Root))
  # THE LOG-LIKELIHOOD FUNCTION
  log.like<-sum(State*log(p)+(1-State)*log(1-p))
  return(log.like)
}
logistic.log.likelihood.int(c(0,0,0,0), dat=flowering) # -40.89568

M2 <- optim(par=c(0,0,0,0), logistic.log.likelihood.int,
            dat=flowering, method='L-BFGS-B',
            lower=c(-1000,-4,-1000,-Inf), upper=c(1000,1000,1000,1000),
            control=list(fnscale=-1), hessian = FALSE)
M2
# Parameters: a = -2.95495534, b = -0.07888641, c =  25.11688994, d = -0.20865900
# Associated log-likelihood value: -18.56412
M2[2][[1]]
M1[2][[1]]
# Likelihood ratio test
D <- 2*(M2[2][[1]]-M1[2][[1]])
D # 16.93986
# Df = 4 - 3
qchisq(0.95, df=1) # 3.841459

# D-statistic > chisq value, therefore the interaction is significant
