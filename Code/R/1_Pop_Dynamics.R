#!/usr/bin/env R
rm(list=ls())

#####################################################################
# Population Dynamics Practical - Day 1
#####################################################################
#1

N0 <- 270000 # Initial population
N <- N0
Nt <- N0
t <- 1000
R <- 0.95 # Rate of population decline

for (i in 1:t) {
  Nt <- Nt*R # Population model equation
  if (Nt < 1) {
    Nt <- 0
    N <- c(N, Nt) # Append final population to list
    print(i) # Print generation when population goes extinct
    break() # Break from loop when population goes extinct
  }
  N <- c(N, Nt) # Append current iteration population to list
}

End <- 2100
tplot <- 1970:End # list of years from start-end time, plot on the x-axis
plot(tplot[1:t], N[1:t], type = "o", xlab = "Generation", ylab = "Population Size", pch = 19, cex = 0.75)
 
min(N) # 1598.543

Nplot <- log(N)
plot(tplot[1:t], Nplot[1:t], type = "o", xlab = "Generation", ylab = "log(Population Size)", pch = 19, cex = 0.75)
# Population will go extinct after 244 years


#2 A proper population model
rm(list=ls())
graphics.off()

s <- 0.85
r <- 0.35

N0 <- 270000
N <- N0
Nt <- N0

t <- 100
for (i in 1:t) {
  Nt <- Nt*s + Nt*r
  if (Nt < 0) {
    Nt <- 0
  }
  N <- c(N, Nt)
}

End <- 1970 + length(N)
tplot <- 1970:End
plot(tplot[1:t], N[1:t], type = "o", xlab = "Generation", ylab = "Population Size", pch = 19, cex = 0.75)


s <- c(0.77, 0.81, 0.85)
r <- c(0.15, 0.25, 0.35)

N0 <- 270000
N <- N0
Nt <- N0
t <- 2015-1970
time <- c(1970:2015)

par(mfrow = c(3,3))
for (j in 1:3) {
  for (k in 1:3) {
    N <- N0
    Nt <- N0
    for (i in 1:t) {
      Nt <- Nt*s[k] + Nt*s[k]*r[j]
      if (Nt < 0) {
        Nt <- 0
      }
      N <- c(N, Nt)
    }
    data <- as.data.frame(cbind(time, N))
    filename <- paste("s", j, "r", k, "Sim2.Rdata", sep = "")
    save(data, file = filename)
    plot(data$t, data$N, xlab = "Year", ylab = "N", main = paste("s=", s[j], ",r=", r[k], pch = 19, cex = 0.75, type = "o"))
  }
}