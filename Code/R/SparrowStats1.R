#!/usr/bin/env R
# setwd("/home/saul/Documents/cmeecoursework/SparrowStats/Code/")

install.packages("lme4")
library(lme4)
require(lme4) # 'require' is basically synonymous with 'library', except that it returns a message telling you its loaded

help(x) # looks for the R help manual on the subject 'x'

sqrt(4) == 4^0.5 # TRUE, synonymous

log(0) # -INF
log(1) # 0
log(Inf) # INF

pi # Reutrns pi

exp(1) # Returns the natural logarithm, e

rm(list=ls()) # Clears workspace

d <- read.table("../Data/SparrowSize.txt", header = TRUE)
e <- read.table("../Data/SparrowSize.txt", header = TRUE, sep = "\t") # Tab-separated files
