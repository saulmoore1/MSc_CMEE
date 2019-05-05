##### LINEAR MODELS #####
# x <- c(1,2,3,4,8)
# y <- c(4,3,5,7,9)
# model1 <- (lm(y~x))
# model1
# summary(model1)
# anova(model1)
# resid(model1)
# cov(x,y)
# var(x)
# var(y)
# plot(y~x)

# Stats with Sparrows 10

rm(list = ls())
setwd("/home/saul/Documents/cmeecoursework/SparrowStats/Code/")

d <- read.table("../Data/SparrowSize.txt", header = TRUE)

plot(d$Mass ~ d$Tarsus, ylab = "Mass (g)", xlab = "Tarsus (mm)", pch=19, cex=0.4)

# Using y = mx + c, the equation of a line
x <- c(1:100)
b <- 0.5
m <- 1.5
y <- m*x + b
plot(x,y, xlim = c(0,100), ylim = c(0,100), pch=19, cex=0.5)

d$Mass[1]

length(d$Mass)
d$Mass[1770]
plot(d$Mass ~ d$Tarsus, ylab = "Mass (g)", xlab = "Tarsus (mm)", pch=19, cex=0.4, ylim=c(-5,38), xlim=c(0,22))

d1 <- subset(d, d$Mass!="NA")
d2 <- subset(d1, d1$Tarsus!="NA")
length(d2$Tarsus) # Removes all observations with no data (NA values)

model1 <- lm(Mass ~ Tarsus, data = d2)
summary(model1)
#####################################################################
# Call:                                                               # R tells you what you asked it to do (Call:)
#   lm(formula = Mass ~ Tarsus, data = d2)                            # In this case, a linear model of sparrow body mass as a function of tarsus length
# 
# Residuals:                                                          # Information about the distribution of model residuals
#   Min      1Q  Median      3Q     Max 
# -7.7271 -1.2202 -0.1302  1.1592  7.5036 
# 
# Coefficients:                                                     
#   Estimate Std. Error t value Pr(>|t|)                              # Paramter estimate, Standard errors, t-values, p-values
# (Intercept)  5.83246    0.98195    5.94 3.48e-09 ***                # p-value that returns *** corresponds to a significance value of 0.001, ie. less than 1 in 1000 observations will differ from the expected value
#   Tarsus       1.18466    0.05295   22.37  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.841 on 1642 degrees of freedom
# Multiple R-squared:  0.2336,	Adjusted R-squared:  0.2332 
# F-statistic: 500.6 on 1 and 1642 DF,  p-value: < 2.2e-16
#####################################################################


hist(model1$residuals)
head(model1$residuals) # Useful for returning model residual values

model2 <-lm(y ~ x)
summary(model2)

#####################################################################
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -1.372e-13 -1.237e-15  1.230e-15  3.407e-15  2.160e-14 
# 
# Coefficients:
#   Estimate Std. Error   t value Pr(>|t|)    
# (Intercept) 5.000e-01  2.891e-15 1.729e+14   <2e-16 ***
#   x           1.500e+00  4.971e-17 3.018e+16   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.435e-14 on 98 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 9.106e+32 on 1 and 98 DF,  p-value: < 2.2e-16
# 
# Warning message:
#   In summary.lm(model2) : essentially perfect fit: summary may be unreliable
####################################################################

d2$z.Tarsus <- scale(d2$Tarsus)
model3 <- lm(Mass ~ z.Tarsus, data = d2)
summary(model3)

####################################################################
# Call:
#   lm(formula = Mass ~ z.Tarsus, data = d2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.7271 -1.2202 -0.1302  1.1592  7.5036 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 27.77895    0.04539  611.94   <2e-16 ***
#   z.Tarsus     1.01596    0.04541   22.37   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.841 on 1642 degrees of freedom
# Multiple R-squared:  0.2336,	Adjusted R-squared:  0.2332 
# F-statistic: 500.6 on 1 and 1642 DF,  p-value: < 2.2e-16
####################################################################

plot(d2$Mass ~ d2$z.Tarsus, pch=19, cex=0.4)
abline(v = 0, lty = "dotted")
head(d)
str(d)

d$Sex <- as.numeric(d$Sex)
par(mfrow = c(1,2))
plot(d$Wing ~ d$Sex.1, ylab = "Wing (mm)")
plot(d$Wing ~ d$Sex, xlab = "Sex", xlim = c(-0.1, 1.1), ylab = "")
abline(lm(d$Wing ~ d$Sex), lwd = 2)
text(0.15, 76, "intercept")
text(0.9, 77.5, "slope", col = "red")

d4 <- subset(d, d$Wing!="NA")
m4 <- lm(Wing~Sex, data = d4)
t4 <- t.test(d4$Wing ~ d4$Sex, var.equal=TRUE)
t4 # T.test output
summary(m4) # Linear model output, which runs a t.test

par(mfrow=c(2,2))
plot(model3)

par(mfrow=c(2,2))
plot(m4)
