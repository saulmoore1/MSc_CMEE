#!/usr/bin/env R

################################
### Statistical Testing in R ###
################################
# Load exemplar data
attach(iris)
str(iris)

####################################
### Subset data for each species ###
####################################
# species <- list("setosa", "versicolor", "virginica")
# (Setosa, Versicolor, Virginica) <- subset(iris, iris$Species %in% species)
Setosa <- subset(iris, iris$Species == "setosa")
Versicolor <- subset(iris, iris$Species == "versicolor")
Virginica <- subset(iris, iris$Species == "virginica")

###########################################
### Visualise data & test for normality ###
###########################################
par(mfrow=c(3,4))
for (i in levels(iris$Species)) 
{
  data <- subset(iris, iris$Species == i)
  # print(head(data))
  hist(data$Sepal.Length, main="Sepal Length (cm)", ylab=i)
  hist(data$Sepal.Width, main="Sepal Width (cm)", ylab=i)
  hist(data$Petal.Length, main="Petal Length (cm)", ylab=i)
  hist(data$Petal.Width, main="Petal Width (cm)", ylab=i)
  print(shapiro.test(data$Sepal.Length))
  print(shapiro.test(data$Sepal.Width))
  print(shapiro.test(data$Petal.Length))
  print(shapiro.test(data$Petal.Width))
}

########################################
### Student's t-test for normal data ###
########################################
t.test(Setosa$Sepal.Length, Versicolor$Sepal.Length)

#######################################################
### Wilcoxon test for non-parametric or ranked data ###
#######################################################
wilcox.test(Setosa$Petal.Width, Versicolor$Petal.Width, alternative="two.sided", conf.level=0.95)


##################################################
### Linear regression (ordinary least squares) ###
##################################################
dev.off(); par(mfrow=c(1,1))
plot(iris$Sepal.Length ~ iris$Species)
sepal.length.lm <- lm(Sepal.Length ~ Species, data=iris)
summary(sepal.length.lm)
dev.off(); par(mfrow=c(2,2))
plot(sepal.length.lm)

############################################################
### Analysis of Variance (ANOVA) - 3 or more group means ###
############################################################
anova(sepal.length.lm)

###################################
### Tukey HSD post-hoc analysis ###
###################################
TukeyHSD(aov(sepal.length.lm))

install.packages("multcomp")
library(multcomp)
dev.off()
confplot <- glht(aov(sepal.length.lm), linfct = mcp(Species = "Tukey"))
plot(confplot, cex.axis=0.5)

plot(iris$Sepal.Length ~ iris$Species)
summary(lm(iris$Sepal.Length ~ iris$Species))
anova(lm(iris$Sepal.Length ~ iris$Species))
TukeyHSD(aov(lm(iris$Sepal.Length~ iris$Species)))

# install.packages("car", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
