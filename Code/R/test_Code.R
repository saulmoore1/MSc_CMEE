require(ggplot2)

#####################
# Most of the following questions will require you to load and use a 
# dataset available in a file called ExamData1.csv [insert 
# ExamData1.csv]. So first import an examine these data into your R workspace as a 
# data frame called "MyExamData". These data are measurements of flower characteristics 
# of species in the genus iris. 
# **Some questions may have more than one correct answer (but there is no negative marking)**

MyExamData <- read.csv("../data/ExamData1.csv")

# 1. Look at the following command: 
# MyExamData <- read.csv("../../MyDataDir/MyData.csv")
# From where is this dataset being loaded?
# a. From a current working directory called "MyDataDir"
# b. From a directory called "MyDataDir" one level higher than the current 
# working directory
# c. From a directory called "MyDataDir" three levels higher than the current 
# working directory  
# *d. From a directory called "MyDataDir" two levels higher than the current 
# working directory
#~ Correct: when using relative paths, each "../" denotes one directory level higher than the current directory 
# e. From a directory called "MyData" two levels higher than the current 
# working directory

# 2. What would be the appropriate R command to re-export the dataset you imported to a  
# tab-delimited file called "MyNewData.txt" into a directory called "data" 
# WITHOUT any row names or numbers? 
# a. write.csv(MyExamData,"../data/MyNewData.txt")
#~ Incorrect: this would result in comma-separated output WITH row numbers
# b. write.table(MyExamData,"../data/MyNewData.txt",sep = "\t")
#~ Incorrect: this would result in tab-separated output WITH row numbers
# c. write.csv(MyExamData,"../data/MyNewData.txt", sep = "\t",row.names = F)
#~ Incorrect: read.csv() cannot be forced to do tab-separated! 
#*d. write.table(MyExamData,"../data/MyNewData.txt",sep = "\t",row.names = F)
#~ Correct
# e. write.csv(MyExamData,"../data/MyNewData.txt",row.names = T)
#~ Incorrect: This will result in comma-separated, WITH row numbers! 
  
##############

str(MyExamData)

summary(MyExamData)

tapply(X = MyExamData$Petal.Length, INDEX = MyExamData$Species, FUN = mean)
tapply(X = MyExamData$Petal.Length, INDEX = MyExamData$Species, FUN = median)
tapply(X = MyExamData$Petal.Width, INDEX = MyExamData$Species, FUN = mean)
tapply(X = MyExamData$Petal.Width, INDEX = MyExamData$Species, FUN = median)
tapply(X = MyExamData$Sepal.Length, INDEX = MyExamData$Species, FUN = mean)
tapply(X = MyExamData$Sepal.Length, INDEX = MyExamData$Species, FUN = median)
tapply(X = MyExamData$Sepal.Width, INDEX = MyExamData$Species, FUN = mean)
tapply(X = MyExamData$Sepal.Width, INDEX = MyExamData$Species, FUN = median)

# 3. What are the mean and median of petal length for the three 
# species?
#*a. setosa mean: 1.462 cm; versicolor mean: 4.260cm;  virginica mean: 5.552cm
#    setosa median:1.50cm;  versicolor median 4.35cm; virginica median: 5.55cm 
#~ Correct: try tapply(X = MyExamData$Petal.Length, INDEX = 
# MyExamData$Species, FUN = mean) & tapply(X = MyExamData$Petal.Length, INDEX = MyExamData$Species, FUN = median)
# b. setosa mean: 2.262 cm; versicolor mean: 2.26cm;  virginica mean: 4.85cm
#    setosa median:1.60cm;  versicolor median 3.55cm; virginica median: 4.55cm 
# c. setosa mean: 1.462 cm; versicolor mean: 4.260cm;  virginica mean: 5.04cm
#    setosa median:3.63cm;  versicolor median 4.21cm; virginica median: 8.09cm 
# d. setosa mean: 2.591 cm; versicolor mean: 4.655cm;  virginica mean: 5.52cm
#    setosa median:3.63cm;  versicolor median 4.21cm; virginica median: 8.09cm 

##############
pdf("../graphics/fig1_setosa.pdf")
par(mfcol = c(2,2))
hist(MyExamData$Sepal.Length[which(MyExamData$Species == "setosa")], 
main = "Sepal Length")
hist(MyExamData$Sepal.Width[which(MyExamData$Species == "setosa")], 
main = "Sepal Width")
hist(MyExamData$Petal.Length[which(MyExamData$Species == 
"setosa")],main = "Petal length")
hist(MyExamData$Petal.Width[which(MyExamData$Species == "setosa")], 
main = "Petal Width")
dev.off()

pdf("../graphics/fig1_versicolor.pdf")
par(mfcol = c(2,2))
hist(MyExamData$Sepal.Length[which(MyExamData$Species == "versicolor")], 
main = "Sepal Length")
hist(MyExamData$Sepal.Width[which(MyExamData$Species == "versicolor")], 
main = "Sepal Width")
hist(MyExamData$Petal.Length[which(MyExamData$Species == 
"versicolor")],main = "Petal length")
hist(MyExamData$Petal.Width[which(MyExamData$Species == "versicolor")], 
main = "Petal Width")
dev.off()

pdf("../graphics/fig1_virginica.pdf")
par(mfcol = c(2,2))
hist(MyExamData$Sepal.Length[which(MyExamData$Species == "virginica")], 
main = "Sepal Length")
hist(MyExamData$Sepal.Width[which(MyExamData$Species == "virginica")], 
main = "Sepal Width")
hist(MyExamData$Petal.Length[which(MyExamData$Species == 
"virginica")],main = "Petal length")
hist(MyExamData$Petal.Width[which(MyExamData$Species == "virginica")], 
main = "Petal Width")
dev.off()

###############
# 4. Which of the following morphological measures tends to be right-skewed? 
#*a. petal width
#~Correct: plot the distributions of measures by species using hist 
# (e.g., hist(MyExamData$Petal.Width[which(MyExamData$Species == "versicolor")]))
# b. petal length
#~Incorrect: Not really - plot the distributions of measures by species using hist
# (e.g., hist(MyExamData$Petal.Length[which(MyExamData$Species == "versicolor")]))
# c. sepal width
#~Incorrect: Not really - plot the distributions of measures by species using hist
# (e.g., hist(MyExamData$Sepal.Width[which(MyExamData$Species == "versicolor")]))
# d. sepal length
#~Incorrect: Not really - plot the distributions of measures by species using hist
# (e.g., hist(MyExamData$Sepal.Length[which(MyExamData$Species == "versicolor")]))
# e. none of the above
#~Incorrect: Not really - plot the distributions of measures by species using hist
# (e.g., hist(MyExamData$Sepal.Length[which(MyExamData$Species == "versicolor")]))

###############

t.test(subset(MyExamData$Petal.Length,MyExamData$Species == "setosa"), 
+ subset(MyExamData$Petal.Length,MyExamData$Species == "virginica"), 
alternative = "less")

t.test(subset(MyExamData$Petal.Length,MyExamData$Species == "setosa"), 
+ subset(MyExamData$Petal.Length,MyExamData$Species == "virginica"), 
alternative = "less")

t.test(subset(MyExamData$Petal.Length,MyExamData$Species == "setosa"), 
+ subset(MyExamData$Petal.Length,MyExamData$Species == "virginica"))

###############
# 5. Perform a two-tailed t-test to test the hypothesis that Iris setosa has 
# petals that are significantly longer than Iris virginica. What can you conclude from the 
# test's result?
# a. Iris setosa has significantly longer petals than I. virginica: t = 47.1, df = 54.23, p-value < 2.2e-16
# b. Iris setosa has significantly shorter petals than I. virginica: t = 47.09, df = 54.23, p-value < 0.001   
#*c. Iris setosa has significantly shorter petals than I. virginica: t = -49.99, df = 58.60, p-value < 2.2e-16
#~Correct!
# d. Iris setosa and I. virginica do not have significantly different petal lengths: t = -49.986, df = 58.609, p-value = 1

##############################
# MAYBE CHANGE THIS QUESTION?
##############################
# 6. Which of the following would be more powerful than the two-sample 
# t-test you performed above to test the hypothesis that Iris setosa has 
# petals that are significantly longer than Iris virginica?
# a. A one-tailed, one-sample test that Iris setosa has longer petals than Iris virginica
#~Incorrect: Test has to be two-sample
# b. A two-tailed, two-sample test that Iris setosa has longer petals than Iris virginica
#~Incorrect: Test has to be one-tailed to be more powerful
# c. A two-tailed, one-sample test that Iris setosa has shorter petals than Iris virginica
#~Incorrect: Test has to be two-sample
# *d. A one-tailed, two-sample test that Iris setosa has shorter petals than Iris virginica
#~ Correct   
###############

var.test(subset(MyExamData$Sepal.Length,MyExamData$Species == "setosa"), 
+ subset(MyExamData$Sepal.Length,MyExamData$Species == "virginica"))

var.test(subset(MyExamData$Sepal.Length,MyExamData$Species == "setosa"), 
+ subset(MyExamData$Sepal.Length,MyExamData$Species == "versicolor"))

var.test(subset(MyExamData$Sepal.Length,MyExamData$Species == 
"versicolor"), 
+ subset(MyExamData$Sepal.Length,MyExamData$Species == "virginica"))

######################
# 7. Which pair(s) of the three species would pass a test of equal 
# variance for sepal length?
# a. None of the pairs
# ~Incorrect: perform var.test() for all species pairs
# b. All of the pairs
# ~Incorrect: perform var.test() for all species pairs
#*c. I. versicolor & virginica have statistically the same variance
# ~Correct: perform var.test() for all species pairs
# d. I. setosa and versicolor have statistically the same variance
#~Incorrect: perform var.test() for this species pair!
# e. Two pairs, I. setosa & versicolor and I. setosa & virginica, have statistically the same variance
# ~Incorrect: perform var.test() for these species pairs!

######################

MySepalW_LM <- lm(Sepal.Width ~ Species, data = MyExamData)
summary(MySepalW_LM)
anova(MySepalW_LM)
plot(MySepalW_LM)

######################
# 8. Perform an ANOVA to test whether the three species 
# have significantly different sepal widths. Which of the following 
# is/are true about the results: 
# a. Sepal widths do not differ between species
# ~ Incorrect - they are different - use lm(), anova() and summary()! 
# b. The model that the mean sepal width differs between species fits well, with a 
# p-value of 0.00056 and a F value pf 9.03
# ~ Incorrect - not the correct p and F values! 
#*c. The model that the mean sepal width differs between species fits well, with a 
# p-value < 2.2e-16 a F value pf 49.16
# ~ Correct - these are the correct p and F values; use lm(Sepal.Width ~ Species, data = MyExamData) and 
# examine  summary() of the fitted model object
# d. The fitted model explains about 56% of the total variance 
#~ Incorrect: use lm(Sepal.Width ~ Species, data = MyExamData) and 
# examine anova() and/or summary() of the fitted model object
#*e. The fitted model explains about 40% of the total variance 
#~ Correct: use lm(Sepal.Width ~ Species, data = MyExamData) and 
# examine anova() and/or summary() of the fitted modelobject

# 9. Why use an ANOVA and not a pairwise t-test to test whether the three species 
# have significantly different sepal widths? 
# a. Just a personal preference - either would do the trick
#~Incorrect!
# b. Because the datasets have unequal variances, and a variance-ratio 
# test is better 
#~Incorrect: not much to do with unequal variances in making this decision!
#*c. Because a t-test would require three pairwise comparisons, 
# inflating the chances of committing a type I error
#~ Correct: See p 66 of notes
# d. Because there are three potential pairwise comparisons, and the chances of 
# committing a type II error are inflated 
#~ incorrect: Type I error (falsely rejecting true null hypothesis), not Type 
# II error (failing to reject an incorrect null hypothesis) would be inflated 

######################

morpho <- c(1, 2, 3, 4)

pdf("../graphics/fig2_pairs.pdf")
pairs(MyExamData[,morpho], col=MyExamData$Species)
legend(0, 1, as.vector(unique(MyExamData$Species)))
dev.off()

pdf("../graphics/fig2_pairs_log.pdf")
pairs(log(MyExamData[,morpho]), col=MyExamData$Species)
legend(0, 1, as.vector(unique(MyExamData$Species)))
dev.off()

# plot((MyExamData$Sepal.Length),log(MyExamData$Petal.Length))
MyExamData_setosa <- subset(MyExamData,Species == "setosa")
MyExamData_virginica <- subset(MyExamData,Species == "virginica")
MyExamData_versicolor <- subset(MyExamData,Species == "versicolor")

# Log is more appropriate for allometric analysis, but give the students a break!
pdf("../graphics/fig3_lm_setosa.pdf")
plot((MyExamData_setosa$Sepal.Length),(MyExamData_setosa$Petal.Length), 
xlab = "Sepal length", ylab = "Petal Length", font.lab=2, cex.lab=1.5)
MyLM_setosa <- lm(Petal.Length~Sepal.Length,data = MyExamData_setosa)
abline(MyLM_setosa)
dev.off()

pdf("../graphics/fig4_MyLM_setosa.pdf")
par(mfcol = c(2,2))
plot(MyLM_setosa,font.lab=2, cex.lab=1.5)
dev.off()
summary(MyLM_setosa)
anova(MyLM_setosa)
confint(MyLM_setosa,level=0.95)
##

pdf("../graphics/fig3_lm_virginica.pdf")
plot((MyExamData_virginica$Sepal.Length),(MyExamData_virginica$Petal.Length), 
xlab = "Sepal length", ylab = "Petal Length",  font.lab=2, cex.lab=1.5)
MyLM_virginica <- lm(Petal.Length~Sepal.Length,data = MyExamData_virginica)
abline(MyLM_virginica)
dev.off()

pdf("../graphics/fig4_MyLM_virginica.pdf")
par(mfcol = c(2,2))
plot(MyLM_virginica,font.lab=2, cex.lab=1.5)
dev.off()

summary(MyLM_virginica)
anova(MyLM_virginica)
confint(MyLM_virginica,level=0.95)

pdf("../graphics/fig3_lm_versicolor.pdf")
plot((MyExamData_versicolor$Sepal.Length),(MyExamData_versicolor$Petal.Length), 
xlab = "Sepal length", ylab = "Petal Length",  font.lab=2, cex.lab=1.5)
MyLM_versicolor <- lm(Petal.Length~Sepal.Length,data = MyExamData_versicolor)
abline(MyLM_versicolor)
dev.off()

pdf("../graphics/fig4_MyLM_versicolor.pdf")
par(mfcol = c(2,2))
plot(MyLM_versicolor,font.lab=2, cex.lab=1.5)
dev.off()

summary(MyLM_versicolor)
anova(MyLM_versicolor)
confint(MyLM_versicolor,level=0.95)

######################
# 10. Perform a regression analysis to determine how well sepal length 
# predicts petal length in each of the three species (so three 
# different regression analyses). Use a p-value for significance 
# cut-off = 0.05.  Which of the following are correct:

# *a. Sepal length does not predict petal length significantly in Iris setosa: 
# slope= 0.13, t value =  1.9, F-test p value > 0.05
# *b. Sepal length predicts petal length significantly in Iris virginica: 
# slope= 0.75, t value = 11.9, F-test p value < 0.05
# c. Sepal length predicts petal length significantly in Iris virginica: 
# slope= 0.75008, t value = 11.901, F-test p value > .05
# d. Sepal length predicts petal length significantly in Iris versicolor: 
# slope= 0.099, t value = 1.901, F-test p value < .05
# e. None of the above

# 11. Look at this figure showing a regression line fitted to data:
# @fig3_lm_versicolor.png 
# Which species does this belong to? 
# a. None of the three species
# b. Iris setosa
# c. Iris virginica
#*d. Iris versicolor

# 12. Examine the diagnostic plots of the petal length vs. sepal length 
# regression analyses for the three species. Which dataset shows the 
# highest deviations from the expected quantiles of the Normal 
# distribution? 
#*a. Iris setosa
# ~ Correct - check out the normal Q-Q plot
# b. Iris virginica
# ~ Incorrect - check out the normal Q-Q plot
# c. Iris versicolor
# ~ Incorrect - check out the normal Q-Q plot
###################

# 13. Look at the following two lines of R code: 

# > lm(Petal.Length ~ Sepal.Width, data = MyExamData)
# > lm(Petal.Length ~ Sepal.Width * Species, data = MyExamData)

# What are these commands doing?

# a. First command performs a regression analysis, second performs an ANOVA    
# b. First command performs an ANOVA, second performs a regression analysis    
#*c. First command performs a regression analysis, second performs an ANCOVA
#~ Correct
# d. Both commands perform an ANCOVA
# e. Both models perform an ANOVA

#######################

SWSL_LM <- lm(Sepal.Length ~ Sepal.Width, data = MyExamData)
anova(SWSL_LM)
summary(SWSL_LM)

pdf("../graphics/fig4_SWSL_LM.pdf")
plot(MyExamData$Sepal.Width,MyExamData$Sepal.Length, 
xlab = "Sepal Width", ylab = "Sepal Length",  font.lab=2, cex.lab=1.5)
abline(SWSL_LM)
dev.off()

SpeciesSWSL_LM <- lm(Sepal.Length ~ Sepal.Width * Species, data = MyExamData)
anova(SpeciesSWSL_LM)
summary(SpeciesSWSL_LM)

anova(SpeciesSWSL_LM, SWSL_LM)

rng <- range(MyExamData$Sepal.Width)
SW_toFit <- seq(rng[1], rng[2], length = 100)

SetoSLPred <- predict(SpeciesSWSL_LM, newdata = data.frame(Sepal.Width = SW_toFit, Species = "setosa"), se.fit = TRUE)
VirgSLPred <- predict(SpeciesSWSL_LM, newdata = data.frame(Sepal.Width = SW_toFit, Species = "virginica"), se.fit = TRUE)
VersSLPred <- predict(SpeciesSWSL_LM, newdata = data.frame(Sepal.Width = SW_toFit, Species = "versicolor"), se.fit = TRUE)

pdf("../graphics/fig4_SpeciesSWSL_LM.pdf")
p <- ggplot(MyExamData, aes(x = Sepal.Width, y = Sepal.Length,
colour = Species)) + 
geom_point(size=I(2)) + 
geom_smooth(method = "lm") + 
labs(x = "Sepal Width", y = "Sepal Length")+ 
scale_colour_manual(values = c("black","blue", "red"))+theme_bw() +
theme(aspect.ratio=1, axis.title.x=element_text(size=14, face="bold"), 
axis.title.y=element_text(size=14, face="bold"))

print(p)
dev.off()

#######################

# 14. Fit the following two models to the iris data and compare them: 
# Model 1: Sepal.Length ~ Sepal.Width * Species
# Model 2: Sepal.Length ~ Sepal.Width
# What can you infer from this analysis?
# *a. Model 1  is better than Model 2 : the dependence of sepal length 
# on sepal width varies by species
#~ Correct
# b. Model 2  is better than Model 1 : the dependence of sepal length 
# on sepal width does not vary by species
# c. Model 1  is better than Model 2 : the dependence of sepal length 
# on sepal width does not vary by species
# d. The two models are no different in how well they fit the data: the dependence of sepal length 
# on sepal width varies by species

# 15. Examine the following figure with regression lines fitted 
# through the data and the corresponding R output: 

# @fig4_SpeciesSWSL_LM.png

# Call:
# lm(formula = Sepal.Length ~ Sepal.Width * Species, data = MyExamData)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.26067 -0.25861 -0.03305  0.18929  1.44917 
#
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     2.6390     0.5715   4.618 8.53e-06 ***
# Sepal.Width                     0.6905     0.1657   4.166 5.31e-05 ***
# Speciesversicolor               0.9007     0.7988   1.128    0.261    
# Speciesvirginica                1.2678     0.8162   1.553    0.123    
# Sepal.Width:Speciesversicolor   0.1746     0.2599   0.672    0.503    
# Sepal.Width:Speciesvirginica    0.2110     0.2558   0.825    0.411    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.4397 on 144 degrees of freedom
# Multiple R-squared:  0.7274,	Adjusted R-squared:  0.718 
# F-statistic: 76.87 on 5 and 144 DF,  p-value: < 2.2e-16

# What are the fitted regression equations for the three species?
# a. setosa:     Sepal.Length = 1.89 + 0.69 x Sepal.Width
#     versicolor: Sepal.Length = 0.29 + 1.99 x Sepal.Width
#     virginica:  Sepal.Length = 1.91 + 2.30 x Sepal.Width
#~Incorrect: Some really random values here!
# b. setosa:     Sepal.Length = 2.64 + 0.18 x Sepal.Width
#     versicolor: Sepal.Length = 0.90 + 0.90 x Sepal.Width
#     virginica:  Sepal.Length = 1.27 + 0.21 x Sepal.Width
#~Incorrect: Only the setosa intercept value is right!
# c. setosa:     Sepal.Length = 0.69 + 2.64 x Sepal.Width
#     versicolor: Sepal.Length = 0.87 + 3.54 x Sepal.Width
#     virginica:  Sepal.Length = 0.90 + 3.91 x Sepal.Width
#~Incorrect: Confusion of intercept vs. slope (values are switched)
#*d. setosa:     Sepal.Length = 2.64 + 0.69 x Sepal.Width
#     versicolor: Sepal.Length = 3.54 + 0.87 x Sepal.Width
#     virginica:  Sepal.Length = 3.91 + 0.90 x Sepal.Width
#~Correct!
# e. setosa:     Sepal.Length = 2.63 + 0.69 x Sepal.Width
#     versicolor: Sepal.Length = 2.63 + 3.54 x Sepal.Width
#     virginica:  Sepal.Length = 2.63 + 3.91 x Sepal.Width
#~Incorrect: Only equation for setosa is correct

############################

PWPL_LM <- lm(Petal.Length ~ Petal.Width, data = MyExamData)
anova(PWPL_LM)
summary(PWPL_LM)

pdf("../graphics/fig4_PWPL_LM.pdf")
plot(MyExamData$Petal.Width,MyExamData$Petal.Length, 
xlab = "Petal Width", ylab = "Petal Length",  font.lab=2, cex.lab=1.5)
abline(PWPL_LM)
dev.off()

SpeciesPWPL_LM <- lm(Petal.Length ~ Petal.Width * Species, data = MyExamData)
anova(SpeciesPWPL_LM)
summary(SpeciesPWPL_LM)

anova(PWPL_LM, SpeciesPWPL_LM)

rng <- range(MyExamData$Petal.Width)
PW_toFit <- seq(rng[1], rng[2], length = 100)

SetoPred <- predict(SpeciesPWPL_LM, newdata = data.frame(Petal.Width = PW_toFit, Species = "setosa"), se.fit = TRUE)
VirgPred <- predict(SpeciesPWPL_LM, newdata = data.frame(Petal.Width = PW_toFit, Species = "virginica"), se.fit = TRUE)
VersPred <- predict(SpeciesPWPL_LM, newdata = data.frame(Petal.Width = PW_toFit, Species = "versicolor"), se.fit = TRUE)

plot(Petal.Length ~ Petal.Width, data = MyExamData, col = Species,
xlab = "Petal Width", ylab = "Petal Length",  font.lab=2, cex.lab=1.5)
lines(SetoPred$fit ~ PW_toFit, col = "black")
lines(VirgPred$fit ~ PW_toFit, col = "red")
lines(VersPred$fit ~ PW_toFit, col = "green")
########################

# 16. What would be the appropriate R code/command for testing whether sepal length 
# and sepal width together predict petal length, WITHOUT any 
# interactions between the two predictor variables? 
# a. lm(Petal.Length ~ Sepal.Length*Sepal.Width, data = MyExamData)
#~ Incorrect: this would also include interactions
#*b. lm(Petal.Length ~ Sepal.Length+Sepal.Width, data = MyExamData)
#~ Correct!
# c. lm(Sepal.Length ~ Petal.Length+Sepal.Width, data = MyExamData)
#~ Incorrect: Wrong response variable!
# d. lm(Sepal.Length ~ Petal.Length*Sepal.Width, data = MyExamData)
#~ Incorrect: Wrong response variable + includes interactions!
# e. lm(Sepal.Length ~ Petal.Length*Sepal.Width, data = MyExamData)
#~ Incorrect: Wrong response variable + includes interactions!

# 17. Have a close look at the following plot:
# @fig2_pairs.png
# Why would it be inappropriate to test whether sepal length 
# and sepal width together predict petal length?
# a. Because these measures belong to the same species
#~ Incorrect
# b. Because petal length is not appropriate as a response variable - 
# it should be a predictor variable 
#~ Incorrect
# c. Because petal length and petal width are strongly correlated  
#~ Incorrect: that's not a problem at all!
#*d. Because sepal length and sepal width are strongly correlated  
#~ Correct: the predictor variables should be independent of each other
# e. All of the above  
#~ Incorrect

#########################################

glm_PW <- glm(formula = Petal.Width ~ Species,
                         family  = Gamma(link = "log"),
                         data    = MyExamData)
summary(glm_PW)

pdf("../graphics/glm_PW.pdf")
par(mfcol = c(2,2))
plot(glm_PW,font.lab=2, cex.lab=1.5)
dev.off()

glm_PW2 <- glm(formula = Petal.Width ~ Species,
                         family  = gaussian,
                         data    = MyExamData)

summary(glm_PW2)

pdf("../graphics/glm_PW2.pdf")
par(mfcol = c(2,2))
plot(glm_PW2,font.lab=2, cex.lab=1.5)
dev.off()
                         
lm_PW <- lm(Petal.Width ~ Species, data = MyExamData)
summary(lm_PW)

# glm_Gamma_PWSW_vers <- glm(formula = Petal.Width ~ Sepal.Width,
#                          family  = Gamma(link = "log"),
#                          data    = MyExamData_virginica)
# glm_Gamma_PWSW_virg <- glm(formula = Petal.Width ~ Sepal.Width,
#                          family  = Gamma(link = "log"),
#                          data    = MyExamData_versicolor)
                         
# summary(glm_Gamma_PWSW_virg)
# summary(glm_Gamma_PWSW_vers)

# 18. What is the following code doing?
# glm(formula = Petal.Width ~ Species, family  = Gamma(link = "log"), data = MyExamData) 
#a. Fitting a linear model to test whether petal widths differ across 
# species
#~Incorrect: this is a GLM fitting command 
#*b. Fitting a generalized linear model to test whether petal widths differ across 
# species, with a gamma distribution because petal 
# widths tend to be right skewed
#~Correct
#c. Fitting a generalized linear model to test whether petal widths differ across 
# species, with a normal distribution because petal 
# widths tend to be right skewed
#~Incorrect: the family is Gamma!
#d. Fitting a generalized linear model to test whether petal widths differ across 
# species, with a normal distribution because petal 
# widths tend to be normally distributed
#~Incorrect: Petal Widths tend to be right-skewed!
#e. Fitting a linear model to test whether petal widths differ across 
# species, with a normal distribution because sepal 
# widths tend to be right-skewed
#~Incorrect: Sepal Widths are quite normally distributed, and a GLM 
# would not do the ob in that case anyway, as sepal width is a 
# predictor, not the response variable

# 19. Look at the following two lines of R code:
# glm(formula = Petal.Width ~ Species, family  = gaussian, data = MyExamData)
# lm(Petal.Width ~ Species, data = MyExamData)
# Which of the following statement(s) is/are correct about these lines 
# of code ?
# a. glm is the correct way to go here, not the lm 
#~ Incorrect: both are doing the same thing, and both are wrong because 
# petal widths are right-skewed!
# b. lm is the correct way to go here, not the glm 
#~ Incorrect: both are doing the same thing, and both are wrong because 
# petal widths are right-skewed!
#*c. The two commands are essentially doing the same statistical test 
# ~Correct: GLM with gaussian (normal) distribution family specification is same as a linear model!
#*d. Neither are the correct way to go here because petal widths are right-skewed
#~ Incorrect: both are doing the same thing, and both are wrong because 
# petal widths are right-skewed!
# e. an ancova would be more appropriate compared to these two tests 
#~ Incorrect: A GLM with an appropriate distribution family choice (not 
# gaussian!) is the best method for the petal width data 

pdf("../graphics/fig1_PW.pdf")
par(mfcol = c(1,3))
hist(sqrt(MyExamData$Petal.Width[which(MyExamData$Species == "setosa")]), 
main = "Petal Width - setosa")
hist(sqrt(MyExamData$Petal.Width[which(MyExamData$Species == 
"virginica")]), 
main = "Petal Width - virginica")
hist(sqrt(MyExamData$Petal.Width[which(MyExamData$Species == 
"versicolor")]), 
main = "Petal Width - versicolor")
dev.off()

# 20. What else could you do to overcome the problem of non-normality 
# (right-skewness) if you needed to test the hypothesis that petal 
# widths differ across species?
# a. Nothing to be done - only a GLM would be appropriate 
#~ Incorrect: a log or weaker transform would be appropriate, or a 
# non-parametric test
#*b. Use a Wilcoxon test 
#~ Correct: this is an approriate non-parametric test to test for 
# differences between distributions 
#*c. Log-transform the data and fit a linear model 
#~ Correct
#*d. Square-root transform the data and fit a linear model 
#~ Correct: in fact, better than log transform as it is a  weaker yet 
# sufficient transformation 
#e. Fit an ANOVA 
#~ Incorrect: same as fitting a linear model without any transformation!  
