#################################################################
########## Test code for the IC Y1 stats test for 2018 ########## 
#################################################################

library(ggplot2)

# sample(c("a","b","c","d","e"),1) # generate correct answer at random

MyData <- read.csv("../Data/TestData.csv")

# NOTE THAT THE FOLLOWING QUESTIONS MAY HAVE MORE THAN ONE CORRECT ANSWER. 
# THERE IS NO NEGATIVE MARKING.

###############################
############## Q1 #############
###############################
length(which(is.na(MyData)))
which(is.na(MyData$Mass..g.))

# 1. Import the data in the TestData.csv [insert TestData.csv] file into your R
# workspace. This is a global dataset on basal metabolic rates of mammals.
# The "Mass..g." column contains the body mass of each mammal in grams, while
# "BMR..W." and "BMR..kJ.h." contain the metabolic rate of each mammal in
# Watts and Kilojoules/hr respectively. All other columns names are
# self-explanatory. 
#
# How many NAs are there in this whole dataset, and which row(s) in "Mass..g."
# contain(s) NA(s)?

# a. 23 NAs in all, no NAs in "Mass..g."
# ~ Incorrect: use length(which(is.na(MyData))) for total NAs and which(is.na(MyData$Mass..g.)) to find the rows with NAs in that column

# b. 5 NAs in all, row 45 and 92 of "Mass..g." are NAs 
# ~ Incorrect: use length(which(is.na(MyData))) for total NAs and which(is.na(MyData$Mass..g.)) to find the rows with NAs in that column

# c. 12 NAs in all, no NAs in "Mass..g."
# ~ Incorrect: use length(which(is.na(MyData))) for total NAs and which(is.na(MyData$Mass..g.)) to find the rows with NAs in that column

# *d. 3 NAs in all, row 342 of "Mass..g." is NA 
# ~ Correct: use length(which(is.na(MyData))) for total NAs and which(is.na(MyData$Mass..g.)) to find the rows with NAs in that column

# e. There are no NAs in this dataset
# ~ Incorrect: use length(which(is.na(MyData))) for total NAs and which(is.na(MyData$Mass..g.)) to find the rows with NAs in that column

###############################
############## Q2 #############
###############################

MyData <- na.omit(MyData)
# str(MyData)
# summary(MyData)

png("../Figures/hists.png")
par(mfrow = c(2,1))
hist(log(MyData$Mass..g.))
hist(log(MyData$BMR..W.))
dev.off()

summary(log(MyData$Mass..g.)) # To get mean, median etc

# 2. Eliminate all NAs in the dataset. Now answer the following question: what is the shape of the distribution of log-transformed body masses of mammals, and what is the median value of this distribution of log-transformed masses?

# a. Left-skewed, with median = 112 grams
# ~ Incorrect: Right-skewed distribution - use hist(log(MyData$Mass..g.)), and to get median, use summary(log(MyData$Mass..g.)) or median(log(MyData$Mass..g.))
# *b. Right-skewed, with median = 4.72 grams
# ~ Correct: Use hist(log(MyData$Mass..g.)), and to get median, use summary(log(MyData$Mass..g.)) or median(log(MyData$Mass..g.))
# c. Left-skewed, with median = 2.52 grams
# ~ Incorrect: Right-skewed distribution - use hist(log(MyData$Mass..g.)), and to get median, use summary(log(MyData$Mass..g.)) or median(log(MyData$Mass..g.))
# d. Right-skewed, with median = 5.38 grams
# ~ Incorrect: Right-skewed distribution - use hist(log(MyData$Mass..g.)), and to get median, use summary(log(MyData$Mass..g.)) or median(log(MyData$Mass..g.))
# e. Left-skewed, with median = 5.3790 grams
# ~ Incorrect: Right-skewed distribution - use hist(log(MyData$Mass..g.)), and to get median, use summary(log(MyData$Mass..g.)) or median(log(MyData$Mass..g.))

###############################
############## Q3 #############
###############################
summary(MyData$Family)

# 3. Which of the following sets of 5 mammalian Families in the dataset have the
# most number of samples/measures of body mass? 

#a. Mustelidae, Muridae, Heteromyidae, Phyllostomidae, Soricidae
# # ~ Incorrect: use sort(summary(MyData$Family),decreasing=T) or sort(summary(MyData$Family),decreasing=T)[1:5]
#b. Muridae, Soricidae, Phyllostomidae, Soricidae, Mustelidae
# # ~ Incorrect: use sort(summary(MyData$Family),decreasing=T) or sort(summary(MyData$Family),decreasing=T)[1:5]
#c. Heteromyidae, Muridae, Sciuridae, Phyllostomidae, Soricidae
# # ~ Incorrect: use sort(summary(MyData$Family),decreasing=T) or sort(summary(MyData$Family),decreasing=T)[1:5]
#*d. Cricetidae, Muridae, Sciuridae, Phyllostomidae, Soricidae
# # ~ Correct: use sort(summary(MyData$Family),decreasing=T) or sort(summary(MyData$Family),decreasing=T)[1:5]
#e. Heteromyidae Dasyuridae Pteropodidae Mustelidae Canidae
# # ~ Incorrect: use sort(summary(MyData$Family),decreasing=T) or sort(summary(MyData$Family),decreasing=T)[1:5]

########################### 
# par(mar=c(12,5,1,1))
# plot(log(MyData$Mass..g.)~ Family, data = MyData, las = 2, xlab = "") # remove x axis label
# mtext("Family", side=1, line=10, las = 1) # add manually
# plot(log(MyData$Mass..g.)~ Family, data = MyData, las = 2)

###############################
############## Q4 #############
###############################

# summary(log(MyData$Mass..g.[which(MyData$Family == "Heteromyidae" | MyData$Family == "Muridae")]))
# summary(log(MyData$Mass..g.[which(MyData$Family == "Cricetidae")]))

DataHetMur <- subset(MyData, Family == "Heteromyidae" | Family == "Muridae")
DataCric <- subset(MyData, Family == "Cricetidae")

hist(log(DataHetMur$Mass..g.))
hist(log(DataCric$Mass..g.))

# 4. Heteromyidae, Muridae, and Cricetidae are three families of rodents. Compare the the distributions of log body masses of Heteromyidae + Muridae combined and Cricetidae alone. Which of the following statements is correct? 

#*a. Both Heteromyidae+Muridae and Cricetidae have slightly right-skewed distributions of log-transformed body mass 
# # ~ Correct: use hist(log())
#b. Both Heteromyidae+Muridae and Cricetidae have slightly left-skewed distributions of log-transformed body mass
# # ~ Incorrect: use hist(log())
#c. Heteromyidae+Muridae have slightly left-skewed, while Cricetidae have slightly right-skewed distributions of log-transformed body mass
# # ~ Incorrect: use hist(log())
#d. Heteromyidae+Muridae have slightly right-skewed, while Cricetidae have slightly left-skewed distributions of log-transformed body mass
# # ~ Incorrect: use hist(log())
#e. Neither Heteromyidae+Muridae nor Cricetidae have skewed distributions of log-transformed body mass
# # ~ Incorrect: use hist(log())

###############################
############## Q5 #############
###############################

t.test(DataHetMur$Mass..g., DataCric$Mass..g.)

# > t.test(DataHetMur$Mass..g., DataCric$Mass..g.)

# Welch Two Sample t-test

# data: DataHetMur$Mass..g. and DataCric$Mass..g.
# t = 0.50189, df = 155.51, p-value = 0.6165
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -28.22541 47.45412
# sample estimates:
# mean of x mean of y
# 80.38289 70.76854

# Welch's t-test is more robust (less likely to lead to a Type II error) than Student's t-test when sample sizes and variances are unequal between groups. 

#5. Suppose you wanted to statistically test whether log Heteromyidae+Muridae vs Cricetidae weights are significantly different using a t-test. Which of the following approaches would be the correct one?

# a. Log transform data and perform Student's t-test. 
# # ~ Incorrect: See correct answer's explanation

# b. Perform Student's t-test on un-transformed data 
# # ~ Incorrect: See correct answer's explanation 

# *c. Log transform data and perform Welch's t-test. 
# # ~ Correct: Firstly, log-transformation (just about) normalizes the data. Secondly, Welch's t-test is more robust (less likely to lead to a Type II error) than Student's t-test when sample sizes (which is definitely true in this case) and variances are unequal between groups. 

# d. Perform Welch's t-test on un-transformed data 
# # ~ Incorrect: See correct answer's explanation 

# e. Perform a Chi-squared test because the data are not normally distributed 
# # ~ Incorrect: The Chi-squared test is completely inappropriate here. 

###############################
############## Q6 #############
###############################

# 6. What alternatives to the t-test could you use to test the hypothesis that Heteromyidae+Muridae vs. Cricetidae weights are significantly different? 
# a. Chi-Squared test on log-transformed body mass data. 
# # ~ Incorrect 

# b. Chi-Squared test on raw (un-transformed) body mass data. 
# # ~ Incorrect 

# c. ANOVA on raw (un-transformed) body mass data. 
# # ~ Incorrect: Normality assumption would be grossly violated 

# *d. Wilcox test on log-transformed body mass data. 
# # ~ Correct 

# *e. Wilcox test on raw (un-transformed) body mass data. 
# # ~ Correct: Wilcox test on log-transformed body masses vs. raw (un-transformed) data would yield the same result.

###############################
############## Q7 #############
###############################

DataSubs <- subset(MyData, Family == "Heteromyidae" | Family == "Muridae" | Family == "Cricetidae" | Family == "Soricidae")

png("../Figures/Mass_MetabolicRate.png",width = 8, height = 6.5, units = 'in', res = 600)
ggplot(DataSubs,aes(y = log(BMR..W.), x = log(Mass..g.), colour = Family,shape=Family)) + geom_point(size = 2.5,alpha = .8) + geom_smooth(method="lm", fill=NA)
dev.off()

MyLM_HetMur <- lm(log(BMR..W.)~ log(Mass..g.), data = DataHetMur)

summary(MyLM_HetMur)

MyLM_Cric <- lm(log(BMR..W.) ~ log(Mass..g.), data = DataCric)
summary(MyLM_Cric)

# 7. Metabolic rate (rate of energy use) of mammals (measured in units of Watts or Joules / s) generally scales positively (increases) with body mass, which can be quantified by fitting a linear model to the relationship between log Body Mass as independent variable and log Metabolic Rate as dependent variable. Perform this analysis for the Heteromyidae+Muridae vs. Cricetidae data subsets. Which of the following can you say based on this analysis?

# a. Metabolic rate increases more rapidly with body mass in Heteromyidae+Muridae (slope = -3.99) relative to Cricetidae (slope = -3.46).
# # ~ Incorrect: This is the intercept

# *b. Metabolic rate increases more rapidly with body mass in Heteromyidae+Muridae (slope = 0.72) relative to Cricetidae (slope = 0.67).
# # ~ Correct: perform the appropriate lm()

# c. Metabolic rate decreases more rapidly with body mass in Heteromyidae+Muridae (slope = -3.99) relative to Cricetidae (slope = -3.46).
# # ~ Incorrect: This is the intercept, and the relationship is positive, not negative

# d. Metabolic rate increases with body mass in Heteromyidae+Muridae in exactly the same way as in Cricetidae .
# # ~ Incorrect: The slopes of the log-log relationship differ

# e. Metabolic rate increases more slowly with body mass in Heteromyidae+Muridae (slope = 0.67) relative to Cricetidae (slope = 0.72).
# # ~ Incorrect: perform the appropriate lm() and look at the slopes coefficients

###############################
############## Q8 #############
###############################

anova(MyLM_HetMur)
anova(MyLM_Cric)

# Analysis of Variance Table

# Response: log(BMR..W.)
# Df Sum Sq Mean Sq F value Pr(>F)
# log(Mass..g.) 1 29.561 29.5609 489.03 < 2.2e-16 ***
# Residuals 87 5.259 0.0604
# ---
# Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# 8. What is the Total and Residual sum of squares (TSS and RSS) for the linear model fitted to log Body Mass as independent variable and log Metabolic Rate as dependent variable for the Cricetidae data subset?

# a. TSS = 42.72, RSS = 22.907
# ~ Incorrect
# b. TSS = 93.32, RSS = 9.235
# ~ Incorrect
# *c. TSS = 34.82, RSS = 5.259
# ~ Correct: use anova() on the object outputted by lm() for RSS and ESS; then TSS = RSS + ESS. Or you can directly calculate the TSS by fitting a null model (see p. 28-30 of the stats notes)
# d. TSS = 17.062, RSS = 28.144
# ~ Incorrect: TSS cannot be less than RSS! 
# e. TSS = 32.558, RSS = 5.912
# ~ Incorrect

###############################
############## Q9 #############
###############################

confint(MyLM_HetMur,level=0.99)
confint(MyLM_Cric,level=0.99)

# 9. Which of the following statements are true about the 99% confidence intervals around the slope of the regression of log Metabolic Rate (dependent variable) on log Body Mass (independent variable) for Heteromyidae+Muridae ?

# *a. They include the value of 0.75
# ~ Correct: bounds are 0.6247762, 0.8130889; use confint(data,level=0.99)

# b. They do not overlap with the confidence intervals of the regression slope of log Metabolic Rate vs. log Body Mass for Cricetidae 
# ~ Incorrect: They do overlap

# *c. The upper bound is 0.0941589 greater than the slope coefficient
# ~ Correct: subtract the upper bound 0.8130889 

# d. They include a value of 0 
# ~ Incorrect

# e. They are same as the standard errors
# ~ Incorrect! CIs and SEs are not equivalent

###############################
############# Q10 #############
###############################

######### Regression: Which ones are outliers? ######

# pdf("../Figures/Diag_HetMur.pdf")
par(mfcol = c(2,2))
plot(MyLM_HetMur)
# dev.off()

# pdf("../Figures/Diag_Cric.pdf")
par(mfcol = c(2,2))
plot(MyLM_Cric)
# dev.off()

# 10. Examine the diagnostic plots of the linear model fitted to the Cricetidae data with log Metabolic Rate as dependent variable and log Body Mass as independent variable. How many outlier data points can you identify?
# a. 4
# ~ Incorrect: there are no significant outliers based on the Residuals vs. Leverage plot 
# b. 3
# ~ Incorrect: there are no significant outliers based on the Residuals vs. Leverage plot 
# c. 2
# ~ Incorrect: there are no significant outliers based on the Residuals vs. Leverage plot 
# d. 1
# ~ Incorrect: there are no significant outliers based on the Residuals vs. Leverage plot 
# e. 0
# ~ Correct: there are no significant outliers based on the Residuals vs. Leverage plot; all points are within the cook's distance lines. 

###############################
############# Q11 #############
###############################

DataSubs <- subset(MyData, Family == "Heteromyidae" | Family == "Muridae" | Family == "Cricetidae" | Family == "Sciuridae")

DataSubs$SpecificMR <- DataSubs$BMR..W./DataSubs$Mass..g.
# png("../Figures/MassSpecific_MetabolicRate.png",width = 8, height = 6.5, units = 'in', res = 600)

ggplot(DataSubs,aes(x = log(SpecificMR), fill = Family)) + geom_density(alpha=0.75) 
# dev.off()

MyANOVA <- lm(SpecificMR ~ Family, data = DataSubs)
summary(MyANOVA)
anova(MyANOVA)

par(mfcol = c(2,2)); plot(MyANOVA)

# Call:
# lm(formula = SpecificMR ~ Family, data = DataSubs)

# Residuals:
#        Min         1Q     Median         3Q        Max
# -0.0060046 -0.0018805 -0.0005537  0.0011120  0.0105256

# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)
# (Intercept)         0.0095728  0.0003325  28.787  < 2e-16 ***
# FamilyHeteromyidae -0.0023470  0.0007470  -3.142  0.00194 **
# FamilyMuridae      -0.0030807  0.0005411  -5.693 4.48e-08 ***
# FamilySciuridae    -0.0050483  0.0006197  -8.147 4.22e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.003137 on 197 degrees of freedom
# Multiple R-squared:  0.2806,    Adjusted R-squared:  0.2696
# F-statistic: 25.61 on 3 and 197 DF,  p-value: 4.924e-14

# 11. In order to compare metabolic rates across major mammalian groups, given that metabolic rates scale with body mass, we can divide the metabolic rate of each species by its body mass, to obtain a new variable: the mass-specific metabolic rate (units of Watts / grams, or Joules / (seconds x grams)). How would you test the hypothesis that mean mass-specific metabolic rate, in units of Watts / grams, differs across the four most data-rich Rodent (Order Rodentia) families (Cricetidae, Muridae, Sciuridae, and Heteromyidae)?

# a. Fit a linear model with those Rodent Families as the dependent variable, and mass-specific metabolic rate as the independent variable
# ~ Incorrect
# *b. Fit a linear model with those Rodent Families as the independent variable, and log mass-specific metabolic rate as the dependent variable
# ~ Correct: Same as an ANOVA; log transformation is needed as distributions are not quite normal
# c. Fit a linear model with those Rodent Families as the independent variable, and mass-specific metabolic rate as the dependent variable
# ~ Incorrect
# d. Perform a t-test for log mass-specific metabolic rate between all pairs of Rodent Families
# ~ Incorrect: Multiple comparisons should not be made in this way
# e. Perform a t-test for mass-specific metabolic rate between all pairs of Rodent Families
# ~ Incorrect: Multiple comparisons should not be made in this way

###############################
############# Q12 #############
###############################

# 12. Which of the following is true about the distributions of mass-specific metabolic rate for the four Families?
# a. They have equal sample sizes
# ~ Incorrect
# b. They have statistically equal variances
# ~ Incorrect
# *c. They do not have statistically equal variances
# ~ Correct: Just plotting the distributions will make this clear, or students may do a pairwise f ratio 
# d. They have statistically equal means
# ~ Incorrect
# e. They have statistically equal medians
# ~ Incorrect


########################################################################


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




 