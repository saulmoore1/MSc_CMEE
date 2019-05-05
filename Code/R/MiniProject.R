#!/usr/bin/env R
rm(list = ls())
graphics.off()
################################################################################
# MINIPROJECT - How well does a mechanistic model based upon biochemical 
# principles fit a dataset of thermal responses of individual fitness in plants?
################################################################################
# Load necessary dependencies
library(data.table) # For concatenating dataframes of results
library(minpack.lm) # For NLS fitting
library(AICcmodavg) # For AICc - correction for small sample sizes
library(ggplot2) # For 'pretty' plots
library(gridExtra) # For multipanel plotting with ggplot
library(reshape2) # For the 'Melt' function

# Load the data into R as 'MyData'
print("Reading in GrowthRespPhotoData.csv...")
MyData <- read.csv("../Data/GrowthRespPhotoData.csv", stringsAsFactors = FALSE)

##############################
# Data cleaning and wrangling
##############################
# Concatenate dataframe of variables (columns) of interest
MyData <- MyData[,c("FinalID", "ConSpecies", "Habitat", "OriginalTraitName", "OriginalTraitValue", "OriginalTraitUnit", "StandardisedTraitName", "StandardisedTraitValue", "StandardisedTraitUnit", "AmbientTemp", "ConTemp")]
MyData <- MyData[-20417,] # Omit 1 trait observation = error in dataset? Possible incorrectly labelled row (FinalID == "MTD4835")

# Subset to replace NA in 'StandardisedTraitValue' column with OriginalTraitValue - not comparing species/units, this is ok
MyData$StandardisedTraitValue <- ifelse((MyData$OriginalTraitName == 'growth rate'), MyData$StandardisedTraitValue, MyData$OriginalTraitValue)
MyData$StandardisedTraitUnit <- ifelse((MyData$OriginalTraitName == 'growth rate'), MyData$StandardisedTraitUnit, MyData$OriginalTraitUnit)
MyData <- subset(MyData, MyData$StandardisedTraitValue!="NA") # 63 trait observations (6 species) removed (NA for both original and standardised trait value)

# Collate temperature recordings into new column
MyData$Temp <- MyData$AmbientTemp # Create new column 'Temp' from 'AmbientTemp' recordings
my.na <- is.na(MyData$AmbientTemp) # Identify NA values in 'AmibentTemp' column, store as object 'my.na'
MyData$Temp[my.na] <- MyData$ConTemp[my.na] # Replace NAs in 'AmbientTemp' with values for 'ConTemp' to yeild complete temperature data

# Create new column for temperatures converted from degrees Celsius into Kelvin
MyData$TempK <- (MyData$Temp + 273.15)

# Fill 'StandardisedTraitName' NA data with 'OriginalTraitName' data - 8093 cases
MyData$StandardisedTraitName <- ifelse(is.na(MyData$StandardisedTraitName), MyData$OriginalTraitName, MyData$StandardisedTraitName)
MyData$ConSpecies <- ifelse(is.na(MyData$ConSpecies), MyData$FinalID, MyData$ConSpecies)
ids <- unique(MyData$FinalID) # Create list of unique species IDs
for(i in ids) { # Parse over list of species IDs - for each species, conduct the following:
  data <- subset(MyData, MyData$FinalID == i) # Subset for species of index i
  if(data$StandardisedTraitName[1] != "growth rate") { # If not growth rate for that species
    MyData$StandardisedTraitValue <- MyData$OriginalTraitValue # replace with data from 'OriginalTraitValue' column in MyData
    MyData$StandardisedTraitUnit <- MyData$OriginalTraitUnit # do the same for units
  }
}

# length(unique(MyData$FinalID)) # 2308
# For each species (FinalID), count the number of unique temperature observations
CountTemp <- data.table(MyData)[,length(unique(TempK)), by=FinalID] # dependecy package 'data.table'
# Remove species with <5 unique temperature observations - minimum established for regression analysis
UniqueTemp <- subset(CountTemp, CountTemp$V1 < 5)
Overfitting <- as.vector(UniqueTemp$FinalID)
for(i in Overfitting) {
  MyData <- subset(MyData, MyData$FinalID!=i) # remove these data
}
# length(unique(MyData$FinalID)) # 1903 species used for best model selection (405 species removed. Too few datapoints for regression analysis)

# Concatenate new dataframe of variables of interest
MyData <- MyData[,c("FinalID", "ConSpecies", "Habitat", "StandardisedTraitName", "StandardisedTraitValue", "StandardisedTraitUnit","Temp", "TempK")]

print("Data successfully wrangled!")
###############################################################################
# Getting parameters for the Schoolfield-Sharpe and Boltzmann-Arrhenius models
###############################################################################
# Assign reference temperature 'Tref' as a global variable, usable from within functions
assign("Tref", 10 + 273.15, envir = .GlobalEnv) # Tref = 10 degrees Celsius
assign("k", 8.617*10^-5, envir = .GlobalEnv) # Boltzmann constant (eV K^-1)

# Function to estimate starting values for the NLLS fit
GetTpk <- function(TempK, StandardisedTraitValue) {
  return(max(TempK[which.max(StandardisedTraitValue)])) # Return peak temperature (Tpk), the temperature value at maximum trait value
}

GetE <- function(TempK, StandardisedTraitValue, Tpk) {
  TempK.w <- which(TempK <= Tpk) # Return the index(s) of temperature values below/up to peak temperature
  if (length(TempK.w) > 1) { # Perfrom a linear model on just the curve rise to extract predicted value of E, the gradient
    m <- lm(log(StandardisedTraitValue[TempK.w]) ~ I(1 / (k * (TempK[TempK.w])))) # 'I(x)' instructs R to treat the object 'as is'
    E <- tryCatch(abs(summary(m)$coefficients[2,1]), # Return E, the gradient of LM (from summary coefficients). If negative, return absolute value
                  error = function(e) 0.65) # If an error, return E as 0.65, roughly what E should be in metabolic reactions
    return(E)
  }
}

GetB0 <- function(TempK, StandardisedTraitValue) {
  if (min(TempK,na.rm=TRUE) > Tref) { # return the minimum temperature greater than the reference temperature 10 degreees Celsius
    return(log(min(StandardisedTraitValue[1],na.rm=TRUE)))
  } else {
    return(log(max(StandardisedTraitValue[which(TempK <= Tref)],na.rm=TRUE)))
  }
}

###########################################################################
# Equations for NLS Regression - Schoolfield-Sharpe and Boltzmann-Arrhenius
###########################################################################
Boltzmann.Arrhenius <- function(B0, E, TempK) { # Define the Boltzmann-Arrhenius reaction-rate equation
  calc <- B0 - E/k * (1/TempK - 1/Tref) # Parameters: B0, E
  return(calc)
}

Schoolfield <- function(B0, E, E_D, T_h, TempK, SchoolTpk=TRUE) {
  if (SchoolTpk==TRUE) {
    Eqn <- (B0 + log(exp((-E/k) * ((1/TempK)-(1/Tref)))/(1 + (E/(E_D - E)) * exp(E_D/k * (1/T_h - 1/TempK)))))
    return(Eqn) # Parameters: B0, E, E_D, T_h
  }
}

##################################################
# Regression (phenomenological/mechanistic) Models
##################################################
# Out of 1617 species of plants only entries for 'growth rate' (825) show variation is 'StandardisedTraitValue' data
# 792 show no trait variation (leading to LM perfect fits), for net photosynthetic rate, gross photosynthetic rate, and respiration rate. Data is incorrectly calculated for 'StandardisedTraitValue'. In these instances, data have been replaced with 'OriginalTraitValue' data. Since comparisons will not be drawn between species in this study, this is ok.

CubicModel <- function(d) { # Define function to perform cubic polynomial linear regression
  xdata <- d$TempK # Define x (temp) and y (logB) data for regression analysis
  ydata <- log(d$StandardisedTraitValue)  # Calculate the logarithm of trait data for plot and predict
  CM <- lm(ydata ~ poly(xdata, 3, raw=TRUE)) # Perform 3rd order (cubic) polynomial regression
  Data2Plt = data.frame(xdata = seq(min(xdata), max(xdata), len = 200)) # Create sequence of x data for regression prediction, length = 200
  lines(Data2Plt$xdata, predict(CM, newdata = Data2Plt), type = "l", lwd = 2, lty = 1, col = "red") # Add regression line to plot for cubic
  CubicStats <- data.frame(Intercept.C = CM$coefficients[1], # Save summary statistics to object 'CubicStats': Int, gradient, R^2, F, p
                           Slope.C = CM$coefficients[2],
                           R.squared.C = summary(CM)$r.squared,
                           F.statistic.C = anova(CM)[1,4],
                           p.value.C = anova(CM)[1,5], AIC(CM), AICc(CM), BIC(CM))
  return(CubicStats)
}

QuadraticModel <- function(d){ # Define function to perform quadratic model linear regression
  xdata <- d$TempK # Define x (temp) and y (logB) data
  ydata <- log(d$StandardisedTraitValue)  # Calculate the log trait data for predict and plotting
  QM <- lm(ydata ~ poly(xdata, 2, raw=TRUE)) # Perform 2nd order polynomial regression (quadratic)
  Data2Plt = data.frame(xdata = seq(min(xdata), max(xdata), len = 200)) # Create sequence of x data for predict, length = 200
  lines(Data2Plt$xdata, predict(QM, newdata = Data2Plt), type = "l", lwd = 2, lty = 2, col = "blue") # Overlay regression line to plot
  QuadStats <- data.frame(Intercept.Q = QM$coefficients[1], # Save summary statistics to object 'QuadStats': Int, slope, R^2, F, p
                          Slope.Q = QM$coefficients[2],
                          R.squared.Q = summary(QM)$r.squared,
                          F.statistic.Q = anova(QM)[1,4],
                          p.value.Q = anova(QM)[1,5], AIC(QM), AICc(QM), BIC(QM))
  return(QuadStats)
}

SchoolfieldModel <- function(d, B0in, Ein, Tpkin) { # Define function to perform Schoolfield-Sharpe non-lineat regression
  xdata <- d$TempK # Define x (temp) and y (logB) data for regression analysis
  ydata <- log(d$StandardisedTraitValue)  # Calculate the log trait data for predict and plotting
  SM <- nlsLM( # dependency package 'minpack.lm'
    ydata ~ Schoolfield(B0, E, E_D, T_h, d$TempK), # perform the regression using the Schoolfield-Sharpe equation
    start=c(B0 = B0in, E = abs(Ein), E_D = 2*abs(Ein), T_h = Tpkin), # E_D should be ~= 2*E
    lower=c(B0=-Inf, E=0, E_D = 1E-10, T_h = 273.15 - 50), # E_D must be greater than E to avoid division by zero - returns error = NaNs produced
    upper=c(B0=Inf, E=5, E_D=10, T_h = 273.15 + 50),
    control=list(minFactor=1 / 2^16, maxiter=1024),
    data=d,
    na.action=na.omit)
  Data2Plt = data.frame(xdata = seq(min(xdata), max(xdata), len = 200)) # Create sequence of x data for regression prediction, length = 200
  B0out <- summary(SM)$coefficients['B0',][[1]] # retrieve parameter estimates
  Eout <- summary(SM)$coefficients['E',][[1]]
  EDout <-summary(SM)$coefficients['E_D',][[1]]
  Tpkout <- summary(SM)$coefficients['T_h',][[1]]
  Data2Plt$ydata <- Schoolfield(B0out, Eout, EDout, Tpkout, Data2Plt$xdata)
  lines(Data2Plt$xdata, Data2Plt$ydata, type = "l", lwd = 2, lty = 1, col = "green")
  SchoolStats <- data.frame(B0.S = B0out, E.S = Eout, E_D.S = EDout, Tpk = Tpkout) # 4 parameters
  TSS.S <- sum((d$StandardisedTraitValue - mean(d$StandardisedTraitValue))^2)
  RSS.S <- sum((d$StandardisedTraitValue - exp(Schoolfield(B0out, Eout, EDout, Tpkout, d$TempK)))^2)
  R2.S <- 1 - (RSS.S/TSS.S)
  SchoolStats <- cbind(SchoolStats, RSS.S, R2.S, AIC(SM), AICc(SM), BIC(SM)) # Bind to 'SchoolStats' statistics
  return(SchoolStats)
}

BoltzmannModel <- function(d, B0inB, EinB) { # Define the Bolztmann-Arrhenius equation for regression analysis
  xdata <- d$TempK # Define x (temp) and y (logB) data for regression analysis
  ydata <- log(d$StandardisedTraitValue)  # Calculate the log  trait data for plot and predict
  BM <- nlsLM(
    ydata ~ Boltzmann.Arrhenius(B0, E, TempK),
    start=c(B0=B0inB, E=EinB), # Starting values for NLS fit = parameter estimates from equation
    lower=c(B0=-Inf, E=0), # Lower bound of parameter values for NLS fit
    upper=c(B0=Inf, E=5), # Upper bound of parameter values for NLS fit
    control=list(minFactor=1/2^16, maxiter=1024), # minFactor = increment of Marquardt search / Maxiter = maximum no. iterations to look for convergence
    data=d,
    na.action=na.omit)
  Data2Plt = data.frame(xdata = seq(min(xdata), max(xdata), len = 200)) # create sequence of x data for regression prediction, length = 200
  B0outB <- summary(BM)$coefficients['B0',][[1]]
  EoutB <-summary(BM)$coefficients['E',][[1]]
  Data2Plt$ydata <- Boltzmann.Arrhenius(B0outB, EoutB, Data2Plt$xdata) # perform the regression using the Boltzmann-Arrhenius function
  lines(Data2Plt$xdata, Data2Plt$ydata, type = "l", lwd = 2, lty = 2, col = "orange") # add regression lines to the plot
  BoltzStats <- data.frame(B0.B = B0outB, E.B = EoutB) # Concatenate dataframe of parameter estimates
  TSS.B <- sum((d$StandardisedTraitValue - mean(d$StandardisedTraitValue))^2)
  RSS.B <- sum((d$StandardisedTraitValue - exp(Boltzmann.Arrhenius(B0outB, EoutB, d$TempK)))^2)
  R2.B <- 1 - (RSS.B/TSS.B)
  BoltzStats <- cbind(BoltzStats, RSS.B, R2.B, AIC(BM), AICc(BM), BIC(BM))
  return(BoltzStats)
}
# THE LOOP
ids <- unique(MyData$FinalID)# Create object of unique species IDs
out <- data.frame(matrix(data = NA, nrow=length(ids), ncol=35)) # Initialize matrix of NAs, approriate dimensions to be filled with the following info
names(out) <- c("SpeciesID","Habitat","Trait.Name","Intercept.Cubic","Slope.Cubic","R.squared.Cubic","F.statistic.Cubic","P.value.Cubic","AIC.Cubic","AICc.Cubic","BIC.Cubic","Intercept.Quadratic","Slope.Quadratic","R.squared.Quadratic","F.statistic.Quadratic","P.value.Quadratic","AIC.Quadratic","AICc.Quadratic","BIC.Quadratic","B0.Schoolfield","E.Schoolfield","E_h.Schoolfield","T_h.Schoolfield","RSS.Schoolfield","R.Squared.Schoolfield","AIC.Schoolfield","AICc.Schoolfield","BIC.Schoolfield","B0.Boltzmann","E.Boltzmann","RSS.Boltzmann","R.Squared.Boltzmann","AIC.Boltzmann","AICc.Boltzmann","BIC.Boltzmann") # Define column names for output dataframe
j = 1 # Initialize index j
set.seed(42) # Set seed for rnorm number generation about mean E=0.65 for NLS fits that do not work given starting values
print("Attempting to fit models to data. This may take some time...")
pdf("../Results/Model_Plots.pdf",8,8) # Initialize new save file for plots
for(i in ids) { # Parse over list of species IDs, for each species, conduct the following:
  data <- subset(MyData, MyData$FinalID == i)
  SpeciesID <- i
  Habitat <- data$Habitat[1]
  Trait <- data$StandardisedTraitName[1]
  if (min(data$StandardisedTraitValue)[1]<=0) { # If min value < 0, add min value plus 1 to make > 0
    MinVal <- min(data$StandardisedTraitValue)[1]
    data$StandardisedTraitValue <- data$StandardisedTraitValue + abs(MinVal) + 1
  }
  if((length(unique(data$TempK)) > 2) && (length(data$StandardisedTraitValue) >= 5)) {
    Tpk <- GetTpk(data$TempK, data$StandardisedTraitValue)
    E <- GetE(data$TempK, data$StandardisedTraitValue, Tpk)
    B0 <- GetB0(data$TempK, data$StandardisedTraitValue)
    xdata <- data$TempK # Define x (temp) and y (logB) data for regression analysis
    ydata <- log(data$StandardisedTraitValue)  # Calculate the logarithm of trait data for plot and predict
    Data2Plt = data.frame(xdata = seq(min(xdata), max(xdata), len = 200)) # Create sequence of x data for regression prediction, length = 200
    # qplot(x = xdata, y = ydata, data = data)
    plot(xdata, ydata, xlab="Temperature (K)", ylab = paste("log(", data$StandardisedTraitName[1],")"), pch = 4, main = data$ConSpecies[1], ylim=c((min(ydata) -0.5),(max(ydata)+0.5)))
    legend("topleft", c("Cubic","Quadratic","Schoolfield","Boltzmann"), lty=c(1,2,1,2), col=c("red","blue","green","orange"), cex = 1)
    FitCubic <- CubicModel(data) # Call the function to perform the polynomial regression for cubic model and plot the graph
    FitQuad <- QuadraticModel(data) # Call the function for the quadratic model
    FitSchool <- try(SchoolfieldModel(data, B0, E, Tpk), silent=T)
    deviants <- abs(rnorm(100, mean=0.65)) # For Schoolfield regressions that do not fit, set up a normal distribution of +ve values about mean 
    for(deviant in deviants){ # Set up a normal distribution to try fit for values of E about 0.65
      if(class(FitSchool)=="try-error"){ # If initial fit returns an error (special class try-error)
        FitSchool <- try(SchoolfieldModel(data, B0, E=deviant, Tpk), silent=T) # Try for a range of values of E (normal distribution)
      }
    }
    sub_data <- data[data$TempK <= Tpk,]
    if(length(sub_data$TempK > 2)) {
      FitBoltz <- try(BoltzmannModel(sub_data, B0, E), silent=T)
      for(deviant in deviants){
        if(class(FitBoltz)=="try-error"){
          FitBoltz <- try(BoltzmannModel(sub_data, B0, E=deviant), silent=T)
        }
      }
    }
  }
  # Concatenate dataframe of SpeciesID & statistics for all four models
  tmpout <-  data.frame(c(as.character(SpeciesID), as.character(Habitat), as.character(Trait), FitCubic, FitQuad, FitSchool, FitBoltz))
  # browser()
  names(tmpout) <- c("SpeciesID","Habitat","Trait.Name",tail(names(tmpout),-3))
  out[j,] <- tmpout 
  out$SpeciesID[j] <- SpeciesID
  out$Habitat[j] <- Habitat
  out$Trait.Name[j] <- Trait
  j = j + 1
}
dev.off()
print(paste("Complete! Models fitted to a total of",j-1,"species!"))
BestModelAIC <- data.frame(out$AIC.Cubic, out$AIC.Quadratic, out$AIC.Schoolfield, out$AIC.Boltzmann)
names(BestModelAIC) <- c("Cubic", "Quadratic", "Schoolfield", "Boltzmann")
BestModelAICc <- data.frame(out$AICc.Cubic, out$AICc.Quadratic, out$AICc.Schoolfield, out$AICc.Boltzmann)
names(BestModelAICc) <- c("Cubic", "Quadratic", "Schoolfield", "Boltzmann")
BestModelBIC <- data.frame(out$BIC.Cubic, out$BIC.Quadratic, out$BIC.Schoolfield, out$BIC.Boltzmann)
names(BestModelBIC) <- c("Cubic", "Quadratic", "Schoolfield", "Boltzmann")
BestModelR2 <- data.frame(out$R.squared.Cubic, out$R.squared.Quadratic, out$R.Squared.Schoolfield, out$R.Squared.Boltzmann)
names(BestModelR2) <- c("Cubic", "Quadratic", "Schoolfield", "Boltzmann")
BestModelAIC[BestModelAIC == "Inf"] <- NA
BestModelAIC[BestModelAIC == "-Inf"] <- NA
BestModelAICc[BestModelAICc == "Inf"] <- NA
BestModelAICc[BestModelAICc == "-Inf"] <- NA
BestModelBIC[BestModelBIC == "Inf"] <- NA
BestModelBIC[BestModelBIC == "-Inf"] <- NA
BestModelR2[BestModelR2 >= 1] <- NA
BestModelR2[BestModelR2 <= 0] <- NA
Best.Model.AIC <- colnames(BestModelAIC)[apply(BestModelAIC,1,which.min)]
Best.Model.AICc <- colnames(BestModelAICc)[apply(BestModelAICc,1,which.min)]
Best.Model.BIC <- colnames(BestModelBIC)[apply(BestModelBIC,1,which.min)]
BestModelR2 <- na.omit(BestModelR2)
Best.Model.R2 <- colnames(BestModelR2)[apply(BestModelR2,1,which.max)]
out <- cbind(out, Best.Model.AIC, Best.Model.AICc, Best.Model.BIC)
write.csv(out, "../Results/Model_Statistics.csv")

# AIC.pie <- c(table(Best.Model.AIC)[[1]],table(Best.Model.AIC)[[2]],table(Best.Model.AIC)[[3]],table(Best.Model.AIC)[[4]])
# AICc.pie <- c(table(Best.Model.AICc)[[1]],table(Best.Model.AICc)[[2]],table(Best.Model.AICc)[[3]],table(Best.Model.AICc)[[4]])
# BIC.pie <- c(table(Best.Model.BIC)[[1]],table(Best.Model.BIC)[[2]],table(Best.Model.BIC)[[3]],table(Best.Model.BIC)[[4]])
# labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
# PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
# PercentAICc <- round(100*AICc.pie/sum(AICc.pie), 1)
# PercentBIC <- round(100*BIC.pie/sum(BIC.pie), 1)
# labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
# labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
# labelsAICc <- paste(labels, PercentAICc) # Add percents to labels
# labelsAICc <- paste(labelsAICc,"%",sep="") # Add % to labels
# labelsBIC <- paste(labels, PercentBIC) # Add percents to labels
# labelsBIC <- paste(labelsBIC,"%",sep="") # Add % to labels
# pdf("../Results/Pie_Charts.pdf",12,5)
# par(mfrow=c(1,3))
# pie(AIC.pie, labels = labelsAIC, col=c("orange","red","blue","green"))
# title(main="Best Model (AIC)", cex.main=2)
# pie(BIC.pie, labels = labelsBIC, col=c("orange","red","blue","green"))
# title(main="Best Model (BIC)", cex.main=2)
# pie(AICc.pie, labels = labelsAICc, col=c("orange","red","blue","green"))
# title(main="Best Model (AICc)", cex.main=2)
# dev.off()

AIC.bar <- c(table(Best.Model.AIC)[[4]],table(Best.Model.AIC)[[1]],table(Best.Model.AIC)[[2]],table(Best.Model.AIC)[[3]])
AICc.bar <- c(table(Best.Model.AICc)[[4]],table(Best.Model.AICc)[[1]],table(Best.Model.AICc)[[2]],table(Best.Model.AICc)[[3]])
BIC.bar <- c(table(Best.Model.BIC)[[4]],table(Best.Model.BIC)[[1]],table(Best.Model.BIC)[[2]],table(Best.Model.BIC)[[3]])
R2.bar <- c(table(Best.Model.R2)[[3]],table(Best.Model.R2)[[1]],table(Best.Model.R2)[[2]], 0)
labels <- c("Schoolfield","Boltzmann","Cubic","Quadratic")
PercentAIC <- round(100*AIC.bar/sum(AIC.bar), 1)
PercentAICc <- round(100*AICc.bar/sum(AICc.bar), 1)
PercentBIC <- round(100*BIC.bar/sum(BIC.bar), 1)
PercentR2 <- round(100*R2.bar/sum(R2.bar), 1)
Bar <- data.frame(PercentR2, PercentAIC, PercentBIC, PercentAICc)
names(Bar) <- c(expression("R^2"), "AIC", "BIC", "AICc")
Bar2 <- melt(cbind(Bar, Model = labels), id.vars = c('Model'))
ggplot(Bar2, aes(x = variable, y = value, fill = Model)) + 
  geom_bar(position = "stack",stat = "identity") +
  xlab("Model Selection Criterion") +
  ylab("Percentage") + 
  scale_fill_manual("Model", values = alpha( c("orange", "red", "blue", "green"), 1)) +
  geom_text(aes(label=c(R2.bar, AIC.bar, BIC.bar, AICc.bar)), size = 3, hjust = 0.5, vjust = 2, position = "stack", check_overlap = TRUE)
ggsave("../Results/Bar_Charts.pdf")

###############################################################
# Twin plot of bar charts for best model - by model complexity
###############################################################
# Best model selection - main two models compared
BestModelAIC2 <- data.frame(out$AIC.Cubic, out$AIC.Schoolfield)
names(BestModelAIC2) <- c("Cubic", "Schoolfield")
BestModelAICc2 <- data.frame(out$AICc.Cubic, out$AICc.Schoolfield)
names(BestModelAICc2) <- c("Cubic", "Schoolfield")
BestModelBIC2 <- data.frame(out$BIC.Cubic, out$BIC.Schoolfield)
names(BestModelBIC2) <- c("Cubic", "Schoolfield")
BestModelR22 <- data.frame(out$R.squared.Cubic, out$R.Squared.Schoolfield)
names(BestModelR22) <- c("Cubic", "Schoolfield")
BestModelAIC2[BestModelAIC2 == "Inf"] <- NA
BestModelAIC2[BestModelAIC2 == "-Inf"] <- NA
BestModelAICc2[BestModelAICc2 == "Inf"] <- NA
BestModelAICc2[BestModelAICc2 == "-Inf"] <- NA
BestModelBIC2[BestModelBIC2 == "Inf"] <- NA
BestModelBIC2[BestModelBIC2 == "-Inf"] <- NA
BestModelR22[BestModelR22 >= 1] <- NA
BestModelR22[BestModelR22 <= 0] <- NA
Best.Model.AIC2 <- colnames(BestModelAIC2)[apply(BestModelAIC2,1,which.min)]
BestModelAICc2 <- na.omit(BestModelAICc2)
Best.Model.AICc2 <- colnames(BestModelAICc2)[apply(BestModelAICc2,1,which.min)]
Best.Model.BIC2 <- colnames(BestModelBIC2)[apply(BestModelBIC2,1,which.min)]
BestModelR22 <- na.omit(BestModelR22)
Best.Model.R22 <- colnames(BestModelR22)[apply(BestModelR22,1,which.max)]
AIC.bar2 <- c(table(Best.Model.AIC2)[[1]],table(Best.Model.AIC2)[[2]])
AICc.bar2 <- c(table(Best.Model.AICc2)[[1]],table(Best.Model.AICc2)[[2]])
BIC.bar2 <- c(table(Best.Model.BIC2)[[1]],table(Best.Model.BIC2)[[2]])
R2.bar2 <- c(table(Best.Model.R22)[[1]], table(Best.Model.R22)[[2]])
labels <- c("Cubic","Schoolfield")
PercentAIC2 <- round(100*AIC.bar2/sum(AIC.bar2), 1)
PercentAICc2 <- round(100*AICc.bar2/sum(AICc.bar2), 1)
PercentBIC2 <- round(100*BIC.bar2/sum(BIC.bar2), 1)
PercentR22 <- round(100*R2.bar2/sum(R2.bar2), 1)
Bar.2 <- data.frame(PercentR22, PercentAIC2, PercentBIC2, PercentAICc2)
names(Bar.2) <- c(expression("R^2"), "AIC", "BIC", "AICc")
Bar2.2 <- melt(cbind(Bar.2, Model = labels), id.vars = c('Model'))
PlotA <- ggplot(Bar2.2, aes(x = variable, y = value, fill = Model)) + 
  geom_bar(position = "stack", stat = "identity") +
  xlab("Model Selection Criterion") +
  ylab("Percentage") + 
  scale_fill_manual("Model", values = alpha(c("red", "green"), 1)) +
  geom_text(aes(label=c(R2.bar2, AIC.bar2, BIC.bar2, AICc.bar2)), size = 3, hjust = 0.5, vjust = 2, position = "stack")

# Best model Selection - Two simpler models compared
BestModelAIC3 <- data.frame(out$AIC.Quadratic, out$AIC.Boltzmann)
names(BestModelAIC3) <- c("Quadratic", "Boltzmann")
BestModelAICc3 <- data.frame(out$AICc.Quadratic, out$AICc.Boltzmann)
names(BestModelAICc3) <- c("Quadratic", "Boltzmann")
BestModelBIC3 <- data.frame(out$BIC.Quadratic, out$BIC.Boltzmann)
names(BestModelBIC3) <- c("Quadratic", "Boltzmann")
BestModelR23 <- data.frame(out$R.squared.Quadratic, out$R.Squared.Boltzmann)
names(BestModelR23) <- c("Quadratic", "Boltzmann")
BestModelAIC3[BestModelAIC3 == "Inf"] <- NA
BestModelAIC3[BestModelAIC3 == "-Inf"] <- NA
BestModelAICc3[BestModelAICc3 == "Inf"] <- NA
BestModelAICc3[BestModelAICc3 == "-Inf"] <- NA
BestModelBIC3[BestModelBIC3 == "Inf"] <- NA
BestModelBIC3[BestModelBIC3 == "-Inf"] <- NA
BestModelR23[BestModelR23 >= 1] <- NA
BestModelR23[BestModelR23 <= 0] <- NA
Best.Model.AIC3 <- colnames(BestModelAIC3)[apply(BestModelAIC3,1,which.min)]
BestModelAICc3 <- na.omit(BestModelAICc3)
Best.Model.AICc3 <- colnames(BestModelAICc3)[apply(BestModelAICc3,1,which.min)]
BestModelBIC3 <- na.omit(BestModelBIC3)
Best.Model.BIC3 <- colnames(BestModelBIC3)[apply(BestModelBIC3,1,which.min)]
BestModelR23 <- na.omit(BestModelR23)
Best.Model.R23 <- colnames(BestModelR23)[apply(BestModelR23,1,which.max)]
AIC.bar3 <- c(table(Best.Model.AIC3)[[1]],table(Best.Model.AIC3)[[2]])
AICc.bar3 <- c(table(Best.Model.AICc3)[[1]],table(Best.Model.AICc3)[[2]])
BIC.bar3 <- c(table(Best.Model.BIC3)[[1]],table(Best.Model.BIC3)[[2]])
R2.bar3 <- c(table(Best.Model.R23)[[1]], table(Best.Model.R23)[[2]])
labels <- c("Boltzmann","Quadratic")
PercentAIC3 <- round(100*AIC.bar3/sum(AIC.bar3), 1)
PercentAICc3 <- round(100*AICc.bar3/sum(AICc.bar3), 1)
PercentBIC3 <- round(100*BIC.bar3/sum(BIC.bar3), 1)
PercentR23 <- round(100*R2.bar3/sum(R2.bar3), 1)
Bar.3 <- data.frame(PercentR23, PercentAIC3, PercentBIC3, PercentAICc3)
names(Bar.3) <- c(expression("R^2"), "AIC", "BIC", "AICc")
Bar2.3 <- melt(cbind(Bar.3, Model = labels), id.vars = c('Model'))
PlotB <- ggplot(Bar2.3, aes(x = variable, y = value, fill = Model)) + 
  geom_bar(position = "stack",stat = "identity") +
  xlab("Model Selection Criterion") +
  ylab("Percentage") + 
  scale_fill_manual("Model", values = alpha(c("orange", "blue"), 1)) +
  geom_text(aes(label=c(R2.bar3, AIC.bar3, BIC.bar3, AICc.bar3)), size = 3, hjust = 0.5, vjust = 2, position = "stack")
pdf("../Results/Bar_Charts2.pdf", 12,5)
grid.arrange(PlotA, PlotB, ncol=2)
dev.off()

###############################################
# Pie charts and plots of data by habitat type
###############################################
pdf("../Results/Pie_Charts_Habitat.pdf",12,9)
par(mfrow=c(3,3))
Hab <- subset(out, out$Habitat == "terrestrial")
AIC.pie <- c(table(Hab$Best.Model.AIC)[[1]],table(Hab$Best.Model.AIC)[[2]],table(Hab$Best.Model.AIC)[[3]],table(Hab$Best.Model.AIC)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
pie(AIC.pie, labels = labelsAIC, col=c("orange","red","blue","green"))
title(main="Best Model (AIC)", cex.main=2)
mtext(paste("Terrestrial (Total:",sum(AIC.pie),")"), side=2)

Hab <- subset(out, out$Habitat == "terrestrial")
BIC.pie <- c(table(Hab$Best.Model.BIC)[[1]],table(Hab$Best.Model.BIC)[[2]],table(Hab$Best.Model.BIC)[[3]],table(Hab$Best.Model.BIC)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentBIC <- round(100*BIC.pie/sum(BIC.pie), 1)
labelsBIC <- paste(labels, PercentBIC) # Add percents to labels
labelsBIC <- paste(labelsBIC,"%",sep="") # Add % to labels
pie(BIC.pie, labels = labelsBIC, col=c("orange","red","blue","green"))
title(main="Best Model (BIC)", cex.main=2)

Hab <- subset(out, out$Habitat == "terrestrial")
AICc.pie <- c(table(Hab$Best.Model.AICc)[[1]],table(Hab$Best.Model.AICc)[[2]],table(Hab$Best.Model.AICc)[[3]],table(Hab$Best.Model.AICc)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentAICc <- round(100*AICc.pie/sum(AICc.pie), 1)
labelsAICc <- paste(labels, PercentAICc) # Add percents to labels
labelsAICc <- paste(labelsAICc,"%",sep="") # Add % to labels
pie(AICc.pie, labels = labelsAICc, col=c("orange","red","blue","green"))
title(main="Best Model (AICc)", cex.main=2)

Hab <- subset(out, out$Habitat == "marine")
AIC.pie <- c(table(Hab$Best.Model.AIC)[[1]],table(Hab$Best.Model.AIC)[[2]],table(Hab$Best.Model.AIC)[[3]],table(Hab$Best.Model.AIC)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
pie(AIC.pie, labels = labelsAIC, col=c("orange","red","blue","green"))
mtext(paste("Marine (Total:",sum(AIC.pie),")"), side=2)

Hab <- subset(out, out$Habitat == "marine")
BIC.pie <- c(table(Hab$Best.Model.BIC)[[1]],table(Hab$Best.Model.BIC)[[2]],table(Hab$Best.Model.BIC)[[3]],table(Hab$Best.Model.BIC)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentBIC <- round(100*BIC.pie/sum(BIC.pie), 1)
labelsBIC <- paste(labels, PercentBIC) # Add percents to labels
labelsBIC <- paste(labelsBIC,"%",sep="") # Add % to labels
pie(BIC.pie, labels = labelsBIC, col=c("orange","red","blue","green"))

Hab <- subset(out, out$Habitat == "marine")
AICc.pie <- c(table(Hab$Best.Model.AICc)[[1]],table(Hab$Best.Model.AICc)[[2]],table(Hab$Best.Model.AICc)[[3]],table(Hab$Best.Model.AICc)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentAICc <- round(100*AICc.pie/sum(AICc.pie), 1)
labelsAICc <- paste(labels, PercentAICc) # Add percents to labels
labelsAICc <- paste(labelsAICc,"%",sep="") # Add % to labels
pie(AICc.pie, labels = labelsAICc, col=c("orange","red","blue","green"))

Hab <- subset(out, out$Habitat == "freshwater")
AIC.pie <- c(table(Hab$Best.Model.AIC)[[1]],table(Hab$Best.Model.AIC)[[2]],table(Hab$Best.Model.AIC)[[3]],table(Hab$Best.Model.AIC)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
pie(AIC.pie, labels = labelsAIC, col=c("orange","red","blue","green"))
mtext(paste("Freshwater (Total:",sum(AIC.pie),")"), side=2)

Hab <- subset(out, out$Habitat == "freshwater")
BIC.pie <- c(table(Hab$Best.Model.BIC)[[1]],table(Hab$Best.Model.BIC)[[2]],table(Hab$Best.Model.BIC)[[3]],table(Hab$Best.Model.BIC)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentBIC <- round(100*BIC.pie/sum(BIC.pie), 1)
labelsBIC <- paste(labels, PercentBIC) # Add percents to labels
labelsBIC <- paste(labelsBIC,"%",sep="") # Add % to labels
pie(BIC.pie, labels = labelsBIC, col=c("orange","red","blue","green"))

Hab <- subset(out, out$Habitat == "freshwater")
AICc.pie <- c(table(Hab$Best.Model.AICc)[[1]],table(Hab$Best.Model.AICc)[[2]],table(Hab$Best.Model.AICc)[[3]],table(Hab$Best.Model.AICc)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentAICc <- round(100*AICc.pie/sum(AICc.pie), 1)
labelsAICc <- paste(labels, PercentAICc) # Add percents to labels
labelsAICc <- paste(labelsAICc,"%",sep="") # Add % to labels
pie(AICc.pie, labels = labelsAICc, col=c("orange","red","blue","green"))
dev.off()

# Hab <- subset(out, out$Habitat == "estuarine")
# AIC.pie <- c(table(Hab$Best.Model.AIC)[[1]],table(Hab$Best.Model.AIC)[[2]],table(Hab$Best.Model.AIC)[[3]],table(Hab$Best.Model.AIC)[[4]])
# labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
# PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
# labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
# labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
# pie(AIC.pie, labels = labelsAIC, col=c("orange","red","blue","green"))
# mtext(paste("Estuarine (Total:",sum(AIC.pie),")"), side=2)
# 
# Hab <- subset(out, out$Habitat == "estuarine")
# BIC.pie <- c(table(Hab$Best.Model.BIC)[[1]],table(Hab$Best.Model.BIC)[[2]],table(Hab$Best.Model.BIC)[[3]],table(Hab$Best.Model.BIC)[[4]])
# labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
# PercentBIC <- round(100*BIC.pie/sum(BIC.pie), 1)
# labelsBIC <- paste(labels, PercentBIC) # Add percents to labels
# labelsBIC <- paste(labelsBIC,"%",sep="") # Add % to labels
# pie(BIC.pie, labels = labelsBIC, col=c("orange","red","blue","green"))
# 
# Hab <- subset(out, out$Habitat == "estuarine")
# AICc.pie <- c(table(Hab$Best.Model.AICc)[[1]],table(Hab$Best.Model.AICc)[[2]],table(Hab$Best.Model.AICc)[[3]],table(Hab$Best.Model.AICc)[[4]])
# labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
# PercentAICc <- round(100*AICc.pie/sum(AICc.pie), 1)
# labelsAICc <- paste(labels, PercentAICc) # Add percents to labels
# labelsAICc <- paste(labelsAICc,"%",sep="") # Add % to labels
# pie(AICc.pie, labels = labelsAICc, col=c("orange","red","blue","green"))

# Hab <- subset(out, out$Habitat == "aquatic")
# AIC.pie <- c(table(Hab$Best.Model.AIC)[[1]],table(Hab$Best.Model.AIC)[[3]])
# labels <- c("Boltzmann","Quadratic")
# PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
# labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
# labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
# pie(AIC.pie, labels = labelsAIC, col=c("orange","blue"), main="Aquatic Plant Species
# Best Model (AIC)", cex=0.8)
# mtext(paste("Total:",sum(AIC.pie)), side=1)

# Hab <- subset(out, out$Habitat == "freshwater / terrestrial")
# AIC.pie <- c(table(Hab$Best.Model.AIC)[[2]],table(Hab$Best.Model.AIC)[[3]])
# labels <- c("Cubic","Quadratic")
# PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
# labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
# labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
# pie(AIC.pie, labels = labelsAIC, col=c("red","blue"), main="Freshwater / Terrestrial Plant Species
# Best Model (AIC)", cex=0.8)
# mtext(paste("Total:",sum(AIC.pie)), side=1)

# Hab <- subset(out, out$Habitat == "hot spring")
# AIC.pie <- c(table(Hab$Best.Model.AIC)[[2]])
# labels <- c("Cubic")
# PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
# labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
# labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
# pie(AIC.pie, labels = labelsAIC, col=c("red"), main="Hot Spring Plant Species
# Best Model (AIC)", cex=0.8)
# mtext(paste("Total:",sum(AIC.pie)), side=1)

# Hab <- subset(out, out$Habitat == "saline lake")
# AIC.pie <- c(table(Hab$Best.Model.AIC)[[1]],table(Hab$Best.Model.AIC)[[2]],table(Hab$Best.Model.AIC)[[4]])
# labels <- c("Boltzmann","Cubic","Schoolfield")
# PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
# labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
# labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
# pie(AIC.pie, labels = labelsAIC, col=c("orange","red","green"), main="Saline Lake Plant Species
# Best Model (AIC)", cex=0.8)
# mtext(paste("Total:",sum(AIC.pie)), side=1)


# Hab <- subset(out, out$Habitat == "unknown")
# AIC.pie <- c(table(Hab$Best.Model.AIC)[[1]],table(Hab$Best.Model.AIC)[[3]])
# labels <- c("Boltzmann","Quadratic")
# PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
# labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
# labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
# pie(AIC.pie, labels = labelsAIC, col=c("orange","blue"), main="Unknown Plant Species
# Best Model (AIC)", cex=0.8)
# mtext(paste("Total:",sum(AIC.pie)), side=1)

###################################
# Pie charts, subset by trait type
###################################
pdf("../Results/Pie_Charts_Trait.pdf",12,9) # Multipanel plot of pie charts
par(mfrow=c(3,3))
Photo <- subset(out, out$Trait.Name=="assumed net photosynthesis" | out$Trait.Name=="gross photosynthesis" | out$Trait.Name=="net photosynthesis rate" | out$Trait.Name=="cell-specific photosynthesis rate" | out$Trait.Name=="gross photosynthesis rate" | out$Trait.Name=="net photosynthesis")
AIC.pie <- c(table(Photo$Best.Model.AIC)[[1]],table(Photo$Best.Model.AIC)[[2]],table(Photo$Best.Model.AIC)[[3]],table(Photo$Best.Model.AIC)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
pie(AIC.pie, labels = labelsAIC, col=c("orange","red","blue","green"))
title(main="Best Model (AIC)", cex.main=2)
mtext(paste("Photosynthetic rate (Total:",sum(AIC.pie),")"), side=2)

Photo <- subset(out, out$Trait.Name=="assumed net photosynthesis" | out$Trait.Name=="gross photosynthesis" | out$Trait.Name=="net photosynthesis rate" | out$Trait.Name=="cell-specific photosynthesis rate" | out$Trait.Name=="gross photosynthesis rate" | out$Trait.Name=="net photosynthesis")
BIC.pie <- c(table(Photo$Best.Model.BIC)[[1]],table(Photo$Best.Model.BIC)[[2]],table(Photo$Best.Model.BIC)[[3]],table(Photo$Best.Model.BIC)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentBIC <- round(100*BIC.pie/sum(BIC.pie), 1)
labelsBIC <- paste(labels, PercentBIC) # Add percents to labels
labelsBIC <- paste(labelsBIC,"%",sep="") # Add % to labels
pie(BIC.pie, labels = labelsBIC, col=c("orange","red","blue","green"))
title(main="Best Model (BIC)", cex.main=2)

Photo <- subset(out, out$Trait.Name=="assumed net photosynthesis" | out$Trait.Name=="gross photosynthesis" | out$Trait.Name=="net photosynthesis rate" | out$Trait.Name=="cell-specific photosynthesis rate" | out$Trait.Name=="gross photosynthesis rate" | out$Trait.Name=="net photosynthesis")
AICc.pie <- c(table(Photo$Best.Model.AICc)[[1]],table(Photo$Best.Model.AICc)[[2]],table(Photo$Best.Model.AICc)[[3]],table(Photo$Best.Model.AICc)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentAICc <- round(100*AICc.pie/sum(AICc.pie), 1)
labelsAICc <- paste(labels, PercentAICc) # Add percents to labels
labelsAICc <- paste(labelsAICc,"%",sep="") # Add % to labels
pie(AICc.pie, labels = labelsAICc, col=c("orange","red","blue","green"))
title(main="Best Model (AICc)", cex.main=2)

Growth <- subset(out, out$Trait.Name=="growth rate")
AIC.pie <- c(table(Growth$Best.Model.AIC)[[1]],table(Growth$Best.Model.AIC)[[2]],table(Growth$Best.Model.AIC)[[3]],table(Growth$Best.Model.AIC)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
pie(AIC.pie, labels = labelsAIC, col=c("orange","red","blue","green"))
mtext(paste("Growth rate (Total:",sum(AIC.pie),")"), side=2)

Growth <- subset(out, out$Trait.Name=="growth rate")
BIC.pie <- c(table(Growth$Best.Model.BIC)[[1]],table(Growth$Best.Model.BIC)[[2]],table(Growth$Best.Model.BIC)[[3]],table(Growth$Best.Model.BIC)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentBIC <- round(100*BIC.pie/sum(BIC.pie), 1)
labelsBIC <- paste(labels, PercentBIC) # Add percents to labels
labelsBIC <- paste(labelsBIC,"%",sep="") # Add % to labels
pie(BIC.pie, labels = labelsBIC, col=c("orange","red","blue","green"))

Growth <- subset(out, out$Trait.Name=="growth rate")
AICc.pie <- c(table(Growth$Best.Model.AICc)[[1]],table(Growth$Best.Model.AICc)[[2]],table(Growth$Best.Model.AICc)[[3]],table(Growth$Best.Model.AICc)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentAICc <- round(100*AICc.pie/sum(AICc.pie), 1)
labelsAICc <- paste(labels, PercentAICc) # Add percents to labels
labelsAICc <- paste(labelsAICc,"%",sep="") # Add % to labels
pie(AICc.pie, labels = labelsAICc, col=c("orange","red","blue","green"))

Resp <- subset(out, out$Trait.Name=="respiration rate")
AIC.pie <- c(table(Resp$Best.Model.AIC)[[1]],table(Resp$Best.Model.AIC)[[2]],table(Resp$Best.Model.AIC)[[3]],table(Resp$Best.Model.AIC)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentAIC <- round(100*AIC.pie/sum(AIC.pie), 1)
labelsAIC <- paste(labels, PercentAIC) # Add percents to labels
labelsAIC <- paste(labelsAIC,"%",sep="") # Add % to labels
pie(AIC.pie, labels = labelsAIC, col=c("orange","red","blue","green"))
mtext(paste("Respiration rate (Total:",sum(AIC.pie),")"), side=2)

Resp <- subset(out, out$Trait.Name=="respiration rate")
BIC.pie <- c(table(Resp$Best.Model.BIC)[[1]],table(Resp$Best.Model.BIC)[[2]],table(Resp$Best.Model.BIC)[[3]],table(Resp$Best.Model.BIC)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentBIC <- round(100*BIC.pie/sum(BIC.pie), 1)
labelsBIC <- paste(labels, PercentBIC) # Add percents to labels
labelsBIC <- paste(labelsBIC,"%",sep="") # Add % to labels
pie(BIC.pie, labels = labelsBIC, col=c("orange","red","blue","green"))

Resp <- subset(out, out$Trait.Name=="respiration rate")
AICc.pie <- c(table(Resp$Best.Model.AICc)[[1]],table(Resp$Best.Model.AICc)[[2]],table(Resp$Best.Model.AICc)[[3]],table(Resp$Best.Model.AICc)[[4]])
labels <- c("Boltzmann","Cubic","Quadratic","Schoolfield")
PercentAICc <- round(100*AICc.pie/sum(AICc.pie), 1)
labelsAICc <- paste(labels, PercentAICc) # Add percents to labels
labelsAICc <- paste(labelsAICc,"%",sep="") # Add % to labels
pie(AICc.pie, labels = labelsAICc, col=c("orange","red","blue","green"))
dev.off()

########################################
# Example fit for report
########################################
pdf("../Results/Example_Plot.pdf",8,8)
data <- subset(MyData, MyData$FinalID == "MTD2493")
if (min(data$StandardisedTraitValue)[1]<=0) {
  MinVal <- min(data$StandardisedTraitValue)[1]
  data$StandardisedTraitValue <- data$StandardisedTraitValue + abs(MinVal) + 1
}
if((length(unique(data$TempK)) > 2) && (length(data$StandardisedTraitValue) >= 5)) {
  Tpk <- GetTpk(data$TempK, data$StandardisedTraitValue)
  E <- GetE(data$TempK, data$StandardisedTraitValue, Tpk)
  B0 <- GetB0(data$TempK, data$StandardisedTraitValue)
  xdata <- data$TempK
  ydata <- log(data$StandardisedTraitValue)
  Data2Plt = data.frame(xdata = seq(min(xdata), max(xdata), len = 200))
  plot(xdata, ydata, xlab="Temperature (K)", ylab = paste("log (",data$StandardisedTraitName[1],")"), pch = 4, ylim=c((min(ydata) -0.5),(max(ydata)+0.5)), xlim=c(272,293), main=data$ConSpecies[1])
  legend("topleft", c("Cubic","Quadratic","Schoolfield","Boltzmann"), lty=c(1,2,1,2), col=c("red","blue","green","orange"), cex = 1)
  FitCubic <- CubicModel(data)
  FitQuad <- QuadraticModel(data)
  FitSchool <- try(SchoolfieldModel(data, B0, E, Tpk), silent=T)
  sub_data <- data[data$TempK <= Tpk,]
  FitBoltz <- try(BoltzmannModel(sub_data, B0, E), silent=T)
  arrows(Tpk,log(max(data$StandardisedTraitValue)+0.5),Tpk,log(max(data$StandardisedTraitValue)+0.1), length=0.05)
  text(Tpk, log(max(data$StandardisedTraitValue)+0.6), labels="T.peak", cex=0.5)
}
dev.off()
print("Finished in R! Model statistics and plots saved to 'Results' directory.")