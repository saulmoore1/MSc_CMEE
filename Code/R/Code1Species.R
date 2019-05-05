##A model of one bacterial species growing on 1 substrate, which assumes that:
##the per capita growth rate is proportional to the rate of metabolism
##the rate of metabolism is a Michaelis-Menten function of the concentration of substrate
##constant rate of dilution introduces new substrate, removes substrate and bacteria

################################################################
# Modified to include pH effects and predation by bacteriophage
################################################################

##assign values to parameters
##a) starting population density of bacteria
N0<-0.1
##b) starting concentration of the substrate (resource)
S0<-3
##c) the dilution rate
d<-0.01

##c) 3 parameters for the Michaelis-Menten part, which specifies
##the rate at which substrate is metabolized, see http://en.wikipedia.org/wiki/Michaelis–Menten_kinetics

##the amount of substrate that a unit amount of enzyme can metabolise per unit time
kcat<-0.1
##the amount of enzyme per cell
E<-1
##NB Vmax = kcat*E, where Vmax is the rate of the reaction per cell when there is excess substrate

##the Michaelis constant - concentration at which the reaction rate is 1/2 its maximum
Km<-1

##the conversion rate from substrate metabolised to density increase
##i.e. per unit substrate metabolise get c.conv more cells produced
c.conv<-1

pHopt<-7 # Define optimum pH for bacterium
pH <- 7.5 # Set pH of system
pH<-(exp((-(pH-pHopt)^2)/2*(1.5)^2))
P0 <- N0/2 # Set initial phage number to be half bacteria number
a <- 0.1 # Conversion rate of bacteria to phage
b <- 0.1 # Feeding rate of phage
dp <- 0.1 # Death rate of phage
##choose time steps and total time
time.step<-0.5
times<-seq(0,500,time.step)

##set initial values of state variables
S<-S0
N<-N0
P<-P0

##set up matrix to store results in
results<-matrix(NA,nrow=length(times)+1,ncol=4)

	##store time 0 values
	results[1,1]<-0
	results[1,2]<-S
	results[1,3]<-N
	results[1,4]<-P

##loop through the times
for (i in (1:length(times))) {
	##assign new values to S and N according to Michaelis Menten
	S <- S-(kcat*E*S*pH/(Km+S))*N*time.step-d*S*time.step+d*S0*time.step # add pH dependence to affect S and N (Michaelis-Menten kinetics)
	N <- N+(c.conv*kcat*E*S*pH/(Km+S))*N*time.step-d*N*time.step - b*P*N*time.step
	P <- P+(a*N*P-dp*P)*P*time.step-d*P*time.step
	##store the results
	##use i+1 so as not to over-write the time 0 values
	results[i+1,1]<-times[i]
	results[i+1,2]<-S
	results[i+1,3]<-N
	results[i+1,4]<-P
}

##plot the results on the same graph
##a) Substrate concentration 
plot(results[,2]~results[,1],type="l", xlab="Generation", ylab="Density /Concentration", ylim=c(0,3))
##b) Density
lines(results[,3]~results[,1],col="red")
lines(results[,4]~results[,1],col="blue")
# Substrate concentration (black line) and population size (red line)
legend("topright",legend=c("Concentration of substrate","Density of bacterium","Density of phage"), col=c("black","red","blue"),lty=c(1,1,1), cex=0.5)
