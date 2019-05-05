rm(list=ls())
##assign values to parameters
N0<-c(0.1,0.1) # Initial concentration of bacteria 1 and 2
S0<-c(5,1) # Initial concentration of substrate 1 and 2
P0<-c(0.1,0.1) # Initial concentration of phage 1 and 2
d<-0.01 # Dilution rate of chemostat
kcat<-matrix(c(0.1,0.1,0.1,0.1),nrow=2) # Michaelis-Menten catalytic rate (NB: Vmax = kcat*E)
Km<-matrix(c(0.5,0.5,0.5,0.5),nrow=2) # Substrate concentration at which the reaction rate is half of Kcat
c.conv<-matrix(c(1,1,1,1),nrow=2) # Conversion rate into growth of bacteria
mu<-c(0.01,0.01) # Mutation rate
pHopt<-7 # Added pH parameter to the model!!!
pH <- 7 # mean pH of system
pH<-(exp((-(pH-pHopt)^2)/2*(1.5)^2)) # Distribution about mean pH
Topt <- c(15,25) # Optimum temperature of bacteria 1 and 2
a <- c(0.1,0.1) # Conversion rate of bacteria 1 and 2 to phage 1 and 2, respectively
b <- c(0.1,0.1) # Feeding rate of phage 1 and 2
dp <- 0.1 # Death rate of phage

##set up so can have different parameter for each species X substrate
##column = species, row = substrate
E<-matrix(c(0.7,0.3,0.3,0.7),nrow=2,ncol=2) # Species 1 and 2 specialisation on substrate 1 and 2, respectively
R<-matrix(c(0.3,0.7,0.3,0.7),nrow=2,ncol=2) # Species 1 and 2 capacity for resistance 

##choose time steps and total time
time.step<-0.1
times<-seq(0,1000,time.step)
Temp<- seq(20,20, length.out = length(times))
##set initial values of state variables
S<-S0
N<-N0
P<-P0

##store results
results<-matrix(NA,nrow=length(times)+1,ncol=17)

	##store time 0 values
	results[1,1]<-0
	results[1,2:3]<-S
	results[1,4:5]<-N
	results[1,6:7]<-P
	results[1,8:11]<-E
	results[1,12:13]<-Topt
	results[1,14:17]<-R

##loop through the times
for (i in (1:length(times))) {
  T1<-exp((-(Temp[i]-Topt[1])^2)/40)
  T2<-exp((-(Temp[i]-Topt[2])^2)/40)
	##assign new values
	S[1] <- S[1]-(kcat[1,1]*E[1,1]*S[1]*pH*T1/(Km[1,1]+S[1]))*N[1]*time.step-(kcat[1,2]*E[1,2]*S[1]*pH*T1/(Km[1,2]+S[1]))*N[2]*time.step-d*S[1]*time.step+d*S0[1]*time.step
	S[2] <- S[2]-(kcat[2,1]*E[2,1]*S[2]*pH*T2/(Km[2,1]+S[2]))*N[1]*time.step-(kcat[2,2]*E[2,2]*S[2]*pH*T2/(Km[2,2]+S[2]))*N[2]*time.step-d*S[2]*time.step+d*S0[2]*time.step
	N[1] <- N[1]+(c.conv[1,1]*(kcat[1,1]*E[1,1]*S[1]*pH*T1/(Km[1,1]+S[1]))*N[1]*time.step)+(c.conv[2,1]*(kcat[2,1]*E[2,1]*S[2]*pH*T1/(Km[2,1]+S[2]))*N[1]*time.step)-d*N[1]*time.step - b[1]*P[1]*N[1]*R[1,2]*time.step
	N[2] <- N[2]+(c.conv[1,2]*(kcat[1,2]*E[1,2]*S[1]*pH*T2/(Km[1,2]+S[1]))*N[2]*time.step)+(c.conv[2,2]*(kcat[2,2]*E[2,2]*S[2]*pH*T2/(Km[2,2]+S[2]))*N[2]*time.step)-d*N[2]*time.step - b[2]*P[2]*N[2]*R[2,2]*time.step
	P[1] <- P[1]+a[1]*N[1]*P[1]*time.step-dp*P[1]*time.step-d*P[1]*time.step
	P[2] <- P[2]+a[2]*N[2]*P[2]*time.step-dp*P[2]*time.step-d*P[2]*time.step

	##change in enzyme for substrate 1 in each species
	##this is mutation parameter X the selection gradient, namely how (1/N)dN.dt changes as you change enzyme allocation
	E[1,1]<- E[1,1]+mu[1]*((c.conv[1,1]*(kcat[1,1]*S[1]*pH*T1/(Km[1,1]+S[1])))-(c.conv[2,1]*(kcat[2,1]*S[2]*pH*T1/(Km[2,1]+S[2]))))*time.step
	E[1,2]<- E[1,2]+mu[2]*((c.conv[1,2]*(kcat[1,2]*S[1]*pH*T2/(Km[1,2]+S[1])))-(c.conv[2,2]*(kcat[2,2]*S[2]*pH*T2/(Km[2,2]+S[2]))))*time.step
	
	Topt[1]<- Topt[1]+mu[1]*((c.conv[1,1]*(kcat[1,1]*E[1,1]*S[1]*pH*T1*(Temp[i]-Topt[1])/20*(Km[1,1]+S[1])))-(c.conv[2,1]*(kcat[2,1]*S[2]*pH*T1/20*(Km[2,1]+S[2]))))*time.step
	Topt[2]<- Topt[2]+mu[2]*((c.conv[1,2]*(kcat[1,2]*E[1,2]*S[1]*pH*T2*(Temp[i]-Topt[2])/20*(Km[1,2]+S[1])))-(c.conv[2,2]*(kcat[2,2]*S[2]*pH*T2/20*(Km[2,2]+S[2]))))*time.step
	
	R[1,1]<-R[1,1]+mu[1]*b[1]*P[1]*time.step
	R[1,2]<-R[1,2]+mu[2]*b[2]*P[2]*time.step

##corresponding opposite change in enzymes for substrate 2
	E[2,1]<-1-E[1,1]
	E[2,2]<-1-E[1,2]
	R[2,1]<-1-R[1,1]
	R[2,2]<-1-R[1,2]
	
##enforce boundary condition that total enzyme production is 1
	if(min(E)<0) E[E<0]<-0
	if(max(E)>1) E[E>1]<-1
	if(min(R)<0) R[R<0]<-0
	if(max(R)>1) R[R>1]<-1
	if(N[1] < 0.01){
	  N[1]<-0
	}
	if(N[2] < 0.01){
	  N[2]<-0
	}
	
	results[i+1,1]<-times[i]
	results[i+1,2:3]<-S
	results[i+1,4:5]<-N
	results[i+1,6:7]<-P
	results[i+1,8:11]<-E
	results[i+1,12:13]<-Topt
	results[i+1,14:17]<-R
	
}

##plot the results

par(mfrow=c(1,3))
matplot(results[,1], results[,2:7], type="l", xlab="Generation", 
        ylab="Density/Concentration",col=c("black","red","black","red","blue","green"),
        lty=c(3,3,1,1,2,2))
legend("topright",legend=c("S1","S2","N1","N2","P1","P2"), col=c("black","red","black","red","blue","green"),lty=c(3,3,1,1,2,2), cex=0.5)

matplot(results[,1], results[,8:11], type="l", xlab="Generation", 
        ylab="Proportion of Enzyme per Substrate",col=c("black","red"),lty=c(1,1,3,3))
legend("topright",legend=c("Spec1.Subst1","Spec1.Subst2","Spec2.subst1","Spec2.subst2"), col=c("black","red"),lty=c(1,1,3,3), cex=0.5)

matplot(results[,1], results[,14:17], type="l", xlab="Generation", 
        ylab="Proportion of Resistant Bacteria",col=c("black","red"),lty=c(1,1,3,3))
legend("topright",legend=c("Spec1.R","Spec1.D","Spec2.R","Spec2.D"), col=c("black","red"),lty=c(1,1,3,3), cex=0.5)
