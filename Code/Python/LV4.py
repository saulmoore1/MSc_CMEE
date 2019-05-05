#!/usr/bin/env python

""" Practical 6.3.3 Extra-extra-extra credit - Script 'LV3.py' modified to add random gaussian fluctuation in resource growth rate at each time-step """

import scipy as sc 
import scipy.integrate as integrate
import pylab as p # Contains matplotlib for plotting
import random


# Define parameters:
r = 1.
a = 0.1
z = 1.5
e = 0.75
K = 30.
t = 50
R = 10
C = 5

	
pops = sc.array([[0, R, C]]) # Pre-allocate array, start time = 0 and R and C as above
for i in range(0, t): # From 0 to t(max)
	E = random.uniform(0, 1) # Generate random uniform numbers using 'random' package
	Rt = (pops[i,1])*(1+(r+E)*(1-(pops[i,1])/K)-a*(pops[i,2])) # Includes random gaussian stochasticity 'E' in the equation
	Ct = (pops[i,2])*(1-z+e*a*(pops[i,1]))
	pops = sc.append(pops, [[i+1, Rt, Ct]], axis = 0) # Append to array 'pops'
	if Rt < 0 or Ct < 0:
		break		

print pops	


t, prey, predators = pops.T
f1 = p.figure() # Open empty figure object
p.plot(t, prey, 'g-', label='Resource density')
p.plot(t, predators  , 'b-', label='Consumer density')
p.grid() # Grid
p.legend(loc='best') # Legend
p.xlabel('Time') # Labels
p.ylabel('Population')
p.title('Consumer-Resource population dynamics') # Title
# Show model parameter values on figure:
p.annotate('r=%r, a=%r, z=%r, e=%r, K=%r' %(r,a,z,e,K), xy=(0,0), xytext=(10,10)) 
#~ p.show()
f1.savefig('../Results/LV4_plot.pdf') # Save figure to results

# Print final population densities of R and C to screen
print "Resource population density is %f" % pops [-1,1]
print "Consumer population density is %f" % pops [-1,2]

