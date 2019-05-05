#!/usr/bin/env python

""" Practical 6.3.3 Extra-extra-extra-extra credit - Lotka-Volterra model script 'LV4.py' modified to include additional random gaussian fluctuations in consumer population  """

import scipy as sc 
import scipy.integrate as integrate
import pylab as p # Contains matplotlib for plotting
import sys
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

	
pops = sc.array([[0, R, C]])
for i in range(0, t):
	E1 = random.uniform(0,1) # Included inside the loop, to be sure that new random numbers are generated each iteration
	E2 = random.uniform(0,1) # Separate random numbers for R and C, else it would not be truly random
	Rt = (pops[i,1])*(1+(r+E1)*(1-(pops[i,1])/K)-a*(pops[i,2]))
	Ct = (pops[i,2])*(1-z+E2+e*a*(pops[i,1])) # Added stachasiticity to predator population
	pops = sc.append(pops, [[i+1, Rt, Ct]], axis = 0)
	if Rt < 0 or Ct < 0:
		break		

print pops	


t, prey, predators = pops.T # Transpose of the array, three groups: t, R and C
f1 = p.figure() # Open empty figure object
p.plot(t, prey, 'g-', label='Resource density')
p.plot(t, predators  , 'b-', label='Consumer density')
p.grid()
p.legend(loc='best') # Legend
p.xlabel('Time') # Axes labels
p.ylabel('Population')
p.title('Consumer-Resource population dynamics') # Title
p.annotate('r=%r, a=%r, z=%r, e=%r, K=%r' %(r,a,z,e,K), xy=(0,0), xytext=(20,13))
#~ p.show()
f1.savefig('../Results/LV5_plot.pdf') # Save figure

# Final population densities of R and C are printed to screen
print "Resource population density is %f" % pops [-1,1]
print "Consumer population density is %f" % pops [-1,2]

