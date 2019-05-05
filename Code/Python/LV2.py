#!/usr/bin/env python

""" Practical 6.3.3 - Lotka-Volterra model problem: script 'LV1.py' modified to incorporate prey density-dependence and take parameter arguments from the commandline
Extra-credit: Both resource and consumer populations persist with prey density-dependence """

import scipy as sc
import scipy.integrate as integrate
import pylab as p # Contains matplotlib for plotting
import sys

# import matplotlip.pylab as p #Some people might need to do this

def dCR_dt(pops, t=0):
    """ Returns the growth rate of predator and prey populations at any
    given time step """

    R = pops[0]
    C = pops[1]
    dRdt = r*R*(1 - (R/K)) - a*C*R # Lotka-Volterra model for resource (prey), with density-dependence
    dCdt = -z*C + e*a*C*R # Lotka=Volterra model for consumer (predator)

    return sc.array([dRdt, dCdt])

# Define parameters:
if len(sys.argv) > 1:
	r = float(sys.argv[1]) # Resource growth rate
	a = float(sys.argv[2]) # Consumer (predator) search rate (determines consumption rate) - area per time - encounter rate
	z = float(sys.argv[3]) # Consumer mortality rate
	e = float(sys.argv[4]) # Consumer production efficiency
	K = float(sys.argv[5]) # The density dependence factor, the carrying capacity of the Resource (prey)
else: # If no commandline arguments are given, use the following default values:
	r = 1.
	a = 0.5
	z = 1.
	e = 0.5
	K = 30.


# Now define time -- integrate from 0 to 15, using 1000 points:
t = sc.linspace(0, 100,  10000) # Sets how tightly you want to follow the function using integration

x0 = 10
y0 = 5
# Pre-allocate array (initial conditions: 10 prey and 5 predators per unit area)
z0 = sc.array([x0, y0])

# Employ the function 'dCR_dt' defined above to integrate over t=100, using 10000 sub-breaks
pops, infodict = integrate.odeint(dCR_dt, z0, t, full_output=True)

infodict['message']     # >>> 'Integration successful.'

prey, predators = pops.T # What's this for? To transpose the output
f1 = p.figure() # Open empty figure object
p.plot(t, prey, 'g-', label='Resource density') # Plot prey population over time (green)
p.plot(t, predators  , 'b-', label='Consumer density') # Add predator population over time
p.grid() # Add grid lines
p.legend(loc='best') # Add legend, automatically allocate position
p.xlabel('Time')
p.ylabel('Population')
p.title('Consumer-Resource population dynamics')
p.annotate('r=%r, a=%r, z=%r, e=%r, K=%r' %(r,a,z,e,K), xy=(0,0), xytext=(45,8))
p.show() # Show figure
f1.savefig('../Results/LV2_plot.pdf') # Save figure to results

# Print final population densities
print "Resource population density is %f" % pops [-1,0]
print "Consumer population density is %f" % pops [-1,1]
