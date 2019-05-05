#!/usr/bin/env python

""" Practical 6.3.3 Extra-extra credit - A discrete-time version of the Lotka-Volterra model """

import scipy as sc
import scipy.integrate as integrate
import pylab as p # Contains matplotlib for plotting
import sys

# Define parameters, giving defaults if no values are specified at the commandline:
if len(sys.argv) > 1:
	r = float(sys.argv[1]) # Resource growth rate
	a = float(sys.argv[2]) # Consumer (predator) search rate (determines consumption rate) - area per time - encounter rate
	z = float(sys.argv[3]) # Consumer mortality rate
	e = float(sys.argv[4]) # Consumer production efficiency
	K = float(sys.argv[5]) # The density dependence factor, the carrying capacity of the Resource (prey)
	t = int(sys.argv[8]) # Time
	R = float(sys.argv[7]) # Initial prey (resource) population
	C = float(sys.argv[6]) # Initital predator (consumer) population
else:
	r = 1.
	a = 0.1
	z = 1.5
	e = 0.75
	K = 30.
	t = 100
	R = 10
	C = 5

pops = sc.array([[0, R, C]]) # Pre-allocate array 'pops', where initial time = 0, and R (resource) and C (consumer) initial populations are as defined above

# Loop to iterate through time, for every discrete integer between 0 and t, and append population of R and C to the array each iteration. If either R or C go extinct, the loop is set to 'break'

for i in range(0, t):
	Rt = (pops[i,1])*(1+r*(1-(pops[i,1])/K)-a*(pops[i,2]))
	Ct = (pops[i,2])*(1-z+e*a*(pops[i,1]))
	pops = sc.append(pops, [[i+1, Rt, Ct]], axis = 0)
	if Rt < 0 or Ct < 0:
		break

print pops


t, prey, predators = pops.T # 'pops.T' transposes the array 'pops', ordering the three columns of the array into three groups called 't' (time), 'prey', 'predators'.

f1 = p.figure() # Open empty figure object
p.plot(t, prey, 'g-', label='Resource density') # Plot prey population over time
p.plot(t, predators  , 'b-', label='Consumer density') # Add predator population over time
p.grid()
p.legend(loc='best') # Auto legend location
p.xlabel('Time') # Axes labels
p.ylabel('Population')
p.title('Consumer-Resource population dynamics') # Title...
# Print the input values of r,a,z,e and K for the model on the figure
p.annotate('r=%r, a=%r, z=%r, e=%r, K=%r' %(r,a,z,e,K), xy=(0,0), xytext=(50,8))
p.show()
f1.savefig('../Results/LV3_plot.pdf') # Save figure

# Print final population densities to screen
print "Resource population density is %f" % pops [-1,1]
print "Consumer population density is %f" % pops [-1,2]
