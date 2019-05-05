#!/usr/bin/env python

""" The typical Lotka-Volterra Model, simulated using scipy to perform numerical integration for solving ordinary differential equations (ODEs) """

import scipy as sc # import scipy, call it sc
import scipy.integrate as integrate
import pylab as p # Contains matplotlib for plotting

# import matplotlip.pylab as p # Some computers might need to do this

# define function dR_dt, to calculate the integral for each point when called later by 'intergrate.odeint'

def dR_dt(pops, t=0): 
    """ Returns the growth rate of predator and prey populations at any 
    given time step """
    
    R = pops[0]
    C = pops[1]
    dRdt = r*R - a*R*C 
    dydt = -z*C + e*a*R*C
    
    return sc.array([dRdt, dydt])

# Define parameters:
r = 1. # Resource growth rate
a = 0.1 # Consumer search rate (determines consumption rate) 
z = 1.5 # Consumer mortality rate
e = 0.75 # Consumer production efficiency

# Now define time -- integrate from 0 to 15, using 1000 points:
t = sc.linspace(0, 15,  1000)

x0 = 10 # Prey (resource)
y0 = 5  # Predator (consumer)
z0 = sc.array([x0, y0]) # initial conditions: 10 prey and 5 predators per unit area

# integrate.odeint(function 'dR_dt' defined above, intial array containing starting conditions, time as defined by 'sc.linspace'): 
pops, infodict = integrate.odeint(dR_dt, z0, t, full_output=True)

infodict['message']     # >>> 'Integration successful.'

prey, predators = pops.T # What is this for?
# 'pops.T' transposes the array 'pops', ordering the two columns of the array into two groups called 'prey' and 'predators'. It is now possible to do some plotting.

f1 = p.figure() # Open empty figure object
p.plot(t, prey, 'g-', label='Resource density') # Plot prey over time, colour green, label
p.plot(t, predators  , 'b-', label='Consumer density') # Plot predator over time, blue
p.grid() # Add a grid for easy interpretation
p.legend(loc='best') # Slap on a legend
p.xlabel('Time') 
p.ylabel('Population') # x and y labels
p.title('Consumer-Resource population dynamics') # ..a title
#~ p.show() # Print to screen
f1.savefig('../Results/prey_and_predators_1.pdf') # Save figure to results directory
