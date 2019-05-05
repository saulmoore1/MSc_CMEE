#!/usr/bin/env python

""" Profiling functions in Python using '%run -p' to compare function speed """

__author__ = 'Saul Moore sm5911@imperial.ac.uk'
__version__ = '0.0.1'

def a_useless_function(x):
	""" Exploring the speed of 'xrange' vs 'range' """
	y = 0
	for i in xrange(100000000): # Eight zeros!
		y = y + 1
	return 0
	
def a_less_useless_function(x):
	""" Exploring the speed of 'xrange' vs 'range' """
	y = 0
	for i in xrange(100000): # Five zeros!
		y = y + 1
	return 0
	
def some_function(x): # A function to call the other two functions and print value of x
	""" Function to print the value of 'x' when called and run the two functions: 'a_useless_function' and 'a_less_useless_function' """
	print x
	a_useless_function(x)
	a_less_useless_function(x)
	return 0
	
some_function(1000) # Execute function cascade


# Run with '%run -p profileme.py', which lists internal time calculations for time taken to perform each function and outputs each result:
# OUTPUT:
# 56 function calls in 4.717 seconds
# ...

# Now try using xrange instead of range:
# OUTPUT:
# 54 function calls in 2.907 seconds
# ...

# When iterating over a large number of values, xrange, unlike range, does not create all the values before iteration, but creates them "on demand" eg. range(1000000) yields a 4Mb+ list. 

# So we saved 1.81 secs!

