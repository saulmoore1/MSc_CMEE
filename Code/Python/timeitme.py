#!/usr/bin/env python

""" Quick profiling in Python using 'timeit' to test code speed """

__author__ = "Saul Moore sm5911@imperial.ac.uk"
__version__ = "0.0.1"

import time
import timeit

def a_not_useful_function():
	""" Simple, useless function to practice profiling using 'timeit' """
	y = 0
	for i in range(100000):
		y = y + i
	return 0

def a_less_useless_function():
	""" Another useless, but faster, function to practice profiling using 'timeit' """
	y = 0
	for i in xrange(100000):
		y = y + i
	return 0
	
# One approach is to time it like this:
start = time.time() # Gets current time
a_not_useful_function()
print "a_not_useful_function takes %f s to run." % (time.time() - start) 
# Subtract start time from final time to get total time for calculation
# Use %d or %f as above for pretty printing of numbers in strings in Python and R

start = time.time() # Take the start time
a_less_useless_function() # Perform the function
print "a_less_useless_function takes %f s to run." % (time.time() - start) # Calculate current time - start time

#~ a_not_useful_function takes 0.007541 s to run.
#~ a_less_useless_function takes 0.003033 s to run.


# But time taken seems to change a bit every time its run. Not very accurate. You can more accurately calculate time using %timeit in iPython:

# Run the following in terminal:
# %timeit a_not_useful_function()
# OUTPUT: 100 loops, best of 3: 3.61 ms per loop

# %timeit a_less_useless_function() 
# VECTORIZED OUTPUT: 100 loops, best of 3: 2.81 ms per loop


# For loops vs. list comprehensions!!!

my_list = range(1000)

def my_squares_loop(x):
	""" Loop function to square input value(s), for profiling using 'timeit' """
	out = []
	for i in x:
		out.append(i ** 2)
	return out
	
def my_squares_lc(x):
	""" List comprehension function to square input value(s), for profiling using 'timeit' """
	out = [i ** 2 for i in x]
	return out

# Run the following in terminal:
	
#~ %timeit my_squares_loop(my_list)
# OUTPUT: 10000 loops, best of 3: 99.5 microsec per loop
#~ %timeit my_squares_lc(my_list)
# VECTORIZED OUTPUT: 10000 loops, best of 3: 62.7 microsec per loop


# For loops vs. join method

import string
my_letters = list(string.ascii_lowercase)

def my_join_loop(l):
	""" Loop function to concatenate letters separated by spaces """
	out = ''
	for letter in l:
		out += letter
	return out

def my_join_method(l):
	""" Faster function for concatenating letters separated by spaces using 'join' """
	out = ''.join(l)
	return out

# Run the following in terminal:

#~ %timeit(my_join_loop(my_letters))
# OUTPUT: 1000000 loops, best of 3: 1.22 microsec per loop
#~ %timeit(my_join_method(my_letters))
# VECTORIZED OUTPUT: 1000000 loops, best of 3: 486 ns per loop


# Getting silly

def getting_silly_pi():
	""" A silly function, to be profiled using 'timeit' """
	y = 0
	for i in xrange(100000):
		y = y + i
	return 0
	
def getting_silly_pii():
	""" Another silly function, to be profiled using 'timeit' """
	y = 0
	for i in xrange(100000):
		y += i
	return 0
	
# Run the following in terminal:

#~ %timeit(getting_silly_pi())
# OUTPUT: 100 loops, best of 3: 2.84 ms per loop
#~ %timeit(getting_silly_pii())
# VECTORIZED OUTPUT: 100 loops, best of 3: 2.8 ms per loop






