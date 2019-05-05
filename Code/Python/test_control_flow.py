#!/usr/bin/env python

"""Using 'Doctest' module to test for any errors in the code"""

# Docstrings are considered part of the running code (normal comments are
# stripped). Hence, you can access your docstrings at run time.

__author__ = 'Saul Moore sm5911@imperial.ac.uk'
__version__ = '0.0.1'

# Imports
import sys
import doctest # Import doctest module

def even_or_odd(x=0): #If not specified, x should take the value 0
	"""Find whether a number x is even or odd.
	
	>>> even_or_odd(10)
	'10 is Even!'
	
	>>> even_or_odd(5)
	'5 is Odd!'
	
	whenever a float is provided, then the closest integer is used:
	>>> even_or_odd(3.2)
	'3 is Odd!'
	
	in case of negative numbers, the positive is taken:
	>>> even_or_odd(-2)
	'-2 is Even!'
	
	"""
	# Define function to be tested
	if x % 2 == 0: # Checks if the remainder is = 0
		return '%d is Even!' % x
	return '%d is Odd!' % x

# Suppressed the following because it is not needed to test my code

#~ def main(argv): # def defines the function 'main'
	#~ # sys.exit("dont want to do this right now!")
	#~ print even_or_odd(22)
	#~ print even_or_odd(33)
	#~ return 0

#~ if (__name__ == "__main__"):
	#~ status = main(sys.argv) # sys.argv is one of the modules from sys package, and makes sure the 'main' function is called from the commandline
	#~ sys.exit(status)
	
doctest.testmod() # To run with embedded tests
