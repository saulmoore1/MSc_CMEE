#!/usr/bin/env python

"""Script introducing basic 'for' loops in Python"""

__author__ = 'Saul Moore (sm5911@imperial.ac.uk)'
__version__ = '0.0.1'

# for loops in Python
for i in range(5): # For input variable 0-4
	print i

my_list = [0, 2, "geronimo!", 3.0, True, False] # [int, int, str, float, bool, bool]
for k in my_list:
	print k # Print list

total = 0
summands = [0, 1, 11, 111, 1111] 
for s in summands:
	print total + s
# Loops through list and returns first element, second element...etc on new lines


# 'While' loops in Python
z = 0
while z < 100: 
	z = z + 1 # Add one each time
	print (z) # Print

b = True
while b:
	print "GERONIMO! infinite loop! ctrl+c to stop!"
# INFIINITE LOOP! ctrl + c to stop!
