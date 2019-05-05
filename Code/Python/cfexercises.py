#!/usr/bin/env python

"""Python script exploring control flow with basic recursive functions (loops) #1-5. Script has been converted into a module, where functions #6-10 can be called from the commandline"""

__author__ = 'Saul Moore sm5911@imperial.ac.uk'
__version__ = '0.0.1'

import sys

#1
for i in range(3, 17):
		print 'hello' # Should print 14 hello's, from 3 to 17

#2	
for j in range(12):
	if j % 3 == 0:
		print 'hello' # For j divided by 3, match any result that has remainder 0. 4 hello's because 0 is divisible by 3 to give infinity, which has no remainder, apparently.

#3		
for j in range(15):
	if j % 5 == 3:
		print 'hello' # For j divided by 5, match any result that has remainder 3. Range 15 means from 0 - 14 in python. Modulus (%) looks at the remainder (eg *.33333333).
	elif j % 4 == 3: # Do the same for j/4
		print 'hello' 

#4		
z = 0
while z != 15: # != is not equal to
	print 'hello'
	z = z + 3 # Adds 3 and loops back; so z = 0, 3, 6, 9, 12, 15. Ends at 15, because no else or elif was specified. As soon as the condition is violated in a 'while' loop, it ends. This is not the case with an 'if' loop.

#5
z = 12 # Starts at 12
while z < 100:
	if z == 31: # When loop reaches z = 31, start loop for k
		for k in range(7):
			print 'hello' # Prints hello 7 times, from 0 - 6, until k = 7, then the condition is violated, and loop commences with z
	elif z == 18:
		print 'hello' # The first hello prints when loop reaches z = 18
	z = z + 1 # 8 hello's in total

"""Function to calculate x to the power of 0.5"""
#6
def Sqroot(x): 
	return x ** 0.5 # x to the power of 0.5

"""Function to return the largest number"""
#7
def Largest(x, y):
	if x > y:
		return x
	return y # With input x=2, y=3, should print y

"""Function to sort/swap variables depending on size"""
#8
def Swap1(x, y, z):
	if x > y:
		x, y = y, x # Coding like a pythonista! :D
	if y > z:
		z, y = y, z
	return [x, y, z] # A cool way of swapping things
	
	
def Swap2(x, y, z):
	if x > y:
		tmp = y
		y = x
		x = tmp
	if y > z:
		tmp = z
		z = y
		y = tmp
	return [x, y, z]
	
"""Recursive function to perform a factorial calculation"""
#9
def Factorial1(x):
	result = 1
	for i in range(1, x +1):
		result = result * i
	return result # 1 * 2 * 3 * 4 * 5 * 6 * 7 (7 factorial, from 1 up to the number specified, in this case 7)
	
# This is a recursive function, meaning that the function calls itself
# en.wikipedia.org/wiki/Recursion_(computer_science)

"""Another recursive function to perform a factorial calculation"""
#10
def Factorial2(x):
	if x == 1:
		return 1
	return x * Factorial2(x - 1)
Factorial2(10) # Does the same factorial calculation as foo4, except it works down from the number specified, in this case 8

# Test arguments to demonstrate that the code works
def main(argv): # Function 'main' calls the following functions
	print Sqroot(1)
	print Largest(2, 3)
	print Swap1(6, 5, 4)
	print Swap2(6, 5, 4)
	print Factorial1(7)
	print Factorial2(8)
	
if (__name__ == "__main__"):
	status = main(sys.argv) # sys.argv is one of the modules from sys package, and allows the 'main' function to be called from the commandline
	sys.exit(status)
