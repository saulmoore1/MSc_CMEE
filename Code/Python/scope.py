#!/usr/bin/env python

"""Variable Scope - Global variables, varaibles that are visible inside and outside of functions. NOTE: careful with assigning names to globals, as you could 'expose' this to other functions in your workspace/namespace"""

__author__ = 'Saul Moore (sm5911@imperial.ac.uk)'
__version__ = '0.0.1'

_a_global = 10

def a_function(): # 'def' defines a function within the module, in this case, 'a_function'
		_a_global = 5 # _a_global variable, assigned as 5
		_a_local = 4 # _a_local variable, assigned as 4
		print "Inside the function, the value is ", _a_global
		print "Inside the function, the value is ", _a_local
		return None 
		
a_function()
print "Outside the function, the value is ", _a_global


_a_global = 10 # Global variable re-assigned as 10

def a_function():
		global _a_global
		_a_global = 5
		_a_local = 4
		print "Inside the function, the value is ", _a_global
		print "Inside the function, the value is ", _a_local
		return None
		
a_function()
print "Outside the function, the value is ", _a_global
