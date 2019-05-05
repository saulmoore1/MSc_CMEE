#!/usr/bin/python

"""This is a docstring! Description: A simple boilerplate for Python to demonstrate proper scripting nomenclature and introduce the 'sys' module, and some of its functions"""
		
__author__ = 'Saul Moore sm5911@imperial.ac.uk'
__version__ = '0.0.1'

# Imports
import sys # sys module provides access to some objects used by the interpreter, and some functions that interact with the interpreter

# Constants can go here: 
# eg Boltsmann / Lotka-Volterra constants

# Functions can go here
def main(argv): # 'def' defines the function called 'main', and 'argv' bestows command-line arguments
	print 'This is a boilerplate' # Indented using one or two tabs (2 or 4 spaces)
	return 0

if (__name__ == "__main__"): # Reads main first, then calls the functions specified by main. With this, the file is usable as a script as well as an importable module!
	status = main(sys.argv) # sys.argv is one of the modules from sys package, and makes sure the 'main' function is called from the commandline.
	sys.exit(status)

# All functions are modules, but not all modules are just one function, they can comprise of multiple functions.
