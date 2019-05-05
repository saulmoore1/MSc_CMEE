#!/usr/bin/env python

"""Script to import 'sys' module and investigate some of its properties"""

__author__ = 'Saul Moore (sm5911@imperial.ac.uk)'
__version__ = '0.0.1'

import sys

print "This is the name of the script: ", sys.argv[0] # Prints the name of the module
print "Number of arguments: ", len(sys.argv) # Shows the number of arguments
print "The arguments are: ", str(sys.argv) # Prints arguments

def main(argv): # Main function, arguments obtained in the if (__name__ == "__main__"): part of the script are fed to this main function, where printing of the following line occurs
	print 'This is boilerplate'
	sys.exit(status)
