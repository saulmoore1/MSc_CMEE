#!/usr/bin/env python

"""Basic script containing a bug to be investigated using pdb"""

__author__ = 'Saul Moore sm5911@imperial.ac.uk'
__version__ = '0.0.1'

import pdb

def createabug(x):
	y = x**4
	z = 0.
	y = y/z # Here is the bug! Cannot divide by zero. Returns: ZeroDivisionError: float division by 0
	return y

createabug(25)
