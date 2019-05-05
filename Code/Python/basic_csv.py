#!/usr/bin/env python

"""Simple script to demonstrate the 'csv' module and its read and write functions. Reads testcsv.csv file and produces bodymass.csv file, located in the 'Sandbox' directory"""

__author__ = 'Saul Moore (sm5911@imperial.ac.uk)'
__version__ = '0.0.1'

import csv # Imports csv module 

f = open('../Sandbox/testcsv.csv','rb') # Assign variable 'f' to testcsv.csv file (read, binary)

csvread = csv.reader(f) # Read csv 
temp = [] # Create empty list
for row in csvread:
	temp.append(tuple(row)) # Add tuples to empty list
	print row
	print "The species is", row[0] # Print only the first element of the tuple

f.close() # Close the file

f = open('../Sandbox/testcsv.csv','rb') # rb only bestows read permissions
g = open('../Sandbox/bodymass.csv','wb') # wb bestows read AND WRITE permissions

csvread = csv.reader(f) # csv.reader function in csv module reads the csv contents only
csvwrite = csv.writer(g) # csv.writer function in csv module creates a csv for file for data entry
for row in csvread:
	print row # Indentation matters greatly in Python
	csvwrite.writerow([row[0], row[4]]) # Enter just species name and body mass in csvwrite ('bodymass.csv')

f.close()
g.close() # Close both files
