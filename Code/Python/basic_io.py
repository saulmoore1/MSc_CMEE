#!/usr/bin/python

"""A basic input/output script to (#1) Read and print the contents of a file; (#2) Remove empty lines in a file; (#3) Create and save a list; (#4) Save objects in Python for later; and (#5) Load the saved objects again. Produces the following files in 'Sandbox' directory: test.txt, testout.txt, testp.p"""

__author__ = 'Saul Moore (sm5911@imperial.ac.uk)'
__version__ = '0.0.1'


#1 - Open a file and read and print the contents
f = open('../Sandbox/test.txt', 'r') # Read-only permissions on the file
for line in f:
	print line, # use "implicit" for loop (file contains strings) if the object is a file, python will cycle over lines. The "," prevents adding a new line

f.close() # Closes the file


#2 - Remove empty lines
f = open('../Sandbox/test.txt', 'r') # 'r' only reads the file
for line in f:
	if len(line.strip()) > 0: # Skip blank lines
		print line, 
		
f.close()


#3 - Create and save a list of numbers from 0-99 on separate lines
list_to_save = range(100) # so from 0-99

f = open('../Sandbox/testout.txt','w') # 'w' bestows write permissions
for i in list_to_save:
	f.write(str(i) + '\n') # Puts each output as a string on a new line
	
f.close()


#4 - Saves an object in a *.p file for later use in Python
my_dictionary = {"a key": 10, "another key": 11}

import pickle # Creates portable serialized representations of Python objects, apparently

f = open('../Sandbox/testp.p','wb') # 'w' gives write permissions and 'b' opens the file in binary mode
pickle.dump(my_dictionary, f) # 'pickle' module dumps the object(s) in the created file
f.close()


#5 - Load the data again
f = open('../Sandbox/testp.p','rb') # Read-only permissions + binary mode
another_dictionary = pickle.load(f)
f.close()

print another_dictionary
