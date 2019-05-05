#!/usr/bin/env python

""" Using the subprocess.os module to loop through home directory and
return a list of files and/or directories beginning with upper- or lower-case 'C' """

__author__ = "Saul Moore sm5911@imperial.ac.uk"
__version__ = "0.0.1"

# Use the subprocess.os module to get a list of files and directories
# Hint: look in subprocess.os and/or subprocess.os.path and/or
# subprocess.os.walk for helpful functions

import subprocess

########################################################################

# Get a list of files and directories in your home/ that start with an uppercase 'C'
# Use a for loop to walk through the home directory

# Type your code here:
# Get the user's home directory
home = subprocess.os.path.expanduser("~") # Catch user's home directory and call it 'home'

# Create an empty list to store the results
FilesDirsStartingWithC = []

# Using 'subprocess.os.walk()' in a for loop to trawl through the home/ directory
for (dir, subdirectories, files) in subprocess.os.walk(home):
	for dirname in subdirectories:
		if dirname.startswith('C'):
			FilesDirsStartingWithC.append(dirname)
	for filename in files:
		if filename.startswith('C'):
			FilesDirsStartingWithC.append(filename)

a = set(FilesDirsStartingWithC)
print "Number of files and directories in home/ that start with an uppercase 'C' is:", len(a)

#################################
# Get files and directories in your home/ that start with either an
# upper or lower case 'C'

# Type your code here:
# Create an empty list to store the results.

FilesDirsStartingWithCc = []

for (dir, subdirectories, files) in subprocess.os.walk(home):
	for dirname in subdirectories:
		if dirname.startswith('C') or dirname.startswith('c'):
			FilesDirsStartingWithCc.append(dirname)
	for filename in files:
		if filename.startswith('C') or filename.startswith('c'):
			FilesDirsStartingWithCc.append(filename)

b = set(FilesDirsStartingWithCc)
print "Number of files and directories in home/ that start with either an uppercase 'C' or lowercase 'c' is:", len(b)


#################################
# Get only directories in your home/ that start with either an upper or
#~lower case 'C'

# Type your code here:
DirsStartingWithCc = []

for dirpath, dirnames, filenames in subprocess.os.walk(home):
	for dirname in dirnames:
		if dirname.startswith('C') or dirname.startswith('c'):
			DirsStartingWithCc.append(dirname)

c = set(DirsStartingWithCc)
print "Number of directories in home/ that start with either an uppercase 'C' or lowercase 'c' is:", len(c)
