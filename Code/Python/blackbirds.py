#!/usr/bin/env python

""" Blackbirds Practical - Using regular expressions to extract kingdom, phylum and genus-species data for each species in a 'messy' text file and present the information neatly for easy readability """

__author__ = "Saul Moore sm5911@imperial.ac.uk"
__version__ = "0.0.2"

import re

# Read the file
f = open('../Data/blackbirds.txt', 'r')
text = f.read()
f.close()

# remove tabs and new lines and replace with a space:
text = text.replace('\t',' ')
text = text.replace('\n',' ')

# note that there are "strange characters" (these are accents and
# non-ascii symbols) because we don't care for them, first transform
# to ASCII:
text = str(text.decode('ascii', 'ignore'))

# Now write a regular expression my_reg that captures # the Kingdom,
# Phylum and Species name for each species and prints it out neatly:

# The following regex expressions employ a 'negative lookbehind' approach, using '?<=\b' to match, in all instances, any word of any length of characters, but specifically after the strings 'Kingdom' and 'Phylum'. Match also the two following words after the string 'Species' (genus-species; binomial nomenclature). Function 'zip' concatenates the results into tuples of respective elements from each 're.findall' search:

my_regex = zip(re.findall(r'(?<=\bKingdom\s)\w*', text), re.findall(r'(?<=\bPhylum\s)\w*', text), re.findall(r'(?<=\bSpecies\s)\w*\s\w*', text))

# Using 'xrange' to sum the length ('len') of Kingdom and iterate through the objects
for i in range(len(re.findall(r'(?<=\bKingdom\s)[\w]*', text))):
	num = i + 1
	print "\nBlackbird species", num, "is:"
	print "Kingdom:", my_regex[i][0]
	print "Phylum :", my_regex[i][1]
	print "Species:", my_regex[i][2]

# Keep in mind that there are multiple ways to skin this cat!
