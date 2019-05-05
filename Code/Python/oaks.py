#!/usr/bin/env python

"""List comprehension exercise: using implicit 'for' loops to investigate oak tree species"""

__author__ = 'Saul Moore (sm5911@imperial.ac.uk)'
__version__ = '0.0.1'

# Let's find just those taxa that are oak trees from a list of species

taxa = [ 'Quercus robur','Fraxinus excelsior','Pinus sylvestris','Quercus cerris','Quercus petraea',]

def is_an_oak(name):
	return name.lower().startswith('quercus ') # Converts to lower case lettering and searches for 'quercus '. The space is important!
	
oaks_loops = set() # This is an 'implicit' loop, not 'explicit' because it is directly using the names, like strings, not indexing eg. name1, name2, etc
for species in taxa:
	if is_an_oak(species):
		oaks_loops.add(species)
print oaks_loops

# Using list comprehensions (lc), called so, because it works on lists, converting the list to a set
oaks_lc = set([species for species in taxa if is_an_oak(species)]) 
print oaks_lc

# Get names in UPPER CASE using for loops
oaks_loops = set()
for species in taxa:
	if is_an_oak(species):
		oaks_loops.add(species.upper()) # Add UPPER CASE species names to the empty list oaks_loops
print oaks_loops

# Get names in UPPER CASE using list comprehensions
oaks_lc = set([species.upper() for species in taxa if is_an_oak(species)]) # Same thing, but in one go
print oaks_lc
