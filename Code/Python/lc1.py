#!/usr/bin/env python

"""Separating tuples of bird species into lists comprising latin names, common names, and mean body mass for each species respectively, using list comprehension and basic loops"""

__author__ = 'Saul Moore sm5911@imperial.ac.uk'
__version__ = '0.0.1'

birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
         )

#(1) Three separate list comprehensions containing the latin names, common names and mean body masses for each species in birds, respectively. 

latin_names_lc = list([i[0] for i in birds]) # Creates a list (latin_names_lc) from the first part of each tuple in birds
print latin_names_lc # Prints the list

common_names_lc = list([j[1] for j in birds]) # Creates a list (common_names_lc) from the first part of each tuple in birds
print common_names_lc # Prints the list

bodymass_names_lc = list([k[2] for k in birds]) # Creates a list (bodymass_names_lc) from the first part of each tuple in birds
print bodymass_names_lc # Prints the list

# (2) As above, but this time using conventional loops: 

latin_names = [] # Creates an empty list called latin_names
for i in birds: 
	latin_names.append(i[0]) # Appends to the empty list just the first part of each tuple
print latin_names

common_names = [] # Creates an empty list called common_names
for j in birds:
	common_names.append(j[1]) # Appends to the empty list the middle part of each tuple
print common_names

bodymass_names = [] # Creates an empty list called bodymass_names
for k in birds:
	bodymass_names.append(k[2]) # Appends to the empty list the last part of each tuple
print bodymass_names
