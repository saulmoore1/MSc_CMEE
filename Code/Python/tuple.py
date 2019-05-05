#!/usr/bin/env python

"""Simple script to print latin name, common name and body mass on separate lines for each species"""

__author__ = 'Saul Moore sm5911@imperial.ac.uk'
__version__ = '0.0.1'

birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
        )

for species in birds: # For each tuple in birds, that you are telling python are grouped as 'species' 
	print species # Return result

