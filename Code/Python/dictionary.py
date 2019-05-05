#!/usr/bin/env python

"""A short python script to populate a dictionary called taxa_dic that maps order names to sets of taxa"""

__author__ = 'Saul Moore sm5911@imperial.ac.uk'
__version__ = '0.0.1'

taxa = [ ('Myotis lucifugus','Chiroptera'),
         ('Gerbillus henleyi','Rodentia',),
         ('Peromyscus crinitus', 'Rodentia'),
         ('Mus domesticus', 'Rodentia'),
         ('Cleithrionomys rutilus', 'Rodentia'),
         ('Microgale dobsoni', 'Afrosoricida'),
         ('Microgale talazaci', 'Afrosoricida'),
         ('Lyacon pictus', 'Carnivora'),
         ('Arctocephalus gazella', 'Carnivora'),
         ('Canis lupus', 'Carnivora'),
        ]

taxa_dic = {} # Create an empty dictionary
for row in taxa:
    if row[1] not in taxa_dic: # If the order name part of the tuple is not in the dictionary (which is the case, since the dictionary is empty)
        taxa_dic[row[1]] = set() # Then add the order name to the dictionary as a set
    taxa_dic[row[1]].add(row[0]) # Now add the species names to their appropriate order name sets
print taxa_dic

#==============================================================================
# row[1] for row in taxa if row[1] not taxa_dic
#==============================================================================


# Cannot do the following, as tuples are immutable???
#~ taxa_dic = {}
#~ species = taxa[0]
#~ order = taxa[1]
#~ for species, order in taxa: 
	#~ if order not in taxa_dic:
		#~ taxa_dic[order] = set()
	#~ taxa_dic[order].add[species]
#~ print taxa_dic
