#!/usr/bin/env python

"""Week2 Practical - Identify bugs in script to identify oak tree species, using 'doctest' module to find and correct the script"""

__author__ = 'Saul Moore sm5911@imperial.ac.uk'
__version__ = '0.0.1'

import csv
import sys
import doctest

#Define function
def is_an_oak(name):
    """ Returns True if name is starts with 'quercus '
        >>> is_an_oak('quercus')
        True
        
        >>> is_an_oak('pinus')
        False
        
        >>> is_an_oak('fraxinus')
        False
        
        >>> is_an_oak('fagus sylvatica')
        False
        
        >>> is_an_oak('quercuss')
        False
        
    """
    return name.lower() == 'quercus' # REMOVE SPACE after 'quercus ' / REMOVE STARTSWITH, replace with '=='
    
print(is_an_oak.__doc__)

def main(argv): 
    f = open('../Data/TestOaksData.csv','rb') # Open 'TestOaksData.csv' file
    g = open('../Results/JustOaksData.csv','wb') # Create 'JustOaksData.csv file in 'Results' directory
    taxa = csv.reader(f)
    csvwrite = csv.writer(g)
    #~ oaks = set() REMOVE - NOT NECESSARY
    for row in taxa:
        print row # Cycle through the lines of the file and print contents
        print "The genus is", row[0]
        if is_an_oak(row[0]):
            print row[0]
            print 'FOUND AN OAK!'
            print " "
            csvwrite.writerow([row[0], row[1]])    
    return 0
    
if (__name__ == "__main__"):
    status = main(sys.argv)

doctest.testmod()
