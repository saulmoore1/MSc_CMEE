#!/usr/bin/env python

"""List comprehension exercise to create lists derived from a dataset of average UK rainfall in 1910"""

__author__ = 'Saul Moore sm5911@imperial.ac.uk'
__version__ = '0.0.1'

# Average UK Rainfall (mm) for 1910 by month
# http://www.metoffice.gov.uk/climate/uk/datasets

rainfall = (('JAN',111.4),
            ('FEB',126.1),
            ('MAR', 49.9),
            ('APR', 95.3),
            ('MAY', 71.8),
            ('JUN', 70.2),
            ('JUL', 97.1),
            ('AUG',140.2),
            ('SEP', 27.0),
            ('OCT', 89.4),
            ('NOV',128.4),
            ('DEC',142.2),
           )

# (1) Use a list comprehension to create a list of month, rainfall tuples where the amount of rain was greater than 100 mm

rainfall100_lc = list(i for i in rainfall if i[1] > 100) # Create a list of tuples where the value of the second element in the tuple is greater than 100
print rainfall100_lc # Return result


# (2) Use a list comprehension to create a list of just month names where the amount of rain was less than 50 mm

rainfall50_lc = list(i[0] for i in rainfall if i[1] < 50) # Create a list of the first element of tuples where the value of the second element in the tuple is less than 50
print rainfall50_lc # Return result


# (3) Now do (1) and (2) using conventional loops

rainfall100 = [] # Create empty list called rainfall100
for i in rainfall: # For tuples in rainfall
	if i[1] > 100: # If the value of the second element is greater than 100
		rainfall100.append(i) # Append the tuple to the empty list
print rainfall100 # Return result

rainfall50 = [] # Create empty list called rainfall50
for i in rainfall: # For tuples in rainfall
	if i[1] < 50: # If the value of the second element is less than 50
		rainfall50.append(i[0]) # Append the first element of the tuple to the empty list
print rainfall50 # Return result
