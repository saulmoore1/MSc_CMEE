#!/usr/bin/env python

""" Regular expressions (regex) in Python, using the 're' module """

__author__ = "Saul Moore sm5911@imperial.ac.uk"
__version__ = "0.0.1"

import re

my_string = "a given string" # find a space in the string
match = re.search(r'\s', my_string) 
# r tells Python to read the string in its literal or 'raw' form, for regular expressions. Magic functions (regex elements, a.k.a metacharacters) can then be applied (see page 75)

print match # This should print something like <_sre.SRE_Match object at 0x7fef00af34a8>
match.group()

match = re.search(r's\w*', my_string) 
# Look from 's', matching any alphanumeric character, including underscore (but wont in this case, as it starts looking from 's'), zero or more times
match.group() # Should return only 'string'

# NOW AN EXAMPLE OF NO MATCH:
match = re.search(r'\d', my_string) # Looks for numeric characters only (a.k.a numbers)
print match # This should print 'None'

# Regular expressions in an if/else loop to know whether a pattern was matched
my_string = 'an example'
match = re.search(r'\w*\s', my_string) 
# Look for alphanumeric characters, zero or more times, that are followed by a space
if match:
	print 'found a match:', match.group() # Returns 'an '
else:
	print 'did not find a match'

MyStr = 'an example' # Exactly the same thing, just showing that you can name the variable whatever you like
match = re.search(r'\w*\s', MyStr) 
if match:
	print 'found a match:', match.group()
else:
	print 'did not find a match'
	
match = re.search(r'\d', "it takes 2 to tango") # Look for numeric characters only
print match.group() # 2

match = re.search(r'\s\w*\s', 'once upon a time') # Just gives you the FIRST example. Its 'lazy'. Use w*.* if you want it to be 'greedy' and return ' upon a '
match.group()

match = re.search(r'\s\w{1,3}\s', 'once upon a time') # Look for a space followed by between 1 to 3 alphanumeric characters together
match.group() # ' a ' as it is the only word of character length <= 3

match = re.search(r'\s\w*$', 'once upon a time') # '$' matches end of line
match.group() # ' time'

match = re.search(r'\w*\s\d.*\d', 'take 2 grams of H2O') # Look for a string of alphanumeric characters of any length, followed by a space, followed by a single number, then any length string of characters, finally followed by a number. So should ignore the letter 'O'
match.group() # 'take 2 grams of H2'

match = re.search(r'^\w*.*\s','once upon a time') # Match, from the beginning of a line, any number of alphanumeric characters, followed by any number of characters, finally followed by a space
match.group() # NOTE: *, +, and {} are all 'greedy': they repeat the previous regex token as many times as possible

match = re.search(r'^\w*.*?\s','once upon a time') # Match, from the beginning of a line, any number of alphanumeric characters, followed by any number of characters, without jumping a space. NB: ? makes the * not greedy.
match.group() # 'once '

# To further illustrate greediness, let's try matching an HTML tag:
match = re.search(r'<.+>', 'This is a <EM>first</EM> test') # Between < and >, match any character one or more times. + is greedy, and attempts to satisfy the regex token for as large a string as possible
match.group() # '<EM>first</EM>' 

# But we didnt want this, we wanted just <EM>
match = re.search(r'<.+?>', 'This is a <EM>first</EM> test') # The ? makes the + 'lazy', returning only the first match
match.group() # '<EM>'

match = re.search(r'\d*\.?\d*','1432.75+60.22i') 
# Note "\" before "." nullifies it as a metacharacter, treating it as a normal character. Any number of numerical characters, dont be greedy, and end with a numerical character.
match.group() # '1432.75'

match = re.search(r'\d*\.?\d*','1432+60.22i')
match.group() # '1432'

match = re.search(r'[AGTC]+', 'the sequence ATTCGT') # Look for any number/order of 'A', 'T', 'C' and 'G' characters
match.group() # 'ATTCGT'

re.search(r'\s+[A-Z]{1}\w+\s\w+', 'The bird-shit frog''s name is Theloderma asper').group() # Match all spaces, followed by a single capital character from A-Z, then match all alphanumeric characters up to and including a space, and then all alphanumeric characters thereafter.
# ' Theloderma asper'
# NB: You can directly return the result by appending .group()



#~ MyStr = 'Saul Moore, s.moore@imperial.ac.uk, Systems biology and ecological theory'

#~ match = re.search(r"[\w\s]*,\s[\w\.@]*,\s[\w\s&]*",MyStr)
#~ match.group()
#~ 'Saul Moore, s.moore@imperial.ac.uk, Systems biology and ecological theory'
#~ match.group(0)

#~ 'Saul Moore, s.moore@imperial.ac.uk, Systems biology and ecological theory'

#~ # now add groups using ( )
#~ match = re.search(r"([\w\s]*),\s([\w\.@]*),\s([\w\s&]*)",MyStr)
#~ match.group(0)
#~ 'Saul Moore, s.moore@imperial.ac.uk, Systems biology and ecological theory'
#~ match.group(1)
#~ 'Saul Moore'
#~ match.group(2)
#~ 's.moore@imperial.ac.uk'
#~ match.group(3)
#~ 'Systems biology and ecological theory'
	

