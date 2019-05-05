#!/usr/bin/python
# Filename: using_name.py

"""Script to demonstrate the importance of name = 'main', such that the file is usable both as a script and as an importable module"""

__author__ = 'Saul Moore (sm5911@imperial.ac.uk)'
__version__ = '0.0.1'

if __name__ == '__main__':
	print 'This program is being run by itself'
else:
	print 'I am being imported from another module'

# If you run the program directly, by calling the name of this script: "run using_name.py" it returns the former message
# If you import the file, or call it indirectly: "import using_name.py", it returns the latter message. Only imports once, afterwards reimmporting wont return the message
