#!/usr/bin/env python

""" SQLite with Python - Managing relational SQLite databases """

__author__ = "Saul Moore sm5911@imperial.ac.uk"
__version__ = "0.0.1"

import sqlite3

conn = sqlite3.connect('../Data/test.db') # Create 'test.db'

# To exceute commands, create a 'cursor'
c = conn.cursor()

# Use the cursor to execute the queries
# Use the triple single quote to write queries on several lines
c.execute('''CREATE TABLE Test
					(ID INTEGER PRIMARY KEY,
					MyVal1 INTEGER,
					MYVal2 TEXT)''')
					
# c.execute('''DROP TABLE test''') # Delete table

# Insert the records, note that because we set the primary key, 
# it will auto-increment, therefore, set it to NULL
c.execute('''INSERT INTO Test VALUES
					(NULL, 3, 'mickey')''')
					
c.execute('''INSERT INTO Test VALUES
					(NULL, 4, 'mouse')''')
					

# When you 'commit', all the commands will be executed
conn.commit()

# Now we select the records (row data):
c.execute("SELECT * FROM TEST")

# Access the next record:
print c.fetchone()
print c.fetchone()

# Let's get all the records at once:
c.execute("SELECT * FROM TEST")
print c.fetchall()

# Insert many records at once, create a list of tuples:
manyrecs = [(5, 'goofy'),
						(6, 'donald'),
						(7, 'duck')]
						
# Now call 'executemany'
c.executemany('''INSERT INTO test
								 VALUES(NULL, ?, ?)''', manyrecs)

# and commit
conn.commit()

# Now let's fetch the records, we can use the query as an iterator!
for row in c.execute('SELECT * FROM test'):
	print 'Val', row[1], 'Name', row[2]
	
# Close the connection before exiting
conn.close()












