#!/usr/bin/env python

"""A Python script to read 'align_seqs.csv' file as an input and calculates the best alignment of the DNA sequences, outputting to score in file 'align_seqs.txt' in the 'Results' directory"""

import sys
import csv

def file_function(argv):
    if len(argv) > 1:
        filename = argv[1]
    else:
        filename = '../Data/align_seqs.csv'
    return str(filename)

# function that computes a score by returning the number of matches, starting from arbitrary startpoint
def calculate_score(s1, s2, l1, l2, startpoint):
    # s1 and s2 are the sequences, l1 and l2 are the lengths of the sequences, startpoint is the point at which we want to start
    matched = "" # Create an empty string called 'matched', that will contain the alignement string
    score = 0 # Score starts at zero
    for i in range(l2):
        if (i + startpoint) < l1: 
            if s1[i + startpoint] == s2[i]: # does it match the character at that position in l1?
                matched = matched + "*"
                score = score + 1 # Every time a character is matched, add +1 to the score
            else:
                matched = matched + "-" # No additional score if characters are different

    # Build some formatted output
    print "." * startpoint + matched
    print "." * startpoint + s2
    print s1
    print score
    print ""

    return score
    
def main(argv):
    f = csv.reader(open(file_function(argv), 'rb')) # Readable binary file

    #~ # These are the two sequences to match
    #~ seq2 = "ATCGCCGGATTACGGG"
    #~ seq1 = "CAATTCGGAT"
    for row in f:
        a=row[0] # Treats the two sequences in the .csv file, spearated by a comma, as tuples
        b=row[1] # Assign 'a' as the first element, 'b' as the second element
    # print a
    # print b
    l1=len(a) # Calculate the length of sequence 'a'
    l2=len(b) # Now for sequence 'b'
    # assign the longest sequence s1, and the shortest to s2
    # l1 is the length of the longest, l2 that of the shortest

    if l1 >= l2:
        s1 = a
        s2 = b # If l1 is longer than l2, cool! Move on..
    else:
        s1 = a
        s2 = b
        l1, l2 = l2, l1 # If l1 is not longer than l2, swap the two lengths

    calculate_score(s1, s2, l1, l2, 0) # Runs calculate_score
    calculate_score(s1, s2, l1, l2, 1)
    calculate_score(s1, s2, l1, l2, 5)

    # now try to find the best match (highest score)
    my_best_align = None
    my_best_score = -1

    for i in range(l1): # For each character in l1
        z = calculate_score(s1, s2, l1, l2, i) # z is the score, calculating the score for all startpoints
        if z > my_best_score: # If z is larger than my_best_score
            my_best_align = "." * i + s2
            my_best_score = z # Best score is z if z is higher than my_best_score

    print my_best_align
    print s1
    print "Best score:", my_best_score

    f = open('../Results/align_seqs.txt', 'w') # Create output file
    f.write(my_best_align + "\n") # Write best alignment result + new line
    f.write(s1 + "\n") # Write s1 for reference + new line
    f.write(str(my_best_score) + "\n") # Write as value of best score (as string)
    f.close()

if (__name__ == "__main__"):
    print 'Running simple python script to calculate FASTA sequence best alignment'
    status = main(sys.argv)
else:
	print 'Importing simple python module for calculating FASTA sequence best alignment'
# sys.exit(status)
