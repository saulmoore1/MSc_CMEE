#!/usr/bin/env python

"""Week2 Extra-credit script - reads the two .fasta files from Week1 practical, using relative paths to 'Week1/Data/fasta/' directory, and calculates a best match alignment of the two nucleobase sequences, outputting the matched sequences and best alignment score in file 'align_seqs_fasta.txt' in the 'Results' directory"""

import re
import sys

def choose_file1(argv):
    if len(argv) == 2:
        print "Please provide two FASTA files at the command line for sequence alignment. \nNote: if no files are provided, default FASTA files are used."
    if len(argv) == 3:
        filename = argv[1]        
    else: 
        filename = '../../Week1/Data/fasta/407228326.fasta'
    return str(filename)
    
def choose_file2(argv):
    if len(argv) == 3:
        filename = argv[2]        
    else: 
        filename = '../../Week1/Data/fasta/407228412.fasta'
    return str(filename)

# function that computes a score by returning the number of matches, starting from arbitrary startpoint
def calculate_score(s1, s2, l1, l2, startpoint):
    # s1 and s2 are the sequences, l1 and l2 are the lengths of the sequences, startpoint is the point at which we want to start
    matched = "" # Create an empty string called 'matched', that will contain the alignement string
    score = 0 # Score starts at zero
    for i in range(l2):
        if (i + startpoint) < l1: 
            if s1[i + startpoint] == s2[i]: # does it match the character at that position?
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
    f = open(choose_file1(argv), 'r')
    g = open(choose_file2(argv), 'r')
    	
    seq1 = re.sub(r'>gi.*\n', '', str(f.read()))
    seq1 = seq1.replace('\n', '')
    print seq1
    
    seq2 = re.sub(r'>gi.*\n', '', str(g.read()))
    seq2 = seq2.replace('\n', '')
    print seq2
    
    f.close()
    g.close()
    
    # Assign the longest sequence s1, and the shortest to s2
    # l1 is the length of the longest, l2 that of the shortest
    
    l1=len(seq1)
    l2=len(seq2)
    if l1 >= l2:
        s1 = seq1
        s2 = seq2
    else:
        s1 = seq2
        s2 = seq1
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
    
    f = open('../Results/align_seqs_fasta.txt', 'w') # Create output file
    f.write(my_best_align + "\n")
    f.write(s1 + "\n") 
    f.write(str(my_best_score) + "\n")
    f.close()
    
    return 0
    
if(__name__ == "__main__"):
    status = main(sys.argv)
    sys.exit(status)
