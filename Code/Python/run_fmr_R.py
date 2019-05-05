#!/usr/bin/env python

""" Python script to run fmr.R, and print results to screen """

__author__ = "Saul Moore sm5911@imperial.ac.uk"
__version__ = "0.0.1"

import subprocess
import os

subprocess.Popen("Rscript --verbose fmr.R", shell=True).wait()

if os.path.isfile("../Results/fmr_plot.pdf"):
	print "R script 'fmr.R' was run successfully! Plot saved to Results!"
else:
	print "Error: R script 'fmr.R' was not run successfully"
	
#~ if os.path.isfile("../Results/species.csv"):
	#~ print species.csv
#~ else:
	#~ print "Error: File 'species.csv' not found"

