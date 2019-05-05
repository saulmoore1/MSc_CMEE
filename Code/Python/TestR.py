#!/usr/bin/env python

""" Test Python script to run the R script TestR.R, using the 'subprocess' module """

__author__ = "Saul Moore sm5911@imperial.ac.uk"
__version__ = "0.0.1"

########### Subprocess to run R ############

import subprocess

subprocess.Popen("Rscript --verbose TestR.R > \
../Results/TestR.Rout 2> ../Results/TestR_errFile.Rout",\
shell=True).wait()

# Outputs 2 files to Results:
# 'TestR.Rout' containing the string: 
# "Hello, this is R" as specified using print in R script 'Test.R'
# 'TestR_errFile.Rout' containing the log information for the operation
