#!/usr/bin/bash
# Author: Saul Moore sm5911@imperial.ac.uk
# Script: run_get_TreeHeight.sh
# Desc: A script to test 'get_TreeHeight.R'
# Arguments: none
# Date: 20 Oct 2015

echo -e "\n This is a Unix shell script to test the script 'get_TreeHeight.R', using 'trees.csv' as example input file \n"


Rscript get_TreeHeight.R "../Data/trees.csv"

