#!/bin/bash

# This script takes URLs from a text file and gathers data from each 

# The text file name is supplied via command line, following the program run command (./getUrl.sh -i file.txt)
file=$1

# Gather data from each URL in the provided file, using wget
wget -o getlog.tsv -i $file
