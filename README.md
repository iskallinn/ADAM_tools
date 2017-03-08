# ADAM_tools
Tools for making life with ADAM faster. Idea is to build up a suite of tools 
that can make setting up scenarios for ADAM faster

Each script has a brief description of what is does. Report issues


The functionality of these tools are built around the replace_prm_files.R

That function finds all input.prm files in a recursive look from the input path (root). It then loops through the files and checks if various inputs are present in the file, if it is, does it fit with the values that are supposed to be changed out and if those checks are passed, the program updates these files, creates a backup and saves the file. There are outstanding issues, looks in the Issues tab or in the project.

If you have additions, find bugs, good ideas. Fork it, test it and I'll put it in :) This is very much a work in progress.

The other small functions are auxilliary functions that are being built ad hoc 
They include a function to make a submit script for the cluster to submit multiple files simultanously to the cluster (SuperSubmit.R)
Note that you still have to add the #!bin/bash line from the editor in the cluster as there is some parsing issue from R in a windows environment to linux (if you have solution, fix it!), along with chmoding the permission for executing the file

There is a function to generate the priors from a given matrix for DMU, which is handy for large matrices
Work in progress is the function to make a dir file for DMU from the input file. It should work as is, IF there are .dir files, I want to add functionility so it creates a .dir file if there is none present. 

A handy function is the ScalePrmFile, which is done to scale up the numbers in the input file. Very good for developing scenarios and then after it works, scale it up. 

PlotAdam script is a work in progress, I want to develop it to handle an arbitrary number of output, so I can just run that on a whole directory tree and then make interactive graphics from that. Currently it is rather cumbersome to use, but still a lot faster than copy/pasting into excel
