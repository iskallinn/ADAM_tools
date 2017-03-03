# mass submit to quoue
# setwd( "C:/Users/au384062/Dropbox/Projects/ADAM/OP_ROUNDUP/MBLUP")

# usage = change root folder to the folder which one wants to run the script
# the script searches recursively through the subdirectories from root and makes a line with all script files

root <- "C:/Users/au384062/Dropbox/Projects/ADAM/OP_ROUNDUP/MBLUP" 
MakeSuperSubmit <- function ( root ) {
  scripts <-as.matrix(
    list.files(path = root,
               pattern = "*.script$",
               recursive = T))
  
submit <- file(description =  "submit")
con <- submit
cat(paste("#!/bin/bash"), file = con)
cat("\n", file = con, append = T)
###############################################################
# NOTE the path below also needs to be changed!!!! ############
###############################################################
cat(paste("qsub ","/usr/home/qgg/kari/OP_ROUNDUP/MBLUP/", apply(scripts, 1, paste, "\n"), sep=""), file = con, append = T)
}
