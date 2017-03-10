# mass submit to quoue
setwd( "C:/Users/au384062/Dropbox/Projects/adam_short/OP_FORTITUDE/GENOMIC/gs_opt/acc_high/SP/All_M_fNUC/")

# usage = change root folder to the folder which one wants to run the script
# the script searches recursively through the subdirectories from root and makes a line with all script files
# best way would be to run this script on the cluster :) then it doesnt need two arguments
root <- "C:/Users/au384062/Dropbox/Projects/adam_short/OP_FORTITUDE/GENOMIC/gs_opt/acc_high/SP/All_M_fNUC/" 
root.on.cluster <- "/usr/home/qgg/kari/OP_FORTITUDE/GENOMIC/gs_opt/acc_high/SP/All_M_fNUC/"
MakeSuperSubmit <- function ( root ,root.on.cluster) {
  # browser()
  scripts <-as.matrix(
    list.files(path = root,
               pattern = "*adam_short.script$",
               recursive = T))
paths <- gsub(pattern = "adam_short.script$",x = scripts, replacement = "")
submit <- file(description =  "submit", open="w")
con <- submit
# cat(paste("#!/bin/bash"), file = con) # this doesnt work bc of windows portability
# cat("\n", file = con, append = T)
for (i in 1:length(scripts)) {
  cat(paste("cd", paste(root.on.cluster, paths[i], sep="")), "\n", file = con, append = T)
  # cat("\n", file = con, append = T)
  cat(paste( "qsub adam_short.script \n"), file = con, append = T) 
  # cat(paste( "qsub ", paste(root.on.cluster,scripts[i]), sep=""),"\n", file = con, append =T)
  # cat("\n", file = con, append = T)
} 
}  
