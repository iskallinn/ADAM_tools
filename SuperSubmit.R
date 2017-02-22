# mass submit to quoue
setwd( "C:/Users/au384062/Dropbox/Projects/ADAM/OP_ROUNDUP/MBLUP")
root <- "C:/Users/au384062/Dropbox/Projects/ADAM/OP_ROUNDUP/MBLUP" 
MakeSuperSubmit <- function ( root ) {
  scripts <-as.matrix(
    list.files(path = root,
               pattern = "*.script$",
               recursive = T))
  
submit <- file(description =  "submit")
con <- submit
cat(paste("qsub ","/usr/home/qgg/kari/OP_ROUNDUP/MBLUP/", apply(scripts, 1, paste, "\n"), sep=""), file = con)
}
