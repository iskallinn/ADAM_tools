# mass submit to quoue
root
MakeSuperSubmit <- function ( root ) {
  scripts <-as.matrix(
    list.files(path = root,
               pattern = "*.script$",
               recursive = T))
  
submit <- file(description =  "submit")
con <- submit
cat(paste("qsub ","/usr/home/qgg", apply(scripts, 1, paste, "\n"), sep=""), file = con)
}