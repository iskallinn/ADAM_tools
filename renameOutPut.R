# Replace folders in adam.script
# function is a bit cumbersome and should be written diffrently if it is run directly on the cluster
# the idea is to change the output folder names, since you often copy/paste input files when designing 
# folder structure
RenameOutPut <- function ( root ) { 
  prmfiles <-
    list.files(path = root,
               pattern = "*.prm$",
               recursive = T)
  for ( i in 1:length(prmfiles)) {
    path <- paste(root,prmfiles[i], sep = "")
  prm.file <- readLines(path)
    pos <- grep(pattern = "(OutDirectory=)", x=prm.file)
    prm.file[pos] <-
      gsub(
        pattern = paste("(OutDirectory=)", "(.*)", sep = ""),
        replacement = paste(
          "\\1",
          "'/usr/home/qgg/kari/OP_FORTITUDE/",
          gsub(
            x = prmfiles[i],
            pattern = "(^.*/)(input.prm)",
            replacement = "\\1"
          ),"'",
          sep = ""
        ),
        "'",
        x = prm.file[pos]
      )
    writeLines(prm.file, path) }
    }

  FixBullshit <- function ( root) { 
    prmfiles <-
      list.files(path = root,
                 pattern = "*.prm$",
                 recursive = T)
    for ( i in 1:length(prmfiles)) {
      path <- paste(root,prmfiles[i], sep = "/")
      prm.file <- readLines(path)
      pos <- grep(pattern = "&MATRICES", x=prm.file)
      prm.file[(pos-1) ] <- "/" 
      writeLines(prm.file, path) }
  }
  root <- "C:/Users/au384062/Dropbox/Projects/ADAM/OP_ROUNDUP/GENOMIC/"
  RenameOutPut(root, old_name, new_name)
  
  # function to copy all the adam.script files, change it to gbi-short and save it with another name
  MakeQuickSubmitFiles <- function ( root ) { 
    prmfiles <-
      list.files(path = root,
                 pattern = "adam.script$",
                 recursive = T)
    for ( i in 1:length(prmfiles)) {
      path <- paste(root,prmfiles[i], sep = "")
      prm.file <- readLines(path)
      pos <- grep(pattern = "(#PBS -q gbi)", x=prm.file)
      prm.file[pos] <- paste("#PBS -q gbi-short")
        
      pos <- grep(pattern = "(#PBS -l walltime=200:00:00)", x=prm.file)
      prm.file[pos] <-paste("#PBS -l walltime=1:59:00", sep = "")
      shortpath  <- gsub(path, pattern = "(.*)(/adam.script)", replacement = "\\1/adam_short.script" )
      writeLines(prm.file, shortpath) }
  }
  