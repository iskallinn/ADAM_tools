# Replace folders in adam.script
RenameOutPut <- function ( root ,old_name ,new_name) { 
  prmfiles <-
    list.files(path = root,
               pattern = "*.prm$",
               recursive = T)
  for ( i in 1:length(prmfiles)) {
    path <- paste(root,prmfiles[i], sep = "/")
  prm.file <- readLines(path)
    pos <- grep(pattern = paste("(OutDirectory='/usr/home/qgg/kari/)","(",old_name,")","(.*)", sep = ""), x=prm.file)
    prm.file[pos ] <- gsub(pattern = paste("(OutDirectory='/usr/home/qgg/kari/)","(",old_name,")","(.*)", sep = ""),
                           replacement = paste("\\1",new_name,"\\3", sep = ""), x=prm.file[pos])
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
  root <- "C:/Users/au384062/Dropbox/Projects/ADAM/OP_ROUNDUP/"
  RenameOutPut(root, old_name, new_name)
  