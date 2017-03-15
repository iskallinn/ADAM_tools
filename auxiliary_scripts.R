# Random culling groups

root <- "C:/Users/au384062/Dropbox/Projects/ADAM/RegEx_practice"

  
RemoveRandomCulling <- function ( root ) { 
  prmfiles <-
    list.files(path = root,
               pattern = "input.prm$",
               recursive = T) 
for ( i in 1:length(prmfiles) )  {
  # browser()
path <- paste( root, prmfiles[i], sep = "/")  
prm_file <- readLines(path)
if ( TRUE %in% (str_detect(string = prm_file, pattern = "randomCullingGroups"))==TRUE) {
pos <- grep (pattern = "nRandomCullingGroups", x = prm_file ) # figure out where it starts
prm_file <- prm_file[-pos]
pos <- grep (pattern = "randomCullingGroups", x = prm_file ) # figure out where it starts
pos1 <- grep (pattern = "/", x =prm_file)[5]
prm_file <- prm_file[-(pos:(pos1-1))]
writeLines(prm_file, con = path)
}
}  
  closeAllConnections()
  }
  
random.culling.lines <-  c("nRandomCullingGroups=3",
                            "randomCullingGroups=",
  "1 1 1 1 1 -9 -9 0.05 3",
  "2 1 1 1 1 -9 -9 0.05 3",
  "2 2 3 1 1 -9 -9 0.03 3")


PutInRandomCulling <- function ( root, random.culling.lines ) { 
  prmfiles <-
    list.files(path = root,
               pattern = "input.prm$",
               recursive = T) 
  for ( i in 1:length(prmfiles) )  {
    # browser()
    path <- paste( root, prmfiles[i], sep = "/")  
    prm_file <- readLines(path)
    pos <- grep (pattern = "OldAgeFemales", x = prm_file ) # figure out where it starts
    prm_file <- append (x = prm_file, values = random.culling.lines , after=pos)
    writeLines(prm_file, con = path)
  }
  closeAllConnections()
}

new.sel.line <- "  1 10 2 1 1 'across_herd' 1 3 1 2 'select_all' ''              0.5  0      1    0 0 0  8 0 0    6 0   0.0   0  0  
"
FixBullShit <- function ( root , new.sel.line ) { 
  prmfiles <-
    list.files(path = root,
               pattern = "input.prm$",
               recursive = T) 
  for ( i in 1:length(prmfiles) )  {
    # browser()
    path <- paste( root, prmfiles[i], sep = "/")  
    prm_file <- readLines(path)
    pos <- grep (pattern = "selection_scheme", x = prm_file ) # figure out where it starts
    pos1<- grep (pattern = "/", x = prm_file )[7] -1  # figure out where it starts
    prm_file <- append (x = prm_file, values = new.sel.line , after=pos1)
    
    # change number of selection lines 
    pos <- grep (pattern = "selection_groups", x = prm_file ) # figure out where it starts
    prm_file[pos] <- paste("selection_groups=", as.numeric(unlist(str_split(string = prm_file[pos], pattern = "="))[2])+1)
    # change obs lines 
    pos <- grep (pattern = "^(\\s+)(5\\s-9)(.*)", x= prm_file)
    prm_file <- sub(pattern = "^(\\s+)(5\\s-9)(.*)$",
        replacement = paste("\\1", "5", as.numeric(unlist(
          str_split(string = prm_file[grep (pattern = "selection_groups", x = prm_file)], pattern = "=")
        )[2]), "\\3"), x =prm_file)
    writeLines(prm_file, con = path)
  }
  closeAllConnections()
  }
