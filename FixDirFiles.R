# Fix dir files function 
# reads dir file and prm file, check if dir file is "legal", if not it is fixed
# install.packages("stringr")
# library(stringr)
root <- "C:/Users/au384062/Dropbox/Projects/ADAM/RegEx_practice/MBLUP"
FixDirFiles <- function ( root ) { 
  ############## Find files #####################
  prmfiles <- # find prm files
    list.files(path = root,
               pattern = "*.prm$",
               recursive = T)
  dirfiles <- # find dir files
    list.files(path = root,
               pattern = "*.dir$",
               recursive = T)
  raw.dir.files <- paste(root,gsub ( pattern = 'dmu.polyblup.dir$', replacement = "", x = dirfiles),sep = "")
  raw.prm.files <- paste(root,gsub ( pattern = 'input.prm', replacement = "", x = prmfiles),sep = "")
  missing.dir <- raw.prm.files[-match(raw.dir.files , raw.prm.files)] # folders lacking prm files
  # TODO a check for missing dir
  
  # Find all folders with prm files and fir files (cannabilize prm file function )
  # note that there will need to be too loops, one for folders where there is both a dir
  # file and a prm file, second loop should create dir files where there is none
  for ( i in 1:length( prmfiles ) ) {
    # browser()
    path <- paste(root, dirfiles[i], sep = "/")
    dir.file <-  readLines(path)
    writeLines(dir.file, paste(path, "bak", sep = "_")) # write backup of each file
    prm.file <- readLines(paste(root, prmfiles[i], sep="/"))
    prm.file <- gsub(pattern = "\t", replacement = " ", x =prm.file)
    
    ############# Information mining #############
    # extract necessary information
    # n.tbv
    pos <- grep(x = prm.file, pattern = "nebv") # find the number of ebv
    n.ebv <- as.numeric(unlist(str_split( pattern = "=", string = prm.file[pos]))[2])
    # tbv in dir file
    pos <- grep(x = dir.file, pattern = "^\\$DATA") # find the number of ebv
    dir.nebv <-
      (as.numeric(unlist(str_split(
        gsub(
          pattern = "[a-z]|[A-z]|\\(|\\)|\\$",
          replacement = "",
          x = dir.file[pos]
        ),
        pattern = ","
      ))[1])-5)/3
    ############ Check if it fits ################
    if (dir.nebv  != n.ebv ) {
    # is the number of traits greater or less than prm file 
    if ( dir.nebv > n.ebv ) { # more traits in dir file than in prm file
      ############ Fix dir file (greater ) ###################
      # $DATA line
      dir.file[pos] <- paste( "$DATA ASCII ", "(", (n.ebv+5)*3, ",", n.ebv*2,"-9999) dmudat")
      # $Variable names
      pos <- grep(x = dir.file, pattern = "^\\$VARIABLE") # find the variable line
      pos1 <- pos -1 + grep(x = dir.file[pos:length(prm.file)], pattern = "^([a-z]|[A-Z]).*") # find the lines with names of variables
      # todo make this a bit smarter, now it only uses two lines to put in variable names
      # ok for me since i only use two lines in my dir files but could be problematic for others
      dir.file[pos1[1]] <-
        paste0(c(
          paste ("id sire dam sex mu"),
          paste(rep("mate"), seq(1:n.ebv), sep = ""),
          paste(rep("hys"), seq(1:n.ebv), sep = "")
        ), collapse = " ")
      dir.file[pos1[2]] <-
        paste0(c(
          paste(rep("ebv"), seq(1:n.ebv), sep = ""), paste(rep("wt"), seq(1:n.ebv), sep =
                                                             "")
        ), collapse = " ")
      
      # $MODEL 
      pos <- grep(x = dir.file, pattern = "^\\$MODEL") # find the variable line
      pos.integers <- pos +grep(x = dir.file[pos:length(dir.file)], pattern = "^[1-9]") 
      pos.zeros<- pos +grep(x = dir.file[pos:length(dir.file)], pattern = "^0") 
      # pos.integers is the line numbers where the line starts int > 0 
      # pos.zeros is line numb of lines in model section with 0
      
      diff.in.traits <- dir.nebv - n.ebv # difference in trait numbers
        # need to change it so that it changes the numbers of the hys effect
        pos.model.lines <- pos-1 +grep(x = dir.file[pos:length(dir.file)], pattern = paste("^", "[", "1-",dir.nebv,"]","(\\s)","0",".*",sep="")) 
        dir.file <- dir.file[-pos.model.lines]
        new.model.lines <- c(
          paste( 
            n.ebv, sep = ""),
          paste(rep("0", times = n.ebv+1), sep=""),  
          paste(
            seq(1:n.ebv), "0 2", (seq(1:n.ebv) + n.ebv + 5)),
          paste(rep( "1 1", times = n.ebv), sep=""),
          paste(rep(0, times= n.ebv)))
        dir.file <- dir.file[-((pos+1):tail(pos.zeros, n=1))]
        dir.file <- append(dir.file, new.model.lines, (pos+1))
        pos.zeros<- pos +grep(x = dir.file[pos:length(dir.file)], pattern = "^0") 
        
    } else if (dir.nebv < n.ebv) { # less traits in dir file than in prm file
      ############ Fix dir file (shorter) ###################
      # $DATA line
      # $Variable names
      # $MODEL
       }
    
    }
    writeLines(dir.file, path)
    
  }
}  