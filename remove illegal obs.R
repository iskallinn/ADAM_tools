# Script that finds illegal observations and deletes lines that have illegal 
# numbers and changes the number  of observations
# Note, this will not work if the first observation line is in the same line as MaleObservations\FemaleObservations=
# Note, the function will crash IF the logfile is out of date, i.e. there are no checks to see if the file has been fixed

log.path <- "C:/Users/au384062/Dropbox/Projects/ADAM/RegEx_practice/MBLUP/log"
# RemoveIllegalObs(log.path)
RemoveIllegalObs <-  function ( log.path ) { 
  # browser()
log.file <-   readLines(log.path)
log.file <-  gsub(pattern = "//", replacement = "/", x = log.file) # dont know why, but there is \\ in the paths
pos.illegals <- grep(pattern = "illegal obs numb in MaleObservations", x= log.file)

for ( i in 1:length(pos.illegals)) { 
  if ( i == 2) { 
    browser()
    }
  prm_path <- str_trim(unlist(str_split(log.file[pos.illegals[i]], pattern = "="))[2])
  prm_file <- readLines(prm_path)
  
  nobs <- unlist(str_split(prm_file[ grep(pattern = "nobs", x= prm_file)], pattern = "="))[2]# figure out how many observations are legal
  pos  <-
    grep(prm_file, pattern = "MaleObservations") # figure out the range where the obs lines are located
  pos1  <-
    grep(prm_file, pattern = "FemaleObservations") # 
  del <- grep(x=prm_file[pos:pos1],pattern = paste("(\\s)(",max.obs.numb+1,")","(.*)", sep=""))
  
  for (k in 1:length(del)) { 
    # k <- k+1
  if ( k == 1 ) {
    prm_file <-   prm_file[-(pos-1+del[k])]
  } else if (k > 1 ) { 
    prm_file <-   prm_file[-(pos+del[k]-k)]
    }
  }
# CHANGE NUMBER OF OBS FOR MALES
  # count the number of lines , using the del method above
  pos  <-
    grep(prm_file, pattern = "MaleObservations") # figure out the range where the obs lines are located
  pos1  <-
    grep(prm_file, pattern = "FemaleObservations") # 
  
  numb.obs <- as.integer(length( grep(x=prm_file[pos:pos1],pattern = "(\\s)(\\d)(\\s).*")))
  pos  <-
    grep(prm_file, pattern = "nMaleObs") # 
  prm_file[pos] <- paste("nMaleObs=",numb.obs, sep = "")
  #FEMALES OBS REMOVAL MECHANISM
  pos  <-
    grep(prm_file, pattern = "FemaleObservations") # figure out the range where the obs lines are located
  pos1  <-
    grep(prm_file[pos:length(prm_file)], pattern = "/") # 
  del <- grep(x=prm_file[pos:(pos+pos1[1])],pattern = paste("(\\s)(",max.obs.numb+1,")","(.*)", sep=""))
  
  for (k in 1:length(del)) { 
    # k <- k+1
    if ( k == 1 ) {
      prm_file <-   prm_file[-(pos-1+del[k])]
    } else if (k > 1 ) { 
      prm_file <-   prm_file[-(pos+del[k]-k)]
    }
  }
  ########### Change number of obs for females
  pos  <-
    grep(prm_file, pattern = "FemaleObservations") # figure out the range where the obs lines are located
  pos1 <-
    grep(prm_file[pos:length(prm_file)], pattern = "/") # look for the end of the namelist
  
  numb.obs <- as.integer(length( grep(x=prm_file[pos:(pos+pos1[1])],pattern = "(\\s)(\\d)(\\s).*")))
  pos  <-
    grep(prm_file, pattern = "nFemaleObs") # 
  prm_file[pos] <- paste("nFemaleObs=",numb.obs, sep = "")
writeLines(prm_file, prm_path)
}

}