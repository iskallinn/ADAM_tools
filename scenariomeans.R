## Root directory and libraries

root <- 'C:/Users/au384062/Dropbox/Projects/ADAM/OP_FORTITUDE/GENOMIC'
setwd(root)

library(stringr)

#######################################
# Read simulation means from res file #

ntime = 10     #Number of time steps
tsyear = 1      #Number of time steps per year


all_paths <-
  list.files(path = root,
             pattern = "geneticTrendsMeans.res$",
             recursive = T) 
number.of.intervals <- round(ntime/5, digits = 0) # figures out how many five year intervals to calculate
results <- matrix(0, nrow = length(all_paths), ncol = (number.of.intervals+2)) # makes matrix to hold results
colnames(results) <- c("scenario", paste("interval", 1:number.of.intervals, sep = ""), "overall") # makes column names
for (i in 1:length(all_paths)) {
  path <- all_paths[i]
  
  x <- read.table(file=path, header = T, nrows=ntime+1)
  x <- x[c("time","BV")]      #change if needed
  for ( n in 1:number.of.intervals) { 
    if ( n == 1 ) { # For the first year we do this
      x1 <- subset(x, time >= 0 & time < 5*tsyear)
      results [i,1] <- path
      results[i,2] <- (x1[5,2]-x1[1,2])/5
      
  } else if (n > 1 ) {
    x1 <- subset(x, time >= (5*n-4*(n-1))*tsyear & time <= (n*5)*(n-1)*tsyear) # this is to make sure the program picks the rights rows
    results [i,1] <- gsub(path, pattern = "geneticTrendsMeans.res", replacement = "") # get the name for the scenario
    results[i,3] <- (x1[5,2]-x1[1,2])/5
    results[i,4] <- (x[10,2]-x[1,2])/10
    }
  }
}

## Writes results to a .csv file
write.csv(results, file="results.csv", row.names = FALSE)
