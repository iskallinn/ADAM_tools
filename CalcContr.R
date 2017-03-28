# Script for calculating where genetic gain comes from 
# it works in a recursive fashion, getting the true economic values from the prm file
# it then gathers all the result in a list of lists.
# put in a vector with the economic values 


GatherResults <- function (root){ all_paths <-
  list.files(path = root,
             pattern = "input.prm$",
             recursive = T) 

prm_file <- readLines(con = all_paths[1])

economic.values <- prm_file[grep(pattern = "economicValueTbv",x = prm_file )]
economic.values <-
  as.numeric(unlist(str_split(str_trim(
    unlist(str_split(string = economic.values, pattern = "="))[2]
  ), pattern = " ")))

all_paths <-
  list.files(path = root,
             pattern = "geneticTrendsMeans.res$",
             recursive = T) 
overall.list <- list()
for (k in 1:length(all_paths)) { 
  
  path <- all_paths[k]
  
  x <- read.table(file=path, header = T, nrows=11)
  
  x <- matrix( NA, nrow=          11 , ncol=          33  )
  x <- read.table( file=path, header = T, nrows=          11  )
  x1 <- matrix(NA, nrow=11,ncol = 199)
  
  x1 <- read.table( file=path, skip=13,header = T, nrows=          11  )
  x1$time <- NULL
  x1$generation <-  NULL
  x1$nReps <- NULL
  
  geneticTrends <- cbind(x,x1)
  breedingValues <-  paste("BV", 1:14, sep = "")
  genetic.means <- geneticTrends[,breedingValues]
  BV <-  as.matrix(genetic.means) %*% economic.values
  results <- matrix (NA, nrow = 11, ncol = 14)
  colnames(results) <- breedingValues
  for (l in 1:nrow(genetic.means)) { 
    for ( i in 1:length(economic.values)) { 
      results[l,i] <- genetic.means[l,i]*economic.values[i] 
      }
  }
  # browser()
  results.list <- list(path, results,BV)
  overall.list[[length(overall.list)+1]] <- results.list
  }
return(overall.list)
}