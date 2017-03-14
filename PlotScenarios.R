library(ggplot2)
library(stringr)

# function that makes plot of progression in ADAM scenarios. Searches folders below the root folder
root <- "C:/Users/au384062/Dropbox/Projects/ADAM/OP_FORTITUDE/MBLUP"

PlotScens <- function( root ,ntime) {
  setwd(root)
all_paths <-
  list.files(path = root,
             pattern = "geneticTrendsMeans.res$",
             recursive = T) 
results <- matrix(0, nrow = length(all_paths)*ntime, ncol = 5)
names <- matrix(0, nrow = length(all_paths)*ntime, ncol = 1)

for (i in 1:length(all_paths)) {
  # browser()
  # 
  path <- all_paths[i]
  names[((i-1)*ntime+(i-(i-1))):(ntime*i),1] <- gsub(path, pattern = "geneticTrendsMeans.res", replacement = "")
  # x <- matrix( NA, nrow=          ntime+1 , ncol=          33  )
  x <- read.table( file=path, header = T, nrows=          11  )
  # x1 <- matrix(NA, nrow=11,ncol = ntime+1)
  # 
  # x1 <- read.table( file=path, skip=ntime+3,header = T, nrows=          11  )
  # x1$time <- NULL
  # x1$generation <-  NULL
  # x1$nReps <- NULL
  # 
  # Scenario <- cbind(x,x1)
  Scenario <- x
results [((i-1)*ntime+(i-(i-1))):(ntime*i),3] <- Scenario[1:ntime,4] # mean BV
results [((i-1)*ntime+(i-(i-1))):(ntime*i),4] <- Scenario[1:ntime,4]-Scenario[1:ntime,5] # lower bound
results [((i-1)*ntime+(i-(i-1))):(ntime*i),5] <- Scenario[1:ntime,4]+Scenario[1:ntime,5] # upper bound
results [((i-1)*ntime+(i-(i-1))):(ntime*i),2] <- Scenario[1:ntime,1] # time


}
# browser()
results <- as.data.frame(results)

colnames(results ) <- c("scenario","time", "BV", "BV_lower", "BV_upper")
results$scenario <- names
results$scenario <- as.character(results$scenario)
results$BV <- as.numeric(results$BV)
results$time <- as.integer(results$time)

p <-  ggplot(data = results, aes(x=time, y=BV,group=scenario))+
  geom_line(aes(colour=scenario),size=1)+geom_ribbon(aes(ymin=BV_lower, ymax=BV_upper,fill=scenario),alpha=0.15)
return(p)
} 
