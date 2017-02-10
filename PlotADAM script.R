library(plyr)
library(ggplot2)
library(doBy)        #
library(reshape2)
path1 <- '' 
path2 <- ''
path3 <- ''
path4 <-  ''

name1 <- 'SBLUP_RL_Presel20'
name2 <- "SBLUP_basis"
name3 <- "SBLUP_RL_SP_Presel20"
name4 <-  "SBLUP_Basis_SP"

PlotADAM <- function (path1, path2, path3, path4, name1, name2, name3, name4) {

################# Scenario 1 #####################
x <- matrix( NA, nrow=          11 , ncol=          33  )
x <- read.table( file=path1, header = T, nrows=          11  )
x1 <- matrix(NA, nrow=11,ncol = 11)

x1 <- read.table( file=path1, skip=13,header = T, nrows=          11  )
x1$time <- NULL
x1$generation <-  NULL
x1$nReps <- NULL

Scenario1 <- cbind(x,x1)
Scenario1$scenario <- name1

################### Scenario 2 ################
x <- matrix( NA, nrow=          11 , ncol=          33  )
x <- read.table( file=path2, header = T, nrows=          11  )
x1 <- matrix(NA, nrow=11,ncol = 11)

x1 <- read.table( file=path2, skip=13,header = T, nrows=          11  )
x1$time <- NULL
x1$generation <-  NULL
x1$nReps <- NULL
Scenario2 <- cbind(x,x1)
Scenario2$scenario <- name2

################## Scenario 3 ##################

x <- matrix( NA, nrow=          11 , ncol=          33  )
x <- read.table( file=path3, header = T, nrows=          11  )
x1 <- matrix(NA, nrow=11,ncol = 11)

x1 <- read.table( file=path3, skip=13,header = T, nrows=          11  )
x1$time <- NULL
x1$generation <-  NULL
x1$nReps <- NULL
Scenario3 <- cbind(x,x1)
Scenario3$scenario <- name3

################## Scenario 3 ##################

x <- matrix( NA, nrow=          11 , ncol=          33  )
x <- read.table( file=path4, header = T, nrows=          11  )
x1 <- matrix(NA, nrow=11,ncol = 11)

x1 <- read.table( file=path4, skip=13,header = T, nrows=          11  )
x1$time <- NULL
x1$generation <-  NULL
x1$nReps <- NULL
Scenario4 <- cbind(x,x1)
Scenario4$scenario <- name4

################ Make Plot ######################

both <- rbind.fill(Scenario1, Scenario2,Scenario3, Scenario4)
both <- melt(both, measure.vars = c("BV"),
             id.vars= c("scenario","time"))
p <-  ggplot(data = both, aes(x=time, y=value,group=scenario))+
geom_line(aes(colour=scenario),size=1)
return(p)
}
PlotADAM(path1, path2, path3, path4, name1, name2, name3, name4)
print(p)
