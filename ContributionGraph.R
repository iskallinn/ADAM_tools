library(reshape)
# script to make graphs. Note that this needs to be adjusted for use with
# non genomic selection scenarios, since the number of traits is wrong
ContributionGraph <- function ( overall.list ) {
for (i in 1:length(overall.list)) {
test <- as.data.frame(matrix(as.numeric(unlist(overall.list[[i]][[2]])), nrow = 11, ncol = 14))

# comment out for non genomic scenarios
# test <- as.data.frame(matrix(as.numeric(unlist(overall.list[[i]][[2]])), nrow = 11, ncol = 7))
year <- as.factor(1:11)
test<- cbind(test, year)
scenario <- rep(unlist(overall.list[[i]][[1]]), times = 11)
test <- cbind(test,scenario)
# test[,1:14] <- as.numeric(test[,1:14])
colnames(test ) <- c("Skin.length.F", "Skin.length.M", "BW.F", "BW.M", "Litter.Size", "Skin.quality", paste("BV", 7:14, sep=""), "year", "scenario")
# for non genomic
# colnames(test ) <- c("Skin.length.F", "Skin.length.M", "BW.F", "BW.M", "Litter.Size", "Skin.quality", "live.qual", "year", "scenario")
# comment out for non genomic 
test <- test[,-(8:14)]
test <- melt(test, varnames = c("year", "scenario") )
test$value <- as.numeric(test$value)


ggplot(test[-(67:77),], aes(x = year, y = value, group = variable)) + geom_area(position = 'stack',aes(colour=variable, fill=variable))+
  labs(
    title = "Total Economic gain",
    x = sub(pattern = "/geneticTrendsMeans.res", x = unlist(overall.list[[i]][[1]]) ,replacement=""),
    y = "Polygenic Breeding Value"
  )+ annotate("text", x = 2, y = 150, label = paste(round(overall.list[[i]][[3]][[11]], digits=0), "DKK" ) )+
annotate("text", x = 2, y = 170, label = "BV at end" )+ ylim(-120,350)
ggsave(filename = paste(
  "Plot_",
  gsub(pattern="/",x=sub(
    pattern = "/geneticTrendsMeans.res",
    x = unlist(overall.list[[i]][[1]]) ,
    replacement = ""
  ), replacement = "_"),".png",
  sep = ""
),width = 8, height = 6, units = 'in', dpi = 300) 

}
}