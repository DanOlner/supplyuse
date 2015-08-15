#Correlate between-sector spearman's to the difference in transport costs between sectors.
#Reason: see if similar sectors have similar transport costs
library(ggplot2)
library(reshape2)

#Load IO data
flows <- read.csv("domesticUse2010ownMatrix_removingImputedRent_SICcodesInHeaderOnly.csv")

#heavy ind subset
# flows <- flows[1:63,1:63]

#Spearman rank of sectors against each other, produces correlation matrix
store <- cor(flows, method="spearman")

#Make a matrix of differences in transport cost per sector
#First, find out land transport costs used by each sector as a % of its total consumption
#Land transport services are 49.3-5
#We don't have the code for the rows, so it's row 63
# flows[63,1]

#colSums(flows) is total consumption of that sector
transportPercent <- (flows[63,]/colSums(flows)) * 100

#Matrix to store differences
#http://stackoverflow.com/questions/9917545/r-define-dimensions-of-empty-data-frame
#transportDiff <- data.frame(matrix(NA, nrow = length(transportPercent), ncol = length(transportPercent)))
# 
# #Find differences in % transport consumption between all sectors, store
# for (i in 1:length(transportDiff)) {
#   for (j in 1:length(transportDiff)) {
#    
#     #Marking out those based on smaller initial transport cost percentages
#     if(transportPercent[i] > 5 & transportPercent[j] > 5) {
#     transportDiff[i,j] <- abs(transportPercent[i] - transportPercent[j])
#   } else {
#     transportDiff[i,j] <- -1
#   }
#     
#   }
# }

#that takes a little while to run - save and just reload
# save(transportDiff, file="transportDiff.Rda")

load("transportDiff.Rda")
#test <- cor(store,transportDiff)



#http://www.r-bloggers.com/using-r-correlation-heatmap-with-ggplot2/
#qplot(x=X1, y=X2, data=melt(store), fill=value, geom="tile")

#This tests that unique values match the number we'd expect to get
#If we want to drop duplicate sector pairs. It works - both output 5357
#Though this won't quite work - it'll strip out the diagonals of both (1 for spearman's, 0 for diffs)
melt1 <- melt(store)
melt2 <- melt(transportDiff)
length(unique(melt1$value))
length(unique(melt2$value))

combine <- NULL

combine$spearmans <- melt1$value
combine$transport <- melt2$value

combine <- data.frame(combine)

plot(combine)
cor(combine)

# combineSub <- combine[(combine$spearmans > 0.8),]
# combineSub <- combine[(combine$transport > 0) & (combine$spearmans > 0.8),]
combineSub <- combine[(combine$transport > 0),]
# combineSub <- combine[1:(length(combine$transport)/8),]

plot(combineSub)
cor(combineSub)

transO <- transportPercent[order(transportPercent, decreasing = TRUE)]





