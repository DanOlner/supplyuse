setwd("C:/Users/geodo/Dropbox/R/Workspace/SupplyUseMatrix")

library(ggplot2)
library(reshape)
library(scales)

#flows <- read.csv("2011_intermediate_May.csv")
flows <- read.csv("domesticUse2010ownMatrix_removingImputedRentSICcodes.csv")

flows <- melt(flows)

#drop low values
colnames(flows)[colnames(flows)=="variable"] <- "SICfrom"
colnames(flows)[colnames(flows)=="SIC"] <- "SICto"

flows <- flows[flows$value > 0.1, ]

#Remove Xs
flows$SICfrom <- gsub("X", "", flows$SICfrom)
  
# flows <- flows[order(flows$value),]
#Get list of unique SICfrom values. 
#They're given in the right order here, which means...
SICfroms <- unique(flows$SICfrom)

#We can use that to make ordered factors. Bing!
flows$SICfrom <- ordered(flows$SICfrom, levels = SICfroms)

#Do same for SICtos. They go in as factors but not ordered.
SICtos <- unique(flows$SICto)
flows$SICto <- ordered(flows$SICto, levels = SICtos)

#save for use elsewhere
write.csv(flows, "intermediateFlowsInLong.csv")


#PLOTS
histyprinty <- ggplot(flows, aes(x=value)) +
  scale_x_continuous(trans=log2_trans(), breaks=c(0.1,0.5,1,2,4,8,16,30,60,120,400,1500,4500,16000)) +  
#   coord_cartesian(xlim = c(0, 10)) +
  ggtitle("Money flows from 2010 domestic use matrix") +
  theme(plot.title = element_text(lineheight=1.5, face="bold")) +
  #   theme_classic() +
  xlab("money flow quantity, millions (log axis)") +
  ylab("number of individual flows") +
  geom_histogram(colour="white", fill="black", binwidth = 1)
#           geom_histogram(aes(fill = value))

histyprinty

ggsave(histyprinty, file="moneyflowsHist_dom2010.jpg", width=5, height=3, dpi = 600)

#Split between different wotsits
# flowsSub <- subset(flows, value > 100)
flowsSub <- flows

histyprinty <- ggplot(flowsSub, aes(x=value)) +
  scale_x_continuous(trans=log2_trans()) +  
#   scale_x_continuous(trans=log2_trans(), breaks=c(2,4,8,16,30,60,120,400,1500,4500,16000)) +  
#   coord_cartesian(xlim = c(0, 10)) +
  ggtitle("Money flows from 2011 combined use matrix") +
  theme(plot.title = element_text(lineheight=1.5, face="bold")) +
  #   theme_classic() +
  xlab("money flow quantity, millions (log axis)") +
  ylab("number of individual flows") +
#   geom_histogram(data=subset(flows, as.numeric(SICfrom) < 50), colour="white", fill="black")
  
  geom_histogram(data=subset(flowsSub, (as.numeric(SICfrom) >= 59)), 
                 colour="white", fill="grey") +
  geom_histogram(data=subset(flowsSub, (as.numeric(SICfrom) < 59)), colour="white", fill="black", 
                 alpha = 0.5)

#   geom_histogram(data=subset(flows, (as.numeric(SICfrom) >= 60 & value > 100)), 
#                  colour="white", fill="grey") +
#   geom_histogram(data=subset(flows, (as.numeric(SICfrom) < 60 & value > 100)), colour="white", fill="black", 
#                  alpha = 0.5)

#   geom_histogram(data=subset(flows, (as.numeric(SICfrom) >= 60 & value > 100)), 
#                  colour="white", fill="grey", binwidth = 0.75) +
#   geom_histogram(data=subset(flows, (as.numeric(SICfrom) < 60 & value > 100)), colour="white", fill="black", 
#                  binwidth = 0.75, alpha = 0.5)

#           geom_histogram(aes(fill = value))

histyprinty

#Get a long version with values doubled up, one set for each of SICfrom and SICto
#First, two new vars containing the appropriate columns
SICfroms <- flows[,c(2,3)]
SICtos <- flows[,c(1,3)]

#Then a new column saying what flow direction is for each
SICfroms$source <- "from"
SICtos$source <- "to"

#Rename old columns identically so they can be linked
colnames(SICfroms)[colnames(SICfroms)=="SICfrom"] <- "SIC"
colnames(SICtos)[colnames(SICtos)=="SICto"] <- "SIC"

#Remove all zero values
SICfroms <- SICfroms[ SICfroms$value!=0,]
SICtos <- SICtos[ SICtos$value!=0,]

#Now keep only their ordered factor value
#That's all I want to use in this correlation
SICfroms$SIC <- as.numeric(SICfroms$SIC)
SICtos$SIC <- as.numeric(SICtos$SIC)

#Join them
SICslong <- rbind(SICfroms, SICtos)


#CORRELATE!
pointyPrinty <- ggplot(SICslong[ SICslong$source=="to",], aes(y = SIC, x = value)) +
# pointyPrinty <- ggplot(SICslong[ SICslong$source=="to",], aes(y = SIC, x = value)) +
  geom_point(size =3, alpha=0.5) +
  scale_x_continuous(trans=log10_trans())

pointyPrinty


# flows$rankedValues <- rank(flows$value)
# 
# sectoral = NULL
# 
# #BREAK DOWN BY SECTOR GROUPING
# manuf <- flows[ as.numeric(flows$SICfrom) < 60,]
# sect <- flows[ as.numeric(flows$SICfrom) >= 60,]
# 
# boxplot(log2(manuf$value),log2(sect$value))
# 
# sectoral$manuf <- manuf$value
# sectoral$service <- sect$value
# 
# sectoral <- data.frame(sectoral)
# 
# output <- ggplot(finalRanks, aes(factor(Source), COR)) +
#   geom_boxplot(outlier.size = 3) +
#   ggtitle("Spearman rank correlations\nfor all Commodities and GORs\nTonnes vs distance") +
#   theme(plot.title = element_text(lineheight=.8, face="bold")) +
#   xlab("Source") +
#   ylab("Rank")
# 
# output

#summarise
summariseFlows <- summary(subset(flows$SICfrom, flows$value > 100))
summariseFlows <- data.frame(summariseFlows)

