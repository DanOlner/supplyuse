setwd("C:/Users/geodo/Dropbox/R/Workspace/SupplyUseMatrix")

library(ggplot2)
library(reshape)
library(scales)

#totals <- read.csv("DemandAndConsumption2.csv")
# totals <- read.csv("domesticUse2010TotalDemandAndConsumption.csv")
# totals <- read.csv("domesticUse2010TotalDemandAndConsumptionProductByProduct_imprentremoved.csv", stringsAsFactors = FALSE)
# totals <- read.csv("domesticUse2010ownMatrix_removingImputedRent.csv")
totals <- read.csv("2010_domUse_demandAndConsumptionColumns_June2015.csv")

#plot(log10(totals$Total.intermediate.consumption), log10(totals$Total.intermediate.demand))

maxSector <- 58
maxSector <- 104

#take a subset of sectors before melting
#totals <- totals[1:57,]
totals <- totals[1:maxSector,]
# levels(totals$SIC) = 

# totals$SIC <- as.character(totals$SIC)

totals$SIC <- reorder(totals$SIC, seq(1:maxSector))
totals$SIC <- reorder(totals$SIC, totals$Demand)

totalsLong <- melt(totals)


#check: the mean should be the same for the same sectors
tapply(totalsLong$value, totalsLong$variable, mean)
#writeClipboard(as.character(tapply(totalsLong$value, totalsLong$variable, sd)))
tapply(totalsLong$value, totalsLong$variable, sd)

hist(log(totalsLong$value[totalsLong$variable=="Consumption"]), breaks = 20)
hist(log(totalsLong$value[totalsLong$variable=="Demand"]), breaks = 20)

output <- ggplot(totalsLong, aes(factor(variable), value)) +
  geom_boxplot(outlier.size = 5) +
  ggtitle("Total intermediate consumption vs\ntotal intermediate demand\nfor all sectors") +
#   scale_y_continuous(trans=log2_trans()) +  
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  xlab("") +
  ylab("Millions of pounds") +
  scale_y_log10()

output

# ggsave(output, file="consumptVsDemand.jpg", width=5, height=5, dpi = 600)

#Make consumption negative (money leaving that sector)
#So that both histograms can be set next to each other
#must be an easier way to do this?
cons <- totalsLong [ totalsLong$variable == "Consumption", ]
cons$value <- cons$value * -1
totalsLongNegCon <- rbind(cons, totalsLong[totalsLong$variable == "Demand",] ) 

#Histograms of same thing... 
# histyprinty <- ggplot(totalsLong, aes(factor(variable), value)) +
histyprinty <- ggplot(totalsLongNegCon, aes(x=SIC, y=value)) +
#   scale_y_continuous(trans=log2_trans()) +  
#     scale_y_continuous(trans=log2_trans(), breaks=c(2,4,8,16,30,60,120,400,1500,4500,16000)) +  
  #   coord_cartesian(xlim = c(0, 10)) +
  ggtitle("All sectors, total demand is green\nconsumption is red") +
  theme(plot.title = element_text(lineheight=1.5, face="bold")) +
  #   theme_classic() +
  xlab("sector") +
  ylab("millions of pounds") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  #   geom_histogram(data=subset(flows, as.numeric(SICfrom) < 50), colour="white", fill="black")
  
  geom_bar(stat="identity", data=subset(totalsLongNegCon, variable=="Consumption"), colour="white", fill="red") +
  geom_bar(stat="identity", data=subset(totalsLongNegCon, variable=="Demand"), 
                 colour="white", fill="green") 
#   geom_histogram(data=subset(totalsLong, variable=="Total.intermediate.consumption"), colour="white", fill="grey") +
#   geom_histogram(data=subset(totalsLong, variable=="Total.intermediate.demand"), 
#                  colour="white", fill="black", alpha = 0.5) 
#   geom_histogram(data=subset(flowsSub, (as.numeric(SICfrom) < 59)), colour="white", fill="black", 
#                  alpha = 0.5)

histyprinty



#Find difference between from and to
diffs <- NULL

# diffs$diffs <- (totals$Total.intermediate.consumption - totals$Total.intermediate.demand)
diffs$diffs <- (totals$Total.intermediate.demand - totals$Total.intermediate.consumption)

diffs$SIC <- "SIC"

diffs <- data.frame(diffs)

output <- ggplot(diffs, aes(y=diffs, x=SIC)) +
  geom_boxplot(outlier.size = 5) +
  ggtitle("Difference between\nintermediate consumption and\ndemand") +
  #   scale_y_continuous(trans=log2_trans()) +  
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  xlab(" ") +
  ylab("Difference, millions of pounds")

output

#Do the polarity of the flows correlate to sector order (and thus `weight', very loosely?)
flows4int <- read.csv("intermediateFlowsInLong.csv")

SICs <- unique(flows4int$SICfrom)
# SICs <- as.character(unique(flows4int$SICfrom))
#SICs <- ordered(SICs, levels = SICs)

#if only looking at subset, need to subset here too
#Do this after getting the SIC labels above
#flows4int <- flows4int[1:58,]
SICs <- SICs[1:maxSector]
SICs <- reorder(SICs, seq(1:maxSector))
#SICs <- data.frame(SICs)

SICs
  

diffs2 <- data.frame(diffs$diffs, SICs)
# diffs <- data.frame(diffs$diffs)
#diffs$SICs <- factor(diffs$SICs, levels = diffs$SICs)

# diffs$SICs <- as.character(SICs)

colnames(diffs2)[colnames(diffs2)=="diffs.diffs"] <- "diffs"

diffs2$SICs <- reorder(diffs2$SICs, seq(1:maxSector))

# cor(diff$diff.diffs, as.numeric(diff$SICs))
test <- lm(diffs$diffs ~ as.numeric(diffs$SICs))

#http://stat.ethz.ch/R-manual/R-patched/library/stats/html/cor.test.html
cor.test(diffs$diffs, as.numeric(diffs$SICs), method="pearson", conf.level = 0.95)

#diffs$diffs <- ordered(diffs$diffs, diffs$diffs[order(diffs$SICs)])

#pos/neg barplot
pointyPrinty <- ggplot(diffs2, aes(y = diffs, x = SICs)) +
  # pointyPrinty <- ggplot(SICslong[ SICslong$source=="to",], aes(y = SIC, x = value)) + +
  # pointyPrinty <- ggplot(SICslong[ SICslong$source=="to",], aes(y = SIC, x = value)) +
  #   geom_point(size =3, alpha=0.5, aes(fill=diffs>0)) +
  geom_bar(data = subset(diffs2, diffs < 0), stat="identity", fill="red") +
  geom_bar(data = subset(diffs2, diffs >= 0), stat="identity", fill="blue")
# geom_bar(data = diffs, stat="identity", fill="blue")
#   geom_bar(stat="identity") +
#   coord_flip()
#   scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"))

#   scale_x_continuous(trans=log10_trans())

pointyPrinty



pointyPrinty <- ggplot(diffs, aes(y = as.numeric(SICs), x = diffs, colour = diffs > 0)) +
# pointyPrinty <- ggplot(SICslong[ SICslong$source=="to",], aes(y = SIC, x = value)) + +
  # pointyPrinty <- ggplot(SICslong[ SICslong$source=="to",], aes(y = SIC, x = value)) +
#   geom_point(size =3, alpha=0.5, aes(fill=diffs>0)) +
  geom_point(size =4) 
#   scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"))
  
#   scale_x_continuous(trans=log10_trans())

pointyPrinty


histyprinty <- ggplot(diffs, aes(x=SIC, y=value)) +
  #   scale_x_continuous(trans=log2_trans()) +  
#   scale_y_continuous(trans=log2_trans(), breaks=c(2,4,8,16,30,60,120,400,1500,4500,16000)) +  
  #   coord_cartesian(xlim = c(0, 10)) +
  ggtitle("Money flows from 2011 combined use matrix") +
  theme(plot.title = element_text(lineheight=1.5, face="bold")) +
  #   theme_classic() +
  xlab("money flow quantity, millions (log axis)") +
  ylab("number of individual flows") +
  #   geom_histogram(data=subset(flows, as.numeric(SICfrom) < 50), colour="white", fill="black")
  
  geom_bar(stat="identity", data=subset(diffs, variable=="Total.intermediate.consumption"), colour="white", fill="grey") +
  geom_bar(stat="identity", data=subset(diffs, variable=="Total.intermediate.demand"), 
           colour="white", fill="black", alpha = 0.5) 
#   geom_histogram(data=subset(totalsLong, variable=="Total.intermediate.consumption"), colour="white", fill="grey") +
#   geom_histogram(data=subset(totalsLong, variable=="Total.intermediate.demand"), 
#                  colour="white", fill="black", alpha = 0.5) 
#   geom_histogram(data=subset(flowsSub, (as.numeric(SICfrom) < 59)), colour="white", fill="black", 
#                  alpha = 0.5)

histyprinty


highest <- diff[ diff$diffs >40000,]






