#Correlate between-sector spearman's to the difference in transport costs between sectors.
#Reason: see if similar sectors have similar transport costs
library(ggplot2)
library(reshape2)

#Load IO data
#Stop it from checking variable names - this keeps the X's at bay
flows <- read.csv("domesticUse2010ownMatrix_removingImputedRent_SICcodesInHeaderOnly.csv", 
                  check.names=FALSE)

percProcess <- function(flows, rowNum, name) {
  #colSums(flows) is total consumption of that sector
  val <- (flows[rowNum,]/colSums(flows)) * 100
  val <- melt(val, variable.name="SIC", value.name=name)
  return(val)
}


#Find out land transport costs used by each sector as a % of its total consumption
#Land transport services are 49.3-5
#We don't have the code for the rows, so it's row 63
# flows[63,1]

#rail
rail <- percProcess(flows,62, "rail")
#give rail an index so we can reorder the SICs correctly
#they're fine here. By the time we get to merge, they're not
#http://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/
rail$index <- seq(1:104)

land <- percProcess(flows,63, "land")
water <- percProcess(flows,64, "water")
air <- percProcess(flows,65, "air")

#now combine in annoying way
transport <- merge(rail, land, by = "SIC")
transport <- merge(transport, water, by = "SIC")
transport <- merge(transport, air, by = "SIC")

#correct row order...
transport <- transport[order(transport$index),]
#correct factor order. Possibly
transport$SIC <- reorder(transport$SIC, transport$index)

names <- read.csv("domesticUse2010_shortSICnames.csv")

#refactor short names to the order they're in, not auto-alphabetic
names$shortname <- reorder(names$shortname, seq(1:104))

#add SIC short name labels
trans <- cbind(transport, names)

#hist(transPerc$percent, breaks=30)

#keep heavy only
#transPerc <- transPerc[1:58,]

#Variable for distinguishing light from heavy
trans$heavy <- "no"
trans$heavy[1:58] <-"yes"

#look at the small 'uns
#transPerc <- transPerc[transPerc$percent < 7,]

trans$shortname <- reorder(trans$shortname, -trans$land)

#summary(trans$percent)

#get a transport total per sector for subsetting later
#http://stackoverflow.com/questions/9651202/sum-variables-within-r-dataframe
trans$tots <- rowSums(subset(trans, select=c("rail","land","air","water")))

#melt! 
meltrans <- melt(trans, id.vars = c("SIC","index","shortname", "heavy", "tots"), 
                 measure.vars = c("rail","land", "water", "air"),
                                  variable.name="mode",
                                  value.name="percent")

meltrans$shortname <- reorder(meltrans$shortname, -meltrans$percent)

#subset some different graphs
# transub <- meltrans[meltrans$tots < 1.5,]
# transub <- meltrans[meltrans$tots < 5 & meltrans$tots > 1.5,]
transub <- meltrans[meltrans$tots > 5,]


printy <- ggplot(transub, aes(x=shortname, y=percent, fill = mode)) +
  #   scale_y_continuous(trans=log2_trans()) +  
  #     scale_y_continuous(trans=log2_trans(), breaks=c(2,4,8,16,30,60,120,400,1500,4500,16000)) +  
  #   coord_cartesian(xlim = c(0, 10)) +
  ggtitle("per-sector water/waste buying") +
  theme(plot.title = element_text(lineheight=1.5, face="bold")) +
  xlab("") +
  # ylab("percent") +
  ylab("percent") +
  # theme(axis.text = element_text(angle = 90, hjust = 1, vjust = -1)) +
  coord_flip() +
  geom_bar(stat="identity")#, colour="black"
#   geom_bar(stat="identity", aes(fill=heavy), colour="white")
#   geom_bar()

printy

#stacked bars
#ggsave(printy, file="transport.jpg", width=5, height=7, dpi = 600)
