#Who consumes the most water?
library(ggplot2)
library(reshape2)

#Load IO data
#Stop it from checking variable names - this keeps the X's at bay
flows <- read.csv("domesticUse2010ownMatrix_removingImputedRent_SICcodesInHeaderOnly.csv", 
                  check.names=FALSE)

#subset heavy
sn = 58
flows <- flows[1:sn,1:sn]

names <- read.csv("domesticUse2010_shortSICnames.csv")
names <- names[1:sn,]
#refactor short names to the order they're in, not auto-alphabetic
names$shortname <- reorder(names$shortname, seq(1:sn))


percProcess <- function(flows, rowNum, name) {
  #colSums(flows) is total consumption of that sector
  val <- (flows[rowNum,]/colSums(flows)) * 100
  #Or absolute... 
  # val <- (flows[rowNum,])
  val <- melt(val, variable.name="SIC", value.name=name)
  return(val)
}


#Find out land transport costs used by each sector as a % of its total consumption
#Land transport services are 49.3-5
#We don't have the code for the rows, so it's row 63
# flows[63,1]

#give rail an index so we can reorder the SICs correctly
#they're fine here. By the time we get to merge, they're not
#http://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/

# sectors = c("water","sewage","waste","remediation")
# sectors = c("water","sewage")
sectors = c("water")


water <- percProcess(flows, 54,"water")
water$index <- seq(1:sn)

#How much water do sectors consume?
# sewage <- percProcess(flows, 55,"sewage")
#waste <- percProcess(flows, 56,"waste")
#remed <- percProcess(flows, 57,"remediation")

#now combine in annoying way
# all <- merge(water, sewage, by = "SIC")
#all <- merge(all, waste, by = "SIC")
#all <- merge(all, remed, by = "SIC")

all <- water

#correct row order...
all <- all[order(all$index),]
#correct factor order. Possibly
all$SIC <- reorder(all$SIC, all$index)


#add SIC short name labels
all <- cbind(all, names)

#hist(transPerc$percent, breaks=30)

#keep heavy only
#transPerc <- transPerc[1:58,]

#Variable for distinguishing light from heavy
all$heavy <- "no"
all$heavy[1:58] <-"yes"

#get a transport total per sector for subsetting later
#http://stackoverflow.com/questions/9651202/sum-variables-within-r-dataframe
# all$tots <- rowSums(subset(all, select=c("water","sewage","waste","remediation")))
all$tots <- rowSums(subset(all, select=sectors))

all$shortname <- reorder(all$shortname, -all$tots)

#melt! 
meltall <- melt(all, id.vars = c("SIC","index","shortname", "heavy", "tots"), 
                measure.vars = sectors,
                                 variable.name="sector",
                                 value.name="percent")



meltall$shortname <- reorder(meltall$shortname, -meltall$percent)

#subset some different graphs
# transub <- meltrans[meltrans$tots < 1.5,]
# transub <- meltrans[meltrans$tots < 5 & meltrans$tots > 1.5,]
sub <- meltall[meltall$percent > 0.1,]
# sub <- meltall

#stacked bars
printy <- ggplot(sub, aes(x=shortname, y=percent, fill = sector)) +
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

# ggsave(printy, file="waterabs.jpg", width=7, height=8, dpi = 600)
ggsave(printy, file="water.jpg", width=7, height=8, dpi = 600)

