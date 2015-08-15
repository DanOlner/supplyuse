#What percentage of a sector's total consumption is spent on itself?
library(ggplot2)

io <- read.csv("2010_domesticUsejustMatrix.csv")

#keep sector labels for later
labels <- unique(io$sector)

io <- io[,-1]

#Matrix form
iom <- data.matrix(io)

#pull out each sector's spending on itself
selfconsume <- diag(iom)

#get total sector consumption
totalconsume <- apply(iom, 2, FUN=sum)

percentOnSelf <- (selfconsume/totalconsume) * 100

boxplot(percentOnSelf)

#keep for later boxplot
percentOnSelfAll <- percentOnSelf

selfSpend = data.frame(labels, percentOnSelf)

selfSpend$labels <- reorder(selfSpend$labels, -selfSpend$percentOnSelf)

output <- ggplot(selfSpend, aes(x = labels, y = percentOnSelf, fill = percentOnSelf)) +
  geom_bar(stat = "identity") +
  coord_flip()

output

#repeat for heavy sectors
iom <- io[1:58,1:58]
iom <- data.matrix(iom)

#pull out each sector's spending on itself
selfconsume <- diag(iom)

#get total sector consumption
totalconsume <- apply(iom, 2, FUN=sum)

percentOnSelf <- (selfconsume/totalconsume) * 100

selfSpend = data.frame(labels[1:58], percentOnSelf)

selfSpend$labels <- reorder(selfSpend$labels, -selfSpend$percentOnSelf)

output <- ggplot(selfSpend, aes(x = labels, y = percentOnSelf, fill = percentOnSelf)) +
  geom_bar(stat = "identity") +
  coord_flip()

output

#compare to all to heavy
selfspendcomp <- data.frame(percent = percentOnSelfAll, type="all")
selfspendcomp <- rbind(selfspendcomp, data.frame(percent = percentOnSelf, type="heavy"))

output <- ggplot(selfspendcomp, aes(x = type, y = percent)) +
  geom_boxplot()

output