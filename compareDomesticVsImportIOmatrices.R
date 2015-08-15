# Turn domestic and export IO matrices into pairs, for comparison
setwd("C:/Users/geodo/Dropbox/R/Workspace/SupplyUseMatrix")

library(ggplot2)
library(reshape)
library(scales)

#domestic IO
matrixDom <- read.csv("domesticUse2010ownMatrix_removingImputedRentSICcodes.csv")

matrixDom <- melt(matrixDom)

#import IO
matrixImp <- read.csv("importUse2010ownMatrix_removingImputedRent.csv")

matrixImp <- melt(matrixImp)

