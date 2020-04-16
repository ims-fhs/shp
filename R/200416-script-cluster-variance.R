# imsbasics::clc()
# library(tidyverse)
# library(kml)
#
# traj <- matrix(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4),6)
#
# myCld <- clusterLongData(
#   traj=traj,
#   idAll=as.character(c(100,102,103,109,115,123)),
#   time=c(1,2,4,8,15),
#   varNames="P",
#   maxNA=3
# )
#
# kml(myCld, 2, toPlot = 'both')
#
# plot(myCld)
