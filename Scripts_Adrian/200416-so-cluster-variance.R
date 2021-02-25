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
# kml(myCld, 2)
#
# plot(myCld)
# plot(myCld, 2, parTraj=parTRAJ(col="clusters"), toPlot = "traj")
# # https://stackoverflow.com/questions/61247205/how-can-we-show-the-trajectories-belonging-to-clusters-in-kml-package
#
