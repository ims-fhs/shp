# imsbasics::clc()
# library(tidyverse)
# library(shp)
# library(kml)
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", cols = c("IDPERS", "PXXF50"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# data <- data %>% pivot_longer(cols = -ID) %>% group_by(ID) %>% mutate(diff = lead(value) - value)
# data <- data[, -3]
# data <- data %>% pivot_wider(names_from = name, values_from = diff)
# data <- data[, -ncol(data)]
# data <- as.data.frame(data)
#
# PXXF50 <- clusterLongData(data)
# kml(PXXF50, c(2:6))
# x11(type = "Xlib"); choice(PXXF50)
# choice(PXXF50)
# plot(PXXF50, 2, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXF50 - Interference <-> private activities / family obligations")
# plot(PXXF50, 3, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXF50 - Interference <-> private activities / family obligations")
# plot(PXXF50, 4, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXF50 - Interference <-> private activities / family obligations")
# plot(PXXF50, 5, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXF50 - Interference <-> private activities / family obligations")
# plot(PXXF50, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXF50 - Interference <-> private activities / family obligations")
#
# plotAllCriterion(PXXF50, criterion=CRITERION_NAMES[1:5])
# PXXF50@criterionActif <- "Ray.Turi"
# choice(PXXF50)
# plot(PXXF50,3,parMean=parMEAN(type="l"))
#
#
# data <- import_long_cols("H_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", cols = c("IDHOUS", "HXXH01"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# data <- data %>% pivot_longer(cols = -ID) %>% group_by(ID) %>% mutate(diff = lead(value) - value)
# data <- data[, -3]
# data <- data %>% pivot_wider(names_from = name, values_from = diff)
# data <- data[, -ncol(data)]
# data <- as.data.frame(data)
#
# HXXH01 <- clusterLongData(data)
# kml(HXXH01, c(2:6))
# choice(HXXH01)
# plot(HXXH01, 2, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr")
# plot(HXXH01, 3, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr")
# plot(HXXH01, 4, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr")
# plot(HXXH01, 5, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr")
# plot(HXXH01, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr")
