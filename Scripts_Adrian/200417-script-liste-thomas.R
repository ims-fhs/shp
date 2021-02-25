# imsbasics::clc()
# library(tidyverse)
# library(shp)
# library(kml)
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXC71"), year_start = "2012", year_end = "2014")
# data <- data %>% filter(complete.cases(.))
#
# PXXC71 <- clusterLongData(data)
# kml(PXXC71, 6)
# plot(PXXC71, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXC71 - Self-Perception: little influence on life event")
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXC75"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXC75 <- clusterLongData(data)
# kml(PXXC75, 6)
# plot(PXXC75, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXC75 - Self-Perception: feeling of selfsatisfaction")
#
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXC106"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXC106 <- clusterLongData(data)
# kml(PXXC106, 6)
# plot(PXXC106, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXC106 - Sense of control: What I want is in my hands")
#
#
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXC108"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXC108 <- clusterLongData(data)
# kml(PXXC108, 6)
# plot(PXXC108, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXC108 - Sense of control: Others determine what I can do")
#
#
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXC06A"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXC06A <- clusterLongData(data)
# kml(PXXC06A, 6)
# plot(PXXC06A, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXC06A - Health problems: Sleeping problems: Last 4 weeks")
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXW604"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXW604 <- clusterLongData(data)
# kml(PXXW604, 6)
# plot(PXXW604, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXW604 - CMJ: Work conditions: stress")
#
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXW93"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXW93 <- clusterLongData(data)
# kml(PXXW93, 6)
# plot(PXXW93, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXW93 - CMJ: Satisfaction: Work conditions")
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXW94"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXW94 <- clusterLongData(data)
# kml(PXXW94, 6)
# plot(PXXW94, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXW94 - CMJ: Satisfaction: Work atmosphere")
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXW228"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXW228 <- clusterLongData(data)
# kml(PXXW228, 6)
# plot(PXXW228, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXW228 - CMJ: Satisfaction: Job in general")
#
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXF50"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXF50 <- clusterLongData(data)
# kml(PXXF50, 6)
# plot(PXXF50, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXF50 - Interference <-> private activities / family obligations")
#
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXF51"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXF51 <- clusterLongData(data)
# kml(PXXF51, 6)
# plot(PXXF51, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXF51 - Exhausted after work to do what you would like")
#
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXF52"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXF52 <- clusterLongData(data)
# kml(PXXF52, 6)
# plot(PXXF52, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXF52 - How difficult to deconnect from work")
#
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXC111"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXC111 <- clusterLongData(data)
# kml(PXXC111, 6)
# plot(PXXC111, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXC111 - Worries: Not keeping my workload up to date")
#
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXC115"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXC115 <- clusterLongData(data)
# kml(PXXC115, 6)
# plot(PXXC115, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr", main = "PXXC115 - Worries: Leaving the work unfinished")
#
#
#
