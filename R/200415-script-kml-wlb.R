# imsbasics::clc()
# library(tidyverse)
# library(shp)
# library(kml)
#
# wlb <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", cols = c("IDPERS", "P04F50"), year_start = "2004", year_end = "2017")
# wlb_complete <- wlb %>% filter(complete.cases(.))
#
# wlb_complete_ld <- clusterLongData(wlb_complete)
# kml(wlb_complete_ld, c(2:8),1,toPlot = 'both')
# choice(wlb_complete_ld)
#
# kml(wlb_complete_ld, 6,1,toPlot = 'both')
