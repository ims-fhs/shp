# imsbasics::clc()
# library(tidyverse)
# library(shp)
# library(kml)
#
# head <- import_SPSS_file_head("SHP04_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W6_2004")
#
# data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", cols = c("IDPERS", "PXXC06A"), year_start = "2004", year_end = "2017")
# data <- data %>% filter(complete.cases(.))
#
# PXXC06A <- clusterLongData(data)
# kml(PXXC06A, c(2:6))
# plot(PXXC06A, 2, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr")
# plot(PXXC06A, 3, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr")
# plot(PXXC06A, 4, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr")
# plot(PXXC06A, 5, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr")
# plot(PXXC06A, 6, parTraj=parTRAJ(col="clusters"), toPlot = "traj", xlab = "Jahr")
# choice(PXXC06A)
#
# data$cluster <- as.character(PXXC06A@c2[[1]]@clusters)
#
# age <- import_cols("SHP10_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W12_2010", cols = c("IDPERS", "AGE10"))
# names(age)[1] <- "ID"
# data <- left_join(data, age)
#
# gender <- import_cols("SHP10_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W12_2010", cols = c("IDPERS", "SEX10"))
# names(gender)[1] <- "ID"
# data <- left_join(data, gender)
#
# ggplot(data = data, aes(x = AGE10, y = SEX10, color = cluster)) + geom_point()
#
#
# data_balanced <- data[, 16:18]
# data_balanced_true <- data_balanced[data_balanced$cluster == "B", ]
# data_balanced_false <- data_balanced[!data_balanced$cluster == "B", ]
# set.seed(123)
# sample <- sample(1:nrow(data_balanced_false), nrow(data_balanced_true))
# stopifnot(length(sample) == length(unique(sample)))
# data_balanced <- rbind(data_balanced_true, data_balanced_false[sample, ])
#
# data_balanced$cluster <- as.factor(data_balanced$cluster)
# data_balanced$SEX10 <- as.factor(data_balanced$SEX10)
#
# table(data_balanced[, -2])/nrow(data_balanced_true)*100
#
# hist(data_balanced$AGE10[data_balanced$cluster == "A"])
# hist(data_balanced$AGE10[data_balanced$cluster == "B"])
#
# data_balanced$mean_age_by_cluster <- data_balanced
# data_balanced <- data_balanced %>% group_by(cluster) %>% mutate(mean_age_by_cluster = mean(AGE10))
# data_balanced <- data_balanced %>% group_by(SEX10) %>% mutate(mean_age_by_sex = mean(AGE10))
#
