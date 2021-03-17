# imsbasics::clc()
# library(tidyverse)
# library(shp)
# library(kml)
# library(lubridate)
#
# source('R/200908-script-schritt1-clustering-wstat.R')
#
# occupied_and_right_age <- occupied_and_right_age %>%
#   mutate(year = as.numeric(paste0("20", year)))
# depression_explained <- left_join(dep_clustering, occupied_and_right_age) %>%
#   rename(cluster = cluster_second_level) %>%
#   filter(complete.cases(.))
#
# # Import potentially explaining variables
# explaining_variables <- import_cols("SHP08_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W10_2008", cols = c("IDPERS", "SEX08", "EDUCAT08", "P08L11", "P08F50", "P08F08", "P08W77", "P08D29"))
# explaining_variables <- explaining_variables %>% rename(ID = IDPERS)
#
# depression_explained <- left_join(depression_explained, explaining_variables)
# depression_explained <- depression_explained %>%
#   group_by(ID, cluster) %>%
#   summarise_all(mean)
#
#
# depression_explained <- depression_explained %>%
#   ungroup() %>%
#   filter(complete.cases(.)) %>%
#   filter(cluster != "A")
# dep_rf <- depression_explained
# dep_rf <- dep_rf %>% group_by(cluster) %>% sample_n(min(table(dep_rf$cluster)))
#
# library(randomForest)
# set.seed(911)
# psy.rf <- randomForest(as.factor(cluster) ~ . - ID - depression - year, data = dep_rf, importance = TRUE)
# print(psy.rf)
# round(importance(psy.rf), 2)
#
# dep_tree <- depression_explained %>% mutate_if(is.character, as.factor)
# dep_tree <- dep_tree %>% group_by(cluster) %>% sample_n(min(table(dep_tree$cluster)))
# library(tree)
# dep.tree <- tree(cluster~.-depression-ID-year, data = dep_tree, control=tree.control(210, mincut = 10))
# summary(dep.tree)
# plot(dep.tree)
# text(dep.tree, pretty = 0)
# dep.tree <- tree(cluster~.-depression-ID-year, data = dep_tree, control=tree.control(210, mincut = 20))
# summary(dep.tree)
# plot(dep.tree)
# text(dep.tree, pretty = 0)
#
# dep.tree <- tree(cluster~.-depression-ID-year, data = dep_tree, control=tree.control(210, mincut = 30))
# summary(dep.tree)
# plot(dep.tree)
# text(dep.tree, pretty = 0)
# dep.tree <- tree(cluster~.-depression-ID-year, data = dep_tree, control=tree.control(210, mincut = 40))
# summary(dep.tree)
# plot(dep.tree)
# text(dep.tree, pretty = 0)
# dep.tree <- tree(cluster~.-depression-ID-year, data = dep_tree, control=tree.control(210, mincut = 50))
# summary(dep.tree)
# plot(dep.tree)
# text(dep.tree, pretty = 0)
#
#
