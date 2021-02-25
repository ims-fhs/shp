# clustering..
source("R/200908-script-schritt1-clustering-wstat.R")
rm(list=setdiff(ls(), "dep_clustering"))

# FE
library(tidyverse)
library(shp)
library(lubridate)
library(plm)       # Panel data analysis library
library(car)       # Companion to applied regression
library(gplots)    # Various programing tools for plotting data
library(tseries)   # For timeseries analysis
library(lmtest)    # For hetoroskedasticity analysis
library(lme4)

data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS",
                         cols = c("IDPERS", "PXXC17", "PXXF51", "PXXW71A", "PXXW77", "PXXW87", "PXXW91", "PXXW94", "PXXW100", "PXXW216", "PXXW229", "PXXW603", "PXXW604", "PXXW605", "PXXW606", "PXXD29", "PXXL11", "PXXF50", "PXXF52", "PXXC08", "PXXC11", "PXXC19A", "PXXF08"), year_start = "2008", year_end = "2017")

longer <- data %>%
  pivot_longer(-ID,
               names_to = c('.value', 'year', '.value'),
               names_pattern = '([A-Z]*)(\\d{2})([A-Z]*\\d*)') %>%
  mutate(year = as.numeric(paste0("20", year)))

dep_clustering_rich <- left_join(dep_clustering, longer) %>%
  mutate(cluster = cluster_second_level) %>%
  select(-cluster_second_level)

rm(list=setdiff(ls(), "dep_clustering_rich"))

paneldata <- pdata.frame(dep_clustering_rich, index = c("ID", "year"))

dep_clus_summary <- dep_clustering_rich %>%
  group_by(cluster) %>%
  rename(cluster = cluster) %>%
  summarise(percentage = round(n()/nrow(.)*100))

dep_clustering_min_median_max <- dep_clustering_rich %>%
  group_by(cluster, year) %>%
  summarise(ymin = quantile(depression, .25),
            ymedian = median(depression),
            ymax = quantile(depression, .75))



ggplot(dep_clustering_min_median_max, aes(x = year, y = ymedian, color = cluster)) +
  # geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = cluster), alpha = 0.1) +
  geom_smooth(aes(fill = cluster), method = lm, se = FALSE) +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_y_continuous(limits = c(0,10), name = "depression level") +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  guides(fill = FALSE)

ermuedung <- plm(PF51 ~ PD29 + PW229 + PW71 + PW77 + PW87 + PW91 + PW94 + PW100 + PW216 + PW603 + PW604 + PW605 + PW606 + PL11 + PF08 + PF50 + PF52 + PC08 + PC11 + PC19, data=dep_clustering_rich, model="within")
summary(ermuedung)

depression <- plm(PC17 ~ PD29 + PW229 + PW71 + PW77 + PW87 + PW91 + PW94 + PW100 + PW216 + PW603 + PW604 + PW605 + PW606 + PL11 + PF08 + PF50 + PF52 + PC08 + PC11 + PC19, data=dep_clustering_rich, model="within")
summary(depression)

dep_clustering_rich_a <- dep_clustering_rich %>% filter(cluster == "A")
depression_a <- plm(PC17 ~ PD29 + PW229 + PW71 + PW77 + PW87 + PW91 + PW94 + PW100 + PW216 + PW603 + PW604 + PW605 + PW606 + PL11 + PF08 + PF50 + PF52 + PC08 + PC11 + PC19, data=dep_clustering_rich_a, model="within")
summary(depression_a)

dep_clustering_rich_c <- dep_clustering_rich %>% filter(cluster == "C")
depression_c <- plm(PC17 ~ PD29 + PW229 + PW71 + PW77 + PW87 + PW91 + PW94 + PW100 + PW216 + PW603 + PW604 + PW605 + PW606 + PL11 + PF08 + PF50 + PF52 + PC08 + PC11 + PC19, data=dep_clustering_rich_c, model="within")
summary(depression_c)

dep_clustering_rich_d <- dep_clustering_rich %>% filter(cluster == "D")
depression_d <- plm(PC17 ~ PD29 + PW229 + PW71 + PW77 + PW87 + PW91 + PW94 + PW100 + PW216 + PW603 + PW604 + PW605 + PW606 + PL11 + PF08 + PF50 + PF52 + PC08 + PC11 + PC19, data=dep_clustering_rich_d, model="within")
summary(depression_d)

dep_clustering_rich_e <- dep_clustering_rich %>% filter(cluster == "E")
depression_e <- plm(PC17 ~ PD29 + PW229 + PW71 + PW77 + PW87 + PW91 + PW94 + PW100 + PW216 + PW603 + PW604 + PW605 + PW606 + PL11 + PF08 + PF50 + PF52 + PC08 + PC11 + PC19, data=dep_clustering_rich_e, model="within")
summary(depression_e)

