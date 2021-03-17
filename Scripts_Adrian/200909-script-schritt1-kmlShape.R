imsbasics::clc()
library(tidyverse)
library(shp)
library(kml)
library(kmlShape)
library(lubridate)

data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS", cols = c("IDPERS", "WSTATXX", "AGEXX", "PXXC17"), year_start = "2008", year_end = "2017")

longer <- data
longer <- longer %>%
  pivot_longer(-ID,
               names_to = c('.value', 'year'),
               names_pattern = '([A-Z]+)(\\d+)')

occupied_and_right_age <- longer %>%
  group_by(ID) %>%
  filter(all(WSTAT %in% c(1))) %>%
  filter(all(AGE %in% c(18:65))) %>%
  rename(depression = P)

# some exploratory plots
df <- occupied_and_right_age
ggplot(df %>% group_by(ID) %>% summarise(AGE = mean(AGE))) +
  geom_histogram(aes(x = AGE), binwidth =  5)

ggplot(df) +
  geom_histogram(aes(x = depression), bins = 11)

ggplot(df %>% group_by(ID) %>% summarise(depression = mean(depression))) +
  geom_histogram(aes(x = depression))

dep_per_year <- df %>%
  mutate(year = as.numeric(paste0("20", year))) %>%
  group_by(year) %>%
  summarise(depression = mean(depression, na.rm = TRUE))
ggplot(dep_per_year, aes(x = as.integer(year), y = depression)) +
  geom_line() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017))

# clustering..
occupied_and_right_age <- occupied_and_right_age %>%
  ungroup() %>%
  select(ID, year, depression) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(year = year - 8)

occupied_and_right_age_clustered <- cldsLong(as.data.frame(occupied_and_right_age))
kmlShape(occupied_and_right_age_clustered, nbClusters = 3, timeScale = 0.001, toPlot = "none")
plot(occupied_and_right_age_clustered)

#
# # clustering..
# occupied_and_right_age_median_dep <- occupied_and_right_age %>%
#   group_by(ID) %>%
#   mutate(depression = depression - median(depression)) %>%
#   ungroup() %>%
#   mutate(depression = depression + 10)
# occupied_and_right_age_clustered_median_dep <- cldsLong(as.data.frame(occupied_and_right_age_median_dep))
# kmlShape(occupied_and_right_age_clustered_median_dep, nbClusters = 3, timeScale = .01, toPlot = "none")
# plot(occupied_and_right_age_clustered_median_dep)

occupied_and_right_age$cluster <- as.character(occupied_and_right_age_clustered@c2[[1]]@clusters)


# Plot Clustering

dep_clustering <- depression_per_id_wide_no_na %>%
  select(-matches("cluster_\\d$")) %>%
  pivot_longer(-c(ID, cluster_second_level), names_to = "year", values_to = "depression") %>%
  mutate(year = as.numeric(paste0("20", year))) %>%
  ungroup()

dep_clustering_median <- dep_clustering %>%
  group_by(cluster_second_level, year) %>%
  summarise(depression = median(depression))

dep_clus_summary <- dep_clustering %>%
  group_by(cluster_second_level) %>%
  rename(cluster = cluster_second_level) %>%
  summarise(percentage = round(n()/nrow(.)*100))

ggplot(dep_clustering_median, aes(x = year, y = depression, color = cluster_second_level)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%"))

ggplot(dep_clustering_median, aes(x = year, y = depression, color = cluster_second_level)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  geom_line(data = dep_clustering, aes(group = ID), alpha = 0.02)

ggplot(dep_clustering_median, aes(x = year, y = depression, color = cluster_second_level)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  geom_line(data = dep_clustering %>% filter(cluster_second_level == "A"), aes(group = ID), alpha = 0.05)

ggplot(dep_clustering_median, aes(x = year, y = depression, color = cluster_second_level)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  geom_line(data = dep_clustering %>% filter(cluster_second_level == "B"), aes(group = ID), alpha = 0.05)

ggplot(dep_clustering_median, aes(x = year, y = depression, color = cluster_second_level)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  geom_line(data = dep_clustering %>% filter(cluster_second_level == "C"), aes(group = ID), alpha = 0.05)

ggplot(dep_clustering_median, aes(x = year, y = depression, color = cluster_second_level)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  geom_line(data = dep_clustering %>% filter(cluster_second_level == "O"), aes(group = ID), alpha = 0.02)

dep_clustering_min_median_max <- dep_clustering %>%
  group_by(cluster_second_level, year) %>%
  summarise(ymin = quantile(depression, .25),
            ymedian = median(depression),
            ymax = quantile(depression, .75))


ggplot(dep_clustering_min_median_max, aes(x = year, y = ymedian, color = cluster_second_level)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = cluster_second_level), alpha = 0.3) +
  geom_smooth(aes(fill = cluster_second_level), se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  guides(fill = FALSE)


# Alternative: 5 Cluster
dep_clustering <- depression_per_id_wide_no_na %>%
  mutate(cluster_second_level = cluster_5) %>%
  mutate(cluster_second_level = ifelse(cluster_second_level == "B", "A", cluster_second_level)) %>%
  select(-matches("cluster_\\d$")) %>%
  pivot_longer(-c(ID, cluster_second_level), names_to = "year", values_to = "depression") %>%
  mutate(year = as.numeric(paste0("20", year))) %>%
  ungroup()

dep_clustering_median <- dep_clustering %>%
  group_by(cluster_second_level, year) %>%
  summarise(depression = median(depression))

dep_clus_summary <- dep_clustering %>%
  group_by(cluster_second_level) %>%
  rename(cluster = cluster_second_level) %>%
  summarise(percentage = round(n()/nrow(.)*100))

ggplot(dep_clustering_median, aes(x = year, y = depression, color = cluster_second_level)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%"))

ggplot(dep_clustering_median, aes(x = year, y = depression, color = cluster_second_level)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  geom_line(data = dep_clustering, aes(group = ID), alpha = 0.02)

ggplot(dep_clustering_median, aes(x = year, y = depression, color = cluster_second_level)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  geom_line(data = dep_clustering %>% filter(cluster_second_level == "A"), aes(group = ID), alpha = 0.05)

ggplot(dep_clustering_median, aes(x = year, y = depression, color = cluster_second_level)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  geom_line(data = dep_clustering %>% filter(cluster_second_level == "B"), aes(group = ID), alpha = 0.05)

ggplot(dep_clustering_median, aes(x = year, y = depression, color = cluster_second_level)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  geom_line(data = dep_clustering %>% filter(cluster_second_level == "C"), aes(group = ID), alpha = 0.05)

ggplot(dep_clustering_median, aes(x = year, y = depression, color = cluster_second_level)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  geom_line(data = dep_clustering %>% filter(cluster_second_level == "O"), aes(group = ID), alpha = 0.2)

dep_clustering_min_median_max <- dep_clustering %>%
  group_by(cluster_second_level, year) %>%
  summarise(ymin = quantile(depression, .25),
            ymedian = median(depression),
            ymax = quantile(depression, .75))


ggplot(dep_clustering_min_median_max, aes(x = year, y = ymedian, color = cluster_second_level)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = cluster_second_level), alpha = 0.1) +
  geom_smooth(aes(fill = cluster_second_level), method = lm, se = FALSE) +
  scale_x_continuous(breaks = c(2008, 2010, 2015, 2017), minor_breaks = c(2008:2017)) +
  scale_y_continuous(limits = c(0,10), name = "depression level") +
  scale_color_discrete(name = "cluster", labels = paste0(dep_clus_summary$cluster,": ", dep_clus_summary$percentage, "%")) +
  guides(fill = FALSE)





