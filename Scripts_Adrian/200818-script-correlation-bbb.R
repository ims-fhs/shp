# imsbasics::clc()
# library(tidyverse)
# library(shp)
#
# # Autonomie -> Selbstwirksamkeitserwartung
# # Versuch 1
# aut_vs_sel <- import_cols("SHP15_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W17_2015", cols = c("IDPERS", "P15W87", "P15C71"))
# aut_vs_sel <- aut_vs_sel %>% filter(complete.cases(.))
# aut_vs_sel <- aut_vs_sel %>%
#   mutate(P15W87 = ifelse(P15W87 == 2, 0, 1)) %>%
#   mutate(P15C71 = P15C71/10)
#
# ggplot(aut_vs_sel) +
#   geom_point(alpha = 0.2, mapping = aes(x = P15C71, y = -P15W87), position = "jitter")
#
# ggplot(aut_vs_sel) +
#   geom_smooth(mapping = aes(x = P15C71, y = P15W87), method = "lm")
#
# # ggplot(aut_vs_sel) +
# #   geom_smooth(mapping = aes(x = P15C71, y = P15W87), method = "glm",
# #               method.args = list(family = "binomial"))
#
#
# aut_vs_sel$cluster <- kmeans(aut_vs_sel[, -1], 4)$cluster
#
# ggplot(aut_vs_sel) +
#   geom_point(alpha = 0.2, mapping = aes(x = P15C71, y = P15W87, color = cluster), position = "jitter")
#
#
#
# cor.test(aut_vs_sel$P15C71, aut_vs_sel$P15W87)
#
# kmeans()
#
#
# # Autonomie -> Selbstwirksamkeitserwartung
# # Versuch 2
# aut_vs_sel <- import_cols("SHP15_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W17_2015", cols = c("IDPERS", "P15W87", "P15C71", "P15W228"))
# aut_vs_sel <- aut_vs_sel %>% filter(complete.cases(.))
# aut_vs_sel <- aut_vs_sel %>%
#   mutate(P15W87 = ifelse(P15W87 == 2, 0, 1)) %>%
#   mutate(P15C71 = P15C71/10)
#
# ggplot(aut_vs_sel) +
#   geom_point(alpha = 0.2, mapping = aes(x = P15C71, y = P15W87), position = "jitter")
#
# ggplot(aut_vs_sel) +
#   geom_point(alpha = 0.2, mapping = aes(x = P15C71, y = P15W87, color = P15W228), position = "jitter")
#
#
# ggplot(aut_vs_sel) +
#   geom_point(alpha = 0.2, mapping = aes(x = P15C71, y = P15W87, color = ifelse(P15W228 > 6, 2, 1)), position = "jitter")
#
#
# ggplot(aut_vs_sel) +
#   geom_smooth(mapping = aes(x = P15C71, y = P15W87), method = "lm")
#
#
#
# # ggplot(aut_vs_sel) +
# #   geom_smooth(mapping = aes(x = P15C71, y = P15W87), method = "glm",
# #               method.args = list(family = "binomial"))
#
#
# aut_vs_sel$cluster <- kmeans(aut_vs_sel[, -1], 4)$cluster
#
# ggplot(aut_vs_sel) +
#   geom_point(alpha = 0.2, mapping = aes(x = P15C71, y = P15W87, color = cluster), position = "jitter")
#
#
#
# cor.test(aut_vs_sel$P15C71, aut_vs_sel$P15W87)
#
# kmeans()
