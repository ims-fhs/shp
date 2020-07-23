imsbasics::clc()
library(tidyverse)
library(shp)

# Autonomie -> Selbstwirksamkeitserwartung

aut_vs_sel <- import_cols("SHP15_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W17_2015", cols = c("IDPERS", "P15C70", "P15W91", "SEX15"))
aut_vs_sel <- aut_vs_sel %>% filter(complete.cases(.))

ggplot(aut_vs_sel) +
  geom_point(alpha = 0.2, mapping = aes(x = P15C70, y = P15W91, color = SEX15), position = "jitter")

cor.test(aut_vs_sel$P15C70, aut_vs_sel$P15W91)
