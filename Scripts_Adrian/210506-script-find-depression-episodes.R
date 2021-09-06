library(tidyverse)

sample <- sample %>% select(id, year, depression) %>% slice_head(n = 10)

sample_without_nas <- sample %>% drop_na

for(i)
