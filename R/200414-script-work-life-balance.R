imsbasics::clc()
library(tidyverse)
library(shp)

wlb <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", cols = c("IDPERS", "P04F50"), year_start = "2004", year_end = "2017")
wlb_complete <- wlb %>% filter(complete.cases(.))

wlb_complete[3,]

wlb_complete_long <- wlb_complete %>% pivot_longer(
  cols = starts_with("P"), names_to = "year")


wlb_complete_long <- wlb_complete_long %>%
  mutate(year = as.numeric(str_sub(year, 2, 3)))

# 1) wlb for first person
wlb_complete_long_person1 <- wlb_complete_long %>% filter(IDPERS == 27102)
p <- ggplot(data = wlb_complete_long_person1, aes(x = year, y = value))
p + geom_line()

# 2) wlb for first 10 persons
ids <- unique(wlb_complete_long$IDPERS)[1:6]
wlb_complete_long_10 <- wlb_complete_long %>% filter(IDPERS %in% ids)
p <- ggplot(data = wlb_complete_long_10, aes(x = year, y = value, group = IDPERS))
p + geom_line(aes(color = as.factor(IDPERS))) +
  ggtitle("Work Life Balance over time (Variable F50; 2004 - 2017)")
