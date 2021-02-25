library(tidyverse)
library(plm)

df <- tibble(
  year = rep(c(1,2,3,4), 3),
  id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
  einkommen = c(4, 1, 2, 3, 3, 4, 5, 6, 2, 4, 2, 4),
  zufriedenheit = c(5, 2, 3, 4, 5, 6, 5, 6, 7, 8, 5, 6)
)

ggplot(df, aes(x = einkommen, y = zufriedenheit)) +
  geom_point(aes(color = id)) +
  geom_smooth(method = "lm", se = FALSE)

lm(zufriedenheit ~ einkommen, df)
plm(zufriedenheit ~ einkommen, df, model = "within")

