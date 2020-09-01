df <- tibble(
  id = c(1,2,3),
  val02 = c(0,1,0),
  val03 = c(1,0,0),
  val04 = c(0,1,1),
  age02 = c(1,2,3),
  age03 = c(2,3,4),
  age04 = c(3,4,5)
)

library(tidyverse)
df1 <- df %>%
  pivot_longer(cols = starts_with("val"), names_to = "year", values_to = "val", names_prefix = "val")
df2 <- df %>%
  pivot_longer(cols = starts_with("age"), names_to = "year", values_to = "age", names_prefix = "age")

left_join(df1, df2) %>%
  select(id, year, val, age)

