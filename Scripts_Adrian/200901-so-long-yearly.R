# df <- tibble(
#   id = c(1,2,3),
#   val02 = c(0,1,0),
#   val03 = c(1,0,0),
#   val04 = c(0,1,1),
#   a02ge = c(1,2,3),
#   a03ge = c(2,3,4),
#   a04ge = c(3,4,5)
# )
#
# library(tidyverse)
# df1 <- df %>%
#   pivot_longer(cols = starts_with("val"), names_to = "year", values_to = "val", names_prefix = "val")
# df2 <- df %>%
#   pivot_longer(cols = starts_with("age"), names_to = "year", values_to = "age", names_prefix = "age")
#
# left_join(df1, df2) %>%
#   select(id, year, val, age)
#
# df %>%
#   pivot_longer(-id,
#                names_to = c('.value', 'year', '.value'),
#                names_pattern = '([a-z]+)(\\d+)([a-z]+)'
#   )
