df1 <- data.frame(id = 1:5, var_a = 1:5)
df2 <- data.frame(id = 6:3, var_a = 2:5)
df3 <- data.frame(id = c(2,4,5), var_a = 3)

df <- merge(df1, df2, all = TRUE)
df
df <- merge(df, df3, all = TRUE)
df

df <- data.frame(id = c(4,5), var_a_df1 = c(4,5), var_a_df2 = c(4,3), var_a_df3 = c(3,3))
df

# https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list

Reduce(function(x, y) merge(x, y, by = 'id'), list(df1, df2, df3))

library(tidyverse)
list(df1, df2, df3) %>% reduce(left_join, by = "id") %>% filter(complete.cases(.))
