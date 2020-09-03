# df <- tibble(
#   id = 1:18,
#   cluster = rep(c(rep(1,3),rep(2,2),3),3),
#   var_a = rep(c("a","b"),9)
# )
# df
#
# min_length <- as.numeric(df %>%
#   group_by(cluster) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   summarise(min = min(n)))
#
# set.seed(1)
# df %>% group_by(cluster) %>% sample_n(min_length)
