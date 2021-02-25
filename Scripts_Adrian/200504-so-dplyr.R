# imsbasics::clc()
# df <- data.frame(
#   cluster = c("A", "A", "B", "A", "B"),
#   age = c(1:5)
# )
#
# library(dplyr)
#
# my_summarise_manual <- function(df) {
#   df %>% group_by(cluster) %>% summarise(age := mean(age))
# }
# my_summarise_manual(df)
#
# my_group_manual <- function(df) {
#   df %>% group_by(cluster)
# }
# my_group_manual(df)
#
# my_group_1 <- function(df, a) {
#   a <- enquo(a)
#   df %>% group_by(!!a)
# }
# my_group_1(df)
#
# my_summarise_1 <- function(df, a, b) {
#   df %>% group_by(!!a) %>% summarise(!!b := mean(!!b))
# }
#
# my_summarise_2 <- function(df, a, b) {
#   a <- enquo(a)
#   b <- enquo(b)
#   df %>% group_by(!!a) %>% summarise(!!b := mean(!!b))
# }
# my_summarise_2(df, cluster, age)
