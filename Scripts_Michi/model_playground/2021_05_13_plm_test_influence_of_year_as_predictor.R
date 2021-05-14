# ------------------------------------------------------------------------------
# Author:  SLC
# Date:    13.05.2021
# Summary: What is the effect of using the time-parameter explicitly in a
#          plm-Model? I'm not quite sure if I understood the difference of
#          the following models:
#             y ~ x1 + x2         -> a plm model implicitly uses the "year"... but how?
#             y ~ x1 + x2 + year
#
#          I want to compare both approchaes on a dummy example, where y neither
#          depends on x1 or x2 but increases with time (depends on year). How do the two models
#          differ in this situation?
#
# ------------------------------------------------------------------------------
#
# Result:  It does not make sense to include time explicitly in a plm-Model
#            -> y ~ x1 + x2 + year  gives no coefficitent for "year" -
#               (it is treated as factor variable - a fixed-effect / dummy-variable is
#               calculated for every year)
#            -> it's the same as using a "twoways" Model !!
#            -> see prediction_1 & prediction_3 are identical !!!!!
#
#          An alternative is to use models with (effect = "time) or (effect = "twoways")
#            -> Here we calculate explicit fixed-effects for time. It's basically
#               the same as above but it's done by the model
#
# see https://stats.stackexchange.com/questions/246548/difference-between-one-way-and-two-way-fixed-effects-and-their-estimation
#
#

imsbasics::clc()



# ------ True Relation: y ~ 8*year ---------------------------------------------

# create the data
df <- expand.grid(year = seq(11,20), id = seq(1,10)); df <- df[,c(2,1)]
df$x1 <- 5 + 0.1*df$id + runif(nrow(df),0,1)
df$x2 <- 8 + 0.5*df$id + runif(nrow(df),0,3)
df$y <- 8*df$year + runif(nrow(df),-6,6)
df <- df %>% mutate(y_mean = mean(y))
df <- df %>% group_by(id) %>% mutate(y_mean_per_id = mean(y))
df <- df %>% group_by(year) %>% mutate(y_mean_per_year = mean(y)) %>% ungroup()

p1 <- ggplot(data = df, mapping = aes(x = year, y = y, color = as.factor(id))) +  geom_line()
p2 <- ggplot(data = df, mapping = aes(x = year, y = x1, color = as.factor(id))) +  geom_line()
p3 <- ggplot(data = df, mapping = aes(x = year, y = x2, color = as.factor(id))) +  geom_line()
ggpubr::ggarrange(p1, p2, p3, ncol = 1,
                  labels = c("Dependent variable `y`",
                             "Independent variable `x1`",
                             "Independent variable `x2`"))


# A "normal" plm-model doesn't fit the situation well, because the dependent variable
# y can only be modeled on x1 & x2 which don't change over time.
m0 <- plm(y ~ x1 + x2, data = df, effect = "individual")
summary(m0)
df$prediction_0 <- fitted(m0) + df$y_mean_per_id


# A plm-model with an explict "time"-variable is more suitable for this case. unfortunately,
# the model recognizes the time-variable as factor and not as numeric
# -> It looks at time as an implicit grouping -> Can we get rid of that?
m1 <- plm(y ~ x1 + x2 + year, data = df)
summary(m1)
df$prediction_1 <- fitted(m1) + df$y_mean_per_id

# A plm-model with an effect on "time" can handle the problem
m2 <- plm(y ~ x1 + x2, data = df, effect = "time")
summary(m2)
df$prediction_2 <- fitted(m2) + df$y_mean_per_year

# A plm-model with a twoways-effect can handle the problem
m3 <- plm(y ~ x1 + x2, data = df, effect = "twoways")
summary(m3)
df$prediction_3 <- fitted(m3) + df$y_mean_per_id + df$y_mean_per_year - df$y_mean



library(tidyr)
df_long <- df %>% pivot_longer(cols = starts_with("prediction_"),
                             names_to = "model", values_to = "prediction")

# Summary:
ggplot(df_long) +
  geom_line(aes(x = year, y = y, group = as.factor(id))) +
  geom_line(aes(x = year, y = prediction, color = as.factor(id))) +
  facet_wrap(~model)
