# ------------------------------------------------------------------------------
# Author:  SLC
# Date:    15.05.2021
# Summary: How to interpret the coefficients in a FD-Model for a factor-variable?
#
#          Let's create a dataset with obvious differences for a factor-variable
#          and then look at the coefficients.
#
#

imsbasics::clc()



# ------ True Relation: y ~ 8*year ---------------------------------------------

# create the data
df <- expand.grid(year = seq(11,20), id = seq(1,10)); df <- df[,c(2,1)]
df$x1 <- 5 - 0.1*df$id + 0.2*df$year + runif(nrow(df),0,1) # x1 is slightly correlated to id
df$x2 <- 8 + 1*df$id + - 1*df$year + runif(nrow(df),0,3)   # x2 is strongly correlated to id
df$y <- 5*df$x1 + 10*df$x2 + runif(nrow(df),-6,6)
df$type <- rep(rep(c("baby", "toddler", "teenie", "middle", "old"), each = 2),10) # Every person was 5times young, then 5 times old
df$type <- as.factor(df$type)

# df <- df %>% mutate(y_mean = mean(y))
# df <- df %>% group_by(id) %>% mutate(y_mean_per_id = mean(y))
# df <- df %>% group_by(year) %>% mutate(y_mean_per_year = mean(y)) %>% ungroup()

p1 <- ggplot(data = df, mapping = aes(x = year, y = y, color = as.factor(id))) +  geom_line()
p2 <- ggplot(data = df, mapping = aes(x = year, y = x1, color = as.factor(id))) +  geom_line()
p3 <- ggplot(data = df, mapping = aes(x = year, y = x2, color = as.factor(id))) +  geom_line()
ggpubr::ggarrange(p1, p2, p3, ncol = 1,
                  labels = c("Dependent variable `y`",
                             "Independent variable `x1`",
                             "Independent variable `x2`"))

# The FE-Model estimates the factors for x1 and x2
m0 <- plm(y ~ x1 + x2 + type, data = df, model = "within")
summary(m0)


m1 <- plm(y ~ x1 + x2 + type, data = df, model = "fd")
summary(m1)


m2 <- plm(y ~ type, data = df, model = "fd")
summary(m2)


# Interpretation: the factors keep their order (baby = baseline, toddler = smallest value,
# old = highes value). --> the model seems to see that the older an individual, the
# higher the dependent variable ... makes that sense?

