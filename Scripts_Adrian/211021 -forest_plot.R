library(tidyverse)
library(plm)
library(stargazer)

### Make Dataframe
id <- sort(rep(c(1:30), times=11))
year <- rep(c(2010:2020), times=30)
df <- data.frame(id, year)
# income
# sample(3000:7000, 20)
df <- df %>% group_by(id) %>% mutate(income = runif(1, 3000,7000)) %>% mutate(income = (income + income*runif(1, 0.005,0.03)*(year-2010)))
df  %>%  ggplot(aes(year, income, group=id, color=as.factor(id))) + geom_line()
df <- df %>% group_by(id) %>% mutate(work_time = round(income/100) + round(runif(11, -4, 4)))
df <- df %>% group_by(id) %>% mutate(sex = round(runif(1, 0, 1)))
df <- df %>% group_by(id) %>% mutate(age = round(runif(1, 20, 40))) %>% mutate(age = age + (year-2010))
df <- df %>% ungroup() %>% mutate(ermuedung = income*age + runif(1,-10000,10000))
df <- df %>% mutate(ermuedung = ((ermuedung - min(ermuedung))/(max(ermuedung) - min(ermuedung))))
df <- df %>% mutate(ermuedung = round(ermuedung*10))


corrplot::corrplot(cor(df[c(3:7)]), method="number")
df %>% ggplot(aes(work_time, income)) + geom_point()

### OLS
ols <- plm(ermuedung ~ income + work_time + age + sex,
            data = df, index = c("id","year"), model="pooling")

stargazer(ols,
          title="Pooled OLS", type="text",
          column.labels=c("all"),
          df=FALSE, digits=4)

label <- row.names(coef(summary(ols)))
mean  <- as.numeric(coef(summary(ols))[,1])
lower <- as.numeric(coef(summary(ols))[,1] + coef(summary(ols))[,2])
upper <- as.numeric(coef(summary(ols))[,1] - coef(summary(ols))[,2])

df_plot <- data.frame(label, mean, lower, upper)
df_plot$label <- factor(df_plot$label, levels=rev(df_plot$label))


ggplot() +
  geom_errorbar(aes(x=mean, y=label, xmin=upper, xmax=lower), width = 0.2) +
  geom_pointrange(data=df_plot, mapping=aes(x=mean, y=label, xmin=upper, xmax=lower), width=0.2, size=1, color="black", fill="grey", shape=22) +
  ggtitle("Forest Plot")


### Normalisierung wichtig um zu visualisieren, sonst wird Verh?ltnis verzerrt
df_norm <- df
pre_proc_val <- caret::preProcess(df_norm, method = c("range"))
df_norm <- predict(pre_proc_val, df_norm)

ols <- plm(ermuedung ~ income + work_time + age + sex,
           data = df_norm, index = c("id","year"), model="pooling")

stargazer(ols,
          title="Pooled OLS", type="text",
          column.labels=c("all"),
          df=FALSE, digits=4)



label <- row.names(coef(summary(ols)))
mean  <- as.numeric(coef(summary(ols))[,1])
lower <- as.numeric(coef(summary(ols))[,1] + coef(summary(ols))[,2])
upper <- as.numeric(coef(summary(ols))[,1] - coef(summary(ols))[,2])

df_plot <- data.frame(label, mean, lower, upper)
df_plot$label <- factor(df_plot$label, levels=rev(df_plot$label))


ggplot(df_plot) +
  geom_pointrange(aes(x=mean, y=label, xmin=upper, xmax=lower)) +
  ggtitle("Forest Plot")


ggplot() +
  geom_pointrange(data=df_plot, mapping=aes(x=mean, y=label, xmin=upper, xmax=lower)) +
  ggtitle("Forest Plot")






