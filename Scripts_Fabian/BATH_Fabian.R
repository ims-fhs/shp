library(tidyverse)
library(shp)
load_path <- paste0("data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS")

load("data/df_long.R")

sample <- df_long %>%
  drop_na(AGE) %>%
  filter(AGE >= 15) %>%
  filter(AGE <= 65) %>%
  filter(OCCUPA %in% c(1,2,3,5,6))

sample %>% group_by(ID) %>% summarise(n = n())

ggplot(sample %>% group_by(ID) %>% summarise(n = n())) +
  geom_bar(aes(x = n))

versuch <- kmeans(sample %>% select(c(ID, PC17, year)) %>% drop_na(), 4)

fuer_plot <- data.frame(versuch$center)






#install.packages("writexl")
library(writexl)


#install.packages('Rcmdr')
library('Rcmdr')

name_col <- colnames(haushaltstyp)
name_col
dt_na <- df %>% select(name_col) %>% subset(select = -c(ID)) %>% sapply(function(x) sum(is.na(x)))
dt_na <- t(dt_na)
dt_na <- as.data.frame(dt_na)
dt_na
#dt <- name_col %>% subset(select = -c(ID)) %>% sapply(function(x) summary(x))
dt <- df %>% select(name_col) %>% subset(select = -c(ID)) %>% sapply(function(x) table(factor(x, levels = 0:10)))
dt <- as.data.frame(dt)
dt
#dt_perc_ohne_na <- dt %>% colPercents(digits=2)
#dt_perc_ohne_na
#dt_perc_ohne_na <- as.data.frame(dt_perc_ohne_na)
dt_mit_na <- rbind(dt, dt_na)
dt_mit_na <- as.data.frame(dt_mit_na)
dt_mit_na
#dt_perc_mit_na <- dt_mit_na %>% colPercents(digits=2)
#dt_perc_mit_na
#dt_perc_mit_na <- as.data.frame(dt_perc_mit_na)
Means=rowMeans(dt_mit_na)
Means <- t(Means)
Means <- as.data.frame(Means)
Means
#liste <- list(dt_mit_na, dt)

write_xlsx(x=haushaltstyp, path = "means.xlsx", col_names=TRUE, format_headers = TRUE)








