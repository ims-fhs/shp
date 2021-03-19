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

versuch <- sample %>% select(c(ID, PC17, year)) %>%
  pivot_wider(names_from = year, values_from = PC17) %>% drop_na() %>% subset(select = -c(ID))

versuch <- kmeans(versuch, 4)
fuer_plot <- data.frame(versuch$center)
fuer_plot <- t(fuer_plot)
Jahr = data.frame(year=c(2004:2018))
fuer_plot <- fuer_plot %>% cbind(year = Jahr)
names(fuer_plot)[names(fuer_plot) == '1'] <- 'line_1'
names(fuer_plot)[names(fuer_plot) == '2'] <- 'line_2'
names(fuer_plot)[names(fuer_plot) == '3'] <- 'line_3'
names(fuer_plot)[names(fuer_plot) == '4'] <- 'line_4'

ggplot(fuer_plot, aes(x = year)) +
  geom_line(aes(y = line_1), color = "darkred") +
  geom_line(aes(y = line_2), color="steelblue") +
  geom_line(aes(y = line_3), color="yellow") +
  geom_line(aes(y = line_4), color="green")



#install.packages("kmlShape")
library("kmlShape")

#install.packages("dplyr")
library(dplyr)
library(tidyverse)
#install.packages("tidyr")
library(tidyr)



versuch2 <- sample %>% select(c(ID, PC17, year)) %>%
  pivot_wider(names_from = year, values_from = PC17) %>% drop_na()

myClds <- cldsWide(data.frame(versuch2))

myClds <- reduceNbId(trajWide = data.frame(fuer_plot), nbSenators = 10, imputationMethod = "linearInterpol")

kmlShape(myClds, nbClusters = 4, timeScale = 0.1, FrechetSumOrMax =
           "max", toPlot="both", parAlgo=parKmlShape())












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

write_xlsx(x=haushaltstyp, path = "data/means.xlsx", col_names=TRUE, format_headers = TRUE)








