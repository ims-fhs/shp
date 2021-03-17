#install.packages("kmlShape")
library("kmlShape")

#install.packages("dplyr")
library(dplyr)
library(tidyverse)
#install.packages("tidyr")
library(tidyr)

depression_ohne_na <- depression %>% drop_na() %>% subset(select = -c(ID))

versuch <- kmeans(depression_ohne_na, 4)

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



depression_ohne_na_2 <- depression %>% drop_na()

myClds <- cldsWide(data.frame(depression_ohne_na))

#myClds <- reduceTraj(myClds,nbSenators=50, nbTimes = 4)

myClds <- reduceNbId(trajWide = data.frame(depression_ohne_na), nbSenators = 10, imputationMethod = "linearInterpol")

kmlShape(myClds, nbClusters = 4, timeScale = 0.1, FrechetSumOrMax =
           "max", toPlot="both", parAlgo=parKmlShape())



