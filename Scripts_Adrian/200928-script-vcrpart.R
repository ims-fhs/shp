imsbasics::clc()
library(tidyverse)
library(shp)
library(vcrpart)

data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS",
                         cols = c("IDPERS", "PXXC17", "PXXF51", "PXXW71A", "PXXW77", "PXXW87", "PXXW91", "PXXW94", "PXXW100", "PXXW216", "PXXW229", "PXXW603", "PXXW604", "PXXW605", "PXXW606", "PXXD29", "PXXL11", "PXXF50", "PXXF52", "PXXC08", "PXXC11", "PXXC19A", "PXXF08"), year_start = "2008", year_end = "2017")

data_complete <- data
# data_complete <- data %>%
# filter(complete.cases(.))


longer <- data_complete %>%
  pivot_longer(-ID,
               names_to = c('.value', 'year', '.value'),
               names_pattern = '([A-Z]*)(\\d{2})([A-Z]*\\d*)')

paneldata <- pdata.frame(longer, index = c("ID", "year"))

ermuedung <- plm(PF51 ~ PD29 + PW229 + PW71 + PW77 + PW87 + PW91 + PW94 + PW100 + PW216 + PW603 + PW604 + PW605 + PW606 + PL11 + PF08 + PF50 + PF52 + PC08 + PC11 + PC19, data=paneldata, model="within")
summary(ermuedung)
depression <- plm(PC17 ~ PD29 + PW229 + PW71 + PW77 + PW87 + PW91 + PW94 + PW100 + PW216 + PW603 + PW604 + PW605 + PW606 + PL11 + PF08 + PF50 + PF52 + PC08 + PC11 + PC19, data=paneldata, model="within")
summary(depression)

beepr::beep()

data1 <- longer
data1 <- data1[1:100, ]
data1$PC17 <- .1 * data1$PC17

tvcglm(PC17 ~ PD29 + PW229 + PW71 + PW77 + PW87 + PW91 + PW94 + PW100 + PW216 + PW603 + PW604 + PW605 + PW606 + PL11 + PF08 + PF50 + PF52 + PC08 + PC11 + PC19, data=data1, family = binomial())

data1$PF51 <- .1 * data1$PF51
tvcglm(PF51 ~ PD29 + PW229 + PW71 + PW77 + PW87 + PW91 + PW94 + PW100 + PW216 + PW603 + PW604 + PW605 + PW606 + PL11 + PF08 + PF50 + PF52 + PC08 + PC11 + PC19, data=data1, family = binomial())

