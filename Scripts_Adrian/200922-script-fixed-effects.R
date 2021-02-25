imsbasics::clc()
library(tidyverse)
library(shp)
library(kml)
library(kmlShape)
library(lubridate)
library(plm)       # Panel data analysis library
library(car)       # Companion to applied regression
library(gplots)    # Various programing tools for plotting data
library(tseries)   # For timeseries analysis
library(lmtest)    # For hetoroskedasticity analysis
library(lme4)


data <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS",
                         cols = c("IDPERS", "PXXF51", "PXXW229", "PXXC17", "AGEXX", "WSTATXX", "PXXD29"), year_start = "2008", year_end = "2017")

data_complete <- data %>%
  filter(complete.cases(.))


longer <- data_complete
longer <- longer %>%
  pivot_longer(-ID,
               names_to = c('.value', 'year', '.value'),
               names_pattern = '([A-Z]*)(\\d{2})([A-Z]*\\d*)') %>%
  mutate(WSTAT = as.factor(WSTAT))

lme_df <- longer %>%
  mutate(ID = as.factor(ID)) %>%
  mutate(year = as.numeric(year))

lme <- lme4::lmer(PF51 ~ PD29 + WSTAT + (year | ID) + (AGE | ID), lme_df)
lme

paneldata <- pdata.frame(longer, index = c("ID", "year"))

fixed <- plm(PF51 ~PD29+WSTAT, data=paneldata, model="within")
summary(fixed)
fixed <- plm(PC17 ~ (PD29 + WSTAT), data=paneldata, model="within")
summary(fixed)



paneldata <- as.data.frame(paneldata)
ggplot(paneldata) +
  geom_line(aes(x = WSTAT, y = yhat, color = ID), show.legend = FALSE) +
  geom_jitter(aes(x = WSTAT, y = yhat, color = ID), show.legend = FALSE) +
  geom_smooth(aes(x = WSTAT, y = yhat), method = "lm", se = FALSE, show.legend = FALSE)
