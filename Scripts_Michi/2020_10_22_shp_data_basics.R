## ---------------------------
## Script name: shp data basics
##
## Purpose of script: I want to learn about the shp-dataset... especially about variables,
## their availability in different waves and what their abbreviations mean.
##
## Author: Michael Schmid
##
## Date Created: 2020-10-22
##
## Copyright (c) Michael Schmid, 2020
## Email: michi.smith@bluewin.ch
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

imsbasics::clc()
library(tidyverse)
library(shp)

## -------------------------- get all variables in all waves --------------------------
# For each wave, I extract the column names, I neglect the year_string (99, 00, 01, ... 18)
# and try to put a data.frame together containing an overview of available variables per wave
varlist <- list()
for (i in seq(1,20)) {
  year_string <- as.character(sprintf('%02d', (1998+i) %% 100))
  file <- paste0("SHP", year_string ,"_P_USER.sav")
  path <- paste0("data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W",i,"_",1998+i)
  wave <- import_SPSS_file(file = file, path = path)
  # Extract variables of the wave with corresponding label -> paste them together
  vars <- attr(wave, which = "variable.labels")
  vars <- paste0(names(vars),"---", vars)
  # it's not perfect to just neglect the year strings (99, 00, 01, ... 18) but it will do for this overview
  vars <- gsub(pattern = year_string, replacement = "" , x = vars)
  varlist[[i]] <- vars
}

# create empty data.frame "available variables per wave"
all_vars <- sort(unique(unlist(varlist, use.names = FALSE)))
df <- data.frame(variable = as.character(all_vars), stringsAsFactors = FALSE)
cols <- c("W1_1999", "W2_2000", "W3_2001", "W4_2002", "W5_2003", "W6_2004",
          "W7_2005", "W8_2006", "W9_2007", "W10_2008", "W11_2009", "W12_2010",
          "W13_2011", "W14_2012", "W15_2013", "W16_2014", "W17_2015", "W18_2016",
          "W19_2017", "W20_2018")
df[,cols] <- NA

# fill the dataframe
for (i in seq(1,20)) {
  df[,i+1] <- ifelse(df$variable %in% varlist[[i]], TRUE, NA)
}

# count the availability of variables over all waves
df$sum <- rowSums(df[,-1], na.rm = TRUE)
df <- df[,c(1,22,2:21)]
# set NA to ""
df[is.na(df)] <- ""

xlsx::write.xlsx(df, file =  "data/variable_availvability_per_wave.xlsx",
                 sheetName = "All Variables", row.names = FALSE)

for (i in seq(1,20)) {
  print(i)
  partial_df <- df[df$sum >= i, ]
  xlsx::write.xlsx(partial_df, file =  "data/variable_availvability_per_wave.xlsx",
                   sheetName = paste0(">=",i), row.names = FALSE, append = TRUE)
}








