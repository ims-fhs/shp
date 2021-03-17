imsbasics::clc()
library(tidyverse)
library(shp)
library(kml)
library(lubridate)
library(arules)
library(arulesViz)
library(arulesCBA)

source('R/200908-script-schritt1-clustering-wstat.R')

occupied_and_right_age <- occupied_and_right_age %>%
  mutate(year = as.numeric(paste0("20", year)))
depression_explained <- left_join(dep_clustering, occupied_and_right_age) %>%
  rename(cluster = cluster_second_level) %>%
  filter(complete.cases(.))

# Import potentially explaining variables
explaining_variables <- import_cols("SHP08_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W10_2008", cols = c("IDPERS", "SEX08", "EDUCAT08", "P08L11", "P08F50", "P08F08", "P08W77", "P08D29"))
explaining_variables <- explaining_variables %>% rename(ID = IDPERS)

depression_explained <- left_join(depression_explained, explaining_variables)
depression_explained <- depression_explained %>%
  group_by(ID, cluster) %>%
  summarise_all(mean)


depression_explained <- depression_explained %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  filter(cluster != "A")
dep_rf <- depression_explained
dep_rf <- dep_rf %>% group_by(cluster) %>% sample_n(min(table(dep_rf$cluster)))


dep_arules <- dep_rf
dep_arules <- dep_arules %>%
  ungroup() %>%
  mutate(cluster = ifelse(cluster == "C", TRUE, FALSE)) %>%
  select(-ID, -year, -depression, -P08D29) %>%
  mutate(SEX08 = as.logical(dep_arules$SEX08 - 1)) %>%
  mutate(WSTAT = as.logical(dep_arules$WSTAT - 1)) %>%
  mutate(EDUCAT08 = ifelse(EDUCAT08 > 6.5, TRUE, FALSE)) %>%
  mutate(P08L11 = as.logical(dep_arules$P08L11 - 1)) %>%
  mutate(AGE = ifelse(AGE > 49, TRUE, FALSE)) %>%
  mutate(P08W77 = ifelse(P08W77 > 44, TRUE, FALSE)) %>%
  mutate(P08F08 = ifelse(P08W77 > median(P08W77), TRUE, FALSE))

dep_arules <- as.data.frame(sapply(dep_arules, as.logical))

iss.apriori <- arules::apriori(dep_arules, appearance = list(rhs = "P08F50"))
arules::inspect(iss.apriori)

#Diagnose Männlichkeit
iss.apriori <- arules::apriori(iss[iss$male == TRUE, -ncol(iss)], parameter = list(support = 0.0002))
arules::inspect(iss.apriori[1:5,])
plot(iss.apriori[1:5, ], method = "grouped")
plot(iss.apriori[1:5, ], method = "paracoord")
plot(iss.apriori, method = "grouped")
plot(iss.apriori, method = "paracoord")


entry_time <- Sys.time()
prediction <- NULL
for (i in 1:nrow(iss_sample)) {
  print(i)
  classifier <- CBA(impact_on_work~., iss_sample[-i, ])
  prediction[[i]] <- predict(classifier, iss_sample[i, ])
  time <- Sys.time()
  duration_left <- (time-entry_time)/i*((nrow(iss_sample) - i))
  print(paste0("Voraussichtliches Ende: ", time + duration_left))
}

impact <- as.logical(iss_sample$impact_on_work)
prediction <- as.logical(prediction-1)

sum(impact == prediction)/nrow
sum(prediction > impact)/nrow
sum(prediction < impact)/nrow


