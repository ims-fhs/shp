library(tidyverse)

load("data/df_long.R")

colnames(df_long) <- c("id","year","depression","ermuedung","stress_arbeit","rueckenschmerzen","arbeit_zufriedenheit_aufgaben","art_arbeitszeit","arbeit_einbezug_entscheidungen","arbeitskontrolle_personen","arbeit_qualifikation","arbeitsstunden_woche","wochenend_arbeit","nachtarbeit","taegl_pendeln_min","arbeit_intensitaet","zufriedenheit_arbeitsatmosphaere","arbeit_laerm_schmutz","arbeit_ermuedende_koerperha","hausarbeit_stunden_woche","beeintraechtigung_arbeit_privat","abschalten_nach_arbeit","einschraenkung_weg_ges_zustand","tage_gesunheits_prob","chronische_krankheit","ausbildung","partnerschaft","tod_person","person_haushalt","migrationshintergrund","geschlecht","alter","status","occupa","haushaltsaequivalenzeinkommen","kinder_betreuung")


sample <- df_long %>%
  drop_na(alter) %>%
  filter(alter >= 15) %>%
  filter(alter <= 65) %>%
  filter(occupa %in% c(1,2,3,5,6))

sample <- mutate_all(sample, function(x) as.numeric(as.character(x)))

colnames(sample)

# 60672 rows / 36 columns
head(sample)

# Anzahl Personen im df = 14081
length(unique(df_long$id))
# Anzahl Personen im sample df = 8731
length(unique(sample$id))

# NA per column
map(sample, ~sum(is.na(.)))
# NA per column in percent
map(sample, ~sum(is.na(.))/nrow(sample))

# Boxplot per variable
scaled_variables <- as.data.frame(sample[3:36])
scaled_variables %>%
  pivot_longer(-person_haushalt, names_to = "variablen", values_to = "daten") %>%
  ggplot() +
  geom_boxplot(aes(y = daten)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~variablen, scales = "free")



ggplot(sample, aes(x =  , y=note, fill=treatment)) +
  geom_boxplot() +
  facet_wrap(~variety, scale="free")

# Barplot per variable
sample %>%
  pivot_longer(3:11, names_to = "variablen", values_to = "daten") %>%
  ggplot(aes(x = daten)) +
  geom_bar() +
  facet_wrap(vars(variablen), ncol = 3) +
  labs(x = "Daten", y = "Anzahl Daten")


# Correlation plot of erm?dung and depression variables
chart.Correlation(sample[c(3,4)], histogram=TRUE, pch=19)

# Correlation plot of possible interesting variables
sample_1000 = sample %>% filter(sample$id <= unique(sample$id)[1000]) # k?rzen wegen rechenleistung
chart.Correlation(sample_1000[c(3,4,12,16,21,22,23,24)], histogram=TRUE, pch=19)


df_long %>% group_by(id) %>% summarise(n = n())



ggplot(sample %>% group_by(id) %>% summarise(n = n())) +
  geom_bar(aes(x = n))

