imsbasics::clc()
library(tidyverse)
load("data/df_long.R")
colnames(df_long) <- c("id","year","depression","ermuedung","stress_arbeit","rueckenschmerzen","arbeit_zufriedenheit_aufgaben","art_arbeitszeit","arbeit_einbezug_entscheidungen","arbeitskontrolle_personen","arbeit_qualifikation","arbeitsstunden_woche","wochenend_arbeit","nachtarbeit","taegl_pendeln_min","arbeit_intensitaet","zufriedenheit_arbeitsatmosphaere","arbeit_laerm_schmutz","arbeit_ermuedende_koerperha","hausarbeit_stunden_woche","beeintraechtigung_arbeit_privat","abschalten_nach_arbeit","einschraenkung_weg_ges_zustand","tage_gesunheits_prob","chronische_krankheit","ausbildung","partnerschaft","tod_person","person_haushalt","migrationshintergrund","geschlecht","alter","status","occupa","ch_nationalitaet","haushaltsaequivalenzeinkommen","kinder_betreuung")


sample <- df_long %>%
  drop_na(alter) %>%
  filter(alter >= 15) %>%
  filter(alter <= 65) %>%
  filter(occupa %in% c(1,2,3,5,6))

sample <- mutate_all(sample, function(x) as.numeric(as.character(x)))
rm(df_long)

sample <- sample %>% select(id)


#
# # 60672 rows / 37 columns
# head(sample)
#
# # Anzahl Personen im df = 14081
# length(unique(df_long$id))
# # Anzahl Personen im sample df = 8731
# length(unique(sample$id))
#
# # NA per column
# map(sample, ~sum(is.na(.)))
# # NA per column in percent
# map(sample, ~sum(is.na(.))/nrow(sample))
#
# # Boxplot per variable
# as.data.frame(sample[3:ncol(sample)]) %>%
#   pivot_longer(-person_haushalt, names_to = "variablen", values_to = "daten") %>%
#   ggplot() +
#   geom_boxplot(aes(y = daten)) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   facet_wrap(~variablen, scales = "free")
#
#
# # Barplot per variable
# as.data.frame(sample[3:ncol(sample)]) %>%
#   pivot_longer(-person_haushalt, names_to = "variablen", values_to = "daten") %>%
#   ggplot() +
#   geom_bar(aes(y = daten)) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   facet_wrap(~variablen, scales = "free")
#
#
# ggplot(sample %>% group_by(id) %>% summarise(n = n())) +
#   geom_bar(aes(x = n))
#
#
#
#
# # library(kml)
# # kml <- sample %>% select(ID, year, PC17)
# # kml <- kml %>% pivot_wider(names_from = year, values_from = PC17)
# #
# #
# # sample_ld <- clusterLongData(as.data.frame(kml))
# # kml(sample_ld, 3,10,toPlot = 'both')
# # choice(clustered)
# #
# # kml(wlb_complete_ld, 6,1,toPlot = 'both')
