imsbasics::clc()
library(tidyverse)
library(shp)
load_path <- paste0("data/rawdata/Data_SPSS/SHP-Data-W1-W20-SPSS")

# depression <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC17"), year_start = "2004", year_end = "2018")
# ermuedung <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYF51"), year_start = "2004", year_end = "2018")
# stress_arbeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW604"), year_start = "2004", year_end = "2018")
# gesundheitliche_erkrankung <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC04A"), year_start = "2004", year_end = "2018")
# arbeit_zufriedenheit_aufgaben <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW229"), year_start = "2004", year_end = "2018")
# art_arbeitszeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW71A"), year_start = "2004", year_end = "2018")
# einbezug_entscheidungen <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW91"), year_start = "2004", year_end = "2018")
# arbeitskontrolle_personen <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW87"), year_start = "2004", year_end = "2018")
# qualifikation_arbeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW100"), year_start = "2004", year_end = "2018")
# arbeitsstunden_woche <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW77"), year_start = "2004", year_end = "2018")
# wochenend_arbeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW218"), year_start = "2004", year_end = "2018")
# nachtarbeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW216"), year_start = "2004", year_end = "2018")
# taegl_pendel_in_min <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW84"), year_start = "2004", year_end = "2018")
# arbeits_intensität <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW603"), year_start = "2004", year_end = "2018")
# zufriedenheit_arbeitsatmosphaere <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW94"),  year_start = "2004", year_end = "2018")
# arbeit_laerm_schmutz <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW605"), year_start = "2004", year_end = "2018")
# arbeit_ermuedende_koerperha <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW606"), year_start = "2004", year_end = "2018")
# hausarbeit_stunden_woche <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYF08"), year_start = "2004", year_end = "2018")
# beeintraechtigung_arbeit_privat <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYF50"), year_start = "2004", year_end = "2018")
# abschalten_nach_arbeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYF52"), year_start = "2004", year_end = "2018")
# einschraenkung_weg_ges_zustand <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC08"), year_start = "2004", year_end = "2018")
# tage_gesunheits_prob <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC11"), year_start = "2004", year_end = "2018")
# chronische_krankheit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC19A"), year_start = "2004", year_end = "2018")
# geschlecht <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","SEXYY"),year_start = "2004", year_end = "2018")
# hoechster_bildungsabschluss <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","EDUCATYY"), year_start = "2004", year_end = "2018")
# partnerschaft <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYD29"), year_start = "2004", year_end = "2018")
# tod_person <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYL11"), year_start = "2004", year_end = "2018")
# person_haushalt<- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","IDHOUSYY"), year_start = "2004", year_end = "2018")
# #schweizerstaatsbürgerschaft <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYD19"), year_start = "2004", year_end = "2018")
# migrationshintergrund <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYD160"),  year_start = "2004", year_end = "2018")
# #pflege_angehoerige <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF74"),  year_start = "2004", year_end = "2012")
# alter <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","AGEYY"),  year_start = "2004", year_end = "2018")
# status <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","STATUSYY"),  year_start = "2004", year_end = "2018")
# beschaeftigung <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","OCCUPAYY"),  year_start = "2004", year_end = "2018")
#
# df_raw_p <- mutate(depression, ermuedung, stress_arbeit, gesundheitliche_erkrankung, arbeit_zufriedenheit_aufgaben, art_arbeitszeit, einbezug_entscheidungen, arbeitskontrolle_personen,
#              qualifikation_arbeit, arbeitsstunden_woche, wochenend_arbeit, nachtarbeit, taegl_pendel_in_min, arbeits_intensität, zufriedenheit_arbeitsatmosphaere,
#              arbeit_laerm_schmutz, arbeit_ermuedende_koerperha, hausarbeit_stunden_woche, beeintraechtigung_arbeit_privat, abschalten_nach_arbeit, einschraenkung_weg_ges_zustand,
#              tage_gesunheits_prob, chronische_krankheit, hoechster_bildungsabschluss, partnerschaft, tod_person, person_haushalt, migrationshintergrund, geschlecht, alter, status,
#              beschaeftigung)
#
# save(df_raw_p, file = "data/df_raw_p.R")
# load("data/df_raw_p.R")
#
# # Zusammen basteln von P und H Datensatz
# load_path <- paste0("data/rawdata/Data_SPSS/SHP-Data-W1-W20-SPSS")
#
# haushaltsaequivalenzeinkommen <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","IYYEQON"), year_start = "2004", year_end = "2018")
# haushaltstyp <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HLDTYPYY"), year_start = "2004", year_end = "2018")
#
# h_datensatz <- mutate(haushaltsaequivalenzeinkommen, haushaltstyp)
#
# h_datensatz_04 <- h_datensatz %>% transmute(ID, I04EQON, HLDTYP04)
# h_datensatz_05 <- h_datensatz %>% transmute(ID, I05EQON, HLDTYP05)
# h_datensatz_06 <- h_datensatz %>% transmute(ID, I06EQON, HLDTYP06)
# h_datensatz_07 <- h_datensatz %>% transmute(ID, I07EQON, HLDTYP07)
# h_datensatz_08 <- h_datensatz %>% transmute(ID, I08EQON, HLDTYP08)
# h_datensatz_09 <- h_datensatz %>% transmute(ID, I09EQON, HLDTYP09)
# h_datensatz_10 <- h_datensatz %>% transmute(ID, I10EQON, HLDTYP10)
# h_datensatz_11 <- h_datensatz %>% transmute(ID, I11EQON, HLDTYP11)
# h_datensatz_12 <- h_datensatz %>% transmute(ID, I12EQON, HLDTYP12)
# h_datensatz_13 <- h_datensatz %>% transmute(ID, I13EQON, HLDTYP13)
# h_datensatz_14 <- h_datensatz %>% transmute(ID, I14EQON, HLDTYP14)
# h_datensatz_15 <- h_datensatz %>% transmute(ID, I15EQON, HLDTYP15)
# h_datensatz_16 <- h_datensatz %>% transmute(ID, I16EQON, HLDTYP16)
# h_datensatz_17 <- h_datensatz %>% transmute(ID, I17EQON, HLDTYP17)
# h_datensatz_18 <- h_datensatz %>% transmute(ID, I18EQON, HLDTYP18)
#
# df <- full_join(df_raw_p, h_datensatz_04, by = c("IDHOUS04" = "ID"))
# df <- left_join(df, h_datensatz_05, by = c("IDHOUS05" = "ID"))
# df <- left_join(df, h_datensatz_06, by = c("IDHOUS06" = "ID"))
# df <- left_join(df, h_datensatz_07, by = c("IDHOUS07" = "ID"))
# df <- left_join(df, h_datensatz_08, by = c("IDHOUS08" = "ID"))
# df <- left_join(df, h_datensatz_09, by = c("IDHOUS09" = "ID"))
# df <- left_join(df, h_datensatz_10, by = c("IDHOUS10" = "ID"))
# df <- left_join(df, h_datensatz_11, by = c("IDHOUS11" = "ID"))
# df <- left_join(df, h_datensatz_12, by = c("IDHOUS12" = "ID"))
# df <- left_join(df, h_datensatz_13, by = c("IDHOUS13" = "ID"))
# df <- left_join(df, h_datensatz_14, by = c("IDHOUS14" = "ID"))
# df <- left_join(df, h_datensatz_15, by = c("IDHOUS15" = "ID"))
# df <- left_join(df, h_datensatz_16, by = c("IDHOUS16" = "ID"))
# df <- left_join(df, h_datensatz_17, by = c("IDHOUS17" = "ID"))
# df <- left_join(df, h_datensatz_18, by = c("IDHOUS18" = "ID"))
#
#
# save(df, file = "data/df.R")
# imsbasics::clc()
# load("data/df.R")
#
#
# df_long <- df %>%
#   pivot_longer(-ID,
#                names_to = c('.value', 'year','.value','.value'),
#                names_pattern = '([A-Z]+)(\\d+)([A-Z_]*)([0-9_]*)')
#
# save(df_long, file = "data/df_long.R")
# imsbasics::clc()
load("data/df_long.R")

sample <- df_long %>%
  drop_na(AGE) %>%
  filter(AGE >= 15) %>%
  filter(AGE <= 65) %>%
  filter(OCCUPA %in% c(1,2,3,5,6))

sample %>% group_by(ID) %>% summarise(n = n())

ggplot(sample %>% group_by(ID) %>% summarise(n = n())) +
  geom_bar(aes(x = n))
