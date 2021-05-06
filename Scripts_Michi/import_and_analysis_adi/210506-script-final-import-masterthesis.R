# … "Pflege Dritter"-> not ok, and "Kinderbetreuung"-> ok, by using different P- and H-variables. See details below and comments in code:
#
#   The new (important) code parts are NOT commented out in 2021_03_17-script-import-for-paper-1.R.
# Variables marked with *...* below have actually been used for incorporation.
#
# Person:
#   - "Staatsbürgerschaft" (PYYD19, PYYD21, PYYD23, *NAT_1_YY*, *NAT_2_YY*, *NAT_3_YY*)
# --> ok (a person is swiss, if one of the 3 nationalities is Swiss)
#
# Haushalt:
#   - "Unentgeldliche Pflege von Personen ausserhalb des eigenen Haushaltes" (HYYF64, HYYF721, HYYF722, HYYF723, HYYF724, HYYF725)
# --> not ok (all variables have no entry in 2013).
#
# - "Kinderbetreuung Ja/Nein"? (*HLDFFSYY*, HYYF53, HYYF54, HYYF55, HYYF56) & HLDTYPYY
# - "Alleinerziehend Ja/Nein"? (*HLDFFSYY*, HYYF53, HYYF54, HYYF55, HYYF56) & HLDTYPYY
# --> ok (mapping (21,22,23,41,42,43,44) -> 0 / (11,12,13) -> 1 / (31,32,33) -> 2)
# --> maybe more details possible with incorporation of HLDTYPYY
#
# imsbasics::clc()
# library(tidyverse)
# # library(shp)
# source("R/import.RData")
# load_path <- paste0("data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS")
# ys <- "2004"; ye <- "2019"
#
# ## ----------------------- P - Import easy variables ---------------------------
# ## Beanspruchungsfolgen
# depression <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC17"), year_start = ys, year_end = ye) # Niedergeschlagenheit, Hoffnungslosigkeit, Angst, Depression: Häufigkeit
# ## Bedingungen/Belastungen: Arbeit
# arbeit_zeit_wochenstunden <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW77"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Anzahl Arbeitsstunden pro Woche
# arbeit_zeit_wochenstunden_vereinbart <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW74"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Anzahl Arbeitsstunden pro Woche vereinbart
# arbeit_zeit_wochenende <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW218"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Wochenendearbeit
# arbeit_zeit_nacht <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW216"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Nachtarbeit
# arbeit_zeit_art <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW71A"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Art der Arbeitszeit
# arbeit_qualifikation <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW100"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Qualifikation für Arbeit
# arbeit_intensitaet <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW603"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Arbeitsrythmus: Intensität
# arbeit_einbezug_entscheidungen <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW91"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Einbezug bei Entscheidungen/Meinungen
# arbeit_zufriedenheit_atmosphaere <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW94"),  year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Zufriedenheit: Arbeitsatmosphäre
# ## Bedingungen/Belastungen: Care-Sphäre
# hausarbeit_wochenstunden <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYF08"), year_start = ys, year_end = ye) # Hausarbeit: Stunden pro Woche
# beeintraechtigung_arbeit_privat <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYF50"), year_start = ys, year_end = ye) # Beeinträchtigung Arbeit <-> private Aktivitäten /Familie
# abschalten_nach_arbeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYF52"), year_start = ys, year_end = ye) # Schwierigkeit nach Arbeit abzuschalten
# ## Bedingungen/Belastungen: Lebenslage & Ressourcen
# einschraenkung_weg_ges_zustand <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC08"), year_start = ys, year_end = ye) # Einschränkungen wegen Gesundheitszustand bei täglichen Aktivitäten: Ausmass
# tage_gesundheits_prob <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC11"), year_start = ys, year_end = ye) # Anzahl Tage die von Gesundheitsproblemen betroffen waren: Letzte 12 Monate
# chronische_krankheit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC19A"), year_start = ys, year_end = ye) # Chronische Krankheit oder längerfristiges gesundheitliches Problem
# geschlecht <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","SEXYY"),year_start = ys, year_end = ye) # Geschlecht
# hoechster_bildungsabschluss <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","EDUCATYY"), year_start = ys, year_end = ye) # Höchster Bildungsabschluss
# partnerschaft <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYD29"), year_start = ys, year_end = ye) # Partnerschaft
# tod_person <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYL11"), year_start = ys, year_end = ye) # Tod einer nahestehenden Person
# ## Allgemeine Informationen zu personen
# person_haushalt<- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","IDHOUSYY"), year_start = ys, year_end = ye) # Identifikationsnummer des Haushaltes
# #schweizerstaatsbürgerschaft <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYD19"), year_start = ys, year_end = ye) # ??
# migrationshintergrund <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYD160"),  year_start = ys, year_end = ye) # In der Schweiz geboren
# #pflege_angehoerige <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF74"),  year_start = ys, year_end = ye) # ??
# alter <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","AGEYY"),  year_start = ys, year_end = ye) # Alter im Jahr der Befragung
# status <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","STATUSYY"),  year_start = ys, year_end = ye) # Art der Datensammlung: raster, proxy, persönlich  (-1 "weiss nicht" 0 "individueller Fragebogen" 1 "proxy Fragebogen" 2 "nur Haushaltsraster" 3 "kinderbetreuung" 4 "Biographischer Fragebogen ")
# beschaeftigung <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","OCCUPAYY"),  year_start = ys, year_end = ye) # Aktuelle Beschäftigung
#
#
# ## --------------------- P - Create constructed variables ----------------------
# ## Manually constructed P-variables 1
# ##  - Staatsbürgerschaft (PYYD19, PYYD21, PYYD23)
# ##  ----> No variable is ever available...?
# ## - Staatsbürgerschaft (NAT_1_YY, NAT_2_YY, NAT_3_YY)
# ## ----> All variables available
# nationalitaet_1 <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","NAT_1_YY"), year_start = ys, year_end = ye) # "Erste Nationalität".
# nationalitaet_2 <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","NAT_2_YY"), year_start = ys, year_end = ye) # "Zweite Nationalität".
# nationalitaet_3 <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","NAT_3_YY"), year_start = ys, year_end = ye) # "Dritte Nationalität".
# nationalitaet <- mutate(nationalitaet_1, nationalitaet_2, nationalitaet_3)
# rm(nationalitaet_1); rm(nationalitaet_2); rm(nationalitaet_3)
#
# nationalitaet_long <- nationalitaet %>%
#   pivot_longer(-ID,
#                names_to = c("nat", "year"),
#                names_pattern = "NAT_(.)_(.*)",
#                values_to = "country")
# ch_nationalitaet <- nationalitaet_long %>%
#   group_by(ID, year) %>%
#   summarise(ch_nationalitaet = ifelse(test = all(is.na(country)),
#                                              yes = NA, # If no answer was given (3 x NA), we also get FALSE !!! - acceptable?
#                                              no = as.numeric(any(country == 8100, na.rm = TRUE)))) %>% # else we check if any answer was "Switzerland"
#   pivot_wider(names_from = year,
#               values_from = ch_nationalitaet,
#               names_glue = "P{year}CH")
#
# ## --------------------------- P - Merge P-variables ---------------------------
#
# df_raw_p <- mutate(depression,
#                    arbeit_zeit_wochenstunden,
#                    arbeit_zeit_wochenstunden_vereinbart,
#                    arbeit_zeit_wochenende,
#                    arbeit_zeit_nacht,
#                    arbeit_zeit_art,
#                    arbeit_qualifikation,
#                    arbeit_intensitaet,
#                    arbeit_einbezug_entscheidungen,
#                    arbeit_zufriedenheit_atmosphaere,
#                    hausarbeit_wochenstunden,
#                    beeintraechtigung_arbeit_privat,
#                    abschalten_nach_arbeit,
#                    einschraenkung_weg_ges_zustand,
#                    tage_gesundheits_prob,
#                    chronische_krankheit,
#                    hoechster_bildungsabschluss,
#                    partnerschaft,
#                    tod_person,
#                    person_haushalt,
#                    migrationshintergrund,
#                    geschlecht,
#                    alter,
#                    status,
#                    beschaeftigung,
#                    ch_nationalitaet)
#
# save(df_raw_p, file = "data/df_raw_p.RData")
# load("data/df_raw_p.RData")
# assertthat::assert_that(ncol(df_raw_p) == 417)
# assertthat::assert_that(nrow(df_raw_p) == 14081)
#
# # ---------------/-------------------------------------------/-----------------
#
# ## --------------------------- H - Import easy variables -----------------------
#
# load_path <- paste0("data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS")
# haushaltsaequivalenzeinkommen <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","IYYEQON"), year_start = ys, year_end = ye) # Jährliches Haushaltseinkommen (Aequivalenz), OECD, netto
#
#
# # --------------------- H - Create constructed variables ----------------------
# # Manually constructed H-variables 1
# #  - Unentgeldliche Pflege von Personen ausserhalb des eigenen Haushaltes (using HYYF64, HYYF721, HYYF722, HYYF723, HYYF724, HYYF725)
# #  ----> They are never avaliable in year 2013 (and partially not available in 2004)
# #
# betreuung_ausserhalb <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF64"), year_start = 2004, year_end = 2012) # "Betreuung ausserhalb der Familie: Ja, nein".
# betreuung_ausserhalb_2 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF64"), year_start = 2014, year_end = 2019) # "Betreuung ausserhalb der Familie: Ja, nein".
# betreuung_ausserhalb <- left_join(betreuung_ausserhalb, betreuung_ausserhalb_2); rm(betreuung_ausserhalb_2)
#
# betreuung_ausserhalb_wer1 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF721"), year_start = 2004, year_end = 2012) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer1_2 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF721"), year_start = 2014, year_end = 2019) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer1 <- left_join(betreuung_ausserhalb_wer1, betreuung_ausserhalb_wer1_2); rm(betreuung_ausserhalb_wer1_2)
#
# betreuung_ausserhalb_wer2 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF722"), year_start = 2004, year_end = 2012) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer2_2 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF722"), year_start = 2014, year_end = 2019) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer2 <- left_join(betreuung_ausserhalb_wer2, betreuung_ausserhalb_wer2_2); rm(betreuung_ausserhalb_wer2_2)
#
# betreuung_ausserhalb_wer3 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF723"), year_start = 2004, year_end = 2012) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer3_2 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF723"), year_start = 2014, year_end = 2019) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer3 <- left_join(betreuung_ausserhalb_wer3, betreuung_ausserhalb_wer3_2); rm(betreuung_ausserhalb_wer3_2)
#
# betreuung_ausserhalb_wer4 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF724"), year_start = 2005, year_end = 2012) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer4_2 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF724"), year_start = 2014, year_end = 2019) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer4 <- left_join(betreuung_ausserhalb_wer4, betreuung_ausserhalb_wer4_2); rm(betreuung_ausserhalb_wer4_2)
#
# betreuung_ausserhalb_wer5 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF725"), year_start = 2005, year_end = 2012) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer5_2 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF725"), year_start = 2014, year_end = 2019) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer5 <- left_join(betreuung_ausserhalb_wer5, betreuung_ausserhalb_wer5_2); rm(betreuung_ausserhalb_wer5_2)
#
# # Manually constructed H-variables 2
# #  - Kinderbetreuung Ja/Nein (HLDFFSYY, HYYF53, HYYF54, HYYF55, HYYF56) or HLDTYPYY ?
# #  - Alleinerziehend Ja/Nein (HLDFFSYY, HYYF53, HYYF54, HYYF55, HYYF56) or HLDTYPYY ?
# #  ----> All Variables are available
# #
# #  HLDFFSYY = Haushaltstyp der Familie und Mikrozensus Familie
# #      -1 "weiss nicht"
# #      11 "Nicht verheiratetes Paar mit Kindern"
# #      12 "Ehepaar mit Kindern"
# #      13 "Nicht mehr verheiratetes Ehepaar mit Kindern"
# #      21 "Nicht verheiratetes Paar ohne Kinder"
# #      22 "Ehepaar ohne Kinder"
# #      23 "Nicht mehr verheiratetes Paar ohne Kinder"
# #      31 "Alleinerziehender, nie verheiratet gewesener Elternteil mit Kindern"
# #      32 "Verheirateter Elternteil mit Kindern"
# #      33 "Nicht mehr verheirateter Elternteil mit Kindern"
# #      41 "Nie verheiratet gewesene, allein lebende Person"
# #      42 "Verheiratete allein lebende Person"
# #      43 "Nicht mehr verheiratete, allein lebende Person"
# #      44 "Andere Situation".
# #
# #  HYYF53, HYYF54, HYYF55, HYYF56 = Kinderbetreuung Krankheit, Spielen, Transport, Hausaufgaben
# #      -1 "weiss nicht"
# #      1 "Meistens ich"
# #      2 "Meistens mein Partner/meine Partnerin"
# #      3 "In der Regel beide ungefähr gleich"
# #      4 "Ein anderes Kind oder andere Kinder von unserem Haushalt"
# #      5 "Meistens sonst jemand in unserem Haushalt"
# #      6 "Meistens die Haushaltshilfe"
# #      7 "Meistens Vater/Mutter der Kinder (nicht im Haushalt lebend)"
# #      8 "Vater/Mutter (der/die nicht im Haushalt lebt) und ich"
# #      9 "Meistens sonst jemand, der nicht zum Haushalt gehört"
# #      10 "andere Lösung"
# #      11 "gegenstandslos".
# #
# # HLDTYPYY = Haushaltstyp (PACO)
# #      -1 "weiss nicht"
# #      1 "eine Person 65 Jahre oder älter"
# #      2 "eine Person 30 Jahre alt oder älter und jünger als 65 Jahre"
# #      3 "eine Person jünger als 30 Jahren"
# #      4 "alleinerziehende Person mit  Kind/ern 16 Jahre alt oder jünger"
# #      5 "alleinerzeihende Person mit mind einem Kind älter als 16"
# #      6 "Paar ohne Kinder mit zumindest jemand >=  65 Jahre alt"
# #      7 "Paar ohne Kinder mit beide jünger als 65 Jahre"
# #      8 "Paar mit einem Kind"
# #      9 "Paar mit zwei Kindern"
# #      10 "Paar mit drei Kindern und mehr"
# #      11 "Paar mit zumindest einem Kind über 16 Jahren"
# #      12 "andere Haushalte, alle Mitglieder verwandt"
# #      13 "andere Haushalte, nicht alle Mitglieder verwandt".
# #
# # Vorschlag Myriel/Adi: (mittels HLDFFSYY) -> aktuell implementiert
# #    - keine Kinderbetreuung = 21,22,23,41,42,43,44
# #    - Geteilte Kinderbetreuung = 11,12,13
# #    - Alleinerziehend = 31,32,33
# #
# # Vorschlag Michi:
# #    - HYYF53, HYYF54, HYYF55, HYYF56 sicher ungünstig, da sie sich auf ein "ich" beziehen (obowhl H-Daten) -> ?
# #    - ev. detailliertere Aufsplittung von Myriels/Adi's Vorschlag mittels HLDTYPYY (Anzahl Kindern & Alter)
# #      wäre möglich, da HLDFFSYY & HLDTYPYY extrem gute Übereinstimmung bezgl. Einträgen zeigen
# #          -> sum(is.na(haushaltstyp_mikrozensus) == is.na(haushaltstyp_PACO)) = 95534
# #          -> sum(is.na(haushaltstyp_mikrozensus) != is.na(haushaltstyp_PACO)) = 23
# haushaltstyp_mikrozensus <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HLDFFSYY"), year_start = ys, year_end = ye) # "Haushaltstyp der Familie und Mikrozensus Familie"
# kinderbetreuung_krankheit <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF53"), year_start = ys, year_end = ye) # "Kinderbetreuung: Im Krankheitsfall"
# kinderbetreuung_spielen <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF54"), year_start = ys, year_end = ye) # "Kinderbetreuung: Mit ihnen spielen"
# kinderbetreuung_transport <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF55"), year_start = ys, year_end = ye) # "Kinderbetreuung:  In Kindergarten, Schule bringen"
# kinderbetreuung_hausaufgaben <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF56"), year_start = ys, year_end = ye) # "Kinderbetreuungt: Bei den Hausaufgaben helfen"
# haushaltstyp_PACO <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HLDTYPYY"), year_start = ys, year_end = ye) # "Haushaltstyp (PACO)"
#
# # Vorschlag Myriel/Adi:
# kinderbetreuung_long <- haushaltstyp_mikrozensus %>%
#   pivot_longer(-ID,
#                names_to = c("year"),
#                names_pattern = "HLDFFS(.*)")
#
# kinderbetreuung <- kinderbetreuung_long %>%
#   mutate(value = ifelse(test = value %in% c(21,22,23,41,42,43,44), yes = 0, no = value), # 0 = keine Kinderbetreuung
#          value = ifelse(test = value %in% c(11,12,13), yes = 1, no = value), # 1 = Geteilte Kinderbetreuung
#          value = ifelse(test = value %in% c(31,32,33), yes = 2, no = value)) # 2 = Alleinerziehend
#
# # par(mfrow = c(2,1)); hist(kinderbetreuung_long$value); hist(kinderbetreuung$value); par(mfrow = c(1,1))
#
# kinderbetreuung <- kinderbetreuung %>%
#   pivot_wider(names_from = year,
#               values_from = value,
#               names_glue = "HLDFFS{year}")
#
#
# ## --------------------------- H - Merge H-variables ---------------------------
# h_datensatz <- left_join(haushaltsaequivalenzeinkommen, kinderbetreuung) %>%
#   left_join(betreuung_ausserhalb) %>%
#   left_join(betreuung_ausserhalb_wer1) %>%
#   left_join(betreuung_ausserhalb_wer2) %>%
#   left_join(betreuung_ausserhalb_wer3) %>%
#   left_join(betreuung_ausserhalb_wer4) %>%
#   left_join(betreuung_ausserhalb_wer5)
#
#
# ## ---------------/-------------------------------------------/-----------------
# ## ----------------- P&H - Merge P & H variables -> df & df_long ---------------
# ## Zusammen basteln von P und H Datensatz
# ## Kommentar Michi: IDHOUS04-IDHOUS19 in df_raw_p hat viele lücken --> Imputation im
# ## Fall   x,x,x,NA,NA,NA,X,X,X   könnte sinnvoll sein.
#
# h_datensatz_04 <- h_datensatz %>% transmute(ID, I04EQON, HLDFFS04, H04F64, H04F721, H04F722, H04F723)
# h_datensatz_05 <- h_datensatz %>% transmute(ID, I05EQON, HLDFFS05, H05F64, H05F721, H05F722, H05F723, H05F724, H05F725)
# h_datensatz_06 <- h_datensatz %>% transmute(ID, I06EQON, HLDFFS06, H06F64, H06F721, H06F722, H06F723, H06F724, H06F725)
# h_datensatz_07 <- h_datensatz %>% transmute(ID, I07EQON, HLDFFS07, H07F64, H07F721, H07F722, H07F723, H07F724, H07F725)
# h_datensatz_08 <- h_datensatz %>% transmute(ID, I08EQON, HLDFFS08, H08F64, H08F721, H08F722, H08F723, H08F724, H08F725)
# h_datensatz_09 <- h_datensatz %>% transmute(ID, I09EQON, HLDFFS09, H09F64, H09F721, H09F722, H09F723, H09F724, H09F725)
# h_datensatz_10 <- h_datensatz %>% transmute(ID, I10EQON, HLDFFS10, H10F64, H10F721, H10F722, H10F723, H10F724, H10F725)
# h_datensatz_11 <- h_datensatz %>% transmute(ID, I11EQON, HLDFFS11, H11F64, H11F721, H11F722, H11F723, H11F724, H11F725)
# h_datensatz_12 <- h_datensatz %>% transmute(ID, I12EQON, HLDFFS12, H12F64, H12F721, H12F722, H12F723, H12F724, H12F725)
# h_datensatz_13 <- h_datensatz %>% transmute(ID, I13EQON, HLDFFS13)
# h_datensatz_14 <- h_datensatz %>% transmute(ID, I14EQON, HLDFFS14, H14F64, H14F721, H14F722, H14F723, H14F724, H14F725)
# h_datensatz_15 <- h_datensatz %>% transmute(ID, I15EQON, HLDFFS15, H15F64, H15F721, H15F722, H15F723, H15F724, H15F725)
# h_datensatz_16 <- h_datensatz %>% transmute(ID, I16EQON, HLDFFS16, H16F64, H16F721, H16F722, H16F723, H16F724, H16F725)
# h_datensatz_17 <- h_datensatz %>% transmute(ID, I17EQON, HLDFFS17, H17F64, H17F721, H17F722, H17F723, H17F724, H17F725)
# h_datensatz_18 <- h_datensatz %>% transmute(ID, I18EQON, HLDFFS18, H18F64, H18F721, H18F722, H18F723, H18F724, H18F725)
# h_datensatz_19 <- h_datensatz %>% transmute(ID, I19EQON, HLDFFS19, H19F64, H19F721, H19F722, H19F723, H19F724, H19F725)
#
# # Add haushaltsaequivalenzeinkommen & haushaltstyp for every year.
# df <- left_join(df_raw_p, h_datensatz_04, by = c("IDHOUS04" = "ID"))
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
# df <- left_join(df, h_datensatz_19, by = c("IDHOUS19" = "ID"))
#
# save(df, file = "data/df.RData")
#
# imsbasics::clc()
# load("data/df.RData")
#
# df_long <- df %>%
#   pivot_longer(-ID,
#                names_to = c('.value', 'year','.value','.value'),
#                names_pattern = '([A-Z]+)(\\d+)([A-Z_]*)([0-9_]*)')
#
# save(df_long, file = "data/df_long.RData")



# ## ---------------/-------------------------------------------/---------------
# ## ---------------------------- Operationalisierung --------------------------
imsbasics::clc()
library(tidyverse)
library(shp)
imsbasics::create_log("log_final_import_masterthesis.txt", "Scripts_Michi/import_and_analysis_adi/")

# Import df_long ---------------------------------------------------------------
cat("\nImport df_long ---------------------------------------------------------------")
load("data/df_long.RData")
assertthat::assert_that(nrow(df_long) == 225296)
assertthat::assert_that(ncol(df_long) == 36)

# Transfom all cols to numeric -------------------------------------------------
cat("\nTransfom all cols to numeric -------------------------------------------------")
table(sapply(df_long, class))
df_long <- mutate_all(df_long, function(x) as.numeric(as.character(x)))
table(sapply(df_long, class))


# Rename columns ---------------------------------------------------------------
cat("\nRename columns ---------------------------------------------------------------")
colnames(df_long) <- c("id",
                       "year",
                       "depression",
                       "arbeit_zeit_wochenstunden",
                       "arbeit_zeit_wochenstunden_vereinbart",
                       "arbeit_zeit_wochenende",
                       "arbeit_zeit_nacht",
                       "arbeit_zeit_art",
                       "arbeit_qualifikation",
                       "arbeit_intensitaet",
                       "arbeit_einbezug_entscheidungen",
                       "arbeit_zufriedenheit_atmosphaere",
                       "hausarbeit_wochenstunden",
                       "beeintraechtigung_arbeit_privat",
                       "abschalten_nach_arbeit",
                       "einschraenkung_weg_ges_zustand",
                       "tage_gesunheits_prob",
                       "chronische_krankheit",
                       "ausbildung",
                       "partnerschaft",
                       "tod_person",
                       "person_haushalt",
                       "migrationshintergrund",
                       "geschlecht",
                       "alter",
                       "status",
                       "occupa",
                       "ch_nationalitaet",
                       "haushaltsaequivalenzeinkommen",
                       "kinder_betreuung",
                       "pflege_extern",
                       "pflege_extern_wer1",
                       "pflege_extern_wer2",
                       "pflege_extern_wer3",
                       "pflege_extern_wer4",
                       "pflege_extern_wer5")
data_summary(df_long)


# Filter 15 <= age <= 65 & occupa %in% c(1,2,3,5,6) ----------------------------
cat("\nFilter 15 <= age <= 65 & occupa %in% c(1,2,3,5,6) ----------------------------")
sample <- df_long %>%
  drop_na(alter) %>%
  filter(alter >= 15) %>%
  filter(alter <= 65) %>%
  filter(occupa %in% c(1,2,3,5,6))

data_summary(sample)


# Create arbeit_zeit_ueberstunden ----------------------------------------------
cat("\nCreate arbeit_zeit_ueberstunden ----------------------------------------------")
sample <- sample %>% mutate(arbeit_zeit_ueberstunden =
                              arbeit_zeit_wochenstunden - arbeit_zeit_wochenstunden_vereinbart)

# Create pflege_angehoerige ----------------------------------------------------
cat("\nCreate pflege_angehoerige ----------------------------------------------------")

sample <- sample %>% mutate(
  pflege_angehoerige = case_when(
    pflege_extern == 2 ~ 1,
    pflege_extern == 1 & !(pflege_extern_wer1 == id | pflege_extern_wer2 == id | pflege_extern_wer3 == id | pflege_extern_wer4 == id | pflege_extern_wer5 == id) ~ 1,
    pflege_extern == 1 & (pflege_extern_wer1 == id | pflege_extern_wer2 == id | pflege_extern_wer3 == id | pflege_extern_wer4 == id | pflege_extern_wer5 == id) ~ 2))

table(sample$pflege_angehoerige, useNA = "ifany")
sample$pflege_angehoerige <- factor(sample$pflege_angehoerige, levels = c(1,2), labels = c("Keine Pflege", "Pflege"))
table(sample$pflege_angehoerige, useNA = "ifany")

data_summary(sample)


# Create kml-clustering --------------------------------------------------------
cat("\nCreate kml-clustering --------------------------------------------------------\n")
library(kml)
df_kml <- sample[1:3]

set.seed(1)
for (cluster_number in c(5)) {
  for (redrawing_number in c(1)) {
    kml_cluster_data <- pivot_wider(df_kml[1:3], names_from = year, values_from = depression)
    cluster = clusterLongData(as.matrix(kml_cluster_data[c(2:16)]))

    start_time <- Sys.time()
    kml(cluster, nbClusters = cluster_number, nbRedrawing = redrawing_number, toPlot='none')
    end_time <- Sys.time()
    runtime = end_time - start_time
    plot(cluster,cluster_number)
    print("KML Algortihm")
    print(runtime)
    print(paste0("Cluster number = ", cluster_number, ". Redrawing number = ", redrawing_number))
  }
}

cluster_size <- 5
# kml_cluster_data <- reshape(df_kml, idvar = "id", timevar = "year", direction = "wide")
# cluster <- clusterLongData(as.matrix(kml_cluster_data[c(2:16)]))
# Shape_kml <- kml(cluster, nbClusters = cluster_size, nbRedrawing = 10, toPlot='both')
plot(cluster,cluster_size)

### code to add clusters to the original data
id_not_na <- as.numeric(str_extract(cluster@idFewNA, "\\-*\\d+\\.*\\d*"))
unique_id <- unique(df_kml$id)
labels <- as.data.frame(cluster@c5[[1]]@clusters)

real_id_not_na <- list()
for(i in 1:length(id_not_na)) {
  real_id_not_na[[i]] <- unique_id[id_not_na[i]]
}
real_id_not_na <- do.call(rbind.data.frame, real_id_not_na)
df_labeled <- cbind(real_id_not_na, labels)
colnames(df_labeled) <- c("id","cluster")

df_clustered <- right_join(sample, df_labeled, by = "id") # right join decreases the size of the dataset
head(df_clustered[c(1,2,3,39)])
df_clustered$cluster <- as.numeric(df_clustered$cluster)

rm(df_kml, df_labeled, kml_cluster_data, labels, real_id_not_na, cluster)

data_summary(df_clustered)




# Recode partnerschaft ---------------------------------------------------------
cat("\nRecode partnerschaft ---------------------------------------------------------")
df_clustered <- df_clustered %>%
  mutate(partnerschaft = case_when(partnerschaft %in% c(1,2) ~ 1,partnerschaft == 3 ~ 2))
df_clustered$partnerschaft <- factor(df_clustered$partnerschaft,
                                     levels = c(1,2), labels = c("Partnerschaft", "Single"))
table(df_clustered$partnerschaft, useNA = "ifany")


# Recode geschlecht ------------------------------------------------------------
cat("\nRecode geschlecht ------------------------------------------------------------")
df_clustered$geschlecht <- factor(df_clustered$geschlecht, levels = c(1,2), labels = c("Maennlich", "Weiblich"))
table(df_clustered$geschlecht, useNA = "ifany")


# Create alter_2 ---------------------------------------------------------------
cat("\nCreate alter_2 ---------------------------------------------------------------")
ggplot(df_clustered, aes(alter, depression)) +
  geom_jitter(alpha = 0.01) +
  geom_smooth()
df_clustered$alter_2 <- df_clustered$alter^2


# Recode ausbildung ------------------------------------------------------------
cat("\nRecode ausbildung ------------------------------------------------------------")
table(df_clustered$ausbildung, useNA = "ifany")
df_clustered <- df_clustered %>%
  mutate(ausbildung = case_when(ausbildung < 4 ~ 1,
                                ausbildung < 7 ~ 2,
                                ausbildung < 10 ~ 3,
                                ausbildung == 10 ~ 4))
df_clustered$ausbildung <- factor(df_clustered$ausbildung, levels = c(1,2,3,4),
                                  labels = c("Tiefer Bildungsstand", "Sekundarstufe II", "Höhere Berufsbildung", "Hochschule"))
table(df_clustered$ausbildung, useNA = "ifany")


# Recode tod_person ------------------------------------------------------------
cat("\nRecode tod_person ------------------------------------------------------------")
table(df_clustered$tod_person, useNA = "ifany")
df_clustered$tod_person <- factor(df_clustered$tod_person, levels = c(1,2),
                                  labels = c("Angehoerige Person gestorben", "Keine angehoerige Person gestorben"))
table(df_clustered$tod_person, useNA = "ifany")


# Recode (log-transform) haushaltsaequivalenzeinkommen -------------------------
cat("\nRecode (log-transform) haushaltsaequivalenzeinkommen -------------------------")
hist(df_clustered$haushaltsaequivalenzeinkommen, breaks = 100)
hist(log(df_clustered$haushaltsaequivalenzeinkommen))
df_clustered$haushaltsaequivalenzeinkommen <- log(df_clustered$haushaltsaequivalenzeinkommen)

data_summary(df_clustered)





df_ols2 <- df_clustered

# Recode arbeit_einbezug_entscheidungen ----------------------------------------
cat("\nRecode arbeit_einbezug_entscheidungen ----------------------------------------")
table(df_ols2$arbeit_einbezug_entscheidungen, useNA = "ifany")
df_ols2 <- df_ols2 %>%
  mutate(arbeit_einbezug_entscheidungen = case_when(
    arbeit_einbezug_entscheidungen %in% c(2,3) ~ 1,
    arbeit_einbezug_entscheidungen == 1 ~ 2))
df_ols2$arbeit_einbezug_entscheidungen <- factor(df_ols2$arbeit_einbezug_entscheidungen,
                                                 levels = c(1,2), labels = c("Kein Einbezug", "Entscheidung"))
table(df_ols2$arbeit_einbezug_entscheidungen, useNA = "ifany")

# Recdoe arbeit_qualifikation --------------------------------------------------
cat("\nRecode arbeit_qualifikation --------------------------------------------------")
table(df_ols2$arbeit_qualifikation, useNA = "ifany")
df_ols2 <- df_ols2 %>%
  mutate(arbeit_qualifikation = case_when(
    arbeit_qualifikation %in% c(1,3,4) ~ 1,
    arbeit_qualifikation == 2 ~ 2))
df_ols2$arbeit_qualifikation <- factor(df_ols2$arbeit_qualifikation,
                                       levels = c(1,2), labels = c("Unpassend", "Passend"))
table(df_ols2$arbeit_qualifikation, useNA = "ifany")

# Recode arbeit_zeit_nacht -----------------------------------------------------
cat("\nRecode arbeit_zeit_nacht -----------------------------------------------------")
table(df_ols2$arbeit_zeit_nacht, useNA = "ifany")
df_ols2 <- df_ols2 %>%
  mutate(arbeit_zeit_nacht = case_when(
    arbeit_zeit_nacht == 1 ~ 2,
    arbeit_zeit_nacht == 2 ~ 1))
df_ols2$arbeit_zeit_nacht <- factor(df_ols2$arbeit_zeit_nacht, levels = c(1,2), labels = c("Nein", "Ja"))
table(df_ols2$arbeit_zeit_nacht, useNA = "ifany")






df_ols3 <- df_ols2

# Recode kinder_betreuung (2nd time) -------------------------------------------
cat("\nRecode kinder_betreuung (2nd time) -------------------------------------------")
table(df_ols3$kinder_betreuung, useNA = "ifany")
df_ols3 <- df_ols3 %>%
  mutate(kinder_betreuung = case_when(
    kinder_betreuung == 0 ~ 1,
    kinder_betreuung %in% c(1,2) ~ 2))
df_ols3$kinder_betreuung <- factor(df_ols3$kinder_betreuung, levels = c(1,2), labels = c("Nein", "Ja"))


data_summary(df_ols3)

# ---------------/-------------------------------------------/---------------
# ---------------------------- Vergleich zu Import Adi ----------------------
cat("\nCompare to import SNF - Are the data equal? ----------------------------------")
df_ols3_adi <- imsbasics::load_rdata("df_ols3_adi", "Scripts_Michi/")
cat("\n ----> ",assertthat::assert_that(all(df_ols3 == df_ols3_adi, na.rm = TRUE)))

# ---------------/-------------------------------------------/---------------
# ---------------------- Reduktion & Abspeichern -------------------------------
cat("\n\nDefine relevant variables ----------------------------------------------------")

df_mt <- df_ols3

var_type <- data.frame(
  variable = c("id", "year", "depression", "cluster", "person_haushalt", # basis
               "ausbildung", "alter", "alter_2", "geschlecht", "ch_nationalitaet", # Lebenslage
               "einschraenkung_weg_ges_zustand", "haushaltsaequivalenzeinkommen",  # Lebenslage
               "partnerschaft", "tod_person",                                      # Lebenslage
               "arbeit_einbezug_entscheidungen",                                                 # Erwerbsarbeit
               "arbeit_qualifikation", "arbeit_zeit_wochenstunden", "arbeit_zeit_ueberstunden",  # Erwerbsarbeit
               "arbeit_zeit_nacht", "arbeit_intensitaet", "arbeit_zufriedenheit_atmosphaere",    # Erwerbsarbeit
               "hausarbeit_wochenstunden", "kinder_betreuung", "pflege_angehoerige"),   # Carearbeit
  type = c(rep("basis", 5), rep("lebenslage", 9), rep("erwerbsarbeit",7), rep("carearbeit",3)))

assertthat::assert_that(all(var_type$variable %in% colnames(df_mt)))
not_relevant_variables <- colnames(df_mt)[!colnames(df_mt) %in% var_type$variable]

cat("\n\nRelevant variables:", length(var_type$variable), "       -->", paste(var_type$variable, collapse = ", "))
cat("\nNot Relevant variables:", length(not_relevant_variables), "   -->", paste(not_relevant_variables, collapse = ", "))

# for now we keep all variables during import
# df_mt <- df_mt[,colnames(df_mt) %in% var_type$variable]

data_summary(df_mt)


cat("\n\nAdd relevant variables as attribute 'var_type' -------------------------------")
attr(df_mt, "var_type") <- var_type


cat("\n\nSave `df_mt` -----------------------------------------------------------------")
imsbasics::save_rdata(df_mt, "df_mt", "Scripts_Michi/", force = TRUE)
closeAllConnections()




# # ---------------/-------------------------------------------/---------------
# # ---------------------- Laden von df_mt & Basisanalysen -----------------------
# imsbasics::clc()
#
# cat("\n\n\nLaden von df_mt & Basisanalysen-------------------------------------------")
# df_mt <- imsbasics::load_rdata("df_mt", "Scripts_Michi/")
#
# cat("\n --> df_mt hat", round(100*sum(is.na(df_mt))/(nrow(df_mt)*ncol(df_mt)),1), "% NA's")
#
# # NA's
# NAs_per_column <- data.frame(na_absolute = unlist(map(df_mt, ~sum(is.na(.)))),
#                              na_relative_percent = 100*unlist(map(df_mt, ~sum(is.na(.))/nrow(df_mt))))
# knitr::kable(NAs_per_column, format = "markdown")
#
#
# # Spalten-Typen
# table(sapply(df_mt, class))
#
# # Verteilung numerischer Variablen
# as.data.frame(df_mt) %>%
#   select_if(is.numeric) %>%
#   pivot_longer(-id, names_to = "variablen", values_to = "daten") %>%
#   ggplot() +
#   geom_bar(aes(y = daten)) +
#   # geom_histogram(aes(y = daten)) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   facet_wrap(~variablen, scales = "free")
#
#
# # Verteilung von Faktor-Variablen
# as.data.frame(df_mt) %>%
#   select(matches("id") | where(is.factor)) %>%
#   pivot_longer(-id, names_to = "variablen", values_to = "daten") %>%
#   ggplot() +
#   geom_bar(aes(y = daten)) + # geom_bar(aes(y = daten)) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   facet_wrap(~variablen, scales = "free")
#
#
#
#
#
#
#
#
#
#
