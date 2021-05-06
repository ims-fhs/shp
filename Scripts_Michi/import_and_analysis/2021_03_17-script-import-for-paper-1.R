imsbasics::clc()
library(tidyverse)
library(shp)
load_path <- paste0("data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS")
ys <- "2004"; ye <- "2019"

## ----------------------- P - Import easy variables ---------------------------
# ## Beanspruchungsfolgen
# depression <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC17"), year_start = ys, year_end = ye) # Niedergeschlagenheit, Hoffnungslosigkeit, Angst, Depression: Häufigkeit
# ermuedung <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYF51"), year_start = ys, year_end = ye) # Erschöpfung nach Arbeit um Sachen zu machen
# stress_arbeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW604"), year_start = ys, year_end = ye) # Stress bei der Arbeit
# gesundheitliche_erkrankung <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC04A"), year_start = ys, year_end = ye) # Gesundheitliche Probleme: Rückenweh
# ## Bedingungen/Belastungen: Arbeit
# arbeit_zufriedenheit_aufgaben <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW229"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Zufriedenheit: Interessante Aufgaben
# art_arbeitszeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW71A"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Art der Arbeitszeit
# einbezug_entscheidungen <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW91"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Einbezug bei Entscheidungen/Meinungen
# arbeitskontrolle_personen <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW87"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Arbeitskontrolle anderer Personen
# qualifikation_arbeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW100"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Qualifikation für Arbeit
# arbeitsstunden_woche <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW77"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Anzahl Arbeitsstunden pro Woche
# wochenend_arbeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW218"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Wochenendearbeit
# nachtarbeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW216"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Nachtarbeit
# taegl_pendel_in_min <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW84"), year_start = ys, year_end = ye) # Arbeitsweg: Tägliches Pendeln in Minuten
# arbeits_intensität <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW603"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Arbeitsrythmus: Intensität
# zufriedenheit_arbeitsatmosphaere <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW94"),  year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Zufriedenheit: Arbeitsatmosphäre
# arbeit_laerm_schmutz <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW605"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Arbeitsrythmus: Lärm, Schmutz
# arbeit_ermuedende_koerperha <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYW606"), year_start = ys, year_end = ye) # Aktuelle Haupttätigkeit: Arbeitsrythmus: ermüdende Körperhaltung
# ## Bedingungen/Belastungen: Care-Sphäre
# hausarbeit_stunden_woche <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYF08"), year_start = ys, year_end = ye) # Hausarbeit: Stunden pro Woche
# beeintraechtigung_arbeit_privat <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYF50"), year_start = ys, year_end = ye) # Beeinträchtigung Arbeit <-> private Aktivitäten /Familie
# abschalten_nach_arbeit <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYF52"), year_start = ys, year_end = ye) # Schwierigkeit nach Arbeit abzuschalten
# ## Bedingungen/Belastungen: Lebenslage & Ressourcen
# einschraenkung_weg_ges_zustand <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC08"), year_start = ys, year_end = ye) # Einschränkungen wegen Gesundheitszustand bei täglichen Aktivitäten: Ausmass
# tage_gesunheits_prob <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","PYYC11"), year_start = ys, year_end = ye) # Anzahl Tage die von Gesundheitsproblemen betroffen waren: Letzte 12 Monate
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
## --------------------- P - Create constructed variables ----------------------
## Manually constructed P-variables 1
##  - Staatsbürgerschaft (PYYD19, PYYD21, PYYD23)
##  ----> No variable is ever available...?
## - Staatsbürgerschaft (NAT_1_YY, NAT_2_YY, NAT_3_YY)
## ----> All variables available
#
av_1 <- data.frame(years = seq(ys,ye))
for (i in c("PYYD19", "PYYD21", "PYYD23", "NAT_1_YY", "NAT_2_YY", "NAT_3_YY")) {
  my_availability <- years_available("P_USER.sav", load_path, i, year_start = ys, year_end = ye)
  names(my_availability)[2] <- i
  av_1 <- left_join(av_1, my_availability)
}
nationalitaet_1 <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","NAT_1_YY"), year_start = ys, year_end = ye) # "Erste Nationalität".
nationalitaet_2 <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","NAT_2_YY"), year_start = ys, year_end = ye) # "Zweite Nationalität".
nationalitaet_3 <- import_long_cols("P_USER.sav", load_path, cols = c("IDPERS","NAT_3_YY"), year_start = ys, year_end = ye) # "Dritte Nationalität".
nationalitaet <- mutate(nationalitaet_1, nationalitaet_2, nationalitaet_3)
rm(nationalitaet_1); rm(nationalitaet_2); rm(nationalitaet_3)

nationalitaet_long <- nationalitaet %>%
    pivot_longer(-ID,
                 names_to = c("nat", "year"),
                 names_pattern = "NAT_(.)_(.*)",
                 values_to = "country")
schweizer_nationalitaet <- nationalitaet_long %>%
    group_by(ID, year) %>%
    summarise(schweizer_nationalitaet = ifelse(test = all(is.na(country)),
                                               yes = NA, # If no answer was given (3 x NA), we also get FALSE !!! - acceptable?
                                               no = as.numeric(any(country == 8100, na.rm = TRUE)))) %>% # else we check if any answer was "Switzerland"
    pivot_wider(names_from = year,
                values_from = schweizer_nationalitaet,
                names_glue = "P{year}CH")

## --------------------------- P - Merge P-variables ---------------------------
#
# df_raw_p <- mutate(depression, ermuedung, stress_arbeit, gesundheitliche_erkrankung, arbeit_zufriedenheit_aufgaben,
#                    art_arbeitszeit, einbezug_entscheidungen, arbeitskontrolle_personen, qualifikation_arbeit,
#                    arbeitsstunden_woche, wochenend_arbeit, nachtarbeit, taegl_pendel_in_min, arbeits_intensität,
#                    zufriedenheit_arbeitsatmosphaere, arbeit_laerm_schmutz, arbeit_ermuedende_koerperha, hausarbeit_stunden_woche,
#                    beeintraechtigung_arbeit_privat, abschalten_nach_arbeit, einschraenkung_weg_ges_zustand, tage_gesunheits_prob,
#                    chronische_krankheit, hoechster_bildungsabschluss, partnerschaft, tod_person, person_haushalt,
#                    migrationshintergrund, geschlecht, alter, status, beschaeftigung, schweizer_nationalitaet)
#
# save(df_raw_p, file = "data/df_raw_p.RData")
# load("data/df_raw_p.RData")
#
## ---------------/-------------------------------------------/-----------------
## --------------------------- H - Import easy variables -----------------------
#
# load_path <- paste0("data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS")
# haushaltsaequivalenzeinkommen <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","IYYEQON"), year_start = ys, year_end = ye) # Jährliches Haushaltseinkommen (Aequivalenz), OECD, netto


## --------------------- H - Create constructed variables ----------------------
## Manually constructed H-variables 1
##  - Unentgeldliche Pflege von Personen ausserhalb des eigenen Haushaltes (using HYYF64, HYYF721, HYYF722, HYYF723, HYYF724, HYYF725)
##  ----> They are never avaliable in year 2013 (and partially not available in 2004)
##
av_2 <- data.frame(years = seq(ys,ye))
for (i in c("HYYF64", "HYYF721", "HYYF722", "HYYF723", "HYYF724", "HYYF725")) {
  my_availability <- years_available("H_USER.sav", load_path, i, year_start = ys, year_end = ye)
  names(my_availability)[2] <- i
  av_2 <- left_join(av_2, my_availability)
}
# betreuung_ausserhalb <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF64"), year_start = ys, year_end = ye) # "Betreuung ausserhalb der Familie: Ja, nein".
# betreuung_ausserhalb_wer1 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF721"), year_start = ys, year_end = ye) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer2 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF722"), year_start = ys, year_end = ye) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer3 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF723"), year_start = ys, year_end = ye) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer4 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF724"), year_start = ys, year_end = ye) # "Betreuung ausserhalb der Familie: wer".
# betreuung_ausserhalb_wer5 <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF725"), year_start = ys, year_end = ye) # "Betreuung ausserhalb der Familie: wer".



## Manually constructed H-variables 2
##  - Kinderbetreuung Ja/Nein (HLDFFSYY, HYYF53, HYYF54, HYYF55, HYYF56) or HLDTYPYY ?
##  - Alleinerziehend Ja/Nein (HLDFFSYY, HYYF53, HYYF54, HYYF55, HYYF56) or HLDTYPYY ?
##  ----> All Variables are available
##
##  HLDFFSYY = Haushaltstyp der Familie und Mikrozensus Familie
##      -1 "weiss nicht"
##      11 "Nicht verheiratetes Paar mit Kindern"
##      12 "Ehepaar mit Kindern"
##      13 "Nicht mehr verheiratetes Ehepaar mit Kindern"
##      21 "Nicht verheiratetes Paar ohne Kinder"
##      22 "Ehepaar ohne Kinder"
##      23 "Nicht mehr verheiratetes Paar ohne Kinder"
##      31 "Alleinerziehender, nie verheiratet gewesener Elternteil mit Kindern"
##      32 "Verheirateter Elternteil mit Kindern"
##      33 "Nicht mehr verheirateter Elternteil mit Kindern"
##      41 "Nie verheiratet gewesene, allein lebende Person"
##      42 "Verheiratete allein lebende Person"
##      43 "Nicht mehr verheiratete, allein lebende Person"
##      44 "Andere Situation".
##
##  HYYF53, HYYF54, HYYF55, HYYF56 = Kinderbetreuung Krankheit, Spielen, Transport, Hausaufgaben
##      -1 "weiss nicht"
##      1 "Meistens ich"
##      2 "Meistens mein Partner/meine Partnerin"
##      3 "In der Regel beide ungefähr gleich"
##      4 "Ein anderes Kind oder andere Kinder von unserem Haushalt"
##      5 "Meistens sonst jemand in unserem Haushalt"
##      6 "Meistens die Haushaltshilfe"
##      7 "Meistens Vater/Mutter der Kinder (nicht im Haushalt lebend)"
##      8 "Vater/Mutter (der/die nicht im Haushalt lebt) und ich"
##      9 "Meistens sonst jemand, der nicht zum Haushalt gehört"
##      10 "andere Lösung"
##      11 "gegenstandslos".
##
## HLDTYPYY = Haushaltstyp (PACO)
##      -1 "weiss nicht"
##      1 "eine Person 65 Jahre oder älter"
##      2 "eine Person 30 Jahre alt oder älter und jünger als 65 Jahre"
##      3 "eine Person jünger als 30 Jahren"
##      4 "alleinerziehende Person mit  Kind/ern 16 Jahre alt oder jünger"
##      5 "alleinerzeihende Person mit mind einem Kind älter als 16"
##      6 "Paar ohne Kinder mit zumindest jemand >=  65 Jahre alt"
##      7 "Paar ohne Kinder mit beide jünger als 65 Jahre"
##      8 "Paar mit einem Kind"
##      9 "Paar mit zwei Kindern"
##      10 "Paar mit drei Kindern und mehr"
##      11 "Paar mit zumindest einem Kind über 16 Jahren"
##      12 "andere Haushalte, alle Mitglieder verwandt"
##      13 "andere Haushalte, nicht alle Mitglieder verwandt".
##
## Vorschlag Myriel/Adi: (mittels HLDFFSYY) -> aktuell implementiert
##    - keine Kinderbetreuung = 21,22,23,41,42,43,44
##    - Geteilte Kinderbetreuung = 11,12,13
##    - Alleinerziehend = 31,32,33
##
## Vorschlag Michi:
##    - HYYF53, HYYF54, HYYF55, HYYF56 sicher ungünstig, da sie sich auf ein "ich" beziehen (obowhl H-Daten) -> ?
##    - ev. detailliertere Aufsplittung von Myriels/Adi's Vorschlag mittels HLDTYPYY (Anzahl Kindern & Alter)
##      wäre möglich, da HLDFFSYY & HLDTYPYY extrem gute Übereinstimmung bezgl. Einträgen zeigen
##          -> sum(is.na(haushaltstyp_mikrozensus) == is.na(haushaltstyp_PACO)) = 95534
##          -> sum(is.na(haushaltstyp_mikrozensus) != is.na(haushaltstyp_PACO)) = 23
#
av_3 <- data.frame(years = seq(ys,ye))
for (i in c("HLDFFSYY", "HYYF53", "HYYF54", "HYYF55", "HYYF56", "HLDTYPYY")) {
  my_availability <- years_available("H_USER.sav", load_path, i, year_start = ys, year_end = ye)
  names(my_availability)[2] <- i
  av_3 <- left_join(av_3, my_availability)
}
haushaltstyp_mikrozensus <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HLDFFSYY"), year_start = ys, year_end = ye) # "Haushaltstyp der Familie und Mikrozensus Familie"
kinderbetreuung_krankheit <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF53"), year_start = ys, year_end = ye) # "Kinderbetreuung: Im Krankheitsfall"
kinderbetreuung_spielen <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF54"), year_start = ys, year_end = ye) # "Kinderbetreuung: Mit ihnen spielen"
kinderbetreuung_transport <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF55"), year_start = ys, year_end = ye) # "Kinderbetreuung:  In Kindergarten, Schule bringen"
kinderbetreuung_hausaufgaben <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HYYF56"), year_start = ys, year_end = ye) # "Kinderbetreuungt: Bei den Hausaufgaben helfen"
haushaltstyp_PACO <- import_long_cols("H_USER.sav", load_path, cols = c("IDHOUS","HLDTYPYY"), year_start = ys, year_end = ye) # "Haushaltstyp (PACO)"

# Vorschlag Myriel/Adi:
kinderbetreuung_long <- haushaltstyp_mikrozensus %>%
    pivot_longer(-ID,
                 names_to = c("year"),
                 names_pattern = "HLDFFS(.*)")

kinderbetreuung <- kinderbetreuung_long %>%
    mutate(value = ifelse(test = value %in% c(21,22,23,41,42,43,44), yes = 0, no = value), # 0 = keine Kinderbetreuung
           value = ifelse(test = value %in% c(11,12,13), yes = 1, no = value), # 1 = Geteilte Kinderbetreuung
           value = ifelse(test = value %in% c(31,32,33), yes = 2, no = value)) # 2 = Alleinerziehend

par(mfrow = c(2,1)); hist(kinderbetreuung_long$value); hist(kinderbetreuung$value); par(mfrow = c(1,1))

kinderbetreuung <- kinderbetreuung %>%
  pivot_wider(names_from = year,
              values_from = value,
              names_glue = "HLDFFS{year}")


## --------------------------- H - Merge H-variables ---------------------------
h_datensatz <- mutate(haushaltsaequivalenzeinkommen, kinderbetreuung)

## ---------------/-------------------------------------------/-----------------
## --------------------------- P&H - Merge P & H variables ---------------------
## Zusammen basteln von P und H Datensatz
## Kommentar Michi: IDHOUS04-IDHOUS19 in df_raw_p hat viele lücken --> Imputation im
## Fall   x,x,x,NA,NA,NA,X,X,X   könnte sinnvoll sein.
#
# h_datensatz_04 <- h_datensatz %>% transmute(ID, I04EQON, HLDFFS04)
# h_datensatz_05 <- h_datensatz %>% transmute(ID, I05EQON, HLDFFS05)
# h_datensatz_06 <- h_datensatz %>% transmute(ID, I06EQON, HLDFFS06)
# h_datensatz_07 <- h_datensatz %>% transmute(ID, I07EQON, HLDFFS07)
# h_datensatz_08 <- h_datensatz %>% transmute(ID, I08EQON, HLDFFS08)
# h_datensatz_09 <- h_datensatz %>% transmute(ID, I09EQON, HLDFFS09)
# h_datensatz_10 <- h_datensatz %>% transmute(ID, I10EQON, HLDFFS10)
# h_datensatz_11 <- h_datensatz %>% transmute(ID, I11EQON, HLDFFS11)
# h_datensatz_12 <- h_datensatz %>% transmute(ID, I12EQON, HLDFFS12)
# h_datensatz_13 <- h_datensatz %>% transmute(ID, I13EQON, HLDFFS13)
# h_datensatz_14 <- h_datensatz %>% transmute(ID, I14EQON, HLDFFS14)
# h_datensatz_15 <- h_datensatz %>% transmute(ID, I15EQON, HLDFFS15)
# h_datensatz_16 <- h_datensatz %>% transmute(ID, I16EQON, HLDFFS16)
# h_datensatz_17 <- h_datensatz %>% transmute(ID, I17EQON, HLDFFS17)
# h_datensatz_18 <- h_datensatz %>% transmute(ID, I18EQON, HLDFFS18)
# h_datensatz_19 <- h_datensatz %>% transmute(ID, I18EQON, HLDFFS19)
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
#
# save(df, file = "data/df.RData")
# imsbasics::clc()
# load("data/df.RData")
#
#
# df_long <- df %>%
#   pivot_longer(-ID,
#                names_to = c('.value', 'year','.value','.value'),
#                names_pattern = '([A-Z]+)(\\d+)([A-Z_]*)([0-9_]*)')
#
# save(df_long, file = "data/df_long.RData")
# imsbasics::clc()
load("data/df_long.RData")

colnames(df_long) <- c("id","year","depression","ermuedung","stress_arbeit","rueckenschmerzen","arbeit_zufriedenheit_aufgaben","art_arbeitszeit","arbeit_einbezug_entscheidungen","arbeitskontrolle_personen","arbeit_qualifikation","arbeitsstunden_woche","wochenend_arbeit","nachtarbeit","taegl_pendeln_min","arbeit_intensitaet","zufriedenheit_arbeitsatmosphaere","arbeit_laerm_schmutz","arbeit_ermuedende_koerperha","hausarbeit_stunden_woche","beeintraechtigung_arbeit_privat","abschalten_nach_arbeit","einschraenkung_weg_ges_zustand","tage_gesunheits_prob","chronische_krankheit","ausbildung","partnerschaft","tod_person","person_haushalt","migrationshintergrund","geschlecht","alter","status","occupa","ch_nationalitaet","haushaltsaequivalenzeinkommen","kinder_betreuung")


sample <- df_long %>%
  drop_na(alter) %>%
  filter(alter >= 15) %>%
  filter(alter <= 65) %>%
  filter(occupa %in% c(1,2,3,5,6))

sample <- mutate_all(sample, function(x) as.numeric(as.character(x)))

colnames(sample)

# 60672 rows / 37 columns
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
as.data.frame(sample[3:ncol(sample)]) %>%
  pivot_longer(-person_haushalt, names_to = "variablen", values_to = "daten") %>%
  ggplot() +
  geom_boxplot(aes(y = daten)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~variablen, scales = "free")


# Barplot per variable
as.data.frame(sample[3:ncol(sample)]) %>%
  pivot_longer(-person_haushalt, names_to = "variablen", values_to = "daten") %>%
  ggplot() +
  geom_bar(aes(y = daten)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~variablen, scales = "free")


ggplot(sample %>% group_by(id) %>% summarise(n = n())) +
  geom_bar(aes(x = n))




# library(kml)
# kml <- sample %>% select(ID, year, PC17)
# kml <- kml %>% pivot_wider(names_from = year, values_from = PC17)
#
#
# sample_ld <- clusterLongData(as.data.frame(kml))
# kml(sample_ld, 3,10,toPlot = 'both')
# choice(clustered)
#
# kml(wlb_complete_ld, 6,1,toPlot = 'both')
