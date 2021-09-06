# Basics: Packages and Import ---------------------------------------------
imsbasics::clc()
library(tidyverse)
library(kml)
library(GGally)
library(plm)
library(stargazer)
library(BBmisc)
library(REEMtree)
library(rattle)
library(knitr)

load("data/df_long.R")

assertthat::assert_that(nrow(df_long) == 225296)
assertthat::assert_that(ncol(df_long) == 36)
# df_long <- df_long %>% select_if(~sum(!is.na(.)) > 0)


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


sample <- df_long %>%
  drop_na(alter) %>%
  filter(alter >= 15) %>%
  filter(alter <= 65) %>%
  filter(occupa %in% c(1,2,3,5,6))
rm(df_long)

sample <- mutate_all(sample, function(x) as.numeric(as.character(x)))

# Ueberstunden ------------------------------------------------------------
sample <- sample %>% mutate(
  arbeit_zeit_ueberstunden = arbeit_zeit_wochenstunden - arbeit_zeit_wochenstunden_vereinbart)

# Pflege von Angehoerigen -------------------------------------------------
sample <- sample %>% mutate(
  pflege_angehoerige = case_when(
    pflege_extern == 2 ~ 1,
    pflege_extern == 1 & !(pflege_extern_wer1 == id | pflege_extern_wer2 == id | pflege_extern_wer3 == id | pflege_extern_wer4 == id | pflege_extern_wer5 == id) ~ 1,
    pflege_extern == 1 & (pflege_extern_wer1 == id | pflege_extern_wer2 == id | pflege_extern_wer3 == id | pflege_extern_wer4 == id | pflege_extern_wer5 == id) ~ 2)
)
table(sample$pflege_angehoerige, useNA = "ifany")
sample$pflege_angehoerige <- factor(sample$pflege_angehoerige, levels = c(1,2), labels = c("Keine Pflege", "Pflege"))
table(sample$pflege_angehoerige, useNA = "ifany")


# depression über die jahre
sample %>% group_by(year) %>% summarise(mean(depression, na.rm = TRUE))
sample %>% mutate(alter = round(alter/10)) %>% group_by(alter) %>% summarise(mean(depression, na.rm = TRUE))

# einschraenkung_weg_ges_zustand über die jahre
sample %>% group_by(year) %>% summarise(mean(einschraenkung_weg_ges_zustand, na.rm = TRUE))
sample %>% mutate(alter = round(alter/10)) %>% group_by(alter) %>% summarise(mean(einschraenkung_weg_ges_zustand, na.rm = TRUE))

# arbeit_intensitaet über die jahre
sample %>% group_by(year) %>% summarise(mean(arbeit_intensitaet, na.rm = TRUE))
sample %>% mutate(alter = round(alter/10)) %>% group_by(alter) %>% summarise(mean(arbeit_intensitaet, na.rm = TRUE))


# # Clustering --------------------------------------------------------------
# df_kml <- sample[1:3]
#
# set.seed(1)
# for (cluster_number in c(5)) {
#   for (redrawing_number in c(1)) {
#     kml_cluster_data <- pivot_wider(df_kml[1:3], names_from = year, values_from = depression)
#     cluster = clusterLongData(as.matrix(kml_cluster_data[c(2:16)]))
#
#     start_time <- Sys.time()
#     kml(cluster, nbClusters = cluster_number, nbRedrawing = redrawing_number, toPlot='none')
#     end_time <- Sys.time()
#     runtime = end_time - start_time
#     plot(cluster,cluster_number)
#     print("KML Algortihm")
#     print(runtime)
#     print(paste0("Cluster number = ", cluster_number, ". Redrawing number = ", redrawing_number))
#   }
# }
#
# cluster_size <- 5
# # kml_cluster_data <- reshape(df_kml, idvar = "id", timevar = "year", direction = "wide")
# # cluster <- clusterLongData(as.matrix(kml_cluster_data[c(2:16)]))
# # Shape_kml <- kml(cluster, nbClusters = cluster_size, nbRedrawing = 10, toPlot='both')
#
# plot(cluster,cluster_size)
#
# ### code to add clusters to the original data
# id_not_na <- as.numeric(str_extract(cluster@idFewNA, "\\-*\\d+\\.*\\d*"))
# unique_id <- unique(df_kml$id)
# labels <- as.data.frame(cluster@c5[[1]]@clusters)
#
#
# real_id_not_na <- list()
# for(i in 1:length(id_not_na)) {
#   real_id_not_na[[i]] <- unique_id[id_not_na[i]]
# }
# real_id_not_na <- do.call(rbind.data.frame, real_id_not_na)
# df_labeled <- cbind(real_id_not_na, labels)
# colnames(df_labeled) <- c("id","cluster")
#
# df_clustered <- right_join(sample, df_labeled, by = "id")
# head(df_clustered[c(1,2,3,39)])
#
# # 1) OLS: L>D --------------------------------------------------------------
#
# ### Pooled OLS Variableset 1
# df_clustered$cluster <- as.numeric(df_clustered$cluster)
#
# # Partnerschaft recodieren
# df_clustered <- df_clustered %>%
#   mutate(partnerschaft = case_when(partnerschaft %in% c(1,2) ~ 1,partnerschaft == 3 ~ 2))
# df_clustered$partnerschaft <- factor(df_clustered$partnerschaft, levels = c(1,2), labels = c("Partnerschaft", "Single"))
# table(df_clustered$partnerschaft, useNA = "ifany")
#
# # Geschlecht recodieren
# df_clustered$geschlecht <- factor(df_clustered$geschlecht, levels = c(1,2), labels = c("Maennlich", "Weiblich"))
# table(df_clustered$geschlecht, useNA = "ifany")
#
# # Alter
# ggplot(df_clustered, aes(alter, depression)) +
#   geom_jitter(alpha = 0.01) +
#   geom_smooth()
#
# df_clustered$alter_2 <- df_clustered$alter^2
#
#
# # Ausbildung recodieren
# table(df_clustered$ausbildung, useNA = "ifany")
# df_clustered <- df_clustered %>%
#   mutate(ausbildung = case_when(ausbildung < 4 ~ 1,
#                                 ausbildung < 7 ~ 2,
#                                 ausbildung < 10 ~ 3,
#                                 ausbildung == 10 ~ 4))
# df_clustered$ausbildung <- factor(df_clustered$ausbildung, levels = c(1,2,3,4),
#                                   labels = c("Tiefer Bildungsstand", "Sekundarstufe II", "Höhere Berufsbildung", "Hochschule"))
# table(df_clustered$ausbildung, useNA = "ifany")
#
# # tod_person recodieren
# table(df_clustered$tod_person, useNA = "ifany")
# df_clustered$tod_person <- factor(df_clustered$tod_person, levels = c(1,2),
#                                   labels = c("Angehoerige Person gestorben", "Keine angehoerige Person gestorben"))
# table(df_clustered$tod_person, useNA = "ifany")
#
# # haushaltsaequivalenzeinkommen
# hist(log(df_clustered$haushaltsaequivalenzeinkommen))
# df_clustered$haushaltsaequivalenzeinkommen <- log(df_clustered$haushaltsaequivalenzeinkommen)
#
# ols1 <- plm(depression ~
#       ausbildung + alter + alter_2 + geschlecht + ch_nationalitaet +
#       einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
#       partnerschaft + tod_person,
#     data = df_clustered, index = c("id","year"), model="pooling")
#
#
# stargazer(ols1,
#           title="Pooled OLS L>D", type="text",
#           column.labels=c("all"),
#           df=FALSE, digits=4)
#
# df_clustered_norm <- normalize(df_clustered, method = "range", range = c(0, 1))
#
# ols1_norm <- plm(depression ~
#                    ausbildung + alter + alter_2 + geschlecht + ch_nationalitaet +
#                    einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
#                    partnerschaft + tod_person,
#                  data = df_clustered_norm, index = c("id","year"), model="pooling")
#
#
# stargazer(ols1_norm,
#           title="Pooled OLS VS1", type="text",
#           column.labels=c("all"),
#           df=FALSE, digits=4)
#
# # ggplot(df_clustered, aes(partnerschaft, depression)) +
# #   geom_jitter(alpha = 0.01) +
# #   geom_smooth(method = lm)
# #
# # ggplot(df_clustered_norm) +
# #   geom_jitter(aes(x = partnerschaft, y = depression), na.rm = TRUE, alpha = 0.01) +
# #   geom_smooth(aes(x = partnerschaft, y = depression), na.rm = TRUE, method = lm)
# #
#
#
# # 2) OLS A+L>D ------------------------------------------------------------
#
# df_ols2 <- df_clustered
# rm(list=setdiff(ls(), "df_ols2"))
#
# # einbezug entscheidungen recodieren
# table(df_ols2$arbeit_einbezug_entscheidungen, useNA = "ifany")
# df_ols2 <- df_ols2 %>%
#   mutate(arbeit_einbezug_entscheidungen = case_when(
#     arbeit_einbezug_entscheidungen %in% c(2,3) ~ 1,
#     arbeit_einbezug_entscheidungen == 1 ~ 2))
# df_ols2$arbeit_einbezug_entscheidungen <- factor(df_ols2$arbeit_einbezug_entscheidungen, levels = c(1,2), labels = c("Kein Einbezug", "Entscheidung"))
# table(df_ols2$arbeit_einbezug_entscheidungen, useNA = "ifany")
#
# # arbeit_qualifikation recodieren
# table(df_ols2$arbeit_qualifikation, useNA = "ifany")
# df_ols2 <- df_ols2 %>%
#   mutate(arbeit_qualifikation = case_when(
#     arbeit_qualifikation %in% c(1,3,4) ~ 1,
#     arbeit_qualifikation == 2 ~ 2))
# df_ols2$arbeit_qualifikation <- factor(df_ols2$arbeit_qualifikation, levels = c(1,2), labels = c("Unpassend", "Passend"))
# table(df_ols2$arbeit_qualifikation, useNA = "ifany")
#
# # # arbeit_qualifikation recodieren
# # table(df_ols2$arbeit_qualifikation, useNA = "ifany")
# # df_ols2 <- df_ols2 %>%
# #   mutate(arbeit_qualifikation = case_when(
# #     arbeit_qualifikation == 2 ~ 1,
# #     arbeit_qualifikation == 1 ~ 3,
# #     arbeit_qualifikation == 4 ~ 2,
# #     arbeit_qualifikation == 3 ~ 4))
# # df_ols2$arbeit_qualifikation <- factor(df_ols2$arbeit_qualifikation, levels = c(1, 2, 3, 4), labels = c("Passend", "Nicht", "Unterqualifiziert", "Ueberqualifiziert"))
# # table(df_ols2$arbeit_qualifikation, useNA = "ifany")
#
#
# # Arbeitsstunden pro Woche recodieren
# # ggplot(df_ols2, aes(arbeit_zeit_wochenstunden, depression)) +
# #   geom_jitter(alpha = 0.01) +
# #   geom_smooth(method = lm)
# #
# #
# # table(df_ols2$arbeit_zeit_wochenstunden, useNA = "ifany")
# # df_ols2 <- df_ols2 %>%
# #   mutate(arbeit_zeit_wochenstunden = case_when(
# #     arbeit_zeit_wochenstunden < 20 ~ 3,
# #     arbeit_zeit_wochenstunden < 50 ~ 1,
# #     arbeit_zeit_wochenstunden >= 50 ~ 2))
# # df_ols2$arbeit_zeit_wochenstunden <- factor(df_ols2$arbeit_zeit_wochenstunden, levels = c(1,2,3), labels = c("Normal", "Workaholic", "Kaum"))
# # table(df_ols2$arbeit_zeit_wochenstunden, useNA = "ifany")
#
# # arbeit_zeit_nacht recodieren
# table(df_ols2$arbeit_zeit_nacht, useNA = "ifany")
# df_ols2 <- df_ols2 %>%
#   mutate(arbeit_zeit_nacht = case_when(
#     arbeit_zeit_nacht == 1 ~ 2,
#     arbeit_zeit_nacht == 2 ~ 1))
# df_ols2$arbeit_zeit_nacht <- factor(df_ols2$arbeit_zeit_nacht, levels = c(1,2), labels = c("Nein", "Ja"))
# table(df_ols2$arbeit_zeit_nacht, useNA = "ifany")
#
# # ggplot(df_ols2 %>% drop_na, aes(arbeit_zeit_nacht, depression)) +
# #   geom_boxplot() +
# #   geom_smooth()
#
# # stargazer(plm(depression ~
# #                 arbeit_zeit_nacht,
# #               data = df_ols2, index = c("id","year"), model="pooling"),
# #           title="arbeit_zeit_nacht", type="text",
# #           column.labels=c("all"),
# #           df=FALSE, digits=4)
#
#
# # Wochenendarbeit recodieren
# # table(df_ols2$arbeit_zeit_wochenende, useNA = "ifany")
# # df_ols2$arbeit_zeit_wochenende <- factor(df_ols2$arbeit_zeit_wochenende, levels = c(1,2), labels = c("Ja", "Nein"))
# # table(df_ols2$arbeit_zeit_wochenende, useNA = "ifany")
# # -> Entscheid 21.4.; Nicht berücksichtigen, hat keinen Effekt
#
# # Arbeitsablauf/Arbeitintensität
# table(df_ols2$arbeit_intensitaet, useNA = "ifany")
#
# # Soziale Beziehungen zu den Kollegen
# table(df_ols2$arbeit_zufriedenheit_atmosphaere, useNA = "ifany")
#
#
# ols2 <- plm(depression ~
#               ausbildung + alter + alter_2 + geschlecht + ch_nationalitaet +
#               einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
#               partnerschaft + tod_person + arbeit_einbezug_entscheidungen +
#               arbeit_qualifikation + arbeit_zeit_wochenstunden +
#               arbeit_zeit_ueberstunden + arbeit_zeit_nacht +
#               arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere,
#             data = df_ols2, index = c("id","year"), model="pooling")
#
#
# stargazer(ols2,
#           title="Pooled OLS A+L>D", type="text",
#           column.labels=c("all"),
#           df=FALSE, digits=4)
#
# # ggplot(df_ols2, aes(arbeit_zeit_wochenstunden, depression)) +
# #   geom_jitter(alpha = 0.01) +
# #   geom_smooth()
#
#
# # 3) OLS A+L+C>D ----------------------------------------------------------
#
# df_ols3 <- df_ols2
#
# # Hausarbeitsstunden
# table(df_ols3$hausarbeit_wochenstunden, useNA = "ifany")
#
# # Kinderbetreuung
# table(df_ols3$kinder_betreuung, useNA = "ifany")
# df_ols3 <- df_ols3 %>%
#   mutate(kinder_betreuung = case_when(
#     kinder_betreuung == 0 ~ 1,
#     kinder_betreuung %in% c(1,2) ~ 2))
# df_ols3$kinder_betreuung <- factor(df_ols3$kinder_betreuung, levels = c(1,2), labels = c("Nein", "Ja"))
# # df_ols3$kinder_betreuung <- factor(df_ols3$kinder_betreuung, levels = c(0,1,2), labels = c("Keine Kinderbetreuung", "Geteilte Kinderbetreuung", "Alleinerziehend"))
# table(df_ols3$kinder_betreuung, useNA = "ifany")
#
# # stargazer(plm(depression ~
# #                 kinder_betreuung,
# #               data = df_ols3, index = c("id","year"), model="within"),
# #           title="Kinderbetreuung", type="text",
# #           column.labels=c("all"),
# #           df=FALSE, digits=4)
#
#
# # Pflege von Angehörigen
# table(df_ols3$pflege_angehoerige, useNA = "ifany")
#
# # Analyse Pooled OLS A+L+C>D
# ols3 <- plm(depression ~
#               ausbildung + alter + alter_2 + geschlecht + ch_nationalitaet +
#               einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
#               partnerschaft + tod_person + arbeit_einbezug_entscheidungen +
#               arbeit_qualifikation + arbeit_zeit_wochenstunden +
#               arbeit_zeit_ueberstunden + arbeit_zeit_nacht +
#               arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere +
#               hausarbeit_wochenstunden + kinder_betreuung + pflege_angehoerige,
#           data = df_ols3, index = c("id","year"), model="pooling")
#
#
# stargazer(ols3,
#           title="Pooled OLS A+L+C>D", type="text",
#           column.labels=c("all"),
#           df=FALSE, digits=4)
#
# # 4) FE A+L+C>D ----------------------------------------------------------
# ols3_fe <- plm(depression ~
#               alter + alter_2 +
#               einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
#               partnerschaft + tod_person + arbeit_einbezug_entscheidungen +
#               arbeit_qualifikation + arbeit_zeit_wochenstunden +
#               arbeit_zeit_ueberstunden + arbeit_zeit_nacht +
#               beeintraechtigung_arbeit_privat +
#                 kinder_betreuung * arbeit_zeit_ueberstunden +
#                 pflege_angehoerige * arbeit_zeit_ueberstunden +
#                 hausarbeit_wochenstunden * arbeit_zeit_ueberstunden +
#                 kinder_betreuung * pflege_angehoerige +
#               arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere +
#               hausarbeit_wochenstunden + kinder_betreuung + pflege_angehoerige,
#             data = df_ols3, index = c("id","year"), model="within", effect = "twoways")
#
#
# stargazer(ols3_fe,
#           title="FE A+L+C>D", type="text",
#           column.labels=c("all"),
#           df=FALSE, digits=4)
#
# df_ols3_timeshift <- df_ols3 %>% group_by(id) %>% mutate(depression = data.table::shift(depression, n = -1))
#
# ols3_fe_ts <- plm(depression ~
#                  alter + alter_2 +
#                  einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
#                  partnerschaft + tod_person + arbeit_einbezug_entscheidungen +
#                  arbeit_qualifikation + arbeit_zeit_wochenstunden +
#                  arbeit_zeit_ueberstunden + arbeit_zeit_nacht +
#                  arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere +
#                  hausarbeit_wochenstunden + kinder_betreuung + pflege_angehoerige,
#                data = df_ols3_timeshift, index = c("id","year"), model="within")
#
#
# stargazer(ols3_fe_ts,
#           title="FE A+L+C>D_t+1", type="text",
#           column.labels=c("all"),
#           df=FALSE, digits=4)
#
# # 5) Pooled OLS A+L+C>D per Cluster
# ## Regressionsanalysis per Cluster
# ### Pooled OLS A+L+C>D
# class(df_ols3$cluster)
#
# regressions <- list()
# for(i in 1:length(unique(df_ols3$cluster))){
#   df_ols3_cluster_i <- df_ols3 %>% filter(cluster == i)
#   regressions[[i]] = plm(depression ~
#                            ausbildung + alter + alter_2 + geschlecht + ch_nationalitaet +
#                            einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
#                            partnerschaft + tod_person + arbeit_einbezug_entscheidungen +
#                            arbeit_qualifikation + arbeit_zeit_wochenstunden +
#                            arbeit_zeit_ueberstunden + arbeit_zeit_nacht +
#                            arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere +
#                            hausarbeit_wochenstunden + kinder_betreuung + pflege_angehoerige,
#                          data = df_ols3_cluster_i, index = c("id","year"), model="pooling")
# }
#
# stargazer(regressions[[1]], regressions[[2]], regressions[[3]], regressions[[4]], regressions[[5]],
#           title="Pooled OLS A+L+C>D per Cluster", type="text",
#           column.labels=c("Cluster A", "Cluster B", "Cluster C", "Cluster D", "Cluster E"),
#           df=FALSE, digits=4)
#
# # 6) FE OLS A+L+C>D per Cluster -----------------------------------------
# ### FE A+L+C>D
# class(df_ols3$cluster)
#
# regressions <- list()
# for(i in 1:length(unique(df_ols3$cluster))){
#   df_ols3_cluster_i <- df_ols3 %>% filter(cluster == i)
#   regressions[[i]] = plm(depression ~
#                            alter +
#                            einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
#                            partnerschaft + tod_person + arbeit_einbezug_entscheidungen +
#                            arbeit_qualifikation + arbeit_zeit_wochenstunden +
#                            arbeit_zeit_ueberstunden + arbeit_zeit_nacht +
#                            arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere +
#                            hausarbeit_wochenstunden + kinder_betreuung + pflege_angehoerige,
#                          data = df_ols3_cluster_i, index = c("id","year"), model="within", effect = "twoways")
# }
#
# stargazer(regressions[[1]], regressions[[2]], regressions[[3]], regressions[[4]], regressions[[5]],
#           title="FE A+L+C>D per Cluster", type="text",
#           column.labels=c("Cluster A", "Cluster B", "Cluster C", "Cluster D", "Cluster E"),
#           df=FALSE, digits=4)
#
# ggplot(df_ols3) +
#   geom_boxplot(aes(x = alter, group = cluster))
#
# df_ols3 %>%
#   group_by(cluster, geschlecht) %>%
#   summarise(n = n()) %>%
#   mutate(freq = n / sum(n))
#
# ggplot(df_ols3, aes(x = ausbildung)) +
#   geom_bar(aes(y = ..prop.., group = 1)) +
#   facet_wrap(~cluster)
#
# # 7) OLS A+L+C>D mit Interaktionen ---------------------------------------
#
# df_ols3_int <- df_ols3
#
# ols3_int <- plm(depression ~
#               ausbildung + alter + alter_2 + geschlecht + ch_nationalitaet +
#               einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
#               partnerschaft + tod_person + arbeit_einbezug_entscheidungen +
#               arbeit_qualifikation + arbeit_zeit_wochenstunden +
#               arbeit_zeit_ueberstunden + arbeit_zeit_nacht +
#               arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere +
#               hausarbeit_wochenstunden + kinder_betreuung + pflege_angehoerige +
#               kinder_betreuung * arbeit_zeit_ueberstunden +
#               pflege_angehoerige * arbeit_zeit_ueberstunden +
#               hausarbeit_wochenstunden * arbeit_zeit_ueberstunden +
#               kinder_betreuung * pflege_angehoerige,
#               data = df_ols3_int, index = c("id","year"), model="pooling")
#
#
# stargazer(ols3_int,
#           title="Pooled OLS A+L+C>D mit Interaktionen", type="text",
#           column.labels=c("all"),
#           df=FALSE, digits=4)
#
# # 7a) OLS A>D
#
# ols_nur_a <- plm(depression ~
#                   arbeit_einbezug_entscheidungen +
#                   arbeit_qualifikation + arbeit_zeit_wochenstunden +
#                   arbeit_zeit_ueberstunden + arbeit_zeit_nacht +
#                   arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere,
#                 data = df_ols3, index = c("id","year"), model="pooling")
#
#
# stargazer(ols_nur_a,
#           title="Pooled OLS A>D", type="text",
#           column.labels=c("all"),
#           df=FALSE, digits=4)
#
# # 8) FE A+L+C>D mit Interaktionen
# ols3_fe_int <- plm(depression ~
#                   alter + alter_2 +
#                   einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
#                   partnerschaft + tod_person + arbeit_einbezug_entscheidungen +
#                   arbeit_qualifikation + arbeit_zeit_wochenstunden +
#                   arbeit_zeit_ueberstunden + arbeit_zeit_nacht +
#                   arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere +
#                   hausarbeit_wochenstunden + kinder_betreuung + pflege_angehoerige +
#                   kinder_betreuung * arbeit_zeit_ueberstunden +
#                   pflege_angehoerige * arbeit_zeit_ueberstunden +
#                   hausarbeit_wochenstunden * arbeit_zeit_ueberstunden +
#                   kinder_betreuung * pflege_angehoerige,
#                 data = df_ols3_int, index = c("id","year"), model="within")
#
#
# stargazer(ols3_fe_int,
#           title="FE A+L+C>D mit Interatktionen", type="text",
#           column.labels=c("all"),
#           df=FALSE, digits=4)
#
# # 9) FE OLS A+L+C>D per Cluster
# ### FE A+L+C>D
# # df_ols3 <- df_ols2
# class(df_ols3$cluster)
# table(df_ols3$cluster)
# df_ols3 <- df_ols3 %>% mutate(cluster =
#   case_when(cluster < 3 ~ 1,
#             TRUE ~ cluster - 1)
# )
# table(df_ols3$cluster)
#
# regressions <- list()
# for(i in 1:length(unique(df_ols3$cluster))){
#   df_ols3_cluster_i <- df_ols3 %>% filter(cluster == i)
#   regressions[[i]] = plm(depression ~
#                            einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
#                            partnerschaft + tod_person + arbeit_einbezug_entscheidungen +
#                            arbeit_qualifikation + arbeit_zeit_wochenstunden +
#                            arbeit_zeit_ueberstunden + arbeit_zeit_nacht +
#                            arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere +
#                            hausarbeit_wochenstunden + kinder_betreuung + pflege_angehoerige,
#                          data = df_ols3_cluster_i, index = c("id","year"), model="within", effect = "twoways")
# }
#
# stargazer(regressions[[1]], regressions[[2]], regressions[[3]], regressions[[4]],
#           title="FE A+L+C>D per Cluster", type="text",
#           column.labels=c("unkritisch", "verbessernd", "verschlechternd", "kritisch"),
#           df=FALSE, digits=4)
#
# ggplot(df_ols3 %>% mutate(
#   cluster = factor(cluster, c(1:4),
#                    labels = c("unkritisch","verbessernd","verschlechternd","kritisch")))) + geom_smooth(
#   aes(x = year, y = depression, group = cluster, color = as.factor(cluster)),
#   method = lm, se = FALSE)
#
# ggplot(df_ols3) +
#   geom_boxplot(aes(x = alter, group = cluster))
#
# df_ols3 %>%
#   group_by(cluster, geschlecht) %>%
#   summarise(n = n()) %>%
#   mutate(freq = n / sum(n))
#
# df_ols3 %>%
#   group_by(cluster) %>%
#   summarise(median = median(alter))
#
# df_ols3 %>%
#   group_by(cluster) %>%
#   summarise(median = median(arbeit_zeit_wochenstunden, na.rm = TRUE))
#
# df_ols3 %>%
#   group_by(cluster) %>%
#   summarise(mean = mean(arbeit_zeit_wochenstunden, na.rm = TRUE))
#
# df_ols3 %>%
#   group_by(cluster) %>%
#   summarise(mean = mean(arbeit_zufriedenheit_atmosphaere, na.rm = TRUE))
#
# df_ols3 %>%
#   group_by(cluster, arbeit_einbezug_entscheidungen) %>%
#   summarise(n = n()) %>%
#   mutate(freq = n / sum(n))
#
# ggplot(df_ols3, aes(x = ausbildung)) +
#   geom_bar(aes(y = ..prop.., group = 1)) +
#   facet_wrap(~cluster)
#
#
#
# # Descriptive Analysis ----------------------------------------------------
#
# # ## Descriptive analysis of cluster ~ variable
# # First, standardize all continous variables.
# # Then reranged all variables to 0-1.
#
# ### Scale all Variables which are not categorical
# df_clustered_not_scaled = df_clustered
# df_clustered = df_clustered %>% mutate_at(c('taegl_pendeln_min',"arbeit_zeit_wochenstunden","hausarbeit_wochenstunden","tage_gesunheits_prob","person_haushalt","haushaltsaequivalenzeinkommen"), ~(scale(.) %>% as.vector))
# ### Rerange all variables to 0-1
# df_clustered = normalize(df_clustered, method = "range", range = c(0, 1))
#
#
# df_plot <- df_clustered %>% select(ausbildung, geschlecht, alter, cluster)
# ggpairs(df_plot,
#         aes(color = cluster, alpha = 0.3),
#         diag = list(continuous = "barDiag"),
#         lower = list(continuous = "blank")) + theme_bw()
#
#
# # 4,7,11,14,15,16,19,20,21,22,23,24,26,32,36
# df_plot <- df_clustered[c(4,7,11,38)]
# ggpairs(df_plot, aes(color = cluster, alpha = 0.3), diag = list(continuous = "barDiag")) + theme_bw()
#
# df_plot <- df_clustered[c(12,16,17,38)]
# ggpairs(df_plot, aes(color = cluster, alpha = 0.3)) + theme_bw()
#
# df_plot <- df_clustered[c(20,21,22,38)]
# ggpairs(df_plot, aes(color = cluster, alpha = 0.3)) + theme_bw()
#
# df_plot <- df_clustered[c(23,26,32,38)]
# ggpairs(df_plot, aes(color = cluster, alpha = 0.3)) + theme_bw()
#
#
#
# ## Regressionsanalysis per Cluster
# ### Pooled OLS Variableset 1
# df_clustered$cluster <- as.numeric(df_clustered$cluster)
#
# regressions <- list()
# for(i in 1:length(unique(df_clustered$cluster))){
#   df_per_cluster = df_clustered %>% filter(cluster == i)
#   regressions[[i]] = plm(depression ~ arbeit_zeit_nacht, data = df_per_cluster, index = c("id","year"), model="pooling")
# }
#
# stargazer(regressions[[1]], regressions[[2]], regressions[[3]], regressions[[4]], regressions[[5]],
#           title="Pooled OLS VS1", type="text",
#           column.labels=c("Cluster A", "Cluster B", "Cluster C", "Cluster D", "Cluster E"),
#           df=FALSE, digits=4)
#
# ### Pooled OLS Variableset 2
#
# regressions = list()
# for(i in 1:length(unique(df_clustered$cluster))){
#   df_per_cluster = df_clustered %>% filter(cluster == i)
#   regressions[[i]] = plm(depression ~ stress_arbeit + arbeit_zufriedenheit_aufgaben + art_arbeitszeit + arbeit_einbezug_entscheidungen + arbeitskontrolle_personen + arbeit_qualifikation + arbeit_zeit_wochenstunden + arbeit_zeit_wochenende + arbeit_zeit_nacht + taegl_pendeln_min + arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere + arbeit_laerm_schmutz + arbeit_ermuedende_koerperha + einschraenkung_weg_ges_zustand + tage_gesunheits_prob + chronische_krankheit + ausbildung + partnerschaft + tod_person + migrationshintergrund + geschlecht + alter + ch_nationalitaet + haushaltsaequivalenzeinkommen, data = df_per_cluster, index = c("id","year"), model="pooling")
# }
#
# stargazer(regressions[[1]], regressions[[2]], regressions[[3]], regressions[[4]], regressions[[5]],
#           title="Pooled OLS VS1", type="text",
#           column.labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
#           df=FALSE, digits=4)
#
# ### Fixed Effects
# regressions = list()
# for(i in 1:length(unique(df_clustered$cluster))){
#   df_per_cluster = df_clustered %>% filter(cluster == i)
#   regressions[[i]] = plm(depression ~ arbeit_zufriedenheit_aufgaben + art_arbeitszeit + arbeit_einbezug_entscheidungen + arbeitskontrolle_personen + arbeit_qualifikation + arbeit_zeit_wochenstunden + arbeit_zeit_wochenende + arbeit_zeit_nacht + taegl_pendeln_min + arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere + arbeit_laerm_schmutz + arbeit_ermuedende_koerperha, data = df_per_cluster, index = c("id","year"), model="within")
# }
#
# stargazer(regressions[[1]], regressions[[2]], regressions[[3]], regressions[[4]], regressions[[5]],
#           title="Pooled OLS VS1", type="text",
#           column.labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
#           df=FALSE, digits=4)
#
#
#
# # ## Regression Tree
# # Regression Trees with Random Effects for Longitudinal (Panel) Data
#
# # https://cran.r-project.org/web/packages/REEMtree/REEMtree.pdf
#
# # Example
# #
# # https://quantdev.ssri.psu.edu/tutorials/apa-ati-intensive-longitudinal-data-session-t-random-effect-tree-models
# #
# ### Regression tree variableset 1
#
#
# set.seed(1234)
# tree = REEMtree(depression ~  ausbildung + geschlecht + ch_nationalitaet, data = df_clustered_not_scaled %>% drop_na(), random=~1|id)
# fancyRpartPlot(tree(tree),
#                digits = 2,
#                sub="",
#                main="")
#
# ### Regression tree all variables
#
# set.seed(1234)
# tree = REEMtree(depression ~ stress_arbeit + rueckenschmerzen + arbeit_zufriedenheit_aufgaben + art_arbeitszeit +               arbeit_einbezug_entscheidungen + arbeitskontrolle_personen + arbeit_qualifikation + arbeit_zeit_wochenstunden + arbeit_zeit_wochenende + arbeit_zeit_nacht +         taegl_pendeln_min + arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere + arbeit_laerm_schmutz + arbeit_ermuedende_koerperha +    hausarbeit_wochenstunden + beeintraechtigung_arbeit_privat +abschalten_nach_arbeit + einschraenkung_weg_ges_zustand + tage_gesunheits_prob + chronische_krankheit + ausbildung + partnerschaft + tod_person + person_haushalt  + migrationshintergrund + geschlecht + alter + status + ch_nationalitaet + haushaltsaequivalenzeinkommen + kinder_betreuung, data = df_clustered_not_scaled %>% drop_na(), random=~1|id)
# fancyRpartPlot(tree(tree),
#                digits = 2,
#                sub="",
#                main="")
#
# df_clustered_not_scaled %>% ggplot(aes(cluster, einschraenkung_weg_ges_zustand, fill=cluster)) +
#   geom_boxplot()
# df_clustered_not_scaled %>% ggplot(aes(cluster, abschalten_nach_arbeit, fill=cluster)) +
#   geom_boxplot()
#
