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
colnames(df_long) <- c("id","year","depression","ermuedung","stress_arbeit","rueckenschmerzen","arbeit_zufriedenheit_aufgaben","art_arbeitszeit","arbeit_einbezug_entscheidungen","arbeitskontrolle_personen","arbeit_qualifikation","arbeitsstunden_woche","wochenend_arbeit","nachtarbeit","taegl_pendeln_min","arbeit_intensitaet","zufriedenheit_arbeitsatmosphaere","arbeit_laerm_schmutz","arbeit_ermuedende_koerperha","hausarbeit_stunden_woche","beeintraechtigung_arbeit_privat","abschalten_nach_arbeit","einschraenkung_weg_ges_zustand","tage_gesunheits_prob","chronische_krankheit","ausbildung","partnerschaft","tod_person","person_haushalt","migrationshintergrund","geschlecht","alter","status","occupa","ch_nationalitaet","haushaltsaequivalenzeinkommen","kinder_betreuung")

sample <- df_long %>%
  drop_na(alter) %>%
  filter(alter >= 15) %>%
  filter(alter <= 65) %>%
  filter(occupa %in% c(1,2,3,5,6))

sample <- as.data.frame(sapply(sample, as.numeric))


# Clustering --------------------------------------------------------------
df_kml = sample[1:3]

set.seed(1)
for (cluster_number in c(5)) {
  for (redrawing_number in c(1)) {
    kml_cluster_data <- reshape(df_kml, idvar = "id", timevar = "year", direction = "wide")
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




# 1) OLS: L>D --------------------------------------------------------------

df_clustered <- right_join(sample, df_labeled, by = "id")

head(df_clustered[c(1,2,3,38)])


### Pooled OLS Variableset 1
df_clustered$cluster <- as.numeric(df_clustered$cluster)

# Partnerschaft recodieren
df_clustered <- df_clustered %>%
  mutate(partnerschaft = case_when(partnerschaft %in% c(1,2) ~ 1,partnerschaft == 3 ~ 2))
df_clustered$partnerschaft <- factor(df_clustered$partnerschaft, levels = c(1,2), labels = c("Partnerschaft", "Single"))
table(df_clustered$partnerschaft, useNA = "ifany")

# Geschlecht recodieren
df_clustered$geschlecht <- factor(df_clustered$geschlecht, levels = c(1,2), labels = c("Maennlich", "Weiblich"))
table(df_clustered$geschlecht, useNA = "ifany")

# Alter
ggplot(df_clustered, aes(alter, depression)) +
  geom_jitter(alpha = 0.01) +
  geom_smooth()

df_clustered$alter_2 <- df_clustered$alter^2


# Ausbildung recodieren
table(df_clustered$ausbildung, useNA = "ifany")
df_clustered <- df_clustered %>%
  mutate(ausbildung = case_when(ausbildung < 4 ~ 1,
                                ausbildung < 7 ~ 2,
                                ausbildung < 10 ~ 3,
                                ausbildung == 10 ~ 4))
df_clustered$ausbildung <- factor(df_clustered$ausbildung, levels = c(1,2,3,4),
                                  labels = c("Tiefer Bildungsstand", "Sekundarstufe II", "Höhere Berufsbildung", "Hochschule"))
table(df_clustered$ausbildung, useNA = "ifany")

# tod_person recodieren
table(df_clustered$tod_person, useNA = "ifany")
df_clustered$tod_person <- factor(df_clustered$tod_person, levels = c(1,2),
                                  labels = c("Angehoerige Person gestorben", "Keine angehoerige Person gestorben"))
table(df_clustered$tod_person, useNA = "ifany")

# haushaltsaequivalenzeinkommen
hist(log(df_clustered$haushaltsaequivalenzeinkommen))
df_clustered$haushaltsaequivalenzeinkommen <- log(df_clustered$haushaltsaequivalenzeinkommen)

ols1 <- plm(depression ~
      ausbildung + alter + alter_2 + geschlecht + ch_nationalitaet +
      einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
      partnerschaft + tod_person,
    data = df_clustered, index = c("id","year"), model="pooling")


stargazer(ols1,
          title="Pooled OLS L>D", type="text",
          column.labels=c("all"),
          df=FALSE, digits=4)

df_clustered_norm <- normalize(df_clustered, method = "range", range = c(0, 1))

ols1_norm <- plm(depression ~
                   ausbildung + alter + alter_2 + geschlecht + ch_nationalitaet +
                   einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
                   partnerschaft + tod_person,
                 data = df_clustered_norm, index = c("id","year"), model="pooling")


stargazer(ols1_norm,
          title="Pooled OLS VS1", type="text",
          column.labels=c("all"),
          df=FALSE, digits=4)

# ggplot(df_clustered, aes(partnerschaft, depression)) +
#   geom_jitter(alpha = 0.01) +
#   geom_smooth(method = lm)
#
# ggplot(df_clustered_norm) +
#   geom_jitter(aes(x = partnerschaft, y = depression), na.rm = TRUE, alpha = 0.01) +
#   geom_smooth(aes(x = partnerschaft, y = depression), na.rm = TRUE, method = lm)
#


# 2) OLS A+L>D ------------------------------------------------------------

df_ols2 <- df_clustered

# einbezug entscheidungen recodieren
table(df_ols2$arbeit_einbezug_entscheidungen, useNA = "ifany")
df_ols2 <- df_ols2 %>%
  mutate(arbeit_einbezug_entscheidungen = case_when(
    arbeit_einbezug_entscheidungen %in% c(2,3) ~ 1,
    arbeit_einbezug_entscheidungen == 1 ~ 2))
df_ols2$arbeit_einbezug_entscheidungen <- factor(df_ols2$arbeit_einbezug_entscheidungen, levels = c(1,2), labels = c("Kein Einbezug", "Entscheidung"))
table(df_ols2$arbeit_einbezug_entscheidungen, useNA = "ifany")

# arbeit_qualifikation recodieren
table(df_ols2$arbeit_qualifikation, useNA = "ifany")
df_ols2 <- df_ols2 %>%
  mutate(arbeit_qualifikation = case_when(
    arbeit_qualifikation %in% c(1,3,4) ~ 1,
    arbeit_qualifikation == 2 ~ 2))
df_ols2$arbeit_qualifikation <- factor(df_ols2$arbeit_qualifikation, levels = c(1,2), labels = c("Unpassend", "Passend"))
table(df_ols2$arbeit_qualifikation, useNA = "ifany")

# # arbeit_qualifikation recodieren
# table(df_ols2$arbeit_qualifikation, useNA = "ifany")
# df_ols2 <- df_ols2 %>%
#   mutate(arbeit_qualifikation = case_when(
#     arbeit_qualifikation == 2 ~ 1,
#     arbeit_qualifikation == 1 ~ 3,
#     arbeit_qualifikation == 4 ~ 2,
#     arbeit_qualifikation == 3 ~ 4))
# df_ols2$arbeit_qualifikation <- factor(df_ols2$arbeit_qualifikation, levels = c(1, 2, 3, 4), labels = c("Passend", "Nicht", "Unterqualifiziert", "Ueberqualifiziert"))
# table(df_ols2$arbeit_qualifikation, useNA = "ifany")


# Arbeitsstunden pro Woche recodieren
# ggplot(df_ols2, aes(arbeitsstunden_woche, depression)) +
#   geom_jitter(alpha = 0.01) +
#   geom_smooth(method = lm)
#
#
# table(df_ols2$arbeitsstunden_woche, useNA = "ifany")
# df_ols2 <- df_ols2 %>%
#   mutate(arbeitsstunden_woche = case_when(
#     arbeitsstunden_woche < 20 ~ 3,
#     arbeitsstunden_woche < 50 ~ 1,
#     arbeitsstunden_woche >= 50 ~ 2))
# df_ols2$arbeitsstunden_woche <- factor(df_ols2$arbeitsstunden_woche, levels = c(1,2,3), labels = c("Normal", "Workaholic", "Kaum"))
# table(df_ols2$arbeitsstunden_woche, useNA = "ifany")

# Nachtarbeit recodieren
table(df_ols2$nachtarbeit, useNA = "ifany")
df_ols2 <- df_ols2 %>%
  mutate(nachtarbeit = case_when(
    nachtarbeit == 1 ~ 2,
    nachtarbeit == 2 ~ 1))
df_ols2$nachtarbeit <- factor(df_ols2$nachtarbeit, levels = c(1,2), labels = c("Nein", "Ja"))
table(df_ols2$nachtarbeit, useNA = "ifany")

ggplot(df_ols2 %>% drop_na, aes(nachtarbeit, depression)) +
  geom_boxplot() +
  geom_smooth()

stargazer(plm(depression ~
                nachtarbeit,
              data = df_ols2, index = c("id","year"), model="pooling"),
          title="Nachtarbeit", type="text",
          column.labels=c("all"),
          df=FALSE, digits=4)


# Wochenendarbeit recodieren
# table(df_ols2$wochenend_arbeit, useNA = "ifany")
# df_ols2$wochenend_arbeit <- factor(df_ols2$wochenend_arbeit, levels = c(1,2), labels = c("Ja", "Nein"))
# table(df_ols2$wochenend_arbeit, useNA = "ifany")
# -> Entscheid 21.4.; Nicht berücksichtigen, hat keinen Effekt

# Arbeitsablauf/Arbeitintensität
table(df_ols2$arbeit_intensitaet, useNA = "ifany")

# Soziale Beziehungen zu den Kollegen
table(df_ols2$zufriedenheit_arbeitsatmosphaere, useNA = "ifany")


ols2 <- plm(depression ~
              ausbildung + alter + alter_2 + geschlecht + ch_nationalitaet +
              einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
              partnerschaft + tod_person + arbeit_einbezug_entscheidungen +
              arbeit_qualifikation + arbeitsstunden_woche + nachtarbeit +
              arbeit_intensitaet + zufriedenheit_arbeitsatmosphaere,
            data = df_ols2, index = c("id","year"), model="pooling")


stargazer(ols2,
          title="Pooled OLS A+L>D", type="text",
          column.labels=c("all"),
          df=FALSE, digits=4)

ggplot(df_ols2, aes(arbeitsstunden_woche, depression)) +
  geom_jitter(alpha = 0.01) +
  geom_smooth()


# 3) OLS A+L+C>D ----------------------------------------------------------

df_ols3 <- df_ols2

# Hausarbeitsstunden
table(df_ols3$hausarbeit_stunden_woche, useNA = "ifany")

# Kinderbetreuung
table(df_ols3$kinder_betreuung, useNA = "ifany")
df_ols3 <- df_ols3 %>%
  mutate(kinder_betreuung = case_when(
    kinder_betreuung == 0 ~ 1,
    kinder_betreuung %in% c(1,2) ~ 2))
df_ols3$kinder_betreuung <- factor(df_ols3$kinder_betreuung, levels = c(1,2), labels = c("Nein", "Ja"))
df_ols3$kinder_betreuung <- factor(df_ols3$kinder_betreuung, levels = c(0,1,2), labels = c("Keine Kinderbetreuung", "Geteilte Kinderbetreuung", "Alleinerziehend"))
table(df_ols3$kinder_betreuung, useNA = "ifany")

stargazer(plm(depression ~
                kinder_betreuung,
              data = df_ols3, index = c("id","year"), model="within"),
          title="Kinderbetreuung", type="text",
          column.labels=c("all"),
          df=FALSE, digits=4)

# Pflege und Betreuung von Angehörigen
table(df_ols3$pflege_extern, useNA = "ifany")

# Analyse
ols3 <- plm(depression ~
              ausbildung + alter + alter_2 + geschlecht + ch_nationalitaet +
              einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
              partnerschaft + tod_person + arbeit_einbezug_entscheidungen +
              arbeit_qualifikation + arbeitsstunden_woche + nachtarbeit +
              arbeit_intensitaet + zufriedenheit_arbeitsatmosphaere +
              hausarbeit_stunden_woche + kinder_betreuung,
            data = df_ols3, index = c("id","year"), model="pooling")


stargazer(ols3,
          title="Pooled OLS A+L+C>D", type="text",
          column.labels=c("all"),
          df=FALSE, digits=4)



# Descriptive Analysis ----------------------------------------------------

# ## Descriptive analysis of cluster ~ variable
# First, standardize all continous variables.
# Then reranged all variables to 0-1.

### Scale all Variables which are not categorical
df_clustered_not_scaled = df_clustered
df_clustered = df_clustered %>% mutate_at(c('taegl_pendeln_min',"arbeitsstunden_woche","hausarbeit_stunden_woche","tage_gesunheits_prob","person_haushalt","haushaltsaequivalenzeinkommen"), ~(scale(.) %>% as.vector))
### Rerange all variables to 0-1
df_clustered = normalize(df_clustered, method = "range", range = c(0, 1))


df_plot <- df_clustered %>% select(ausbildung, geschlecht, alter, cluster)
ggpairs(df_plot,
        aes(color = cluster, alpha = 0.3),
        diag = list(continuous = "barDiag"),
        lower = list(continuous = "blank")) + theme_bw()


# 4,7,11,14,15,16,19,20,21,22,23,24,26,32,36
df_plot <- df_clustered[c(4,7,11,38)]
ggpairs(df_plot, aes(color = cluster, alpha = 0.3), diag = list(continuous = "barDiag")) + theme_bw()

df_plot <- df_clustered[c(12,16,17,38)]
ggpairs(df_plot, aes(color = cluster, alpha = 0.3)) + theme_bw()

df_plot <- df_clustered[c(20,21,22,38)]
ggpairs(df_plot, aes(color = cluster, alpha = 0.3)) + theme_bw()

df_plot <- df_clustered[c(23,26,32,38)]
ggpairs(df_plot, aes(color = cluster, alpha = 0.3)) + theme_bw()



## Regressionsanalysis per Cluster
### Pooled OLS Variableset 1
df_clustered$cluster <- as.numeric(df_clustered$cluster)

regressions <- list()
for(i in 1:length(unique(df_clustered$cluster))){
  df_per_cluster = df_clustered %>% filter(cluster == i)
  regressions[[i]] = plm(depression ~ nachtarbeit, data = df_per_cluster, index = c("id","year"), model="pooling")
}

stargazer(regressions[[1]], regressions[[2]], regressions[[3]], regressions[[4]], regressions[[5]],
          title="Pooled OLS VS1", type="text",
          column.labels=c("Cluster A", "Cluster B", "Cluster C", "Cluster D", "Cluster E"),
          df=FALSE, digits=4)

### Pooled OLS Variableset 2

regressions = list()
for(i in 1:length(unique(df_clustered$cluster))){
  df_per_cluster = df_clustered %>% filter(cluster == i)
  regressions[[i]] = plm(depression ~ stress_arbeit + arbeit_zufriedenheit_aufgaben + art_arbeitszeit + arbeit_einbezug_entscheidungen + arbeitskontrolle_personen + arbeit_qualifikation + arbeitsstunden_woche + wochenend_arbeit + nachtarbeit + taegl_pendeln_min + arbeit_intensitaet + zufriedenheit_arbeitsatmosphaere + arbeit_laerm_schmutz + arbeit_ermuedende_koerperha + einschraenkung_weg_ges_zustand + tage_gesunheits_prob + chronische_krankheit + ausbildung + partnerschaft + tod_person + migrationshintergrund + geschlecht + alter + ch_nationalitaet + haushaltsaequivalenzeinkommen, data = df_per_cluster, index = c("id","year"), model="pooling")
}

stargazer(regressions[[1]], regressions[[2]], regressions[[3]], regressions[[4]], regressions[[5]],
          title="Pooled OLS VS1", type="text",
          column.labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
          df=FALSE, digits=4)

### Fixed Effects
regressions = list()
for(i in 1:length(unique(df_clustered$cluster))){
  df_per_cluster = df_clustered %>% filter(cluster == i)
  regressions[[i]] = plm(depression ~ arbeit_zufriedenheit_aufgaben + art_arbeitszeit + arbeit_einbezug_entscheidungen + arbeitskontrolle_personen + arbeit_qualifikation + arbeitsstunden_woche + wochenend_arbeit + nachtarbeit + taegl_pendeln_min + arbeit_intensitaet + zufriedenheit_arbeitsatmosphaere + arbeit_laerm_schmutz + arbeit_ermuedende_koerperha, data = df_per_cluster, index = c("id","year"), model="within")
}

stargazer(regressions[[1]], regressions[[2]], regressions[[3]], regressions[[4]], regressions[[5]],
          title="Pooled OLS VS1", type="text",
          column.labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
          df=FALSE, digits=4)



# ## Regression Tree
# Regression Trees with Random Effects for Longitudinal (Panel) Data

# https://cran.r-project.org/web/packages/REEMtree/REEMtree.pdf

# Example
#
# https://quantdev.ssri.psu.edu/tutorials/apa-ati-intensive-longitudinal-data-session-t-random-effect-tree-models
#
### Regression tree variableset 1


set.seed(1234)
tree = REEMtree(depression ~  ausbildung + geschlecht + ch_nationalitaet, data = df_clustered_not_scaled %>% drop_na(), random=~1|id)
fancyRpartPlot(tree(tree),
               digits = 2,
               sub="",
               main="")

### Regression tree all variables

set.seed(1234)
tree = REEMtree(depression ~ stress_arbeit + rueckenschmerzen + arbeit_zufriedenheit_aufgaben + art_arbeitszeit +               arbeit_einbezug_entscheidungen + arbeitskontrolle_personen + arbeit_qualifikation + arbeitsstunden_woche + wochenend_arbeit + nachtarbeit +         taegl_pendeln_min + arbeit_intensitaet + zufriedenheit_arbeitsatmosphaere + arbeit_laerm_schmutz + arbeit_ermuedende_koerperha +    hausarbeit_stunden_woche + beeintraechtigung_arbeit_privat +abschalten_nach_arbeit + einschraenkung_weg_ges_zustand + tage_gesunheits_prob + chronische_krankheit + ausbildung + partnerschaft + tod_person + person_haushalt  + migrationshintergrund + geschlecht + alter + status + ch_nationalitaet + haushaltsaequivalenzeinkommen + kinder_betreuung, data = df_clustered_not_scaled %>% drop_na(), random=~1|id)
fancyRpartPlot(tree(tree),
               digits = 2,
               sub="",
               main="")

df_clustered_not_scaled %>% ggplot(aes(cluster, einschraenkung_weg_ges_zustand, fill=cluster)) +
  geom_boxplot()
df_clustered_not_scaled %>% ggplot(aes(cluster, abschalten_nach_arbeit, fill=cluster)) +
  geom_boxplot()

