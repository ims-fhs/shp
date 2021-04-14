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

for (cluster_number in c(5)) {
  for (redrawing_number in c(5)) {
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
kml_cluster_data <- reshape(df_kml, idvar = "id", timevar = "year", direction = "wide")
cluster <- clusterLongData(as.matrix(kml_cluster_data[c(2:16)]))
Shape_kml <- kml(cluster, nbClusters = cluster_size, nbRedrawing = 5, toPlot='none')

plot(cluster,cluster_size)


### code to add clusters to the original data
id_not_na <- as.numeric(str_extract(cluster@idFewNA, "\\-*\\d+\\.*\\d*"))
unique_id <- unique(df_kml$id)
labels <- as.data.frame(cluster@c5[[5]]@clusters)


real_id_not_na <- list()
for(i in 1:length(id_not_na)) {

  real_id_not_na[[i]] <- unique_id[id_not_na[i]]

}
real_id_not_na <- do.call(rbind.data.frame, real_id_not_na)
df_labeled <- cbind(real_id_not_na, labels)
colnames(df_labeled) <- c("id","cluster")


df_clustered <- right_join(sample, df_labeled, by = "id")

head(df_clustered[c(1,2,3,38)])


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

df_clustered$cluster <- as.numeric(df_clustered$cluster)


## Regressionsanalysis per Cluster
### Pooled OLS Variableset 1

regressions <- list()
for(i in 1:length(unique(df_clustered$cluster))){
  df_per_cluster = df_clustered %>% filter(cluster == i)
  regressions[[i]] = plm(depression ~ ausbildung + geschlecht + ch_nationalitaet, data = df_per_cluster, index = c("id","year"), model="pooling")
}

stargazer(regressions[[1]], regressions[[2]], regressions[[3]], regressions[[4]], regressions[[5]],
          title="Pooled OLS VS1", type="text",
          column.labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
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

