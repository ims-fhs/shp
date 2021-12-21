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

load("data/df_long.RData")

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




# Clustering functions ---------------------------------------------------------

# for now -> we can't call kml within a function due to saving (I guess we need
# global.evn() as parent environment) -> maybe we can find a solution using:
# https://stackoverflow.com/questions/12279076/r-specify-function-environment


extract_kml_clusters <- function(cluster, df_kml, j) {
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
  colnames(df_labeled) <- c("id",paste0("cluster_",j))
  df_kml <- left_join(df_kml, df_labeled, by = "id")

  return(df_kml)
}

do_kml_clustering <- function(df_kml, n = 3, nbRedrawing = 1) {
  for (j in 1:n) {
    # create clustered data an then a ClusterLongData object.
    kml_cluster_data <- pivot_wider(df_kml[1:3], names_from = year, values_from = depression)
    cluster = clusterLongData(as.matrix(kml_cluster_data[c(2:17)]))
    # cluster data
    kml(cluster, nbClusters = 5, nbRedrawing = nbRedrawing, toPlot='none')

    # plot and print
    png(file=paste0(getwd(),"/Scripts_KML/pics_kml/",j,".png"), width = 1000, height = 600)
    plot(cluster,5, toPlot = "traj")
    dev.off()

    df_kml <- extract_kml_clusters(cluster, df_kml, j)
  }
  return(df_kml)
}


# ------------------------------- run functions  -------------------------------
df_kml <- sample[1:3]
set.seed(1)
clustering_10_1 <- do_kml_clustering(df_kml, n = 20, nbRedrawing = 1)


# make changes of clusters visible
df1 <- clustering_10_1 %>%
  select(-year, - depression) %>%
  distinct_all() %>% # take unique rows (since we don't have year and depression anymore)
  pivot_longer(cols = starts_with("cluster"), names_to = "iteration", names_prefix = "cluster_",
               values_to = "cluster")
# changes between clusters by iterations
ggplot(data = df1, aes(x = as.numeric(iteration), y = cluster, group = id)) +
  geom_line()
# Assignment to clusters per ID (for first 1000 Entries)
ggplot(data = df1[1:1000,], aes(x = factor(id), y = 1, fill = cluster)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# -> by looking at the plots we see, that one main problem at the moment is the
# fact, that the name of clusters change between iterations (although the overall
# systematic of clusters look similar 2 clusters can change between iterations).
#
# We must overcome this to further unterdtand the stochastic instabilities of the
# kml-Clustering algorithms !!
☻
